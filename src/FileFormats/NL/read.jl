# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

function _next_token(io::IO, cache::Vector{UInt8})
    byte = UInt8(' ')
    while byte in (UInt8(' '), UInt8('\n'), UInt8('\r'))
        byte = read(io, UInt8)
    end
    i = 0
    while byte != UInt8(' ') && byte != UInt8('\n')
        i += 1
        cache[i] = byte
        byte = read(io, UInt8)
    end
    return i
end

function _next(::Type{T}, io::IO, cache::Vector{UInt8}) where {T}
    nnz = _next_token(io, cache)
    return parse(T, String(cache[1:nnz]))
end

function _next(::Type{Int}, io::IO, cache::Vector{UInt8})
    nnz = _next_token(io, cache)
    y = 0
    mult = 1
    for i in nnz:-1:1
        y += mult * (cache[i] - UInt8('0'))
        mult *= 10
    end
    return y
end

function _get_trailing_int(line)
    y = 0
    mult = 1
    for i in length(line):-1:2
        y += mult * Int(line[i] - '0')
        mult *= 10
    end
    return y
end

_force_expr(expr::Expr) = expr
_force_expr(expr) = Expr(:call, :+, expr)

function _parse_expr(io::IO)
    line = readline(io)
    if line[1] == 'o'
        opcode = _get_trailing_int(line)
        arity, op_func = _AMPL_TO_JULIA[opcode]
        op_sym = Symbol(op_func)
        if arity == -1
            arity = parse(Int, readline(io))
            if op_sym == :sum
                op_sym = :+
            elseif op_sym == :minimum
                op_sym = :min
            elseif op_sym == :maximum
                op_sym = :max
            end
        end
        parent = Expr(:call, op_sym)
        for _ in 1:arity
            child = _parse_expr(io)
            push!(parent.args, child)
        end
        return parent
    elseif line[1] == 'v'
        index = _get_trailing_int(line)
        return MOI.VariableIndex(index + 1)
    else
        @assert line[1] == 'n'
        return parse(Float64, line[2:end])
    end
end

mutable struct _CacheModel
    cache::Vector{UInt8}
    variable_type::Vector{_VariableType}
    variable_primal::Vector{Float64}
    variable_lower::Vector{Float64}
    variable_upper::Vector{Float64}
    constraints::Vector{Expr}
    constraint_lower::Vector{Float64}
    constraint_upper::Vector{Float64}
    objective::Expr
    sense::MOI.OptimizationSense
    function _CacheModel()
        return new(
            zeros(UInt8, 64),
            _VariableType[],
            Float64[],
            Float64[],
            Float64[],
            Expr[],
            Float64[],
            Float64[],
            :(),
            MOI.FEASIBILITY_SENSE,
        )
    end
end

function _resize_variables(model::_CacheModel, n::Int)
    resize!(model.variable_type, n)
    fill!(model.variable_type, _CONTINUOUS)
    resize!(model.variable_lower, n)
    fill!(model.variable_lower, -Inf)
    resize!(model.variable_upper, n)
    fill!(model.variable_upper, Inf)
    resize!(model.variable_primal, n)
    fill!(model.variable_primal, 0.0)
    return
end

function _resize_constraints(model::_CacheModel, n::Int)
    resize!(model.constraint_lower, n)
    fill!(model.constraint_lower, -Inf)
    resize!(model.constraint_upper, n)
    fill!(model.constraint_upper, Inf)
    resize!(model.constraints, n)
    for i in 1:n
        model.constraints[i] = :()
    end
    return
end

function to_model(data::_CacheModel)
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x = MOI.add_variables(model, length(data.variable_primal))
    for (xi, lb, ub) in zip(x, data.variable_lower, data.variable_upper)
        if lb > -Inf
            MOI.add_constraint(model, xi, MOI.GreaterThan(lb))
        end
        if ub < Inf
            MOI.add_constraint(model, xi, MOI.LessThan(ub))
        end
    end
    for (xi, type) in zip(x, data.variable_type)
        if type == _INTEGER
            MOI.add_constraint(model, xi, MOI.Integer())
        elseif type == _BINARY
            MOI.add_constraint(model, xi, MOI.ZeroOne())
        end
    end
    MOI.set.(model, MOI.VariablePrimalStart(), x, data.variable_primal)
    MOI.set(model, MOI.ObjectiveSense(), data.sense)
    nlp = MOI.Nonlinear.Model()
    MOI.Nonlinear.set_objective(nlp, data.objective)
    for (i, expr) in enumerate(data.constraints)
        lb, ub = data.constraint_lower[i], data.constraint_upper[i]
        if lb == ub
            MOI.Nonlinear.add_constraint(nlp, expr, MOI.EqualTo(lb))
        elseif -Inf < lb < ub < Inf
            MOI.Nonlinear.add_constraint(nlp, expr, MOI.Interval(lb, ub))
        elseif -Inf == lb && ub < Inf
            MOI.Nonlinear.add_constraint(nlp, expr, MOI.LessThan(ub))
        else
            @assert -Inf < lb && ub == Inf
            MOI.Nonlinear.add_constraint(nlp, expr, MOI.GreaterThan(lb))
        end
    end
    evaluator =
        MOI.Nonlinear.Evaluator(nlp, MOI.Nonlinear.SparseReverseMode(), x)
    block = MOI.NLPBlockData(evaluator)
    MOI.set(model, MOI.NLPBlock(), block)
    return model
end

function Base.read(io::IO, ::Type{Model})
    model = _CacheModel()
    # ==========================================================================
    # Header
    # Line 1
    # This has some magic bytes for AMPL internals. We don't support the binary
    # format.
    @assert readline(io) == "g3 1 1 0"
    # Line 2
    line = items = parse.(Int, split(readline(io), ' '; keepempty = false))
    @assert length(items) == 6
    num_variables = items[1]
    _resize_variables(model, num_variables)
    _resize_constraints(model, items[2])
    @assert 0 <= items[3] <= 1  # num objectives
    @assert items[4] >= 0
    @assert items[5] >= 0
    @assert items[6] == 0  # logical constraints
    # Line 3
    items = parse.(Int, split(readline(io), ' '; keepempty = false))
    @assert items[1] >= 0
    @assert 0 <= items[2] <= 1
    # Line 4
    _ = readline(io)
    # Line 5
    nlvc = _next(Int, io, model.cache)
    nlvo = _next(Int, io, model.cache)
    nlvb = _next(Int, io, model.cache)
    # Line 6
    _ = readline(io)
    # Line 7
    nbv = _next(Int, io, model.cache)
    niv = _next(Int, io, model.cache)
    nl_both = _next(Int, io, model.cache)
    nl_cons = _next(Int, io, model.cache)
    nl_obj = _next(Int, io, model.cache)
    # Line 8
    _ = readline(io)
    # Line 9
    _ = readline(io)
    # Line 10
    _ = readline(io)
    # ==========================================================================
    # Deal with the integrality of variables
    offsets = [
        nlvb - nl_both,
        nl_both,
        nlvc - nl_cons,
        nl_cons,
        nlvo - nl_obj,
        nlvo,
        num_variables - (nbv + niv) - (nlvc + nlvo - nlvb),
        nbv,
        niv,
    ]
    if nlvo > nlvc
        offsets[3], offsets[5] = offsets[5], offsets[3]
        offsets[4], offsets[6] = offsets[6], offsets[4]
    end
    types = [
        _CONTINUOUS,
        _INTEGER,
        _CONTINUOUS,
        _INTEGER,
        _CONTINUOUS,
        _INTEGER,
        _CONTINUOUS,
        _BINARY,
        _INTEGER,
    ]
    offset = 0
    for i in 1:9
        for _ in 1:offsets[i]
            offset += 1
            model.variable_type[offset] = types[i]
        end
    end
    # ==========================================================================
    while !eof(io)
        line = readline(io)
        _parse_section(io, Val(line[1]), model, line)
    end
    return to_model(model)
end

function _parse_section(::IO, ::Val, ::_CacheModel, line::String)
    return error("Unable to parse NL file: unhandled header:\n\n$line")
end

function _parse_section(io::IO, ::Val{'C'}, model::_CacheModel, line::String)
    index = _get_trailing_int(line) + 1
    model.constraints[index] = _force_expr(_parse_expr(io))
    return
end

function _parse_section(io::IO, ::Val{'O'}, model::_CacheModel, line::String)
    if line[end] == '1'
        model.sense = MOI.MAX_SENSE
    else
        @assert line[end] == '0'
        model.sense = MOI.MIN_SENSE
    end
    model.objective = _force_expr(_parse_expr(io))
    return
end

function _parse_section(io::IO, ::Val{'x'}, model::_CacheModel, line::String)
    index = _get_trailing_int(line)
    for _ in 1:index
        xi = _next(Int, io, model.cache) + 1
        v = _next(Float64, io, model.cache)
        model.variable_primal[xi] = v
    end
    return
end

function _parse_section(io::IO, ::Val{'r'}, model::_CacheModel, ::String)
    for i in 1:length(model.constraint_lower)
        type = _next(Int, io, model.cache)
        if type == 0
            model.constraint_lower[i] = _next(Float64, io, model.cache)
            model.constraint_upper[i] = _next(Float64, io, model.cache)
        elseif type == 1
            model.constraint_upper[i] = _next(Float64, io, model.cache)
        elseif type == 2
            model.constraint_lower[i] = _next(Float64, io, model.cache)
        elseif type == 3
            # Free constraint
        else
            @assert type == 4
            value = _next(Float64, io, model.cache)
            model.constraint_lower[i] = value
            model.constraint_upper[i] = value
        end
    end
    return
end

function _parse_section(io::IO, ::Val{'b'}, model::_CacheModel, ::String)
    for i in 1:length(model.variable_lower)
        type = _next(Int, io, model.cache)
        if type == 0
            model.variable_lower[i] = _next(Float64, io, model.cache)
            model.variable_upper[i] = _next(Float64, io, model.cache)
        elseif type == 1
            model.variable_upper[i] = _next(Float64, io, model.cache)
        elseif type == 2
            model.variable_lower[i] = _next(Float64, io, model.cache)
        elseif type == 3
            # Free variable
        else
            @assert type == 4
            value = _next(Float64, io, model.cache)
            model.variable_lower[i] = value
            model.variable_upper[i] = value
        end
    end
    return
end

function _parse_section(io::IO, ::Val{'k'}, model::_CacheModel, ::String)
    # We ignore jacobian counts for now
    for _ in 2:length(model.variable_lower)
        _ = readline(io)
    end
    return
end

function _parse_section(io::IO, ::Val{'J'}, model::_CacheModel, line::String)
    index = findfirst(isequal(' '), line)
    i = parse(Int, line[2:index]) + 1
    expr = Expr(:call, :+)
    for _ in 1:parse(Int, line[index+1:end])
        x = _next(Int, io, model.cache)
        c = _next(Float64, io, model.cache)
        if !iszero(c)
            push!(expr.args, Expr(:call, :*, c, MOI.VariableIndex(x + 1)))
        end
    end
    if length(expr.args) == 1
        # Linear part is just zeros
    elseif model.constraints[i] == :()
        model.constraints[i] = expr
    else
        model.constraints[i] = Expr(:call, :+, expr, model.constraints[i])
    end
    return
end

function _parse_section(io::IO, ::Val{'G'}, model::_CacheModel, line::String)
    index = findfirst(isequal(' '), line)
    expr = Expr(:call, :+)
    for _ in 1:parse(Int, line[index+1:end])
        x = _next(Int, io, model.cache)
        c = _next(Float64, io, model.cache)
        if !iszero(c)
            push!(expr.args, Expr(:call, :*, c, MOI.VariableIndex(x + 1)))
        end
    end
    if length(expr.args) == 1
        # Linear part is just zeros
    elseif model.objective == :()
        model.objective = expr
    else
        model.objective = Expr(:call, :+, expr, model.objective)
    end
    return
end
