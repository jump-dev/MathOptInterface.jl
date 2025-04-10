# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

mutable struct _CacheModel
    is_binary::Bool
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
            false,
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

function Base.read!(io::IO, model::Model)
    cache = _CacheModel()
    _parse_header(io, cache)
    while !eof(io)
        _parse_section(io, cache)
    end
    model.model = _to_model(cache; use_nlp_block = model.use_nlp_block)
    return
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

_is_valid_number(::Type{Int}, x::UInt8) = UInt8('0') <= x <= UInt8('9')

function _is_valid_number(::Type{Float64}, x::UInt8)
    if _is_valid_number(Int, x)
        return true
    elseif x == UInt8('+') || x == UInt8('-')
        return true
    elseif x == UInt8('.') || x == UInt8('e') || x == UInt8('E')
        return true
    end
    return false
end

function _next_token(::Type{T}, io::IO, cache::Vector{UInt8}) where {T}
    # Skip all spaces
    byte = UInt8(' ')
    while byte == UInt8(' ')
        byte = read(io, UInt8)
    end
    @assert _is_valid_number(T, byte)
    cache[1] = byte
    i = 1
    while _is_valid_number(T, peek(io, UInt8))
        i += 1
        cache[i] = read(io, UInt8)
    end
    return i
end

function _next(::Type{Float64}, io::IO, model::_CacheModel)
    if model.is_binary
        return read(io, Float64)
    end
    nnz = _next_token(Float64, io, model.cache)
    @assert nnz > 0
    return parse(Float64, String(model.cache[1:nnz]))
end

function _next(::Type{Int}, io::IO, model::_CacheModel)
    if model.is_binary
        return convert(Int, read(io, Int32))
    end
    nnz = _next_token(Int, io, model.cache)
    @assert nnz > 0
    y = 0
    mult = 1
    for i in nnz:-1:1
        y += mult * (model.cache[i] - UInt8('0'))
        mult *= 10
    end
    return y
end

function _next(::Type{Cchar}, io::IO, model::_CacheModel)
    if model.is_binary
        return read(io, Cchar)
    end
    byte = UInt8(' ')
    while byte == UInt8(' ')
        byte = read(io, UInt8)
    end
    return Cchar(byte)
end

"""
    _read_til_newline(io::IO, model::_CacheModel)

This function reads until it finds a new line character. This is useful for
skipping comments.
"""
function _read_til_newline(io::IO, model::_CacheModel)
    if model.is_binary
        return
    end
    while read(io, UInt8) != UInt8('\n')
    end
    return
end

_force_expr(expr::Expr) = expr
_force_expr(expr) = Expr(:call, :+, expr)

function _parse_expr(io::IO, model::_CacheModel)
    char = Char(read(io, UInt8))
    if char == 'o'
        opcode = _next(Int, io, model)
        _read_til_newline(io, model)
        arity, op_func = _AMPL_TO_JULIA[opcode]
        op_sym = Symbol(op_func)
        if arity == -1
            arity = _next(Int, io, model)
            _read_til_newline(io, model)
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
            child = _parse_expr(io, model)
            push!(parent.args, child)
        end
        return parent
    elseif char == 'v'
        index = _next(Int, io, model)
        _read_til_newline(io, model)
        return MOI.VariableIndex(index + 1)
    else
        @assert char == 'n'
        ret = _next(Float64, io, model)
        _read_til_newline(io, model)
        return ret
    end
end

function _to_model(data::_CacheModel; use_nlp_block::Bool)
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
    if data.objective != :()
        MOI.set(model, MOI.ObjectiveSense(), data.sense)
    end
    if use_nlp_block
        nlp = MOI.Nonlinear.Model()
        if data.objective != :()
            MOI.Nonlinear.set_objective(nlp, data.objective)
        end
        for (i, expr) in enumerate(data.constraints)
            lb, ub = data.constraint_lower[i], data.constraint_upper[i]
            if lb == ub
                MOI.Nonlinear.add_constraint(nlp, expr, MOI.EqualTo(lb))
            elseif -Inf == lb && ub < Inf
                MOI.Nonlinear.add_constraint(nlp, expr, MOI.LessThan(ub))
            elseif -Inf < lb && ub == Inf
                MOI.Nonlinear.add_constraint(nlp, expr, MOI.GreaterThan(lb))
            else
                MOI.Nonlinear.add_constraint(nlp, expr, MOI.Interval(lb, ub))
            end
        end
        evaluator =
            MOI.Nonlinear.Evaluator(nlp, MOI.Nonlinear.SparseReverseMode(), x)
        block = MOI.NLPBlockData(evaluator)
        MOI.set(model, MOI.NLPBlock(), block)
    else
        if data.objective != :()
            obj = _expr_to_function(data.objective)
            MOI.set(model, MOI.ObjectiveFunction{typeof(obj)}(), obj)
        end
        for (i, expr) in enumerate(data.constraints)
            lb, ub = data.constraint_lower[i], data.constraint_upper[i]
            f = _expr_to_function(expr)
            if lb == ub
                MOI.add_constraint(model, f, MOI.EqualTo(lb))
            elseif -Inf == lb && ub < Inf
                MOI.add_constraint(model, f, MOI.LessThan(ub))
            elseif -Inf < lb && ub == Inf
                MOI.add_constraint(model, f, MOI.GreaterThan(lb))
            else
                MOI.add_constraint(model, f, MOI.Interval(lb, ub))
            end
        end
    end
    return model
end

_expr_to_function(expr) = expr

function _expr_to_function(expr::Expr)
    @assert Meta.isexpr(expr, :call)
    f = _try_scalar_affine_function(expr)
    if f !== nothing
        return convert(MOI.ScalarAffineFunction{Float64}, f)
    end
    return MOI.ScalarNonlinearFunction(
        expr.args[1],
        Any[_expr_to_function(arg) for arg in expr.args[2:end]],
    )
end

_try_scalar_affine_function(x::Float64) = x

_try_scalar_affine_function(x::MOI.VariableIndex) = x

function _try_scalar_affine_function(expr::Expr)
    if expr.args[1] == :+
        args = _try_scalar_affine_function.(expr.args[2:end])
        if !any(isnothing, args)
            return MOI.Utilities.operate(+, Float64, args...)
        end
    elseif expr.args[1] == :*
        args = _try_scalar_affine_function.(expr.args[2:end])
        n_affine_terms = 0
        for arg in args
            n_affine_terms += arg isa MOI.VariableIndex
            n_affine_terms += arg isa MOI.ScalarAffineFunction{Float64}
        end
        if n_affine_terms <= 1
            return MOI.Utilities.operate(*, Float64, args...)
        end
    end
    return nothing
end

function _parse_header(io::IO, model::_CacheModel)
    # Line 1
    byte = read(io, UInt8)
    # Detect the format. We delay setting `model.is_binary` until the end of
    # this function because the header is _not_ in binary format.
    is_binary = false
    if byte == UInt8('b')
        is_binary = true
    elseif byte != UInt8('g')
        error("Unable to parse NL file : unsupported mode $(Char(byte))")
    end
    # L1 has some magic bytes for AMPL internals (to quote David, "The numbers
    # on the first line matter to AMPL; for other uses, it is best simply to
    # supply the ones shown above.")
    _read_til_newline(io, model)
    # Line 2
    # The number of variables
    n_var = _next(Int, io, model)
    _resize_variables(model, n_var)
    # The number of constraints
    _resize_constraints(model, _next(Int, io, model))
    # The number of linear objectives
    @assert 0 <= _next(Int, io, model) <= 1
    # The number of range constraints
    @assert _next(Int, io, model) >= 0
    # The number of equations
    @assert _next(Int, io, model) >= 0
    # The number of logical constraints. This one is optional, so just read til
    # the end of the line.
    # @assert _next(Int, io, model) == 0
    _read_til_newline(io, model)
    # Line 3
    # The number of nonlinear constraints
    @assert _next(Int, io, model) >= 0
    # The number of nonlinear objectives
    @assert 0 <= _next(Int, io, model) <= 1
    _read_til_newline(io, model)
    # Line 4
    # The number of nonlinear network constraints
    @assert _next(Int, io, model) == 0
    # The number of linear network constraints
    @assert _next(Int, io, model) == 0
    _read_til_newline(io, model)
    # Line 5
    # The number of nonlienar variables in constraints
    nlvc = _next(Int, io, model)
    # The number of nonlienar variables in objectives
    nlvo = _next(Int, io, model)
    # The number of nonlienar variables in constraints and objectives (both)
    nlvb = _next(Int, io, model)
    _read_til_newline(io, model)
    # Line 6
    # The number of linear network variables
    @assert _next(Int, io, model) == 0
    # The number of user-defined functions
    @assert _next(Int, io, model) == 0
    # The number of "arith"
    # TODO(odow): I don't know what this is.
    _next(Int, io, model)
    # The "flags" entry. This is mainly used for specifying that we want duals.
    # Ignore when reading.
    _next(Int, io, model)
    _read_til_newline(io, model)
    # Line 7
    # Number of binary variables
    nbv = _next(Int, io, model)
    # Number of integer variables
    niv = _next(Int, io, model)
    # Number of integer variables in nonlinear constraints AND objectives
    nlvbi = _next(Int, io, model)
    # Number of integer variables in nonlinear constraints
    nlvci = _next(Int, io, model)
    # Number of integer variables in nonlinear objectives
    nlvoi = _next(Int, io, model)
    _read_til_newline(io, model)
    # Line 8
    # Read the number of nonzeros in Jacobian and gradient, but don't do
    # anything with that information.
    @assert _next(Int, io, model) >= 0
    @assert _next(Int, io, model) >= 0
    _read_til_newline(io, model)
    # Line 9
    # We don't support reading variable and constraint names, so just ignore
    # them
    _read_til_newline(io, model)
    # Line 10
    # We don't support reading common subexpressions
    for _ in 1:5
        if _next(Int, io, model) > 0
            error("Unable to parse NL file : we don't support common exprs")
        end
    end
    _read_til_newline(io, model)
    # ==========================================================================
    # Deal with the integrality of variables. This is quite complicated, so go
    # read the README in this folder.
    offsets = [
        nlvb - nlvbi,
        nlvbi,
        nlvc - (nlvb + nlvci),
        nlvci,
        max(0, nlvo - nlvc - nlvoi),
        nlvoi,
        n_var - (max(nlvc, nlvo) + nbv + niv),
        nbv,
        niv,
    ]
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
    # Delay setting is_binary until the end of the header section
    model.is_binary = is_binary
    return
end

function _parse_section(io::IO, model::_CacheModel)
    char = Char(read(io, UInt8))
    _parse_section(io, Val(char), model)
    return
end

function _parse_section(::IO, ::Val{T}, ::_CacheModel) where {T}
    return error("Unable to parse NL file: unhandled header $T")
end

function _parse_section(::IO, ::Val{'F'}, ::_CacheModel)
    return error(
        "Unable to parse NL file: imported function descriptions ('F' " *
        "sections) are not yet supported. To request support, please open an " *
        "issue at https://github.com/jump-dev/MathOptInterface.jl with a " *
        "reproducible example.",
    )
end

function _parse_section(io::IO, ::Val{'S'}, model::_CacheModel)
    k = _next(Int, io, model)
    n = _next(Int, io, model)
    suffix_name = if model.is_binary
        len = _next(Int, io, model)
        String(read(io, len))
    else
        strip(readline(io))
    end
    @warn("Skipping suffix: `S$k $n $suffix_name`")
    # The “4” bit of k indicates whether the suﬃx is real (that is, double)
    # valued or integer valued: (k&4) != 0 --> real valued.
    T = ifelse(k & 4 != 0, Float64, Int)
    for _ in 1:n
        _ = _next(Int, io, model)
        _ = _next(T, io, model)
        _read_til_newline(io, model)
    end
    return
end

function _parse_section(::IO, ::Val{'V'}, ::_CacheModel)
    return error(
        "Unable to parse NL file: defined variable definitions ('V' sections)" *
        " are not yet supported. To request support, please open an issue at " *
        "https://github.com/jump-dev/MathOptInterface.jl with a reproducible " *
        "example.",
    )
end

function _parse_section(::IO, ::Val{'L'}, ::_CacheModel)
    return error(
        "Unable to parse NL file: logical constraints ('L' sections) are not " *
        "yet supported. To request support, please open an issue at " *
        "https://github.com/jump-dev/MathOptInterface.jl with a reproducible " *
        "example.",
    )
end

function _parse_section(io::IO, ::Val{'C'}, model::_CacheModel)
    index = _next(Int, io, model) + 1
    _read_til_newline(io, model)
    expr = _force_expr(_parse_expr(io, model))
    current = model.constraints[index]
    if current == :()
        model.constraints[index] = expr
    else
        model.constraints[index] = Expr(:call, :+, current, expr)
    end
    return
end

function _parse_section(io::IO, ::Val{'O'}, model::_CacheModel)
    @assert _next(Int, io, model) == 0
    sense = _next(Int, io, model)
    if sense == 1
        model.sense = MOI.MAX_SENSE
    else
        @assert sense == 0
        model.sense = MOI.MIN_SENSE
    end
    _read_til_newline(io, model)
    expr = _force_expr(_parse_expr(io, model))
    if model.objective == :()
        model.objective = expr
    else
        model.objective = Expr(:call, :+, model.objective, expr)
    end
    return
end

function _parse_section(io::IO, ::Val{'x'}, model::_CacheModel)
    index = _next(Int, io, model)
    _read_til_newline(io, model)
    for _ in 1:index
        xi = _next(Int, io, model) + 1
        v = _next(Float64, io, model)
        model.variable_primal[xi] = v
        _read_til_newline(io, model)
    end
    return
end

# TODO(odow): we don't read in dual starts.
function _parse_section(io::IO, ::Val{'d'}, model::_CacheModel)
    n = _next(Int, io, model)
    _read_til_newline(io, model)
    for _ in 1:n
        _ = _next(Int, io, model)
        _ = _next(Float64, io, model)
        _read_til_newline(io, model)
    end
    return
end

function _parse_section(io::IO, ::Val{'r'}, model::_CacheModel)
    _read_til_newline(io, model)
    for i in 1:length(model.constraint_lower)
        type = _next(Cchar, io, model)
        if type == Cchar('0')
            model.constraint_lower[i] = _next(Float64, io, model)
            model.constraint_upper[i] = _next(Float64, io, model)
        elseif type == Cchar('1')
            model.constraint_upper[i] = _next(Float64, io, model)
        elseif type == Cchar('2')
            model.constraint_lower[i] = _next(Float64, io, model)
        elseif type == Cchar('3')
            # Free constraint
        else
            @assert type == Cchar('4')
            value = _next(Float64, io, model)
            model.constraint_lower[i] = value
            model.constraint_upper[i] = value
        end
        _read_til_newline(io, model)
    end
    return
end

function _parse_section(io::IO, ::Val{'b'}, model::_CacheModel)
    _read_til_newline(io, model)
    for i in 1:length(model.variable_lower)
        type = _next(Cchar, io, model)
        if type == Cchar('0')
            model.variable_lower[i] = _next(Float64, io, model)
            model.variable_upper[i] = _next(Float64, io, model)
        elseif type == Cchar('1')
            model.variable_upper[i] = _next(Float64, io, model)
        elseif type == Cchar('2')
            model.variable_lower[i] = _next(Float64, io, model)
        elseif type == Cchar('3')
            # Free variable
        else
            @assert type == Cchar('4')
            value = _next(Float64, io, model)
            model.variable_lower[i] = value
            model.variable_upper[i] = value
        end
        _read_til_newline(io, model)
    end
    return
end

# We ignore jacobian counts for now
function _parse_section(io::IO, ::Val{'k'}, model::_CacheModel)
    n = _next(Int, io, model)
    _read_til_newline(io, model)
    for _ in 1:n
        _ = _next(Int, io, model)
        _read_til_newline(io, model)
    end
    return
end

function _parse_section(io::IO, ::Val{'J'}, model::_CacheModel)
    i = _next(Int, io, model) + 1
    nnz = _next(Int, io, model)
    _read_til_newline(io, model)
    expr = Expr(:call, :+)
    for _ in 1:nnz
        x = _next(Int, io, model)
        c = _next(Float64, io, model)
        if !iszero(c)
            push!(expr.args, Expr(:call, :*, c, MOI.VariableIndex(x + 1)))
        end
        _read_til_newline(io, model)
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

function _parse_section(io::IO, ::Val{'G'}, model::_CacheModel)
    i = _next(Int, io, model) + 1
    nnz = _next(Int, io, model)
    _read_til_newline(io, model)
    expr = Expr(:call, :+)
    for _ in 1:nnz
        x = _next(Int, io, model)
        c = _next(Float64, io, model)
        if !iszero(c)
            push!(expr.args, Expr(:call, :*, c, MOI.VariableIndex(x + 1)))
        end
        _read_til_newline(io, model)
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
