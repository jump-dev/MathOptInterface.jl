# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Julia 1.6 removes Grisu from Base. Previously, we went
#   _print_shortest(io, x) = Base.Grisu.print_shortest(io, x)
# To avoid adding Grisu as a dependency, use the following printing heuristic.
# TODO(odow): consider printing 1.0 as 1.0 instead of 1, that is, without the
# rounding branch.
function _print_shortest(io::IO, x::Float64)
    if x == -Inf
        print(io, "-inf")
    elseif x == Inf
        print(io, "inf")
    elseif isinteger(x) && (typemin(Int) <= x <= typemax(Int))
        print(io, round(Int, x))
    else
        print(io, x)
    end
    return
end

_print_shortest(io::IO, x::Integer) = print(io, x)

const _START_REG = r"^([\.0-9eE])"
const _NAME_REG = r"([^a-zA-Z0-9\!\"\#\$\%\&\(\)\/\,\.\;\?\@\_\`\'\{\}\|\~])"

function _write_function(
    io::IO,
    ::Model,
    func::MOI.VariableIndex,
    variable_names::Dict{MOI.VariableIndex,String};
    kwargs...,
)
    print(io, variable_names[func])
    return
end

function _write_function(
    io::IO,
    ::Model,
    func::MOI.ScalarAffineFunction,
    variable_names::Dict{MOI.VariableIndex,String};
    print_one::Bool = true,
    kwargs...,
)
    is_first_item = true
    if !iszero(func.constant)
        _print_shortest(io, func.constant)
        is_first_item = false
    end
    for term in func.terms
        if !iszero(term.coefficient)
            if is_first_item
                if print_one || !isone(term.coefficient)
                    _print_shortest(io, term.coefficient)
                end
                is_first_item = false
            else
                print(io, term.coefficient < 0 ? " - " : " + ")
                _print_shortest(io, abs(term.coefficient))
            end
            print(io, " ", variable_names[term.variable])
        end
    end
    return
end

function _write_function(
    io::IO,
    ::Model,
    func::MOI.ScalarQuadraticFunction,
    variable_names::Dict{MOI.VariableIndex,String};
    print_half::Bool = true,
    kwargs...,
)
    is_first_item = true
    if !iszero(func.constant)
        _print_shortest(io, func.constant)
        is_first_item = false
    end
    for term in func.affine_terms
        if !iszero(term.coefficient)
            if is_first_item
                _print_shortest(io, term.coefficient)
                is_first_item = false
            else
                print(io, term.coefficient < 0 ? " - " : " + ")
                _print_shortest(io, abs(term.coefficient))
            end
            print(io, " ", variable_names[term.variable])
        end
    end
    if length(func.quadratic_terms) > 0
        if is_first_item
            print(io, "[ ")
        else
            print(io, " + [ ")
        end
        is_first_item = true
        for term in func.quadratic_terms
            coefficient = term.coefficient
            if !print_half && term.variable_1 == term.variable_2
                coefficient /= 2
            end
            if print_half && term.variable_1 != term.variable_2
                coefficient *= 2
            end
            if is_first_item
                _print_shortest(io, coefficient)
                is_first_item = false
            else
                print(io, coefficient < 0 ? " - " : " + ")
                _print_shortest(io, abs(coefficient))
            end
            print(io, " ", variable_names[term.variable_1])
            if term.variable_1 == term.variable_2
                print(io, " ^ 2")
            else
                print(io, " * ", variable_names[term.variable_2])
            end
        end
        if print_half
            print(io, " ]/2")
        else
            print(io, " ]")
        end
    end
    return
end

function _write_constraint_suffix(io::IO, set::MOI.LessThan)
    print(io, " <= ")
    _print_shortest(io, set.upper)
    println(io)
    return
end

function _write_constraint_suffix(io::IO, set::MOI.GreaterThan)
    print(io, " >= ")
    _print_shortest(io, set.lower)
    println(io)
    return
end

function _write_constraint_suffix(io::IO, set::MOI.EqualTo)
    print(io, " = ")
    _print_shortest(io, set.value)
    println(io)
    return
end

function _write_constraint_suffix(io::IO, set::MOI.Interval)
    print(io, " <= ")
    _print_shortest(io, set.upper)
    println(io)
    return
end

function _write_constraint_prefix(io::IO, set::MOI.Interval)
    _print_shortest(io, set.lower)
    print(io, " <= ")
    return
end

_write_constraint_prefix(::IO, ::Any) = nothing

function _write_constraint(
    io::IO,
    model::Model,
    index::MOI.ConstraintIndex,
    variable_names::Dict{MOI.VariableIndex,String};
    write_name::Bool = true,
)
    func = MOI.get(model, MOI.ConstraintFunction(), index)
    set = MOI.get(model, MOI.ConstraintSet(), index)
    if write_name
        print(io, MOI.get(model, MOI.ConstraintName(), index), ": ")
    end
    _write_constraint_prefix(io, set)
    _write_function(io, model, func, variable_names; print_half = false)
    _write_constraint_suffix(io, set)
    return
end

function _write_sense(io::IO, model::Model)
    if MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
        println(io, "maximize")
    else
        println(io, "minimize")
    end
    return
end

function _write_objective(
    io::IO,
    model::Model,
    variable_names::Dict{MOI.VariableIndex,String},
)
    print(io, "obj: ")
    F = MOI.get(model, MOI.ObjectiveFunctionType())
    f = MOI.get(model, MOI.ObjectiveFunction{F}())
    _write_function(io, model, f, variable_names)
    println(io)
    return
end

function _write_integrality(
    io::IO,
    model::Model,
    key::String,
    ::Type{S},
    variable_names::Dict{MOI.VariableIndex,String},
) where {S}
    indices = MOI.get(model, MOI.ListOfConstraintIndices{MOI.VariableIndex,S}())
    if length(indices) == 0
        return
    end
    println(io, key)
    for index in indices
        f = MOI.get(model, MOI.ConstraintFunction(), index)
        _write_function(io, model, f, variable_names)
        println(io)
    end
    return
end

function _write_constraints(io, model::Model{T}, S, variable_names) where {T}
    F = MOI.ScalarAffineFunction{T}
    for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        _write_constraint(io, model, index, variable_names; write_name = true)
    end
    F = MOI.ScalarQuadraticFunction{T}
    for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        _write_constraint(io, model, index, variable_names; write_name = true)
    end
    return
end

function _write_sos_constraints(io, model::Model{T}, variable_names) where {T}
    F = MOI.VectorOfVariables
    sos1_indices = MOI.get(model, MOI.ListOfConstraintIndices{F,MOI.SOS1{T}}())
    sos2_indices = MOI.get(model, MOI.ListOfConstraintIndices{F,MOI.SOS2{T}}())
    if length(sos1_indices) + length(sos2_indices) == 0
        return
    end
    println(io, "SOS")
    for index in sos1_indices
        _write_constraint(io, model, index, variable_names)
    end
    for index in sos2_indices
        _write_constraint(io, model, index, variable_names)
    end
    return
end

_to_string(::Type{<:MOI.SOS1}) = "S1::"
_to_string(::Type{<:MOI.SOS2}) = "S2::"

function _write_constraint(
    io::IO,
    model::Model,
    index::MOI.ConstraintIndex{MOI.VectorOfVariables,S},
    variable_names::Dict{MOI.VariableIndex,String},
) where {S<:Union{MOI.SOS1,MOI.SOS2}}
    f = MOI.get(model, MOI.ConstraintFunction(), index)
    s = MOI.get(model, MOI.ConstraintSet(), index)
    name = MOI.get(model, MOI.ConstraintName(), index)
    if name !== nothing && !isempty(name)
        print(io, name, ": ")
    end
    print(io, _to_string(S))
    for (w, x) in zip(s.weights, f.variables)
        print(io, " ", variable_names[x], ":", w)
    end
    println(io)
    return
end

function _write_indicator_constraints(
    io,
    model::Model{T},
    ::Type{S},
    variable_names,
) where {T,S}
    F = MOI.VectorAffineFunction{T}
    for A in (MOI.ACTIVATE_ON_ONE, MOI.ACTIVATE_ON_ZERO)
        Set = MOI.Indicator{A,S}
        for index in MOI.get(model, MOI.ListOfConstraintIndices{F,Set}())
            _write_constraint(
                io,
                model,
                index,
                variable_names;
                write_name = true,
            )
        end
    end
    F = MOI.VectorOfVariables
    for A in (MOI.ACTIVATE_ON_ONE, MOI.ACTIVATE_ON_ZERO)
        Set = MOI.Indicator{A,S}
        for index in MOI.get(model, MOI.ListOfConstraintIndices{F,Set}())
            _write_constraint(
                io,
                model,
                index,
                variable_names;
                write_name = true,
            )
        end
    end
    return
end

function _write_constraint(
    io::IO,
    model::Model{T},
    index::MOI.ConstraintIndex{F,MOI.Indicator{A,S}},
    variable_names::Dict{MOI.VariableIndex,String};
    write_name::Bool = true,
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},A,S}
    func = MOI.get(model, MOI.ConstraintFunction(), index)
    set = MOI.get(model, MOI.ConstraintSet(), index)
    if write_name
        print(io, MOI.get(model, MOI.ConstraintName(), index), ": ")
    end
    z, f = MOI.Utilities.scalarize(func)
    flag = A == MOI.ACTIVATE_ON_ONE ? 1 : 0
    _write_function(io, model, z, variable_names; print_one = false)
    print(io, " = ", flag, " -> ")
    _write_function(io, model, f, variable_names)
    _write_constraint_suffix(io, set.set)
    return
end

"""
    Base.write(io::IO, model::FileFormats.LP.Model)

Write `model` to `io` in the LP file format.
"""
function Base.write(io::IO, model::Model{T}) where {T}
    options = get_options(model)
    FileFormats.create_unique_names(
        model,
        warn = options.warn,
        replacements = [
            s -> match(_START_REG, s) !== nothing ? "_" * s : s,
            s -> replace(s, _NAME_REG => "_"),
            s -> s[1:min(length(s), options.maximum_length)],
        ],
    )
    variable_names = Dict{MOI.VariableIndex,String}(
        index => MOI.get(model, MOI.VariableName(), index) for
        index in MOI.get(model, MOI.ListOfVariableIndices())
    )
    free_variables = Set(keys(variable_names))
    _write_sense(io, model)
    _write_objective(io, model, variable_names)
    println(io, "subject to")
    _write_constraints(io, model, MOI.LessThan{T}, variable_names)
    _write_indicator_constraints(io, model, MOI.LessThan{T}, variable_names)
    _write_constraints(io, model, MOI.GreaterThan{T}, variable_names)
    _write_indicator_constraints(io, model, MOI.GreaterThan{T}, variable_names)
    _write_constraints(io, model, MOI.EqualTo{T}, variable_names)
    _write_indicator_constraints(io, model, MOI.EqualTo{T}, variable_names)
    _write_constraints(io, model, MOI.Interval{T}, variable_names)
    _write_indicator_constraints(io, model, MOI.Interval{T}, variable_names)
    println(io, "Bounds")
    CI = MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}
    for x in MOI.get(model, MOI.ListOfVariableIndices())
        lb, ub = MOI.Utilities.get_bounds(model, T, x)
        if lb == typemin(T) && ub == typemax(T)
            if MOI.is_valid(model, CI(x.value))
                # If a variable is binary, it should not be listed as `free` in
                # the bounds section.
                continue
            end
            print(io, variable_names[x], " free")
        elseif lb == ub
            print(io, variable_names[x], " = ")
            _print_shortest(io, lb)
        elseif lb == typemin(T)
            print(io, "-infinity <= ", variable_names[x], " <= ")
            _print_shortest(io, ub)
        elseif ub == typemax(T)
            print(io, variable_names[x], " >= ")
            _print_shortest(io, lb)
        else
            _print_shortest(io, lb)
            print(io, " <= ", variable_names[x], " <= ")
            _print_shortest(io, ub)
        end
        println(io)
    end
    _write_integrality(io, model, "General", MOI.Integer, variable_names)
    _write_integrality(io, model, "Binary", MOI.ZeroOne, variable_names)
    _write_sos_constraints(io, model, variable_names)
    println(io, "End")
    return
end
