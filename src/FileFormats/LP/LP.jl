# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module LP

import ..FileFormats
import MathOptInterface as MOI

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

const _ILT1{T} = MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.LessThan{T}}
const _IGT1{T} = MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.GreaterThan{T}}
const _IET1{T} = MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.EqualTo{T}}
const _ILT0{T} = MOI.Indicator{MOI.ACTIVATE_ON_ZERO,MOI.LessThan{T}}
const _IGT0{T} = MOI.Indicator{MOI.ACTIVATE_ON_ZERO,MOI.GreaterThan{T}}
const _IET0{T} = MOI.Indicator{MOI.ACTIVATE_ON_ZERO,MOI.EqualTo{T}}

MOI.Utilities.@model(
    Model,
    (MOI.ZeroOne, MOI.Integer),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval),
    (),
    (MOI.SOS1, MOI.SOS2, _ILT1, _IET1, _IGT1, _ILT0, _IGT0, _IET0),
    (),
    (MOI.ScalarQuadraticFunction, MOI.ScalarAffineFunction),
    (MOI.VectorOfVariables,),
    (MOI.VectorAffineFunction,)
)

function MOI.supports_constraint(
    ::Model,
    ::Type{MOI.VariableIndex},
    ::Type{<:Union{MOI.Parameter,MOI.Semicontinuous,MOI.Semiinteger}},
)
    return false
end

function MOI.supports_constraint(
    ::Model{T},
    ::Type{MOI.VectorAffineFunction{T}},
    ::Type{MOI.SOS1{T}},
) where {T}
    return false
end

function MOI.supports_constraint(
    ::Model{T},
    ::Type{MOI.VectorAffineFunction{T}},
    ::Type{MOI.SOS2{T}},
) where {T}
    return false
end

struct Options
    maximum_length::Int
    warn::Bool
end

function get_options(m::Model)
    default_options = Options(255, false)
    return get(m.ext, :LP_OPTIONS, default_options)
end

"""
    Model(;
        maximum_length::Int = 255,
        warn::Bool = false,
        coefficient_type::Type{T} = Float64,
    ) where {T}

Create an empty instance of FileFormats.LP.Model.

Keyword arguments are:

 - `maximum_length::Int=255`: the maximum length for the name of a variable.
   lp_solve 5.0 allows only 16 characters, while CPLEX 12.5+ allow 255.

 - `warn::Bool=false`: print a warning when variables or constraints are renamed.

 - `coefficient_type::Type{T} = Float64`: the supported type to use when reading
   and writing files.
"""
function Model(;
    maximum_length::Int = 255,
    warn::Bool = false,
    coefficient_type::Type{T} = Float64,
) where {T}
    model = Model{T}()
    options = Options(maximum_length, warn)
    model.ext[:LP_OPTIONS] = options
    return model
end

Base.summary(io::IO, ::Model) = print(io, "MOI.FileFormats.LP.Model")

# ==============================================================================
#
#   Base.write
#
# ==============================================================================

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

# ==============================================================================
#
#   `Base.read!`
#
# ==============================================================================

const _KW_OBJECTIVE = Val{:objective}()
const _KW_CONSTRAINTS = Val{:constraints}()
const _KW_BOUNDS = Val{:bounds}()
const _KW_INTEGER = Val{:integer}()
const _KW_BINARY = Val{:binary}()
const _KW_SOS = Val{:sos}()
const _KW_END = Val{:end}()

const _KEYWORDS = Dict(
    # _KW_OBJECTIVE
    "max" => _KW_OBJECTIVE,
    "maximize" => _KW_OBJECTIVE,
    "maximise" => _KW_OBJECTIVE,
    "maximum" => _KW_OBJECTIVE,
    "min" => _KW_OBJECTIVE,
    "minimize" => _KW_OBJECTIVE,
    "minimise" => _KW_OBJECTIVE,
    "minimum" => _KW_OBJECTIVE,
    # _KW_CONSTRAINTS
    "subject to" => _KW_CONSTRAINTS,
    "such that" => _KW_CONSTRAINTS,
    "st" => _KW_CONSTRAINTS,
    "s.t." => _KW_CONSTRAINTS,
    # _KW_BOUNDS
    "bounds" => _KW_BOUNDS,
    "bound" => _KW_BOUNDS,
    # _KW_INTEGER
    "gen" => _KW_INTEGER,
    "general" => _KW_INTEGER,
    "generals" => _KW_INTEGER,
    "integer" => _KW_INTEGER,
    "integers" => _KW_INTEGER,
    # _KW_BINARY
    "bin" => _KW_BINARY,
    "binary" => _KW_BINARY,
    "binaries" => _KW_BINARY,
    # _KW_SOS
    "sos" => _KW_SOS,
    # _KW_END
    "end" => _KW_END,
)

mutable struct _ReadCache{T}
    objective::MOI.ScalarAffineFunction{T}
    quad_obj_terms::Vector{MOI.ScalarQuadraticTerm{T}}
    constraint_function::MOI.ScalarAffineFunction{T}
    quad_terms::Vector{MOI.ScalarQuadraticTerm{T}}
    constraint_name::String
    num_constraints::Int
    name_to_variable::Dict{String,MOI.VariableIndex}
    has_default_bound::Set{MOI.VariableIndex}
    indicator::Union{Nothing,Pair{MOI.VariableIndex,MOI.ActivationCondition}}
    function _ReadCache{T}() where {T}
        return new(
            zero(MOI.ScalarAffineFunction{T}),
            MOI.ScalarQuadraticTerm{T}[],
            zero(MOI.ScalarAffineFunction{T}),
            MOI.ScalarQuadraticTerm{T}[],
            "",
            0,
            Dict{String,MOI.VariableIndex}(),
            Set{MOI.VariableIndex}(),
            nothing,
        )
    end
end

function _get_variable_from_name(
    model::Model{T},
    cache::_ReadCache,
    name::String,
) where {T}
    current_variable = get(cache.name_to_variable, name, nothing)
    if current_variable !== nothing
        return current_variable
    end
    options = get_options(model)
    if length(name) > options.maximum_length
        error("Name exceeds maximum length: $name")
    elseif match(r"^([\.0-9])", name) !== nothing
        error("Name starts with invalid character: $name")
    elseif match(_NAME_REG, name) !== nothing
        error("Name contains with invalid character: $name")
    end
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, name)
    # By default, all variables have a lower bound of 0 unless otherwise
    # specified.
    MOI.add_constraint(model, x, MOI.GreaterThan(zero(T)))
    push!(cache.has_default_bound, x)
    cache.name_to_variable[name] = x
    return x
end

_tokenize(line::AbstractString) = String.(split(line, " "; keepempty = false))

@enum(
    _TokenType,
    _TOKEN_VARIABLE,
    _TOKEN_COEFFICIENT,
    _TOKEN_SIGN,
    _TOKEN_QUADRATIC_OPEN,
    _TOKEN_QUADRATIC_CLOSE,
    _TOKEN_QUADRATIC_DIAG,
    _TOKEN_QUADRATIC_OFF_DIAG,
)

function _parse_token(::Type{T}, token::String) where {T}
    if token == "+"
        return _TOKEN_SIGN, one(T)
    elseif token == "-"
        return _TOKEN_SIGN, -one(T)
    elseif startswith(token, "[")
        return _TOKEN_QUADRATIC_OPEN, zero(T)
    elseif startswith(token, "]")
        return _TOKEN_QUADRATIC_CLOSE, zero(T)
    elseif token == "^"
        return _TOKEN_QUADRATIC_DIAG, zero(T)
    elseif token == "*"
        return _TOKEN_QUADRATIC_OFF_DIAG, zero(T)
    end
    coef = tryparse(T, token)
    if coef === nothing
        return _TOKEN_VARIABLE, token
    else
        return _TOKEN_COEFFICIENT, coef
    end
end

function _get_term(token_types, token_values::Vector{T}, offset) where {T}
    coef = one(T)
    if token_types[offset] == _TOKEN_SIGN
        coef = token_values[offset]
        offset += 1
    end
    if token_types[offset] == _TOKEN_COEFFICIENT
        coef *= token_values[offset]
        offset += 1
    elseif token_types[offset] == _TOKEN_SIGN
        error("Invalid line")
    end
    if offset > length(token_types) || token_types[offset] == _TOKEN_SIGN
        return coef, offset  # It's a standalone constant
    end
    if token_types[offset] == _TOKEN_QUADRATIC_OPEN
        return _get_term(token_types, token_values, offset + 1)
    end
    @assert token_types[offset] == _TOKEN_VARIABLE
    x = MOI.VariableIndex(Int64(token_values[offset]))
    offset += 1
    if offset > length(token_types) ||
       token_types[offset] in (_TOKEN_SIGN, _TOKEN_COEFFICIENT)
        return MOI.ScalarAffineTerm(coef, x), offset
    end
    term = if token_types[offset] == _TOKEN_QUADRATIC_DIAG
        MOI.ScalarQuadraticTerm(coef, x, x)
    else
        @assert token_types[offset] == _TOKEN_QUADRATIC_OFF_DIAG
        y = MOI.VariableIndex(Int64(token_values[offset+1]))
        MOI.ScalarQuadraticTerm(coef, x, y)
    end
    if get(token_types, offset + 2, nothing) == _TOKEN_QUADRATIC_CLOSE
        return term, offset + 3
    else
        return term, offset + 2
    end
end

_half(x) = x / 2
_half(x::Integer) = div(x, 2)

function _parse_function(
    f::MOI.ScalarAffineFunction{T},
    model::Model,
    cache::_ReadCache,
    tokens::Vector{String},
) where {T}
    N = length(tokens)
    token_types = Vector{_TokenType}(undef, N)
    token_values = Vector{T}(undef, N)
    for i in 1:length(tokens)
        token_type, token = _parse_token(T, tokens[i])
        token_types[i] = token_type
        if token_type in (_TOKEN_SIGN, _TOKEN_COEFFICIENT)
            token_values[i] = token::T
        elseif token_type in (_TOKEN_QUADRATIC_OPEN, _TOKEN_QUADRATIC_CLOSE)
            token_values[i] = zero(T)
        elseif token_type in (_TOKEN_QUADRATIC_DIAG, _TOKEN_QUADRATIC_OFF_DIAG)
            token_values[i] = zero(T)
        else
            @assert token_type == _TOKEN_VARIABLE
            x = _get_variable_from_name(model, cache, token::String)
            # A cheat for type-stability. Store `T` of the variable index
            token_values[i] = T(x.value)
        end
    end
    offset = 1
    while offset <= length(tokens)
        term, offset = _get_term(token_types, token_values, offset)
        if term isa MOI.ScalarAffineTerm{T}
            push!(f.terms, term::MOI.ScalarAffineTerm{T})
        elseif term isa MOI.ScalarQuadraticTerm{T}
            push!(cache.quad_terms, term::MOI.ScalarQuadraticTerm{T})
            if tokens[offset-1] in ("]", "]/2")
                is_half = tokens[offset-1] == "]/2"
                for (i, term) in enumerate(cache.quad_terms)
                    x, y = term.variable_1, term.variable_2
                    coef = (x == y ? 2 : 1) * term.coefficient
                    if is_half
                        coef = _half(coef)
                    end
                    cache.quad_terms[i] = MOI.ScalarQuadraticTerm(coef, x, y)
                end
            end
        else
            f.constant += term::T
        end
    end
    return
end

# _KW_OBJECTIVE

_set_objective_sense(::Any, ::Model, ::String) = nothing

function _set_objective_sense(
    ::typeof(_KW_OBJECTIVE),
    model::Model,
    sense::String,
)
    if sense in ("max", "maximize", "maximise", "maximum")
        MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    else
        @assert sense in ("min", "minimize", "minimise", "minimum")
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    end
    return
end

function _parse_section(
    ::typeof(_KW_OBJECTIVE),
    model::Model,
    cache::_ReadCache,
    line::AbstractString,
)
    if occursin(":", line)  # Strip name of the objective
        m = match(r"(.*?)\:(.*)", line)::RegexMatch
        line = String(m[2]::AbstractString)
    end
    if occursin("^", line)
        line = replace(line, "^" => " ^ ")
    end
    if occursin(r"\][\s/][\s/]+2", line)
        line = replace(line, r"\][\s/][\s/]+2" => "]/2")
    end
    tokens = _tokenize(line)
    if length(tokens) == 0
        # Can happen if the name of the objective is on one line and the
        # expression is on the next.
        return
    end
    _parse_function(cache.objective, model, cache, tokens)
    append!(cache.quad_obj_terms, cache.quad_terms)
    empty!(cache.quad_terms)
    return
end

# _KW_CONSTRAINTS

function _parse_section(
    ::typeof(_KW_CONSTRAINTS),
    model::Model{T},
    cache::_ReadCache,
    line::AbstractString,
) where {T}
    # SOS constraints should be in their own "SOS" section, but we can also
    # recognize them if they're mixed into the constraint section.
    if match(r" S([1-2])\w*:: ", line) !== nothing
        _parse_section(_KW_SOS, model, cache, line)
        return
    end
    if isempty(cache.constraint_name)
        if occursin(":", line)
            m = match(r"(.*?)\:(.*)", line)::RegexMatch
            cache.constraint_name = String(m[1]::AbstractString)
            line = String(m[2]::AbstractString)
        else
            # Give it a temporary name for now
            cache.constraint_name = "R$(cache.num_constraints)"
        end
    end
    if cache.indicator === nothing
        if (m = match(r"\s*(.+?)\s*=\s*(0|1)\s*->(.+)", line)) !== nothing
            z = _get_variable_from_name(model, cache, String(m[1]))
            cond = m[2] == "0" ? MOI.ACTIVATE_ON_ZERO : MOI.ACTIVATE_ON_ONE
            cache.indicator = z => cond
            line = String(m[3])
        end
    end
    if occursin("^", line)
        # Simplify parsing of constraints with ^2 terms by turning them into
        # explicit " ^ 2" terms. This avoids ambiguity when parsing names.
        line = replace(line, "^" => " ^ ")
    end
    if occursin(r"\][\s/][\s/]+2", line)
        # Simplify parsing of ]/2 end blocks, which may contain whitespace.
        line = replace(line, r"\][\s/][\s/]+2" => "]/2")
    end
    tokens = _tokenize(line)
    if length(tokens) == 0
        # Can happen if the name is on one line and the constraint on the next.
        return
    end
    # This checks if the constaint is finishing on this line.
    constraint_set = nothing
    if length(tokens) >= 2 && tokens[end-1] in ("<", "<=", ">", ">=", "=", "==")
        rhs = parse(T, pop!(tokens))
        sym = pop!(tokens)
        constraint_set = if sym in ("<", "<=")
            MOI.LessThan(rhs)
        elseif sym in (">", ">=")
            MOI.GreaterThan(rhs)
        else
            @assert sym in ("=", "==")
            MOI.EqualTo(rhs)
        end
    end
    _parse_function(cache.constraint_function, model, cache, tokens)
    if constraint_set !== nothing
        f = if isempty(cache.quad_terms)
            cache.constraint_function
        else
            MOI.ScalarQuadraticFunction(
                cache.quad_terms,
                cache.constraint_function.terms,
                cache.constraint_function.constant,
            )
        end
        if cache.indicator !== nothing
            f = MOI.Utilities.operate(vcat, T, cache.indicator[1], f)
            constraint_set = MOI.Indicator{cache.indicator[2]}(constraint_set)
        end
        c = MOI.add_constraint(model, f, constraint_set)
        MOI.set(model, MOI.ConstraintName(), c, cache.constraint_name)
        cache.num_constraints += 1
        empty!(cache.constraint_function.terms)
        empty!(cache.quad_terms)
        cache.constraint_function.constant = zero(T)
        cache.constraint_name = ""
        cache.indicator = nothing
    end
    return
end

# _KW_BOUNDS

function _parse_float(::Type{T}, token::String) where {T}
    coef = lowercase(token)
    if coef in ("-inf", "-infinity")
        return typemin(T)
    elseif coef in ("+inf", "+infinity")
        return typemax(T)
    end
    return tryparse(T, coef)
end

# Yes, the last elements here are really accepted by CPLEX...
_is_less_than(token) = token in ("<=", "<", "=<")
_is_greater_than(token) = token in (">=", ">", "=>")
_is_equal_to(token) = token in ("=", "==")

function _parse_section(
    ::typeof(_KW_BOUNDS),
    model::Model{T},
    cache::_ReadCache,
    line::AbstractString,
) where {T}
    tokens = _tokenize(line)
    if length(tokens) == 2 && lowercase(tokens[2]) == "free"
        x = _get_variable_from_name(model, cache, tokens[1])
        _delete_default_lower_bound_if_present(model, cache, x)
        return
    end
    lb, ub, name = nothing, nothing, ""
    if length(tokens) == 5
        name = tokens[3]
        if _is_less_than(tokens[2]) && _is_less_than(tokens[4])
            lb = _parse_float(T, tokens[1])::T
            ub = _parse_float(T, tokens[5])::T
        elseif _is_greater_than(tokens[2]) && _is_greater_than(tokens[4])
            lb = _parse_float(T, tokens[5])::T
            ub = _parse_float(T, tokens[1])::T
        else
            error("Unable to parse bound due to invalid inequalities: $(line)")
        end
    elseif length(tokens) == 3
        lhs, rhs = _parse_float(T, tokens[1]), _parse_float(T, tokens[3])
        if lhs === nothing  # name [comparison] bound
            @assert rhs !== nothing
            name = tokens[1]
            if _is_less_than(tokens[2])
                # name <= bound
                ub = rhs
            elseif _is_greater_than(tokens[2])
                # name >= bound
                lb = rhs
            elseif _is_equal_to(tokens[2])
                lb = ub = rhs
            else
                error(
                    "Unable to parse bound due to invalid inequalities: $(line)",
                )
            end
        else # bound [comparison] name
            @assert rhs === nothing
            name = tokens[3]
            if _is_less_than(tokens[2])
                # bound <= name
                lb = lhs
            elseif _is_greater_than(tokens[2])
                # bound >= name
                ub = lhs
            elseif _is_equal_to(tokens[2])
                lb = ub = lhs
            else
                error(
                    "Unable to parse bound due to invalid inequalities: $(line)",
                )
            end
        end
    else
        error("Unable to parse bound: $(line)")
    end
    x = _get_variable_from_name(model, cache, name)
    if lb !== nothing && ub !== nothing
        if lb == ub
            _delete_default_lower_bound_if_present(model, cache, x)
            MOI.add_constraint(model, x, MOI.EqualTo(lb))
            return
        elseif typemin(T) < lb < ub < typemax(T)
            _delete_default_lower_bound_if_present(model, cache, x)
            # Do not add MOI.Interval constraints because we want to follow
            # JuMP's convention of adding separate lower and upper bounds.
            MOI.add_constraint(model, x, MOI.GreaterThan(lb))
            MOI.add_constraint(model, x, MOI.LessThan(ub))
            return
        elseif lb == typemin(T)
            _delete_default_lower_bound_if_present(model, cache, x)
            if ub == typemax(T)
                return  # Explicitly free variable
            end
        end
    end
    if lb !== nothing && typemin(T) < lb
        _delete_default_lower_bound_if_present(model, cache, x)
        MOI.add_constraint(model, x, MOI.GreaterThan(lb))
    end
    if ub !== nothing && ub < typemax(T)
        if ub < 0
            # We only need to delete the default lower bound if the upper bound
            # is less than 0.
            _delete_default_lower_bound_if_present(model, cache, x)
        end
        MOI.add_constraint(model, x, MOI.LessThan(ub))
    end
    return
end

function _delete_default_lower_bound_if_present(
    model::Model{T},
    cache,
    x,
) where {T}
    if !(x in cache.has_default_bound)
        return
    end
    c = MOI.ConstraintIndex{MOI.VariableIndex,MOI.GreaterThan{T}}(x.value)
    MOI.delete(model, c)
    delete!(cache.has_default_bound, x)
    return
end

# _KW_INTEGER

function _parse_section(::typeof(_KW_INTEGER), model, cache, line)
    for token in _tokenize(line)
        x = _get_variable_from_name(model, cache, token)
        MOI.add_constraint(model, x, MOI.Integer())
    end
    return
end

# _KW_BINARY

function _parse_section(::typeof(_KW_BINARY), model, cache, line)
    for token in _tokenize(line)
        x = _get_variable_from_name(model, cache, token)
        MOI.add_constraint(model, x, MOI.ZeroOne())
    end
    return
end

# _KW_SOS

function _parse_section(
    ::typeof(_KW_SOS),
    model::Model{T},
    cache::_ReadCache,
    line::AbstractString,
) where {T}
    # SOS constraints can have all manner of whitespace issues with them.
    # Normalize them here before attempting to do anything else.
    line = replace(line, r"\s+:\s+" => ":")
    line = replace(line, r"\s+::" => "::")
    tokens = _tokenize(line)
    if length(tokens) < 3
        error("Malformed SOS constraint: $(line)")
    end
    name = String(split(tokens[1], ":")[1])
    if tokens[2] == "S1::"
        order = 1
    elseif tokens[2] == "S2::"
        order = 2
    else
        error("SOS of type $(tokens[2]) not recognised")
    end
    variables, weights = MOI.VariableIndex[], T[]
    for token in tokens[3:end]
        items = String.(split(token, ":"))
        if length(items) != 2
            error("Invalid token in SOS constraint: $(token)")
        end
        push!(variables, _get_variable_from_name(model, cache, items[1]))
        push!(weights, parse(T, items[2]))
    end
    c_ref = if tokens[2] == "S1::"
        MOI.add_constraint(model, variables, MOI.SOS1(weights))
    else
        @assert tokens[2] == "S2::"
        MOI.add_constraint(model, variables, MOI.SOS2(weights))
    end
    MOI.set(model, MOI.ConstraintName(), c_ref, name)
    return
end

# _KW_END

function _parse_section(
    ::typeof(_KW_END),
    ::Model,
    ::_ReadCache,
    line::AbstractString,
)
    return error("Corrupted LP File. You have the lne $(line) after an end.")
end

function _strip_comment(line::String)
    if occursin("\\", line)
        m = match(r"(.*?)\\(.*)", line)::RegexMatch
        return strip(String(m[1]::AbstractString))
    else
        return strip(line)
    end
end

function _parse_section(
    ::Val{:header},
    ::Model,
    ::_ReadCache,
    line::AbstractString,
)
    return error("Unable to read LP file: unexpected line: $(line)")
end

"""
    Base.read!(io::IO, model::FileFormats.LP.Model)

Read `io` in the LP file format and store the result in `model`.

This reader attempts to follow the CPLEX LP format, because others like the
lpsolve version are very...flexible...in how they accept input. Read more about
them here: http://lpsolve.sourceforge.net
"""
function Base.read!(io::IO, model::Model{T}) where {T}
    if !MOI.is_empty(model)
        error("Cannot read in file because model is not empty.")
    end
    cache = _ReadCache{T}()
    section = Val{:header}()
    peeked_line = ""
    while peeked_line !== nothing
        line, peeked_line = _readline(io, peeked_line)
        lower_line = lowercase(line)
        if haskey(_KEYWORDS, lower_line)
            section = _KEYWORDS[lower_line]
            _set_objective_sense(section, model, lower_line)
            continue
        end
        while _line_continues(section, peeked_line)
            line, peeked_line = _readline(io, string(line, ' ', peeked_line))
        end
        _parse_section(section, model, cache, line)
    end
    obj = if isempty(cache.quad_obj_terms)
        cache.objective
    else
        MOI.ScalarQuadraticFunction(
            cache.quad_obj_terms,
            cache.objective.terms,
            cache.objective.constant,
        )
    end
    MOI.set(model, MOI.ObjectiveFunction{typeof(obj)}(), obj)
    return
end

function _line_continues(
    ::Union{typeof(_KW_OBJECTIVE),typeof(_KW_CONSTRAINTS)},
    peeked_line::AbstractString,
)
    return any(Base.Fix1(startswith, peeked_line), ('+', '-'))
end

_line_continues(::Any, ::Any) = false

function _readline(io::IO, line::AbstractString)
    if eof(io)
        return line, nothing
    end
    peeked_line = _strip_comment(string(readline(io)))
    if isempty(line)
        # If the line is empty, go to the next
        return _readline(io, peeked_line)
    elseif isempty(peeked_line)
        # If the peeked line is empty, get another
        return _readline(io, line)
    elseif any(Base.Fix1(endswith, line), ('+', '-', '[', '='))
        # If the line ends with a continuation character, read in the next line.
        return _readline(io, string(line, " ", peeked_line))
    elseif any(Base.Fix1(startswith, peeked_line), (']', '/'))
        # Always read in the next line if it starts with ] or /, which are used
        # in quadratic functions.
        return _readline(io, string(line, " ", peeked_line))
    end
    return line, peeked_line
end

end
