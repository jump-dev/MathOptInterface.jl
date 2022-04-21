module LP

import ..FileFormats
import MathOptInterface
const MOI = MathOptInterface

# Julia 1.6 removes Grisu from Base. Previously, we went
#   _print_shortest(io, x) = Base.Grisu.print_shortest(io, x)
# To avoid adding Grisu as a dependency, use the following printing heuristic.
# TODO(odow): consider printing 1.0 as 1.0 instead of 1, i.e., without the
# rounding branch.
function _print_shortest(io::IO, x::Real)
    x_int = round(Int, x)
    if isapprox(x, x_int)
        print(io, x_int)
    else
        print(io, x)
    end
    return
end

MOI.Utilities.@model(
    Model,
    (MOI.ZeroOne, MOI.Integer),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval),
    (),
    (MOI.SOS1, MOI.SOS2),
    (),
    (MOI.ScalarAffineFunction,),
    (MOI.VectorOfVariables,),
    ()
)
function MOI.supports(
    ::Model{T},
    ::MOI.ObjectiveFunction{<:MOI.ScalarQuadraticFunction{T}},
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
    Model(; kwargs...)

Create an empty instance of FileFormats.LP.Model.

Keyword arguments are:

 - `maximum_length::Int=255`: the maximum length for the name of a variable.
   lp_solve 5.0 allows only 16 characters, while CPLEX 12.5+ allow 255.

 - `warn::Bool=false`: print a warning when variables or constraints are renamed.
"""
function Model(; maximum_length::Int = 255, warn::Bool = false)
    model = Model{Float64}()
    options = Options(maximum_length, warn)
    model.ext[:LP_OPTIONS] = options
    return model
end

function Base.show(io::IO, ::Model)
    print(io, "A .LP-file model")
    return
end

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
    variable_names::Dict{MOI.VariableIndex,String},
)
    print(io, variable_names[func])
    return
end

function _write_function(
    io::IO,
    ::Model,
    func::MOI.ScalarAffineFunction{Float64},
    variable_names::Dict{MOI.VariableIndex,String},
)
    is_first_item = true
    if !(func.constant ≈ 0.0)
        _print_shortest(io, func.constant)
        is_first_item = false
    end
    for term in func.terms
        if !(term.coefficient ≈ 0.0)
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
    _write_function(io, model, func, variable_names)
    _write_constraint_suffix(io, set)
    return
end

const _SCALAR_SETS = (
    MOI.LessThan{Float64},
    MOI.GreaterThan{Float64},
    MOI.EqualTo{Float64},
    MOI.Interval{Float64},
)

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

function _write_constraints(io, model, S, variable_names)
    F = MOI.ScalarAffineFunction{Float64}
    for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        _write_constraint(io, model, index, variable_names; write_name = true)
    end
    return
end

function _write_bounds(io, model, S, variable_names, free_variables)
    F = MOI.VariableIndex
    for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        delete!(free_variables, MOI.VariableIndex(index.value))
        _write_constraint(io, model, index, variable_names; write_name = false)
    end
    return
end

function _write_sos_constraints(io, model, variable_names)
    T, F = Float64, MOI.VectorOfVariables
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

_to_string(::Type{MOI.SOS1{Float64}}) = "S1::"
_to_string(::Type{MOI.SOS2{Float64}}) = "S2::"

function _write_constraint(
    io::IO,
    model::Model,
    index::MOI.ConstraintIndex{MOI.VectorOfVariables,S},
    variable_names::Dict{MOI.VariableIndex,String},
) where {S<:Union{MOI.SOS1{Float64},MOI.SOS2{Float64}}}
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

"""
    Base.write(io::IO, model::FileFormats.LP.Model)

Write `model` to `io` in the LP file format.
"""
function Base.write(io::IO, model::Model)
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
    for S in _SCALAR_SETS
        _write_constraints(io, model, S, variable_names)
    end
    println(io, "Bounds")
    for S in _SCALAR_SETS
        _write_bounds(io, model, S, variable_names, free_variables)
    end
    # If a variable is binary, it should not be listed as `free` in the bounds
    # section.
    attr = MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.ZeroOne}()
    for index in MOI.get(model, attr)
        delete!(free_variables, MOI.VariableIndex(index.value))
    end
    # By default, variables have bounds of [0, ∞), so we need to explicitly
    # declare variables as free.
    for variable in sort(collect(free_variables), by = x -> x.value)
        println(io, variable_names[variable], " free")
    end
    _write_integrality(io, model, "General", MOI.Integer, variable_names)
    _write_integrality(io, model, "Binary", MOI.ZeroOne, variable_names)
    _write_sos_constraints(io, model, variable_names)
    println(io, "End")
    return
end

# ==============================================================================
#
#   Base.read!
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
    # _KW_BINARY
    "bin" => _KW_BINARY,
    "binary" => _KW_BINARY,
    "binaries" => _KW_BINARY,
    # _KW_SOS
    "sos" => _KW_SOS,
    # _KW_END
    "end" => _KW_END,
)

mutable struct _ReadCache
    objective::MOI.ScalarAffineFunction{Float64}
    constraint_function::MOI.ScalarAffineFunction{Float64}
    constraint_name::String
    num_constraints::Int
    name_to_variable::Dict{String,MOI.VariableIndex}
    has_default_bound::Set{MOI.VariableIndex}
    function _ReadCache()
        return new(
            MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Float64}[], 0.0),
            MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Float64}[], 0.0),
            "",
            0,
            Dict{String,MOI.VariableIndex}(),
            Set{MOI.VariableIndex}(),
        )
    end
end

function _get_variable_from_name(model::Model, cache::_ReadCache, name::String)
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
    MOI.add_constraint(model, x, MOI.GreaterThan(0.0))
    push!(cache.has_default_bound, x)
    cache.name_to_variable[name] = x
    return x
end

_tokenize(line::AbstractString) = String.(split(line, " "; keepempty = false))

@enum(_TokenType, _TOKEN_VARIABLE, _TOKEN_COEFFICIENT, _TOKEN_SIGN)

function _parse_token(token::String)
    if token == "+"
        return _TOKEN_SIGN, +1.0
    elseif token == "-"
        return _TOKEN_SIGN, -1.0
    end
    coef = tryparse(Float64, token)
    if coef === nothing
        return _TOKEN_VARIABLE, token
    else
        return _TOKEN_COEFFICIENT, coef
    end
end

function _get_term(token_types, token_values, offset)
    coef = 1.0
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
        return coef, offset  # It's a standalone constant!
    end
    @assert token_types[offset] == _TOKEN_VARIABLE
    x = MOI.VariableIndex(Int64(token_values[offset]))
    return MOI.ScalarAffineTerm(coef, x), offset + 1
end

function _parse_affine_terms(
    f::MOI.ScalarAffineFunction{Float64},
    model::Model,
    cache::_ReadCache,
    tokens::Vector{String},
)
    N = length(tokens)
    token_types = Vector{_TokenType}(undef, N)
    token_values = Vector{Float64}(undef, N)
    for i in 1:length(tokens)
        token_type, token = _parse_token(tokens[i])
        token_types[i] = token_type
        if token_type in (_TOKEN_SIGN, _TOKEN_COEFFICIENT)
            token_values[i] = token::Float64
        else
            @assert token_type == _TOKEN_VARIABLE
            x = _get_variable_from_name(model, cache, token::String)
            # A cheat for type-stability. Store `Float64` of the variable index!
            token_values[i] = Float64(x.value)
        end
    end
    offset = 1
    while offset <= length(tokens)
        term, offset = _get_term(token_types, token_values, offset)
        if term isa MOI.ScalarAffineTerm{Float64}
            push!(f.terms, term::MOI.ScalarAffineTerm{Float64})
        else
            f.constant += term::Float64
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
        line = String(match(r"(.*?)\:(.*)", line)[2])
    end
    tokens = _tokenize(line)
    if length(tokens) == 0
        # Can happen if the name of the objective is on one line and the
        # expression is on the next.
        return
    end
    _parse_affine_terms(cache.objective, model, cache, tokens)
    return
end

# _KW_CONSTRAINTS

function _parse_section(
    ::typeof(_KW_CONSTRAINTS),
    model::Model,
    cache::_ReadCache,
    line::AbstractString,
)
    # SOS constraints should be in their own "SOS" section, but we can also
    # recognize them if they're mixed into the constraint section.
    if match(r" S([1-2])\w*:: ", line) !== nothing
        _parse_section(_KW_SOS, model, cache, line)
        return
    end
    if isempty(cache.constraint_name)
        if occursin(":", line)
            m = match(r"(.*?)\:(.*)", line)
            cache.constraint_name = String(m[1])
            line = String(m[2])
        else
            # Give it a temporary name for now
            cache.constraint_name = "R$(cache.num_constraints)"
        end
    end
    tokens = _tokenize(line)
    if length(tokens) == 0
        # Can happen if the name is on one line and the constraint on the next.
        return
    end
    # This checks if the constaint is finishing on this line.
    constraint_set = nothing
    if length(tokens) >= 2 && tokens[end-1] in ("<", "<=", ">", ">=", "=", "==")
        rhs = parse(Float64, pop!(tokens))
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
    _parse_affine_terms(cache.constraint_function, model, cache, tokens)
    if constraint_set !== nothing
        c = MOI.add_constraint(model, cache.constraint_function, constraint_set)
        MOI.set(model, MOI.ConstraintName(), c, cache.constraint_name)
        cache.num_constraints += 1
        empty!(cache.constraint_function.terms)
        cache.constraint_function.constant = 0.0
        cache.constraint_name = ""
    end
    return
end

# _KW_BOUNDS

function _parse_float(token::String)
    coef = lowercase(token)
    if coef in ("-inf", "-infinity")
        return -Inf
    elseif coef in ("+inf", "+infinity")
        return Inf
    else
        return parse(Float64, coef)
    end
end

# Yes, the last elements here are really accepted by CPLEX...
_is_less_than(token) = token in ("<=", "<", "=<")
_is_greater_than(token) = token in (">=", ">", "=>")
_is_equal_to(token) = token in ("=", "==")

function _parse_section(
    ::typeof(_KW_BOUNDS),
    model::Model,
    cache::_ReadCache,
    line::AbstractString,
)
    tokens = _tokenize(line)
    if length(tokens) == 2 && tokens[2] == "free"
        x = _get_variable_from_name(model, cache, tokens[1])
        _delete_default_lower_bound_if_present(model, cache, x)
        return
    end
    lb, ub, name = -Inf, Inf, ""
    if length(tokens) == 5
        name = tokens[3]
        if _is_less_than(tokens[2]) && _is_less_than(tokens[4])
            lb = _parse_float(tokens[1])
            ub = _parse_float(tokens[5])
        elseif _is_greater_than(tokens[2]) && _is_greater_than(tokens[4])
            lb = _parse_float(tokens[5])
            ub = _parse_float(tokens[1])
        else
            error("Unable to parse bound due to invalid inequalities: $(line)")
        end
    elseif length(tokens) == 3
        name = tokens[1]
        if _is_less_than(tokens[2])
            ub = _parse_float(tokens[3])
            # LP files have default lower bounds of 0, unless the upper bound is
            # less than 0.
            lb = ub > 0.0 ? 0.0 : -Inf
        elseif _is_greater_than(tokens[2])
            lb = _parse_float(tokens[3])
        elseif _is_equal_to(tokens[2])
            lb = ub = _parse_float(tokens[3])
        else
            error("Unable to parse bound due to invalid inequalities: $(line)")
        end
    else
        error("Unable to parse bound: $(line)")
    end
    x = _get_variable_from_name(model, cache, name)
    if lb == ub
        _delete_default_lower_bound_if_present(model, cache, x)
        MOI.add_constraint(model, x, MOI.EqualTo(lb))
    elseif -Inf < lb < ub < Inf
        _delete_default_lower_bound_if_present(model, cache, x)
        MOI.add_constraint(model, x, MOI.Interval(lb, ub))
    elseif -Inf < lb
        _delete_default_lower_bound_if_present(model, cache, x)
        MOI.add_constraint(model, x, MOI.GreaterThan(lb))
    else
        if ub < 0
            # We only need to delete the default lower bound if the upper bound
            # is less than 0.
            _delete_default_lower_bound_if_present(model, cache, x)
            MOI.delete(model, c)
        end
        MOI.add_constraint(model, x, MOI.LessThan(ub))
    end
    return
end

function _delete_default_lower_bound_if_present(model, cache, x)
    if !(x in cache.has_default_bound)
        return
    end
    c = MOI.ConstraintIndex{MOI.VariableIndex,MOI.GreaterThan{Float64}}(x.value)
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
    model::Model,
    cache::_ReadCache,
    line::AbstractString,
)
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
    variables, weights = MOI.VariableIndex[], Float64[]
    for token in tokens[3:end]
        items = String.(split(token, ":"))
        if length(items) != 2
            error("Invalid sequence: $(token)")
        end
        push!(variables, _get_variable_from_name(model, cache, items[1]))
        push!(weights, parse(Float64, items[2]))
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
        m = match(r"(.*?)\\(.*)", line)
        return strip(String(m[1]))
    else
        return strip(line)
    end
end

"""
    Base.read!(io::IO, model::FileFormats.LP.Model)

Read `io` in the LP file format and store the result in `model`.

This reader attempts to follow the CPLEX LP format, because others like the
lpsolve version are very...flexible...in how they accept input. Read more about
them here: http://lpsolve.sourceforge.net
"""
function Base.read!(io::IO, model::Model)
    if !MOI.is_empty(model)
        error("Cannot read in file because model is not empty.")
    end
    cache = _ReadCache()
    section = Val{:header}()
    while !eof(io)
        line = _strip_comment(string(readline(io)))
        if isempty(line)
            continue
        end
        lower_line = lowercase(line)
        if haskey(_KEYWORDS, lower_line)
            section = _KEYWORDS[lower_line]
            _set_objective_sense(section, model, lower_line)
            continue
        end
        _parse_section(section, model, cache, line)
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        cache.objective,
    )
    return
end

end
