const _COMMENT_REG = r"(.*?)\\(.*)"

const _READ_START_REG = r"^([\.0-9])"

function _strip_comment(line::String)
    if occursin("\\", line)
        m = match(_COMMENT_REG, line)
        return strip(String(m[1]))
    else
        return strip(line)
    end
end

const _KW_OBJECTIVE = Val{:objective}()
const _KW_CONSTRAINTS = Val{:constraints}()
const _KW_BOUNDS = Val{:bounds}()
const _KW_INTEGER = Val{:integer}()
const _KW_BINARY = Val{:binary}()
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
    # _KW_END
    "end" => _KW_END,
)

const _CONSTRAINT_SENSE = Dict(
    "<" => :le,
    "<=" => :le,
    "=" => :eq,
    "==" => :eq,
    ">" => :ge,
    ">=" => :ge,
)

mutable struct _CacheLPModel
    objective_function::MOI.ScalarAffineFunction{Float64}
    constraint_function::MOI.ScalarAffineFunction{Float64}
    constraint_set::MOI.AbstractScalarSet
    constraint_open::Bool
    contraint_name::String
    num_constraints::Int
    name_to_variable::Dict{String,MOI.VariableIndex}
    function _CacheLPModel()
        return new(
            MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Float64}[], 0.0),
            MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Float64}[], 0.0),
            MOI.EqualTo(0.0),
            false,
            "",
            0,
            Dict{String,MOI.VariableIndex}(),
        )
    end
end

function _verify_name(name::String, maximum_length::Int)
    if length(name) > maximum_length
        error("Name exceeds maximum length: $name")
    elseif match(_READ_START_REG, name) !== nothing
        error("Name starts with invalid character: $name")
    elseif match(NAME_REG, name) !== nothing
        error("Name contains with invalid character: $name")
    end
    return
end

function _get_variable_from_name(
    model::Model,
    cache::_CacheLPModel,
    name::String,
)
    current_variable = get(cache.name_to_variable, name, nothing)
    if current_variable !== nothing
        return current_variable
    end
    options = get_options(model)
    _verify_name(name, options.maximum_length)
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, name)
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
    terms::Vector{MOI.ScalarAffineTerm{Float64}},
    model::Model,
    cache::_CacheLPModel,
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
    offset, constant = 1, 0.0
    while offset <= length(tokens)
        term, offset = _get_term(token_types, token_values, offset)
        if term isa MOI.ScalarAffineTerm{Float64}
            push!(terms, term::MOI.ScalarAffineTerm{Float64})
        else
            constant += term::Float64
        end
    end
    return constant
end

# _KW_HEADER

_parse_section(::Val{:header}, ::Model, ::_CacheLPModel, ::Any) = nothing

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
    cache::_CacheLPModel,
    line::AbstractString,
)
    if occursin(":", line)  # Strip name of the objective
        line = String(match(r"(.*?)\:(.*)", line)[2])
    end
    tokens = _tokenize(line)
    if length(tokens) == 0
        return
    end
    terms = cache.objective_function.terms
    constant = _parse_affine_terms(terms, model, cache, tokens)
    cache.objective_function.constant += constant
    return
end

# _KW_CONSTRAINTS

function _parse_sos_constraint(
    model::Model,
    cache::_CacheLPModel,
    line::AbstractString,
)
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
    c_ref = MOI.add_constraint(
        model,
        variables,
        order == 1 ? MOI.SOS1(weights) : MOI.SOS2(weights),
    )
    MOI.set(model, MOI.ConstraintName(), c_ref, name)
    return
end

function _parse_section(
    ::typeof(_KW_CONSTRAINTS),
    model::Model,
    cache::_CacheLPModel,
    line::AbstractString,
)
    if match(r" S([0-9]):: ", line) !== nothing
        _parse_sos_constraint(model, cache, line)
        return
    end
    if cache.constraint_open == false
        # We're starting a new constraint. Give it a name for now, but we might
        # replace it with a proper strinng in the next if-block.
        cache.contraint_name = "R$(cache.num_constraints)"
    end
    if occursin(":", line)
        if cache.constraint_open == true
            error("Malformed constraint $(line). Is the previous one valid?")
        end
        m = match(r"(.*?)\:(.*)", line)
        cache.contraint_name = String(m[1])
        line = String(m[2])
    end
    cache.constraint_open = true
    tokens = _tokenize(line)
    if length(tokens) == 0
        return
    end
    is_finished = false
    # This checks if the constaint is finishing on this like.
    if length(tokens) >= 2 && tokens[end-1] in ("<", "<=", ">", ">=", "=", "==")
        rhs = parse(Float64, pop!(tokens))
        sym = pop!(tokens)
        if sym in ("<", "<=")
            cache.constraint_set = MOI.LessThan(rhs)
        elseif sym in (">", ">=")
            cache.constraint_set = MOI.GreaterThan(rhs)
        elseif sym in ("=", "==")
            cache.constraint_set = MOI.EqualTo(rhs)
        end
        is_finished = true
    end
    terms = cache.constraint_function.terms
    constant = _parse_affine_terms(terms, model, cache, tokens)
    cache.constraint_function.constant += constant
    if is_finished
        c = MOI.add_constraint(
            model,
            cache.constraint_function,
            cache.constraint_set,
        )
        MOI.set(model, MOI.ConstraintName(), c, cache.contraint_name)
        cache.num_constraints += 1
        cache.constraint_set = MOI.EqualTo(0.0)
        empty!(cache.constraint_function.terms)
        cache.constraint_function.constant = 0.0
        cache.contraint_name = ""
        cache.constraint_open = false
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

_is_less_than(token) = token in ("<=", "<")
_is_greater_than(token) = token in (">=", ">")
_is_equal_to(token) = token in ("==", "=")

function _parse_section(
    ::typeof(_KW_BOUNDS),
    model::Model,
    cache::_CacheLPModel,
    line::AbstractString,
)
    tokens = _tokenize(line)
    if length(tokens) == 2 && tokens[2] == "free"
        return  # Do nothing. Variable is free
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
        MOI.add_constraint(model, x, MOI.EqualTo(lb))
    elseif -Inf < lb < ub < Inf
        MOI.add_constraint(model, x, MOI.Interval(lb, ub))
    elseif -Inf < lb
        MOI.add_constraint(model, x, MOI.GreaterThan(lb))
    else
        MOI.add_constraint(model, x, MOI.LessThan(ub))
    end
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

# _KW_END

function _parse_section(
    ::typeof(_KW_END),
    ::Model,
    ::_CacheLPModel,
    line::AbstractString,
)
    return error("Corrupted LP File. You have the lne $(line) after an end.")
end

"""
    Base.read!(io::IO, model::FileFormats.LP.Model)

Read `io` in the LP file format and store the result in `model`.
"""
function Base.read!(io::IO, model::Model)
    if !MOI.is_empty(model)
        error("Cannot read in file because model is not empty.")
    end
    cache = _CacheLPModel()
    section = Val{:header}()
    while !eof(io)
        line = _strip_comment(string(readline(io)))
        if isempty(line)
            continue
        end
        lower_line = lowercase(line)
        if haskey(_KEYWORDS, lower_line) # section has changed
            section = _KEYWORDS[lower_line]
            _set_objective_sense(section, model, lower_line)
            continue
        end
        _parse_section(section, model, cache, line)
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        cache.objective_function,
    )
    return
end
