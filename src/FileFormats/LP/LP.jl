module LP

import ..FileFormats
import MathOptInterface
const MOI = MathOptInterface

# Julia 1.6 removes Grisu from Base. Previously, we went
#   print_shortest(io, x) = Base.Grisu.print_shortest(io, x)
# To avoid adding Grisu as a dependency, use the following printing heuristic.
# TODO(odow): consider printing 1.0 as 1.0 instead of 1, i.e., without the
# rounding branch.
function print_shortest(io::IO, x::Real)
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

const START_REG = r"^([\.0-9eE])"
const NAME_REG = r"([^a-zA-Z0-9\!\"\#\$\%\&\(\)\/\,\.\;\?\@\_\`\'\{\}\|\~])"

function write_function(
    io::IO,
    model::Model,
    func::MOI.VariableIndex,
    variable_names::Dict{MOI.VariableIndex,String},
)
    print(io, variable_names[func])
    return
end

function write_function(
    io::IO,
    model::Model,
    func::MOI.ScalarAffineFunction{Float64},
    variable_names::Dict{MOI.VariableIndex,String},
)
    is_first_item = true
    if !(func.constant ≈ 0.0)
        print_shortest(io, func.constant)
        is_first_item = false
    end
    for term in func.terms
        if !(term.coefficient ≈ 0.0)
            if is_first_item
                print_shortest(io, term.coefficient)
                is_first_item = false
            else
                print(io, term.coefficient < 0 ? " - " : " + ")
                print_shortest(io, abs(term.coefficient))
            end

            print(io, " ", variable_names[term.variable])
        end
    end
    return
end

function write_constraint_suffix(io::IO, set::MOI.LessThan)
    print(io, " <= ")
    print_shortest(io, set.upper)
    println(io)
    return
end

function write_constraint_suffix(io::IO, set::MOI.GreaterThan)
    print(io, " >= ")
    print_shortest(io, set.lower)
    println(io)
    return
end

function write_constraint_suffix(io::IO, set::MOI.EqualTo)
    print(io, " = ")
    print_shortest(io, set.value)
    println(io)
    return
end

function write_constraint_suffix(io::IO, set::MOI.Interval)
    print(io, " <= ")
    print_shortest(io, set.upper)
    println(io)
    return
end

function write_constraint_prefix(io::IO, set::MOI.Interval)
    print_shortest(io, set.lower)
    print(io, " <= ")
    return
end

write_constraint_prefix(io::IO, set) = nothing

function write_constraint(
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
    write_constraint_prefix(io, set)
    write_function(io, model, func, variable_names)
    return write_constraint_suffix(io, set)
end

const SCALAR_SETS = (
    MOI.LessThan{Float64},
    MOI.GreaterThan{Float64},
    MOI.EqualTo{Float64},
    MOI.Interval{Float64},
)

function write_sense(io::IO, model::Model)
    if MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
        println(io, "maximize")
    else
        println(io, "minimize")
    end
    return
end

function write_objective(
    io::IO,
    model::Model,
    variable_names::Dict{MOI.VariableIndex,String},
)
    print(io, "obj: ")
    obj_func_type = MOI.get(model, MOI.ObjectiveFunctionType())
    obj_func = MOI.get(model, MOI.ObjectiveFunction{obj_func_type}())
    write_function(io, model, obj_func, variable_names)
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
            s -> match(START_REG, s) !== nothing ? "_" * s : s,
            s -> replace(s, NAME_REG => "_"),
            s -> s[1:min(length(s), options.maximum_length)],
        ],
    )
    variable_names = Dict{MOI.VariableIndex,String}(
        index => MOI.get(model, MOI.VariableName(), index) for
        index in MOI.get(model, MOI.ListOfVariableIndices())
    )
    write_sense(io, model)
    write_objective(io, model, variable_names)
    println(io, "subject to")
    for S in SCALAR_SETS
        for index in MOI.get(
            model,
            MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64},S}(),
        )
            write_constraint(
                io,
                model,
                index,
                variable_names;
                write_name = true,
            )
        end
    end
    println(io, "Bounds")
    free_variables = Set(keys(variable_names))
    for S in SCALAR_SETS
        for index in
            MOI.get(model, MOI.ListOfConstraintIndices{MOI.VariableIndex,S}())
            delete!(free_variables, MOI.VariableIndex(index.value))
            write_constraint(
                io,
                model,
                index,
                variable_names;
                write_name = false,
            )
        end
    end
    for index in MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.ZeroOne}(),
    )
        delete!(free_variables, MOI.VariableIndex(index.value))
    end
    for variable in sort(collect(free_variables), by = x -> x.value)
        println(io, variable_names[variable], " free")
    end
    for (S, str_S) in [(MOI.Integer, "General"), (MOI.ZeroOne, "Binary")]
        indices =
            MOI.get(model, MOI.ListOfConstraintIndices{MOI.VariableIndex,S}())
        if length(indices) > 0
            println(io, str_S)
            for index in indices
                write_function(
                    io,
                    model,
                    MOI.get(model, MOI.ConstraintFunction(), index),
                    variable_names,
                )
                println(io)
            end
        end
    end
    println(io, "End")
    return
end

# ==============================================================================
#
#   Base.read!
#
# ==============================================================================
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

# a list of section keywords in lower-case
const _KEYWORDS = Dict(
    "max" => Val{:obj},
    "maximize" => Val{:obj},
    "maximise" => Val{:obj},
    "maximum" => Val{:obj},
    "min" => Val{:obj},
    "minimize" => Val{:obj},
    "minimise" => Val{:obj},
    "minimum" => Val{:obj},
    "subject to" => Val{:constraints},
    "such that" => Val{:constraints},
    "st" => Val{:constraints},
    "s.t." => Val{:constraints},
    "bounds" => Val{:bounds},
    "bound" => Val{:bounds},
    "gen" => Val{:integer},
    "general" => Val{:integer},
    "generals" => Val{:integer},
    "bin" => Val{:binary},
    "binary" => Val{:binary},
    "binaries" => Val{:binary},
    "end" => Val{:quit},
)

const _SENSE_ALIAS = Dict(
    "max" => MOI.MAX_SENSE,
    "maximize" => MOI.MAX_SENSE,
    "maximise" => MOI.MAX_SENSE,
    "maximum" => MOI.MAX_SENSE,
    "min" => MOI.MIN_SENSE,
    "minimize" => MOI.MIN_SENSE,
    "minimise" => MOI.MIN_SENSE,
    "minimum" => MOI.MIN_SENSE,
)

const _SUBJECT_TO_ALIAS = ["subject to", "such that", "st", "s.t."]

const _CONSTRAINT_SENSE = Dict(
    "<" => :le,
    "<=" => :le,
    "=" => :eq,
    "==" => :eq,
    ">" => :ge,
    ">=" => :ge,
)

function _verify_name(variable::String, maximum_length::Int)
    if length(variable) > maximum_length
        return false
    end
    m = match(_READ_START_REG, variable)
    if m !== nothing
        return false
    end
    m = match(NAME_REG, variable)
    if m !== nothing
        return false
    end
    return true
end

mutable struct CacheLPModel
    objective_function::MOI.ScalarAffineFunction
    linear_constraint_function::MOI.ScalarAffineFunction
    linear_constraint_set::MOI.AbstractScalarSet
    linear_constraint_open::Bool
    linear_constraint_name::String
    num_linear_constraints::Int
    variables_in_model::Dict{String,MOI.VariableIndex}
    function CacheLPModel()
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

_set_sense!(T, model::Model, line) = nothing
function _set_sense!(::Type{Val{:obj}}, model::Model, line)
    return MOI.set(model, MOI.ObjectiveSense(), _SENSE_ALIAS[lowercase(line)])
end

function _add_new_variable!(model::Model, data_cache::CacheLPModel, name)
    var = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), var, name)
    data_cache.variables_in_model[name] = var
    return var
end

function _get_variable_from_name(
    model::Model,
    data_cache::CacheLPModel,
    variable_name,
)
    var_inside_model = get(data_cache.variables_in_model, variable_name, "")
    if var_inside_model != ""
        return var_inside_model
    end
    options = get_options(model)
    if !_verify_name(variable_name, options.maximum_length)
        error("Invalid variable name $variable_name")
    end
    return _add_new_variable!(model, data_cache, variable_name)
end

function _tokenize(line::AbstractString)
    return String.(split(line, " "; keepempty = false))
end

function _parse_float_from_bound(val::String)
    lower_case_val = lowercase(val)
    if lower_case_val == "-inf" || lower_case_val == "-infinity"
        return -Inf
    elseif lower_case_val == "+inf" || lower_case_val == "+infinity"
        return Inf
    else
        return parse(Float64, lower_case_val)
    end
end

function _parse_affine_terms!(
    model::Model,
    data_cache::CacheLPModel,
    tokens::Vector{String},
    section::String,
    line::AbstractString,
)
    affine_terms = MOI.ScalarAffineTerm{Float64}[]
    while length(tokens) > 0
        variable = String(pop!(tokens))
        # In the case of objective functions this can be an objective constant
        if section == "objective"
            try
                obj_constant = parse(Float64, variable)
                if length(tokens) > 0
                    _sign = pop!(tokens)
                    if _sign == "-"
                        obj_constant *= -1
                    elseif _sign == "+"
                    else
                        error(
                            "Unable to parse $section due to bad operator: $(_sign) $(line)",
                        )
                    end
                end
                data_cache.objective_function.constant += obj_constant
                continue
            catch
            end
        end
        var = _get_variable_from_name(model, data_cache, variable)
        if length(tokens) > 0
            coef_token = pop!(tokens)
        else
            coeff = 1.0
            push!(affine_terms, MOI.ScalarAffineTerm(coeff, var))
            continue
        end
        try
            if coef_token == "+"
                coeff = 1.0
                push!(affine_terms, MOI.ScalarAffineTerm(coeff, var))
                continue
            elseif coef_token == "-"
                coeff = -1.0
                push!(affine_terms, MOI.ScalarAffineTerm(coeff, var))
                continue
            end
            coeff = parse(Float64, coef_token)
        catch
            error(
                "Unable to parse $section due to bad operator: $(_sign) $(line)",
            )
        end
        if length(tokens) > 0
            _sign = pop!(tokens)
            if _sign == "-"
                coeff *= -1
            elseif _sign == "+"
            else
                error(
                    "Unable to parse $section due to bad operator: $(_sign) $(line)",
                )
            end
        end
        push!(affine_terms, MOI.ScalarAffineTerm(coeff, var))
    end
    return affine_terms
end

function _parse_sos!(
    model::Model,
    data_cache::CacheLPModel,
    line::AbstractString,
)
    tokens = _tokenize(line)
    if length(tokens) < 3
        error(string("Malformed SOS constraint: ", line))
    end
    sos_con_name = String.(split(tokens[1], ":"))[1]
    if tokens[2] == "S1::"
        order = 1
    elseif tokens[2] == "S2::"
        order = 2
    else
        error("SOS of type $(tokens[2]) not recognised")
    end
    variables = MOI.VariableIndex[]
    weights = Float64[]
    for token in tokens[3:end]
        items = String.(split(token, ":"))
        if length(items) != 2
            error(string("Invalid sequence: ", token))
        end
        push!(variables, _get_variable_from_name(model, data_cache, items[1]))
        push!(weights, parse(Float64, items[2]))
    end
    sos_con = MOI.add_constraint(
        model,
        variables,
        order == 1 ? MOI.SOS1(weights) : MOI.SOS2(weights),
    )
    MOI.set(model, MOI.ConstraintName(), sos_con, sos_con_name)
    # TODO I think this only works for SOS of one line
    return
end

function _parse_variable_type!(
    model::Model,
    data_cache::CacheLPModel,
    line::AbstractString,
    set::MOI.AbstractSet,
)
    items = _tokenize(line)
    for v in items
        var = _get_variable_from_name(model, data_cache, v)
        MOI.add_constraint(model, var, set)
    end
    return nothing
end

function _parse_section!(
    ::Type{Val{:none}},
    model::Model,
    data_cache::CacheLPModel,
    line::AbstractString,
)
    return nothing
end
function _parse_section!(
    ::Type{Val{:quit}},
    model::Model,
    data_cache::CacheLPModel,
    line::AbstractString,
)
    return error("Corrupted LP File. You have the lne $(line) after an end.")
end
function _parse_section!(::Type{Val{:integer}}, model, data_cache, line)
    return _parse_variable_type!(model, data_cache, line, MOI.Integer())
end
function _parse_section!(::Type{Val{:binary}}, model, data_cache, line)
    return _parse_variable_type!(model, data_cache, line, MOI.ZeroOne())
end

function _parse_section!(
    ::Type{Val{:obj}},
    model::Model,
    data_cache::CacheLPModel,
    line::AbstractString,
)
    # okay so line should be the start of the objective
    if occursin(":", line)
        # throw away name
        m = match(r"(.*?)\:(.*)", line)
        line = String(m[2])
    end
    tokens = _tokenize(line)
    if length(tokens) == 0 # no objective
        return MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    end
    affine_terms =
        _parse_affine_terms!(model, data_cache, tokens, "objective", line)
    push!(data_cache.objective_function.terms, affine_terms...)
    return
end

function _parse_section!(
    ::Type{Val{:constraints}},
    model::Model,
    data_cache::CacheLPModel,
    line::AbstractString,
)
    if match(r" S([0-9]):: ", line) !== nothing
        # it's an SOS constraint
        _parse_sos!(model, data_cache, line)
        return
    end
    if data_cache.linear_constraint_open == false
        # parse the number of rows and add this name
        data_cache.linear_constraint_name = "R$(data_cache.num_linear_constraints)"
    end
    if occursin(":", line)
        if data_cache.linear_constraint_open == true
            error("Malformed constraint $(line). Is the previous one valid?")
        end
        # throw away name
        m = match(r"(.*?)\:(.*)", line)
        data_cache.linear_constraint_name = String(m[1])
        line = String(m[2])
    end
    data_cache.linear_constraint_open = true

    tokens = _tokenize(line)
    if length(tokens) == 0 # no entries
        return
    elseif length(tokens) >= 2 && haskey(_CONSTRAINT_SENSE, tokens[end-1])# test if constraint ends this line
        rhs = parse(Float64, pop!(tokens))
        sym = pop!(tokens)
        if _CONSTRAINT_SENSE[sym] == :le
            data_cache.linear_constraint_set = MOI.LessThan(rhs)
        elseif _CONSTRAINT_SENSE[sym] == :ge
            data_cache.linear_constraint_set = MOI.GreaterThan(rhs)
        elseif _CONSTRAINT_SENSE[sym] == :eq
            data_cache.linear_constraint_set = MOI.EqualTo(rhs)
        end
        # Finished
        # Add constraint
        c = MOI.add_constraint(
            model,
            data_cache.linear_constraint_function,
            data_cache.linear_constraint_set,
        )
        MOI.set(
            model,
            MOI.ConstraintName(),
            c,
            data_cache.linear_constraint_name,
        )
        data_cache.num_linear_constraints += 1
        # Clear the constraint part of data_cache
        data_cache.linear_constraint_set = MOI.EqualTo(0.0)
        data_cache.linear_constraint_function =
            MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Float64}[], 0.0)
        data_cache.linear_constraint_name = ""
        data_cache.linear_constraint_open = false
    end
    affine_terms =
        _parse_affine_terms!(model, data_cache, tokens, "constraint", line)
    push!(data_cache.linear_constraint_function.terms, affine_terms...)
    return
end

_bound_error(line::AbstractString) = error("Unable to parse bound: $(line)")
function _parse_section!(
    ::Type{Val{:bounds}},
    model::Model,
    data_cache::CacheLPModel,
    line::AbstractString,
)
    items = _tokenize(line)
    v = ""
    lb = -Inf
    ub = Inf
    if length(items) == 5 # ranged bound
        v = items[3]
        if (items[2] == "<=" || items[2] == "<") &&
           (items[4] == "<=" || items[4] == "<") # le
            lb = _parse_float_from_bound(items[1])
            ub = _parse_float_from_bound(items[5])
        elseif (items[2] == ">=" || items[2] == ">") &&
               (items[4] == ">=" || items[4] == ">") # ge
            lb = _parse_float_from_bound(items[5])
            ub = _parse_float_from_bound(items[1])
        else
            _bound_error(line)
        end
    elseif length(items) == 3 # one sided
        v = items[1]
        if items[2] == "<=" || items[2] == "<" # le
            ub = _parse_float_from_bound(items[3])
            if ub > 0.0
                lb = 0.0
            else
                lb = -Inf
            end
        elseif items[2] == ">=" || items[2] == ">" # ge
            lb = _parse_float_from_bound(items[3])
            ub = +Inf
        elseif items[2] == "==" || items[2] == "=" # eq
            lb = ub = _parse_float_from_bound(items[3])
        else
            _bound_error(line)
        end
    elseif length(items) == 2 # free
        if items[2] != "free"
            _bound_error(line)
        end
        v = items[1]
    else
        _bound_error(line)
    end
    var = _get_variable_from_name(model, data_cache, v)
    set = bounds_to_set(lb, ub)
    if set !== nothing
        MOI.add_constraint(model, var, set)
    end
    return
end

function bounds_to_set(lower::Float64, upper::Float64)
    if -Inf < lower < upper < Inf
        return MOI.Interval(lower, upper)
    elseif -Inf < lower && upper == Inf
        return MOI.GreaterThan(lower)
    elseif -Inf == lower && upper < Inf
        return MOI.LessThan(upper)
    elseif lower == upper
        return MOI.EqualTo(upper)
    end
    return  # free variable
end

function _add_objective!(model::Model, data_cache::CacheLPModel)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        data_cache.objective_function,
    )
    return
end

"""
    Base.read!(io::IO, model::FileFormats.LP.Model)

Read `io` in the LP file format and store the result in `model`.
"""
function Base.read!(io::IO, model::Model)
    if !MOI.is_empty(model)
        error("Cannot read in file because model is not empty.")
    end
    data_cache = CacheLPModel()
    section = Val{:none}
    while !eof(io)
        line = string(strip(readline(io)))
        line = _strip_comment(line)
        if line == "" # skip blank lines
            continue
        end
        if haskey(_KEYWORDS, lowercase(line)) # section has changed
            section = _KEYWORDS[lowercase(line)]
            _set_sense!(section, model, line)
            continue
        end
        _parse_section!(section, model, data_cache, line)
    end
    _add_objective!(model, data_cache)
    return
end

end
