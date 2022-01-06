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
    "max" => :Max,
    "maximize" => :Max,
    "maximise" => :Max,
    "maximum" => :Max,
    "min" => :Min,
    "minimize" => :Min,
    "minimise" => :Min,
    "minimum" => :Min,
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
    # m = match(START_REG, variable)
    # if !isnothing(m)
    #     return false
    # end
    m = match(NAME_REG, variable)
    if m !== nothing
        return false
    end
    return true
end

struct _SOSConstraint
    type::Int
    weights::Vector{Float64}
    columns::Vector{String}
end

# TODO Obj constant
mutable struct TempLPModel
    model_name::String
    A::Vector{Vector{Tuple{Int,Float64}}}
    c::Vector{Float64}
    col_lower::Vector{Float64}
    col_upper::Vector{Float64}
    row_lower::Vector{Float64}
    row_upper::Vector{Float64}
    sense::Symbol
    colcat::Vector{Symbol}
    sos_constraints::Vector{_SOSConstraint}
    col_to_name::Vector{String}
    row_to_name::Vector{String}
    open_constraint::Bool
    maximum_length::Int
    objective_constant::Float64
    function TempLPModel(options::Options)
        return new(
            "",
            Vector{Vector{Tuple{Int,Float64}}}[],
            Float64[],
            Float64[],
            Float64[],
            Float64[],
            Float64[],
            :Min,
            Symbol[],
            _SOSConstraint[],
            String[],
            String[],
            false,
            options.maximum_length,
            0.0,
        )
    end
end

_set_sense!(T, data::TempLPModel, line) = nothing
function _set_sense!(::Type{Val{:obj}}, data::TempLPModel, line)
    return data.sense = _SENSE_ALIAS[lowercase(line)]
end

function _add_new_variable!(data::TempLPModel, name::String)
    push!(data.col_lower, -Inf)
    push!(data.col_upper, Inf)
    push!(data.c, 0)
    push!(data.colcat, :Cont)
    push!(data.col_to_name, name)
    return
end

function _get_variable_index!(data::TempLPModel, variable::String)
    i = findfirst(isequal(variable), data.col_to_name)
    if isnothing(i)
        if !_verify_name(variable, data.maximum_length)
            error("Invalid variable name $variable")
        end
        _add_new_variable!(data, variable)
        return length(data.col_to_name)
    end
    return i
end

function _tokenize(line::AbstractString)
    items = String.(split(line, " "))
    return items[items.!=""]
end

function _parse_float(val::AbstractString)
    if lowercase(val) == "-inf" || lowercase(val) == "-infinity"
        return -Inf
    elseif lowercase(val) == "+inf" || lowercase(val) == "+infinity"
        return Inf
    else
        return parse(Float64, val)
    end
end

function _parse_affine_terms!(
    data::TempLPModel,
    tokens::Vector{String},
    section::AbstractString,
    line::AbstractString,
)
    v_idx = Int[]
    v_coeff = Float64[]
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
                data.objective_constant += obj_constant
                continue
            catch
            end
        end
        idx = _get_variable_index!(data, variable)
        push!(v_idx, idx)
        if length(tokens) > 0
            coef_token = pop!(tokens)
        else
            coeff = 1.0
            push!(v_coeff, coeff)
            continue
        end
        try
            if coef_token == "+"
                coeff = 1.0
                push!(v_coeff, coeff)
                continue
            elseif coef_token == "-"
                coeff = -1.0
                push!(v_coeff, coeff)
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
        push!(v_coeff, coeff)
    end
    return v_idx, v_coeff
end

function _parse_sos!(data::TempLPModel, line::AbstractString)
    tokens = _tokenize(line)
    if length(tokens) < 3
        error(string("Malformed SOS constraint: ", line))
    end
    if tokens[2] == "S1::"
        order = 1
    elseif tokens[2] == "S2::"
        order = 2
    else
        error("SOS of type $(tokens[2]) not recognised")
    end
    names = String[]
    weights = Float64[]
    for token in tokens[3:end]
        items = split(token, ":")
        if length(items) != 2
            error(string("Invalid sequence: ", token))
        end
        push!(names, String(items[1]))
        _get_variable_index!(data, names[end])
        push!(weights, _parse_float(String(items[2])))
    end
    push!(data.sos_constraints, _SOSConstraint(order, weights, names))
    return
end

function _parse_variable_type!(
    data::TempLPModel,
    line::AbstractString,
    cat::Symbol,
)
    items = _tokenize(line)
    for v in items
        i = _get_variable_index!(data, v)
        data.colcat[i] = cat
    end
end

function _parse_section!(
    ::Type{Val{:none}},
    data::TempLPModel,
    line::AbstractString,
)
    return nothing
end
function _parse_section!(
    ::Type{Val{:quit}},
    data::TempLPModel,
    line::AbstractString,
)
    return error("Corrupted LP File. You have the lne $(line) after an end.")
end
function _parse_section!(::Type{Val{:integer}}, data, line)
    return _parse_variable_type!(data, line, :Int)
end
function _parse_section!(::Type{Val{:binary}}, data, line)
    return _parse_variable_type!(data, line, :Bin)
end

function _parse_section!(
    ::Type{Val{:obj}},
    data::TempLPModel,
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
        return
    end
    v_idx, v_coeff = _parse_affine_terms!(data, tokens, "objective", line)
    data.c[v_idx] = v_coeff
    return
end

function _parse_section!(
    ::Type{Val{:constraints}},
    data::TempLPModel,
    line::AbstractString,
)
    if match(r" S([0-9]):: ", line) !== nothing
        # it's an SOS constraint
        _parse_sos!(data, line)
        return
    end
    if data.open_constraint == false
        push!(data.row_to_name, "R$(length(data.row_to_name) + 1)")
        push!(data.row_lower, -Inf)
        push!(data.row_upper, Inf)
    end
    if occursin(":", line)
        if data.open_constraint == true
            error("Malformed constraint $(line). Is the previous one valid?")
        end
        # throw away name
        m = match(r"(.*?)\:(.*)", line)
        data.row_to_name[end] = String(m[1])
        line = String(m[2])
    end
    data.open_constraint = true

    tokens = _tokenize(line)
    if length(tokens) == 0 # no entries
        return
    elseif length(tokens) >= 2 && haskey(_CONSTRAINT_SENSE, tokens[end-1])# test if constraint ends this line
        rhs = _parse_float(pop!(tokens))
        sym = pop!(tokens)
        if _CONSTRAINT_SENSE[sym] == :le
            data.row_upper[end] = rhs
        elseif _CONSTRAINT_SENSE[sym] == :ge
            data.row_lower[end] = rhs
        elseif _CONSTRAINT_SENSE[sym] == :eq
            data.row_lower[end] = rhs
            data.row_upper[end] = rhs
        end
        data.open_constraint = false # finished
    end
    v_idx, v_coeff = _parse_affine_terms!(data, tokens, "constraint", line)
    row = length(data.row_to_name)
    push!(data.A, Tuple{Int,Float64}[])
    for j in 1:length(v_idx)
        push!(data.A[row], (v_idx[j], v_coeff[j]))
    end
    return
end

_bound_error(line::AbstractString) = error("Unable to parse bound: $(line)")
function _parse_section!(
    ::Type{Val{:bounds}},
    data::TempLPModel,
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
            lb = _parse_float(items[1])
            ub = _parse_float(items[5])
        elseif (items[2] == ">=" || items[2] == ">") &&
               (items[4] == ">=" || items[4] == ">") # ge
            lb = _parse_float(items[5])
            ub = _parse_float(items[1])
        else
            _bound_error(line)
        end
    elseif length(items) == 3 # one sided
        v = items[1]
        if items[2] == "<=" || items[2] == "<" # le
            ub = _parse_float(items[3])
            if ub > 0.0
                lb = 0.0
            else
                lb = -Inf
            end
        elseif items[2] == ">=" || items[2] == ">" # ge
            lb = _parse_float(items[3])
            ub = +Inf
        elseif items[2] == "==" || items[2] == "=" # eq
            lb = ub = _parse_float(items[3])
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
    i = _get_variable_index!(data, v)
    data.col_lower[i] = lb
    data.col_upper[i] = ub
    return
end

# Repeated from MPS
function bounds_to_set(lower, upper)
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

# Very close from MPS
function _add_variable(model::Model, data::TempLPModel, variable_map, i, name)
    x = MOI.add_variable(model)
    variable_map[name] = x
    MOI.set(model, MOI.VariableName(), x, name)
    set = bounds_to_set(data.col_lower[i], data.col_upper[i])
    if set !== nothing
        MOI.add_constraint(model, x, set)
    end
    if data.colcat[i] == :Int
        MOI.add_constraint(model, x, MOI.Integer())
    elseif data.colcat[i] == :Bin
        MOI.add_constraint(model, x, MOI.ZeroOne())
    end
    return
end

# Very close from MPS
function _add_objective(
    model::Model,
    data::TempLPModel,
    variable_map::Dict{String,MOI.VariableIndex},
)
    if data.sense == :Min
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    else
        MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            [
                MOI.ScalarAffineTerm(data.c[i], variable_map[v]) for
                (i, v) in enumerate(data.col_to_name) if !iszero(data.c[i])
            ],
            data.objective_constant,
        ),
    )
    return
end

# Very close from MPS
function _add_linear_constraint(
    model::Model,
    data::TempLPModel,
    variable_map,
    j,
    c_name,
    set,
)
    terms = MOI.ScalarAffineTerm{Float64}[
        MOI.ScalarAffineTerm(coef, variable_map[data.col_to_name[i]]) for
        (i, coef) in data.A[j]
    ]
    c = MOI.add_constraint(model, MOI.ScalarAffineFunction(terms, 0.0), set)
    MOI.set(model, MOI.ConstraintName(), c, c_name)
    return
end

# Very close from MPS
function copy_to(model::Model, data::TempLPModel)
    MOI.set(model, MOI.Name(), data.model_name)
    variable_map = Dict{String,MOI.VariableIndex}()
    for (i, name) in enumerate(data.col_to_name)
        _add_variable(model, data, variable_map, i, name)
    end
    _add_objective(model, data, variable_map)
    for (j, c_name) in enumerate(data.row_to_name)
        set = bounds_to_set(data.row_lower[j], data.row_upper[j])
        if set !== nothing
            _add_linear_constraint(model, data, variable_map, j, c_name, set)
        end
        # `else` is a free constraint. Don't add it.
    end
    for sos in data.sos_constraints
        MOI.add_constraint(
            model,
            MOI.VectorOfVariables([variable_map[x] for x in sos.columns]),
            sos.type == 1 ? MOI.SOS1(sos.weights) : MOI.SOS2(sos.weights),
        )
    end
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
    options = get_options(model)
    data = TempLPModel(options)
    section = Val{:none}
    while !eof(io)
        line = string(strip(readline(io)))
        line = _strip_comment(line)
        if line == "" # skip blank lines
            continue
        end
        if haskey(_KEYWORDS, lowercase(line)) # section has changed
            section = _KEYWORDS[lowercase(line)]
            _set_sense!(section, data, line)
            continue
        end
        _parse_section!(section, data, line)
    end
    copy_to(model, data)
    return
end

end
