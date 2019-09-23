module LP

import MathOptInterface
const MOI = MathOptInterface

MOI.Utilities.@model(InnerModel,
    (MOI.ZeroOne, MOI.Integer),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval),
    (),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (),
    ()
)

struct Options
    maximum_length::Int
    warn::Bool
    warn_once::Bool
    warned_start::Set{Char}
    warned_illegal::Set{Char}
end

function get_options(m::InnerModel)
    default_options = Options(255, false, false, Set{Char}(), Set{Char}())
    return get(m.ext, :LP_OPTIONS, default_options)
end

"""
    Model(; kwargs...)

Create an empty instance of FileFormats.LP.Model.

Keyword arguments are:

 - `maximum_length::Int=255`: the maximum length for the name of a variable.
   lp_solve 5.0 allows only 16 characters, while CPLEX 12.5+ allow 255.

 - `warn::Bool=false`: print a warning when variables or constraints are renamed.

 - `warn_once::Bool=false`: print a warning when variables or constraints are
   renamed, but only once per kind of replacement (e.g., once per illegal
   character).
"""
function Model(;
    maximum_length::Int = 255, warn::Bool = false, warn_once::Bool = false
)
    model = InnerModel{Float64}()
    options = Options(maximum_length, warn, warn_once, Set{Char}(), Set{Char}())
    model.ext[:LP_OPTIONS] = options
    return model
end

function Base.show(io::IO, ::InnerModel)
    print(io, "A .LP-file model")
    return
end

# ==============================================================================
#
#   MOI.write_to_file
#
# ==============================================================================


const START_REG = r"^([\.0-9eE])"
const NAME_REG = r"([^a-zA-Z0-9\!\"\#\$\%\&\(\)\/\,\.\;\?\@\_\`\'\{\}\|\~])"

function sanitized_name(name::String, options::Options)
    m = match(START_REG, name)
    if m !== nothing
        plural = length(m.match) > 1

        if options.warn || (options.warn_once && !(m.match[1] in options.warned_start))
            @warn("Name $(name) cannot start with a period, a number, e, or E. " *
                  "Prepending an underscore to name.")
            push!(options.warned_start, m.match[1])
        end

        return sanitized_name("_" * name, options)
    end

    m = match(NAME_REG, name)
    if m !== nothing
        plural = length(m.match) > 1

        if options.warn || (options.warn_once && !(m.match[1] in options.warned_illegal))
            @warn("Name $(name) contains $(ifelse(plural, "", "an "))" *
                  "illegal character$(ifelse(plural, "s", "")): " *
                  "\"$(m.match)\". Removing the offending " *
                  "character$(ifelse(plural, "s", "")) from name.")
            push!(options.warned_illegal, m.match[1])
        end

        return sanitized_name(replace(name, NAME_REG => s"_"), options)
    end

    # Truncate at the end to fit as many characters as possible.
    if length(name) > options.maximum_length
        @warn("Name $(name) too long (length: $(length(name)); " *
              "maximum: $(options.maximum_length)). Truncating.")
        return sanitized_name(String(name[1:options.maximum_length]), options)
    end

    return name
end

function write_function(io::IO, model::InnerModel, func::MOI.SingleVariable, sanitized_names::Dict{MOI.VariableIndex, String})
    print(io, sanitized_names[func.variable])
    return
end

function write_function(io::IO, model::InnerModel, func::MOI.ScalarAffineFunction{Float64}, sanitized_names::Dict{MOI.VariableIndex, String})
    is_first_item = true
    if !(func.constant ≈ 0.0)
        Base.Grisu.print_shortest(io, func.constant)
        is_first_item = false
    end
    for term in func.terms
        if !(term.coefficient ≈ 0.0)
            if is_first_item
                Base.Grisu.print_shortest(io, term.coefficient)
                is_first_item = false
            else
                print(io, term.coefficient < 0 ? " - " : " + ")
                Base.Grisu.print_shortest(io, abs(term.coefficient))
            end

            print(io, " ", sanitized_names[term.variable_index])
        end
    end
    return
end

function write_constraint_suffix(io::IO, set::MOI.LessThan)
    print(io, " <= ", )
    Base.Grisu.print_shortest(io, set.upper)
    println(io)
    return
end

function write_constraint_suffix(io::IO, set::MOI.GreaterThan)
    print(io, " >= ", )
    Base.Grisu.print_shortest(io, set.lower)
    println(io)
    return
end

function write_constraint_suffix(io::IO, set::MOI.EqualTo)
    print(io, " = ", )
    Base.Grisu.print_shortest(io, set.value)
    println(io)
    return
end

function write_constraint_suffix(io::IO, set::MOI.Interval)
    print(io, " <= ", )
    Base.Grisu.print_shortest(io, set.upper)
    println(io)
    return
end

function write_constraint_prefix(io::IO, set::MOI.Interval)
    Base.Grisu.print_shortest(io, set.lower)
    print(io, " <= ")
    return
end

write_constraint_prefix(io::IO, set) = nothing

function write_constraint(io::IO, model::InnerModel, index, sanitized_names::Dict{MOI.VariableIndex, String}; write_name::Bool = true)
    func = MOI.get(model, MOI.ConstraintFunction(), index)
    set = MOI.get(model, MOI.ConstraintSet(), index)
    if write_name
        print(io, MOI.get(model, MOI.ConstraintName(), index), ": ")
    end
    write_constraint_prefix(io, set)
    write_function(io, model, func, sanitized_names)
    write_constraint_suffix(io, set)
end

const SCALAR_SETS = (
    MOI.LessThan{Float64}, MOI.GreaterThan{Float64}, MOI.EqualTo{Float64},
    MOI.Interval{Float64}
)

function write_sense(io::IO, model::InnerModel)
    if MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
        println(io, "maximize")
    else
        println(io, "minimize")
    end
    return
end

function write_objective(io::IO, model::InnerModel, sanitized_names::Dict{MOI.VariableIndex, String})
    print(io, "obj: ")
    obj_func_type = MOI.get(model, MOI.ObjectiveFunctionType())
    obj_func = MOI.get(model, MOI.ObjectiveFunction{obj_func_type}())
    write_function(io, model, obj_func, sanitized_names)
    println(io)
    return
end

function MOI.write_to_file(model::InnerModel, io::IO)
    options = get_options(model)
    max_length = options.maximum_length
    # Ensure each variable has a unique name that does not infringe LP constraints.
    MOI.FileFormats.create_unique_names(model, warn = options.warn)
    sanitized_names = Dict{MOI.VariableIndex, String}()
    sanitized_names_set = Set{String}()
    for v in MOI.get(model, MOI.ListOfVariableIndices())
        proposed_sanitized_name = sanitized_name(MOI.get(model, MOI.VariableName(), v), options)
        # In case of duplicate names after sanitization, add a number at the end.
        if proposed_sanitized_name in sanitized_names_set
            # If the name is already too long, make some space for the suffix.
            if length(proposed_sanitized_name) >= max_length
                proposed_sanitized_name = String(proposed_sanitized_name[1:max_length - 2])
            end
            i = 1
            while proposed_sanitized_name * '_' * string(i) in sanitized_names_set
                i += 1

                # If the maximum length constraint would be broken with the *next* i,
                # truncate a bit more the proposed_sanitized_name.
                if length(proposed_sanitized_name * '_' * string(i)) > max_length
                    proposed_sanitized_name = String(proposed_sanitized_name[1:length(proposed_sanitized_name) - 1])
                end
            end
            proposed_sanitized_name *= '_' * string(i)
        end
        push!(sanitized_names_set, proposed_sanitized_name)
        sanitized_names[v] = proposed_sanitized_name
    end

    write_sense(io, model)
    write_objective(io, model, sanitized_names)
    println(io, "subject to")
    for S in SCALAR_SETS
        for index in MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, S}())
            write_constraint(io, model, index, sanitized_names; write_name = true)
        end
    end

    println(io, "Bounds")
    for S in SCALAR_SETS
        for index in MOI.get(model, MOI.ListOfConstraintIndices{MOI.SingleVariable, S}())
            write_constraint(io, model, index, sanitized_names; write_name = false)
        end
    end

    for (S, str_S) in [(MOI.Integer, "General"), (MOI.ZeroOne, "Binary")]
        indices = MOI.get(model, MOI.ListOfConstraintIndices{MOI.SingleVariable, S}())
        if length(indices) > 0
            println(io, str_S)
            for index in indices
                write_function(io, model, MOI.get(model, MOI.ConstraintFunction(), index), sanitized_names)
                println(io)
            end
        end
    end

    println(io, "End")

    return
end

# ==============================================================================
#
#   MOI.read_from_to_file
#
# ==============================================================================

function MOI.read_from_file(model::InnerModel, io::IO)
    error("Read from file is not implemented for LP files.")
end

end
