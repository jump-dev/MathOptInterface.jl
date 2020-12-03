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

MOI.Utilities.@model(Model,
    (MOI.ZeroOne, MOI.Integer),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval),
    (),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (),
    ()
)
function MOI.supports(
    ::Model{T},
    ::MOI.ObjectiveFunction{
        <:MOI.ScalarQuadraticFunction{T}
    }
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
function Model(;
    maximum_length::Int = 255, warn::Bool = false
)
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
    func::MOI.SingleVariable,
    variable_names::Dict{MOI.VariableIndex, String}
)
    print(io, variable_names[func.variable])
    return
end

function write_function(
    io::IO,
    model::Model,
    func::MOI.ScalarAffineFunction{Float64},
    variable_names::Dict{MOI.VariableIndex, String}
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

            print(io, " ", variable_names[term.variable_index])
        end
    end
    return
end

function write_constraint_suffix(io::IO, set::MOI.LessThan)
    print(io, " <= ", )
    print_shortest(io, set.upper)
    println(io)
    return
end

function write_constraint_suffix(io::IO, set::MOI.GreaterThan)
    print(io, " >= ", )
    print_shortest(io, set.lower)
    println(io)
    return
end

function write_constraint_suffix(io::IO, set::MOI.EqualTo)
    print(io, " = ", )
    print_shortest(io, set.value)
    println(io)
    return
end

function write_constraint_suffix(io::IO, set::MOI.Interval)
    print(io, " <= ", )
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
    variable_names::Dict{MOI.VariableIndex, String};
    write_name::Bool = true
)
    func = MOI.get(model, MOI.ConstraintFunction(), index)
    set = MOI.get(model, MOI.ConstraintSet(), index)
    if write_name
        print(io, MOI.get(model, MOI.ConstraintName(), index), ": ")
    end
    write_constraint_prefix(io, set)
    write_function(io, model, func, variable_names)
    write_constraint_suffix(io, set)
end

const SCALAR_SETS = (
    MOI.LessThan{Float64}, MOI.GreaterThan{Float64}, MOI.EqualTo{Float64},
    MOI.Interval{Float64}
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
    io::IO, model::Model, variable_names::Dict{MOI.VariableIndex, String}
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
            s -> s[1:min(length(s), options.maximum_length)]
        ]
    )
    variable_names = Dict{MOI.VariableIndex, String}(
        index => MOI.get(model, MOI.VariableName(), index)
        for index in MOI.get(model, MOI.ListOfVariableIndices())
    )
    write_sense(io, model)
    write_objective(io, model, variable_names)
    println(io, "subject to")
    for S in SCALAR_SETS
        for index in MOI.get(
            model,
            MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, S}()
        )
            write_constraint(
                io, model, index, variable_names; write_name = true
            )
        end
    end
    println(io, "Bounds")
    free_variables = Set(keys(variable_names))
    for S in SCALAR_SETS
        for index in MOI.get(
            model,
            MOI.ListOfConstraintIndices{MOI.SingleVariable, S}()
        )
            delete!(free_variables, MOI.VariableIndex(index.value))
            write_constraint(
                io, model, index, variable_names; write_name = false
            )
        end
    end
    for index in MOI.get(
        model, MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.ZeroOne}()
    )
        delete!(free_variables, MOI.VariableIndex(index.value))
    end
    for variable in sort(collect(free_variables), by = x -> x.value)
        println(io, variable_names[variable], " free")
    end
    for (S, str_S) in [(MOI.Integer, "General"), (MOI.ZeroOne, "Binary")]
        indices = MOI.get(
            model, MOI.ListOfConstraintIndices{MOI.SingleVariable, S}()
        )
        if length(indices) > 0
            println(io, str_S)
            for index in indices
                write_function(
                    io,
                    model,
                    MOI.get(model, MOI.ConstraintFunction(), index),
                    variable_names
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

function Base.read!(io::IO, model::Model)
    error("read! is not implemented for LP files.")
end

end
