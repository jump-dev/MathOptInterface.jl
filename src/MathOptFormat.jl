module MathOptFormat

using JSON, DataStructures

using MathOptInterface
const MOI = MathOptInterface

# we use an ordered dict to make the JSON printing nicer
const Object = OrderedDict{String, Any}

immutable MOFFile
    d::Object
    variables::Dict{MOI.VariableReference, String}
end
MOFFile() = MOFFile(
    OrderedDict(
        "version" => "0.0",
        "sense"   => "min",
        "variables" => String[],
        "objective" => Object(),
        "constraints" => Object[]
    ),
    Dict{MOI.VariableReference, String}()
)
Base.getindex(m, key) = getindex(m.d, key)
Base.setindex!(m, key, value) = setindex!(m.d, key, value)

save(io::IO, m::MOFFile) = write(io, JSON.json(m.d, 1))
function save(f::String, m::MOFFile)
    open(f, "w") do io
        write(io, m)
    end
end

function Object(sense::MOI.OptimizationSense)
    if sense == MOI.MaxSense
        return "max"
    elseif sense == MOI.MinSense
        return "min"
    end
    error("Sense $(sense) not recognised.")
end

function MOI.setobjective!(m::MOFFile, sense, func)
    m["sense"] = Object(sense)
    m["objective"] = Object!(m, func)
end

function MOI.addconstraint!(m::MOFFile, func, set)
    push!(
        m["constraints"],
        Object(
            "set" => Object(set),
            "function" => Object!(m, func)
        )
    )
end

#=
    FUNCTIONS
=#

function getvariable!(m::MOFFile, v::MOI.VariableReference)
    if !haskey(m.variables, v)
        m.variables[v] = "x$(v.value)"
        push!(m.d["variables"], m.variables[v])
    end
    m.variables[v]
end

"""
    {
        "head": "variable",
        "value": "x1"
    }
"""
Object!(m::MOFFile, func::MOI.SingleVariable) = Object("head"=>"variable", "name"=>getvariable!(m, func.variable))

"""
    {
        "head": "variableset",
        "value": ["x1", "x2"]
    }
"""
Object!(m::MOFFile, func::MOI.VectorOfVariables) = Object("head"=>"variableset", "names"=>getvariable!.(m, func.variables))

"""
    MathOptInterface.ScalarAffineFunction

A special case to express an affine expression <variables, coefficients> +
constant. Other fields are:

 - `variables`: an array of variable names. Every element must be a string that
 corresponds to the name of a variable. Objects and numeric values are illegal.
 - `coefficients`: an array of numeric values. Every element must be a numeric
 value. Objects and strings are illegal.
 - `constant`: This must be a numeric value. Objects and strings are illegal.

### Example: x + y + 3

    {
      head: "linear",
      variables: ["x", "y"],
      coefficients: [1.0, 2.0],
      constant: 3.0
    }
"""
Object!(m::MOFFile, func::MOI.ScalarAffineFunction) = Object(
    "head"         => "linear",
    "variables"    => getvariable!.(m, func.variables),
    "coefficients" => func.coefficients,
    "constant"     => func.constant
)

#=
    SETS
=#

"""
    {
        "head": "EqualTo",
        "value": 3.0
    }
"""
Object(set::MOI.EqualTo) = Object("head" => "EqualTo", "value"=>set.value)

"""
    {
        "head": "LessThan",
        "value": 3.0
    }
"""
Object(set::MOI.LessThan) = Object("head" => "LessThan", "value"=>set.upper)

"""
    {
        "head": "GreaterThan",
        "value": 3.0
    }
"""
Object(set::MOI.GreaterThan) = Object("head" => "GreaterThan", "value"=>set.lower)

end # module
