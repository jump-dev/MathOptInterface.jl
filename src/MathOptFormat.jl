module MathOptFormat

using JSON

using MathOptInterface
const MOI = MathOptInterface

const Object = Dict{String, Any}

immutable MOFFile
    d::Object
    variables::Dict{MOI.VariableReference, String}
end
MOFFile() = MOFFile(
    Dict(
        "version" => v"0.1",
        "sense"   => "min",
        "variables" => String[],
        "objective" => Object(),
        "constraints" => Object[]
    ),
    Dict{MOI.VariableReference, String}()
)
Base.getindex(m, key) = getindex(m.d, key)
Base.setindex!(m, key, value) = setindex!(m.d, key, value)

save(f::IO, m::MOFFile) = write(io, JSON.json(m.d))
function save(f::String, m::MOFFile)
    open(f, "w") do io
        write(io, m
    end
end

Object(::MOI.MaxSense) = "max"
Object(::MOI.MinSense) = "min"
function MOI.setobjective!(m::MOFFile, sense, func)
    m["sense"] = Object(sense)
    m["objective"] = Object(m, func)
end

function MOI.addconstraint!(m::MOFFile, func, set)
    push!(
        m["constraints"],
        Dict(
            "set" => Object(set),
            "function" => Object!(m, func)
        )
    )
end

#=
    FUNCTIONS
=#

function getvariable!(m::MOFFile, v::VariableReference)
    if !haskey(m.variables, v)
        m.variables[v] = "x$(v.value)"
        push!(m.d["variables"], m.variables[v])
    end
    m.variables[v]
end

Object!(m::MOFFile, func::MOI.SingleVariable) = Object("head"=>"variable", "name"=>getvariable!(m, func.variable))

Object!(m::MOFFile, func::MOI.VectorOfVariables) = Object("head"=>"variable", "names"=>getvariable!.(m, func.variables))

#=
    SETS
=#

Object(set::MOI.EqualTo) = Object("head" => "EqualTo", "value"=>set.value)
Object(set::MOI.LessThan) = Object("head" => "LessThan", "value"=>set.upper)
Object(set::MOI.GreaterThan) = Object("head" => "GreaterThan", "value"=>set.lower)

end # module
