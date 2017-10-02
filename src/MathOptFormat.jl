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

save(f::IO, m::MOFFile) = write(io, JSON.json(m.d))
function save(f::String, m::MOFFile)
    open(f, "w") do io
        write(io, m
    end
end

function MOI.setobjective!(m::MOFFile, sense, func)

end

function MOI.addconstraint!(m::MOFFile, func, set)
    Dict(
        "set" => getset(set),
        "function" => getfunction!(func)
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

getfunction!(m::MOFFile, func::MOI.SingleVariable) = Object("head"=>"variable", "name"=>getvariable!(m, func.variable))

getfunction!(m::MOFFile, func::MOI.VectorOfVariables) = Object("head"=>"variable", "names"=>getvariable!.(m, func.variables))


#=
    SETS
=#

getset(set::MOI.EqualTo) = Object("head" => "EqualTo", "value"=>set.value)
getset(set::MOI.LessThan) = Object("head" => "LessThan", "value"=>set.upper)
getset(set::MOI.GreaterThan) = Object("head" => "GreaterThan", "value"=>set.lower)

end # module
