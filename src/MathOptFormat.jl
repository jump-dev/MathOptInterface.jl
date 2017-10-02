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

function getvariable!(m::MOFFile, v::MOI.VariableReference)
    if !haskey(m.variables, v)
        m.variables[v] = "x$(v.value)"
        push!(m.d["variables"], m.variables[v])
    end
    m.variables[v]
end

include("sets.jl")
include("functions.jl")
include("mathoptinterface.jl")

end # module
