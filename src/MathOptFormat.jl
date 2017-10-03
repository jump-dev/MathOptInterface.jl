module MathOptFormat

using JSON, DataStructures

using MathOptInterface
const MOI = MathOptInterface

# we use an ordered dict to make the JSON printing nicer
const Object = OrderedDict{String, Any}

immutable MOFFile
    d::Object
    variables::Dict{Any, String}
end
MOFFile() = MOFFile(
    OrderedDict(
        "version" => "0.0",
        "sense"   => "min",
        "variables" => String[],
        "objective" => Object(),
        "constraints" => Object[]
    ),
    Dict{Any, String}()
)
Base.getindex(m, key) = getindex(m.d, key)
Base.setindex!(m, key, value) = setindex!(m.d, key, value)

function save(io::IO, m::MOFFile, indent::Int=0)
    if indent > 0
        write(io, JSON.json(m.d, indent))
    else
        write(io, JSON.json(m.d))
    end
end
function save(f::String, m::MOFFile, indent::Int=0)
    open(f, "w") do io
        write(io, m, indent)
    end
end

include("sets.jl")
include("functions.jl")

function addvariable!(m::MOFFile, name::String)
    push!(m["variables"], name)
end

function addconstraint!(m::MOFFile, func::Object, set::Object)
    push!(m["constraints"],
        Object(
            "set" => set,
            "function" => func
        )
    )
end

function setobjective!(m::MOFFile, sense::String, func::Object)
    if sense != "min" && sense != "max"
        error("Sense $(sense) must be min or max")
    end
    m["sense"] = sense
    m["objective"] = func
end


include("mathoptinterface.jl")

end # module
