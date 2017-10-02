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

save(io::IO, m::MOFFile) = write(io, JSON.json(m.d, 1))
function save(f::String, m::MOFFile)
    open(f, "w") do io
        write(io, m)
    end
end

include("sets.jl")
include("functions.jl")
include("mathoptinterface.jl")

end # module
