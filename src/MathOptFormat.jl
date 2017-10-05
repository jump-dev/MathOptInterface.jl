module MathOptFormat

using JSON, DataStructures

using MathOptInterface
const MOI = MathOptInterface

# we use an ordered dict to make the JSON printing nicer
const Object = OrderedDict{String, Any}

immutable MOFFile <: MOI.AbstractStandaloneInstance
    d::Object
    # an extension dictionary to help MOI reading/writing
    # should be improved later
    ext::Dict
end
MOFFile() = MOFFile(
    OrderedDict(
        "version" => "0.0",
        "sense"   => "min",
        "variables" => String[],
        "objective" => Object(),
        "constraints" => Object[]
    ),
    Dict()
)

immutable MOFWriter <: MOI.AbstractSolver end
MOI.SolverInstance(::MOFWriter) = MOFFile()

"""
    MOFFile(file::String)

Read a MOF file located at `file`

### Example

    MOFFile("path/to/model.mof.json")
"""
function MOFFile(file::String)
    d = open(file, "r") do io
        JSON.parse(io, dicttype=OrderedDict{String, Any})
    end
    MOFFile(d, Dict{Any, Any}())
end

# overload getset for m.d
Base.getindex(m::MOFFile, key) = getindex(m.d, key)
Base.setindex!(m::MOFFile, key, value) = setindex!(m.d, key, value)

include("sets.jl")
include("functions.jl")

include("writer.jl")
include("reader.jl")

end # module
