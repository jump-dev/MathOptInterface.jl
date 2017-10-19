module MathOptFormat

using JSON, DataStructures

using MathOptInterface
const MOI = MathOptInterface

# we use an ordered dict to make the JSON printing nicer
const Object = OrderedDict{String, Any}

mutable struct CurrentReference
    variable::UInt64
    constraint::UInt64
end
struct MOFFile <: MOI.AbstractStandaloneInstance
    d::Object
    # an extension dictionary to help MOI reading/writing
    # should be improved later
    namemap::Dict{String, MOI.VariableReference}
    # varmap
    varmap::Dict{MOI.VariableReference, Int}
    # constrmap
    constrmap::Dict{UInt64, Int}
    current_reference::CurrentReference
end
MOFFile() = MOFFile(
    OrderedDict(
        "version" => "0.0",
        "sense"   => "min",
        "variables" => Object[],
        "objective" => Object(),
        "constraints" => Object[]
    ),
    Dict{String, MOI.VariableReference}(),
    Dict{MOI.VariableReference, Int}(),
    Dict{UInt64, Int}(),
    CurrentReference(UInt64(0), UInt64(0))
)

struct MOFWriter <: MOI.AbstractSolver end
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
    MOFFile(d, Dict{String, MOI.VariableReference}(), Dict{MOI.VariableReference, Int}(), Dict{UInt64, Int}(), CurrentReference(UInt64(0), UInt64(0)))
end

# overload getset for m.d
Base.getindex(m::MOFFile, key) = getindex(m.d, key)
Base.setindex!(m::MOFFile, key, value) = setindex!(m.d, key, value)

function MOI.writeinstance(m::MOFFile, io::IO, indent::Int=0)
    if indent > 0
        write(io, JSON.json(m.d, indent))
    else
        write(io, JSON.json(m.d))
    end
end
function MOI.writeinstance(m::MOFFile, f::String, indent::Int=0)
    open(f, "w") do io
        MOI.writeinstance(m, io, indent)
    end
end

include("variables.jl")
include("sets.jl")
include("functions.jl")
include("constraints.jl")
include("attributes.jl")
include("reader.jl")

function MOI.supportsproblem(m::MOFWriter, obj, constraints::Vector)
    if !Base.method_exists(object!, (MOFFile, obj))
        return false
    end
    for (f, s) in constraints
        if !(Base.method_exists(object!, (MOFFile, f)) && Base.method_exists(object, (s,)))
            return false
        end
    end
    return true
end

end # module
