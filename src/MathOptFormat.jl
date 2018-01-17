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
struct MOFInstance <: MOI.AbstractStandaloneInstance
    d::Object
    # an extension dictionary to help MOI reading/writing
    # should be improved later
    namemap::Dict{String, MOI.VariableIndex}
    # varmap
    varmap::Dict{MOI.VariableIndex, Int}
    # constrmap
    constrmap::Dict{UInt64, Int}
    current_reference::CurrentReference
end

MOFInstance() = MOFInstance(
    OrderedDict(
        "name"    => "MathOptFormat Model",
        "version" => "0.0",
        "sense"   => "min",
        "variables" => Object[],
        "objective" => Object("head"=>"ScalarAffineFunction", "variables"=>String[], "coefficients"=>Float64[], "constant"=>0.0),
        "constraints" => Object[]
    ),
    Dict{String, MOI.VariableIndex}(),
    Dict{MOI.VariableIndex, Int}(),
    Dict{UInt64, Int}(),
    CurrentReference(UInt64(0), UInt64(0))
)

# overload getset for m.d
Base.getindex(m::MOFInstance, key) = getindex(m.d, key)
Base.setindex!(m::MOFInstance, key, value) = setindex!(m.d, key, value)

function MOI.write(m::MOFInstance, io::IO, indent::Int=0)
    if indent > 0
        write(io, JSON.json(m.d, indent))
    else
        write(io, JSON.json(m.d))
    end
end
function MOI.write(m::MOFInstance, f::String, indent::Int=0)
    open(f, "w") do io
        MOI.write(m, io, indent)
    end
end

include("variables.jl")
include("sets.jl")
include("functions.jl")
include("constraints.jl")
include("attributes.jl")
include("reader.jl")

end # module
