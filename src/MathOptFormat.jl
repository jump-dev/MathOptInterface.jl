module MathOptFormat

using MathOptInterface
const MOI = MathOptInterface

import GZip

include("MOF/MOF.jl")
include("MPS/MPS.jl")
include("CBF/CBF.jl")

function gzip_open(f::Function, filename::String, mode::String)
    if endswith(filename, ".gz")
        GZip.open(f, filename, mode)
    else
        return open(f, filename, mode)
    end
end

const MATH_OPT_FORMATS = Union{MOF.Model, MPS.Model, CBF.Model}

function MOI.write_to_file(model::MATH_OPT_FORMATS, filename::String)
    gzip_open(filename, "w") do io
        MOI.write_to_file(model, io)
    end
end

function MOI.read_from_file(model::MATH_OPT_FORMATS, filename::String)
    gzip_open(filename, "r") do io
        MOI.read_from_file(model, io)
    end
end

"""
    read_from_file(filename::String)

Create a MOI model by reading `filename`. Type of the returned model depends on
the extension of `filename`.
"""
function read_from_file(filename::String)
    model = if endswith(filename, ".mof.json.gz") || endswith(filename, ".mof.json")
        MOF.Model()
    elseif endswith(filename, ".cbf.gz") || endswith(filename, ".cbf")
        CBF.Model()
    elseif endswith(filename, ".mps.gz") || endswith(filename, ".mps")
        MPS.Model()
    else
        error("File-type of $(filename) not supported by MathOptFormat.jl.")
    end
    MOI.read_from_file(model, filename)
    return model
end

end
