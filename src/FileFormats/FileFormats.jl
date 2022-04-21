module FileFormats

import MathOptInterface
const MOI = MathOptInterface

import CodecBzip2
import CodecZlib

include("utils.jl")

include("CBF/CBF.jl")
include("LP/LP.jl")
include("MOF/MOF.jl")
include("MPS/MPS.jl")
include("NL/NL.jl")
include("SDPA/SDPA.jl")

"""
    FileFormat

List of accepted export formats.

- `FORMAT_AUTOMATIC`: try to detect the file format based on the file name
- `FORMAT_CBF`: the Conic Benchmark format
- `FORMAT_LP`: the LP file format
- `FORMAT_MOF`: the MathOptFormat file format
- `FORMAT_MPS`: the MPS file format
- `FORMAT_NL`: the AMPL .nl file format
- `FORMAT_REW`: the .rew file format, which is MPS with generic names
- `FORMAT_SDPA`: the SemiDefinite Programming Algorithm format
"""
@enum(
    FileFormat,
    FORMAT_AUTOMATIC,
    FORMAT_CBF,
    FORMAT_LP,
    FORMAT_MOF,
    FORMAT_MPS,
    FORMAT_NL,
    FORMAT_REW,
    FORMAT_SDPA,
)

"""
    Model(
        ;
        format::FileFormat = FORMAT_AUTOMATIC,
        filename::Union{Nothing, String} = nothing,
        kwargs...
    )

Return model corresponding to the `FileFormat` `format`, or, if
`format == FORMAT_AUTOMATIC`, guess the format from `filename`.

The `filename` argument is only needed if `format == FORMAT_AUTOMATIC`.

`kwargs` are passed to the underlying model constructor.
"""
function Model(;
    format::FileFormat = FORMAT_AUTOMATIC,
    filename::Union{Nothing,String} = nothing,
    kwargs...,
)
    if format == FORMAT_CBF
        return CBF.Model(; kwargs...)
    elseif format == FORMAT_LP
        return LP.Model(; kwargs...)
    elseif format == FORMAT_MOF
        return MOF.Model(; kwargs...)
    elseif format == FORMAT_MPS
        return MPS.Model(; kwargs...)
    elseif format == FORMAT_NL
        return NL.Model(; kwargs...)
    elseif format == FORMAT_REW
        return MPS.Model(; generic_names = true, kwargs...)
    elseif format == FORMAT_SDPA
        return SDPA.Model(; kwargs...)
    else
        @assert format == FORMAT_AUTOMATIC
        if filename === nothing
            error("When `format==FORMAT_AUTOMATIC` you must pass a `filename`.")
        end
        for (ext, model) in [
            (".cbf", CBF.Model),
            (".lp", LP.Model),
            (".mof.json", MOF.Model),
            (".mps", MPS.Model),
            (
                ".rew",
                (; kwargs...) -> MPS.Model(; generic_names = true, kwargs...),
            ),
            (".nl", NL.Model),
            (".dat-s", SDPA.Model),
            (".sdpa", SDPA.Model),
        ]
            if endswith(filename, ext) || occursin("$(ext).", filename)
                return model(; kwargs...)
            end
        end
        error("Unable to automatically detect format of $(filename).")
    end
end

const MATH_OPT_FORMATS =
    Union{CBF.Model,LP.Model,MOF.Model,MPS.Model,NL.Model,SDPA.Model}

function MOI.write_to_file(model::MATH_OPT_FORMATS, filename::String)
    compressed_open(filename, "w", AutomaticCompression()) do io
        return write(io, model)
    end
end

function MOI.read_from_file(model::MATH_OPT_FORMATS, filename::String)
    compressed_open(filename, "r", AutomaticCompression()) do io
        return read!(io, model)
    end
end

end
