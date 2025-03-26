# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module FileFormats

import MathOptInterface as MOI

import CodecBzip2
import CodecZlib

include("utils.jl")

include("CBF/CBF.jl")
include("LP/LP.jl")
include("MOF/MOF.jl")
include("MPS/MPS.jl")
include("NL/NL.jl")
include("SDPA/SDPA.jl")

MOI.@_documented_enum(
    """
        FileFormat

    List of accepted export formats.
    """,
    FileFormat,
    """
    Try to detect the file format based on the file name.
    """,
    FORMAT_AUTOMATIC,
    """
    The Conic Benchmark format.

    See [`FileFormats.CBF.Model`](@ref) for more details.
    """,
    FORMAT_CBF,
    """
    The LP file format.

    See [`FileFormats.LP.Model`](@ref) for more details.
    """,
    FORMAT_LP,
    """
    The MathOptFormat file format.

    See [`FileFormats.MOF.Model`](@ref) for more details.
    """,
    FORMAT_MOF,
    """
    The MPS file format.

    See [`FileFormats.MPS.Model`](@ref) for more details.
    """,
    FORMAT_MPS,
    """
    The AMPL .nl file format.

    See [`FileFormats.NL.Model`](@ref) for more details.
    """,
    FORMAT_NL,
    """
    The .rew file format, which is equivalent to the MPS format
    ([`FileFormats.FORMAT_MPS`](@ref)) with the `generic_names = true` keyword
    argument set by default.

    See [`FileFormats.MPS.Model`](@ref) for more details.
    """,
    FORMAT_REW,
    """
    The SemiDefinite Programming Algorithm format.

    See [`FileFormats.SDPA.Model`](@ref) for more details.
    """,
    FORMAT_SDPA,
)

"""
    Model(;
        format::FileFormat = FORMAT_AUTOMATIC,
        filename::Union{Nothing, String} = nothing,
        kwargs...
    )

Return model corresponding to the [`FileFormats.FileFormat`](@ref) `format`, or,
if `format == FORMAT_AUTOMATIC`, guess the format from `filename`.

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
