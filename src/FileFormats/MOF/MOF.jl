# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module MOF

import ..FileFormats
import JSON
import MathOptInterface as MOI

"""
    SCHEMA_PATH::String

The path to the latest version of the MathOptFormat schema supported by
MathOptInterface.
"""
const SCHEMA_PATH = joinpath(@__DIR__, "mof.schema.json")

const _SUPPORTED_VERSIONS = (
    v"1.7",
    v"1.6",
    v"1.5",
    v"1.4",
    v"1.3",
    v"1.2",
    v"1.1",
    v"1.0",
    v"0.6",
    v"0.5",
    v"0.4",
)

function _parse_mof_version(version)
    return VersionNumber(version["major"], version["minor"])
end

struct Nonlinear <: MOI.AbstractScalarFunction
    expr::Expr
end
Base.copy(nonlinear::Nonlinear) = Nonlinear(copy(nonlinear.expr))
MOI.Utilities.canonicalize!(nonlinear::Nonlinear) = nonlinear

MOI.Utilities.@model(
    InnerModel,
    (MOI.ZeroOne, MOI.Integer),
    (
        MOI.EqualTo,
        MOI.GreaterThan,
        MOI.LessThan,
        MOI.Interval,
        MOI.Semicontinuous,
        MOI.Semiinteger,
        MOI.Parameter,
    ),
    (
        MOI.Reals,
        MOI.Zeros,
        MOI.Nonnegatives,
        MOI.Nonpositives,
        MOI.HyperRectangle,
        MOI.SecondOrderCone,
        MOI.RotatedSecondOrderCone,
        MOI.GeometricMeanCone,
        MOI.ExponentialCone,
        MOI.DualExponentialCone,
        MOI.NormOneCone,
        MOI.NormInfinityCone,
        MOI.NormCone,
        MOI.NormSpectralCone,
        MOI.NormNuclearCone,
        MOI.RelativeEntropyCone,
        MOI.RootDetConeTriangle,
        MOI.RootDetConeSquare,
        MOI.LogDetConeTriangle,
        MOI.LogDetConeSquare,
        MOI.PositiveSemidefiniteConeTriangle,
        MOI.Scaled{MOI.PositiveSemidefiniteConeTriangle},
        MOI.PositiveSemidefiniteConeSquare,
        MOI.HermitianPositiveSemidefiniteConeTriangle,
        MOI.AllDifferent,
        MOI.Circuit,
        MOI.CountAtLeast,
        MOI.CountBelongs,
        MOI.CountDistinct,
        MOI.CountGreaterThan,
        MOI.Cumulative,
        MOI.Path,
    ),
    (
        MOI.PowerCone,
        MOI.DualPowerCone,
        MOI.SOS1,
        MOI.SOS2,
        MOI.BinPacking,
        MOI.Table,
    ),
    (Nonlinear, MOI.ScalarNonlinearFunction),
    (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
    (MOI.VectorOfVariables, MOI.VectorNonlinearFunction),
    (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction)
)

# Indicator is handled by UniversalFallback.
# Reified is handled by UniversalFallback.
# Scaled is handled by UniversalFallback.

const Model{T} = MOI.Utilities.UniversalFallback{InnerModel{T}}

struct Options
    print_compact::Bool
    warn::Bool
    differentiation_backend::MOI.Nonlinear.AbstractAutomaticDifferentiation
    use_nlp_block::Union{Bool,Nothing}
end

function get_options(m::Model)
    return get(
        m.model.ext,
        :MOF_OPTIONS,
        Options(false, false, MOI.Nonlinear.SparseReverseMode(), nothing),
    )
end

"""
    Model(;
        print_compact::Bool = false,
        warn::Bool = false,
        differentiation_backend::MOI.Nonlinear.AbstractAutomaticDifferentiation =
            MOI.Nonlinear.SparseReverseMode(),
        use_nlp_block::Union{Bool,Nothing} = nothing,
        coefficient_type::Type{T} = Float64,
    ) where {T}

Create an empty instance of FileFormats.MOF.Model.

Keyword arguments are:

 - `print_compact::Bool=false`: print the JSON file in a compact format without
   spaces or newlines.

 - `warn::Bool=false`: print a warning when variables or constraints are renamed

 - `differentiation_backend::MOI.Nonlinear.AbstractAutomaticDifferentiation = MOI.Nonlinear.SparseReverseMode()`:
   automatic differentiation backend to use when reading models with nonlinear
   constraints and objectives.

  `use_nlp_block::Bool=true`: if `true` parse `"ScalarNonlinearFunction"`
   into an `MOI.NLPBlock`. If `false`, `"ScalarNonlinearFunction"` are parsed as
   `MOI.ScalarNonlinearFunction` functions.

 - `coefficient_type::Type{T} = Float64`: the supported type to use when reading
   and writing files.
"""
function Model(;
    print_compact::Bool = false,
    warn::Bool = false,
    differentiation_backend::MOI.Nonlinear.AbstractAutomaticDifferentiation = MOI.Nonlinear.SparseReverseMode(),
    use_nlp_block::Union{Bool,Nothing} = nothing,
    coefficient_type::Type{T} = Float64,
) where {T}
    model = MOI.Utilities.UniversalFallback(InnerModel{T}())
    model.model.ext[:MOF_OPTIONS] =
        Options(print_compact, warn, differentiation_backend, use_nlp_block)
    return model
end

Base.summary(io::IO, ::Model) = print(io, "MOI.FileFormats.MOF.Model")

include("read.jl")
include("write.jl")

end
