module MOF

import ..FileFormats
import OrderedCollections
import JSON
import MathOptInterface

const MOI = MathOptInterface

const SCHEMA_PATH = joinpath(@__DIR__, "mof.0.6.schema.json")
const VERSION = v"0.6"

const OrderedObject = OrderedCollections.OrderedDict{String,Any}
const UnorderedObject = Dict{String,Any}
const Object = Union{OrderedObject,UnorderedObject}

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
    ),
    (
        MOI.Reals,
        MOI.Zeros,
        MOI.Nonnegatives,
        MOI.Nonpositives,
        MOI.SecondOrderCone,
        MOI.RotatedSecondOrderCone,
        MOI.GeometricMeanCone,
        MOI.ExponentialCone,
        MOI.DualExponentialCone,
        MOI.NormOneCone,
        MOI.NormInfinityCone,
        MOI.NormSpectralCone,
        MOI.NormNuclearCone,
        MOI.RelativeEntropyCone,
        MOI.RootDetConeTriangle,
        MOI.RootDetConeSquare,
        MOI.LogDetConeTriangle,
        MOI.LogDetConeSquare,
        MOI.PositiveSemidefiniteConeTriangle,
        MOI.PositiveSemidefiniteConeSquare,
    ),
    (MOI.PowerCone, MOI.DualPowerCone, MOI.SOS1, MOI.SOS2),
    (Nonlinear,),
    (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
    (MOI.VectorOfVariables,),
    (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction)
)

# IndicatorSet is handled by UniversalFallback.

const Model = MOI.Utilities.UniversalFallback{InnerModel{Float64}}

struct Options
    print_compact::Bool
    warn::Bool
end

function get_options(m::Model)
    return get(m.model.ext, :MOF_OPTIONS, Options(false, false))
end

"""
    Model(; kwargs...)

Create an empty instance of FileFormats.MOF.Model.

Keyword arguments are:

 - `print_compact::Bool=false`: print the JSON file in a compact format without
   spaces or newlines.

 - `warn::Bool=false`: print a warning when variables or constraints are renamed
"""
function Model(; print_compact::Bool = false, warn::Bool = false)
    model = MOI.Utilities.UniversalFallback(InnerModel{Float64}())
    model.model.ext[:MOF_OPTIONS] = Options(print_compact, warn)
    return model
end

function Base.show(io::IO, ::Model)
    print(io, "A MathOptFormat Model")
    return
end

include("nonlinear.jl")

include("read.jl")
include("write.jl")

end
