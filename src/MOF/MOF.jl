module MOF

import ..MathOptFormat
import DataStructures, JSON, JSONSchema, MathOptInterface

# The file /deps/deps.jl contains a constant `SCHEMA_PATH` that points to the
# latest version of MathOptFormat, which should have been downloaded on `build`.
include(joinpath(dirname(dirname(@__DIR__)), "deps", "deps.jl"))

# Extract the MathOptFormat version number from the JSON schema.
const VERSION = let data = JSON.parsefile(SCHEMA_PATH, use_mmap=false)
    data["properties"]["version"]["const"]
end

# we use an ordered dict to make the JSON printing nicer
const Object = DataStructures.OrderedDict{String, Any}

const MOI = MathOptInterface

struct Nonlinear <: MOI.AbstractScalarFunction
    expr::Expr
end
Base.copy(nonlinear::Nonlinear) = Nonlinear(copy(nonlinear.expr))

MOI.Utilities.@model(InnerModel,
    (MOI.ZeroOne, MOI.Integer),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval,
        MOI.Semicontinuous, MOI.Semiinteger),
    (MOI.Reals, MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives,
        MOI.SecondOrderCone, MOI.RotatedSecondOrderCone,
        MOI.GeometricMeanCone,
        MOI.RootDetConeTriangle, MOI.RootDetConeSquare,
        MOI.LogDetConeTriangle, MOI.LogDetConeSquare,
        MOI.PositiveSemidefiniteConeTriangle, MOI.PositiveSemidefiniteConeSquare,
        MOI.ExponentialCone, MOI.DualExponentialCone, MOI.NormOneCone,
        MOI.NormInfinityCone),
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
    validate::Bool
    warn::Bool
end

function get_options(m::Model)
    return get(m.model.ext, :MOF_OPTIONS, Options(false, true, false))
end

"""
    Model(; kwargs...)

Create an empty instance of MathOptFormat.MOF.Model.

Keyword arguments are:

 - `print_compact::Bool=false`: print the JSON file in a compact format without
   spaces or newlines.

 - `validate::Bool=true`: validate each file prior to reading against the MOF
   schema

 - `warn::Bool=false`: print a warning when variables or constraints are renamed
"""
function Model(;
    print_compact::Bool = false, validate::Bool = true, warn::Bool = false
)
    model = MOI.Utilities.UniversalFallback(InnerModel{Float64}())
    model.model.ext[:MOF_OPTIONS] = Options(print_compact, validate, warn)
    return model
end

function Base.show(io::IO, ::Model)
    print(io, "A MathOptFormat Model")
    return
end

"""
    validate(filename::String)

Validate that the MOF file `filename` conforms to the MOF JSON schema. Returns
`nothing` if the file is valid, otherwise throws an error describing why the
file is not valid.
"""
function validate(filename::String; compression::MathOptFormat.AbstractCompressionScheme=MathOptFormat.AutomaticCompression())
    compression = MathOptFormat._automatic_compression(filename, compression)
    MathOptFormat._compressed_open(filename, "r", compression) do io
        validate(io)
    end
    return
end

function validate(io::IO)
    object = JSON.parse(io)
    seekstart(io)
    mof_schema = JSONSchema.Schema(JSON.parsefile(SCHEMA_PATH, use_mmap=false))
    if !JSONSchema.isvalid(object, mof_schema)
        error("Unable to read file because it does not conform to the MOF " *
              "schema: ", JSONSchema.diagnose(object, mof_schema))
    end
    return
end

include("nonlinear.jl")

include("read.jl")
include("write.jl")

end
