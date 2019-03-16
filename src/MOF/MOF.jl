module MOF

const VERSION = 0

using DataStructures, JSON

import ..MathOptFormat

import ..MathOptFormat
import JSONSchema
const SCHEMA_PATH = joinpath(@__DIR__, "schema", "mof.schema.json")

# we use an ordered dict to make the JSON printing nicer
const Object = OrderedDict{String, Any}

import MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

struct Nonlinear <: MOI.AbstractScalarFunction
    expr::Expr
end
Base.copy(nonlinear::Nonlinear) = Nonlinear(copy(nonlinear.expr))

MOIU.@model(InnerModel,
    (MOI.ZeroOne, MOI.Integer),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval,
        MOI.Semicontinuous, MOI.Semiinteger),
    (MOI.Reals, MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives,
        MOI.SecondOrderCone, MOI.RotatedSecondOrderCone,
        MOI.GeometricMeanCone,
        MOI.RootDetConeTriangle, MOI.RootDetConeSquare,
        MOI.LogDetConeTriangle, MOI.LogDetConeSquare,
        MOI.PositiveSemidefiniteConeTriangle, MOI.PositiveSemidefiniteConeSquare,
        MOI.ExponentialCone, MOI.DualExponentialCone),
    (MOI.PowerCone, MOI.DualPowerCone, MOI.SOS1, MOI.SOS2),
    (MOI.SingleVariable, Nonlinear),
    (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
    (MOI.VectorOfVariables,),
    (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction)
)

const Model = MOIU.UniversalFallback{InnerModel{Float64}}

"""
    Model()

Create an empty instance of MathOptFormat.Model.
"""
function Model()
    return MOIU.UniversalFallback(InnerModel{Float64}())
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
function validate(filename::String)
    MathOptFormat.gzip_open(filename, "r") do io
        object = JSON.parse(io)
        mof_schema = JSONSchema.Schema(JSON.parsefile(SCHEMA_PATH))
        if !JSONSchema.isvalid(object, mof_schema)
            error("Unable to read file because it does not conform to the MOF " *
                  "schema: ", JSONSchema.diagnose(object, mof_schema))
        end
    end
    return
end

include("nonlinear.jl")

include("read.jl")
include("write.jl")

end
