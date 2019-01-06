module MOF

const VERSION = 0

using DataStructures, JSON, MathOptInterface

# we use an ordered dict to make the JSON printing nicer
const Object = OrderedDict{String, Any}

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

include("nonlinear.jl")

include("read.jl")
include("write.jl")

end
