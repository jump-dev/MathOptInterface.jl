module Bridges

using Compat

using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

const MOIB = MOI.Bridges # used in macro

const SVF = MOI.SingleVariable
const VVF = MOI.VectorOfVariables
const SAF{T} = MOI.ScalarAffineFunction{T}
const VAF{T} = MOI.VectorAffineFunction{T}
const SQF{T} = MOI.ScalarQuadraticFunction{T}
const VQF{T} = MOI.VectorQuadraticFunction{T}

const VI = MOI.VariableIndex
const CI = MOI.ConstraintIndex

# TODO move to MOIU ?
mapcoefficient(coefmap, t::MOI.ScalarAffineTerm) = MOI.ScalarAffineTerm(coefmap(t.coefficient), t.variable_index)
mapcoefficient(coefmap, f::MOI.ScalarAffineFunction) = MOI.ScalarAffineFunction(mapcoefficient.(coefmap, f.terms), coefmap(f.constant))

include("bridge.jl")
include("bridgeoptimizer.jl")
include("singlebridgeoptimizer.jl")
include("lazybridgeoptimizer.jl")

# This is used by JuMP and removes the need to update JuMP everytime a bridge is added
MOIU.@model AllBridgedConstraints () (Interval,) (SecondOrderCone, RotatedSecondOrderCone, GeometricMeanCone, LogDetConeTriangle, RootDetConeTriangle) () () (ScalarAffineFunction,) (VectorOfVariables,) (VectorAffineFunction,)
"""
    fullbridgeoptimizer(model::MOI.ModelLike, ::Type{T}) where T

Returns a `LazyBridgeOptimizer` bridging `model` for every bridge defined in this package and for the coefficient type `T`.
"""
function fullbridgeoptimizer(model::MOI.ModelLike, ::Type{T}) where T
    bridgedmodel = MOIB.LazyBridgeOptimizer(model, AllBridgedConstraints{T}())
    addbridge!(bridgedmodel, MOIB.SplitIntervalBridge{T})
    addbridge!(bridgedmodel, MOIB.GeoMeanBridge{T})
    addbridge!(bridgedmodel, MOIB.LogDetBridge{T})
    addbridge!(bridgedmodel, MOIB.RootDetBridge{T})
    addbridge!(bridgedmodel, MOIB.RSOCBridge{T})
    addbridge!(bridgedmodel, MOIB.RSOCtoPSDCBridge{T})
    addbridge!(bridgedmodel, MOIB.SOCtoPSDCBridge{T})
    bridgedmodel
end

include("intervalbridge.jl")
@bridge SplitInterval SplitIntervalBridge () (Interval,) () () (SingleVariable,) (ScalarAffineFunction, ScalarQuadraticFunction) () ()
include("rsocbridge.jl")
@bridge RSOC RSOCBridge () () (RotatedSecondOrderCone,) () () () (VectorOfVariables,) (VectorAffineFunction,)
include("geomeanbridge.jl")
@bridge GeoMean GeoMeanBridge () () (GeometricMeanCone,) () () () (VectorOfVariables,) (VectorAffineFunction,)
include("detbridge.jl")
@bridge LogDet LogDetBridge () () (LogDetConeTriangle,) () () () (VectorOfVariables,) (VectorAffineFunction,)
@bridge RootDet RootDetBridge () () (RootDetConeTriangle,) () () () (VectorOfVariables,) (VectorAffineFunction,)
include("soctopsdbridge.jl")
@bridge SOCtoPSD SOCtoPSDCBridge () () (SecondOrderCone,) () () () (VectorOfVariables,) (VectorAffineFunction,)
@bridge RSOCtoPSD RSOCtoPSDCBridge () () (RotatedSecondOrderCone,) () () () (VectorOfVariables,) (VectorAffineFunction,)
include("vaftosafbridge.jl")
# @bridge VAFtoSAF VectorAffineBridge () () () (Zeros, Nonnegatives, Nonpositives) () () () (VectorAffineFunction,)

end # module
