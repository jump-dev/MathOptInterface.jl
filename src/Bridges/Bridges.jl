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

include("bridge.jl")
include("bridgeoptimizer.jl")
include("singlebridgeoptimizer.jl")
include("lazybridgeoptimizer.jl")

# This is used by JuMP and removes the need to update JuMP everytime a bridge is added
MOIU.@model(AllBridgedConstraints,
            (),
            (MOI.Interval,),
            (MOI.SecondOrderCone, MOI.RotatedSecondOrderCone, MOI.GeometricMeanCone,
             MOI.LogDetConeTriangle, MOI.RootDetConeTriangle),
            (),
            (),
            (MOI.ScalarAffineFunction,),
            (MOI.VectorOfVariables,),
            (MOI.VectorAffineFunction,))
"""
    fullbridgeoptimizer(model::MOI.ModelLike, ::Type{T}) where T

Returns a `LazyBridgeOptimizer` bridging `model` for every bridge defined in this package and for the coefficient type `T`.
"""
function fullbridgeoptimizer(model::MOI.ModelLike, ::Type{T}) where T
    bridgedmodel = MOIB.LazyBridgeOptimizer(model, AllBridgedConstraints{T}())
    add_bridge(bridgedmodel, MOIB.SplitIntervalBridge{T})
    add_bridge(bridgedmodel, MOIB.GeoMeanBridge{T})
    add_bridge(bridgedmodel, MOIB.SquarePSDBridge{T})
    add_bridge(bridgedmodel, MOIB.LogDetBridge{T})
    add_bridge(bridgedmodel, MOIB.RootDetBridge{T})
    add_bridge(bridgedmodel, MOIB.RSOCBridge{T})
    add_bridge(bridgedmodel, MOIB.RSOCtoPSDBridge{T})
    add_bridge(bridgedmodel, MOIB.SOCtoPSDBridge{T})
    bridgedmodel
end

include("intervalbridge.jl")
@bridge SplitInterval SplitIntervalBridge () (MOI.Interval,) () () (MOI.SingleVariable,) (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction) () ()
include("rsocbridge.jl")
@bridge RSOC RSOCBridge () () (MOI.RotatedSecondOrderCone,) () () () (MOI.VectorOfVariables,) (MOI.VectorAffineFunction,)
include("geomeanbridge.jl")
@bridge GeoMean GeoMeanBridge () () (MOI.GeometricMeanCone,) () () () (MOI.VectorOfVariables,) (MOI.VectorAffineFunction,)
include("squarepsdbridge.jl")
@bridge SquarePSD SquarePSDBridge () () (MOI.PositiveSemidefiniteConeSquare,) () () () (MOI.VectorOfVariables,) (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction)
include("detbridge.jl")
@bridge LogDet LogDetBridge () () (MOI.LogDetConeTriangle,) () () () (MOI.VectorOfVariables,) (MOI.VectorAffineFunction,)
@bridge RootDet RootDetBridge () () (MOI.RootDetConeTriangle,) () () () (MOI.VectorOfVariables,) (MOI.VectorAffineFunction,)
include("soctopsdbridge.jl")
@bridge SOCtoPSD SOCtoPSDBridge () () (MOI.SecondOrderCone,) () () () (MOI.VectorOfVariables,) (MOI.VectorAffineFunction,)
@bridge RSOCtoPSD RSOCtoPSDBridge () () (MOI.RotatedSecondOrderCone,) () () () (MOI.VectorOfVariables,) (MOI.VectorAffineFunction,)

end # module
