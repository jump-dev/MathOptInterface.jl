module Bridges

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
            (MOI.EqualTo, MOI.LessThan, MOI.GreaterThan, MOI.Interval,),
            (MOI.Nonnegatives, MOI.Nonpositives, MOI.SecondOrderCone,
             MOI.RotatedSecondOrderCone, MOI.GeometricMeanCone,
             MOI.PositiveSemidefiniteConeSquare,
             MOI.LogDetConeTriangle, MOI.RootDetConeTriangle),
            (),
            (MOI.SingleVariable,),
            (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
            (MOI.VectorOfVariables,),
            (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction))
"""
    full_bridge_optimizer(model::MOI.ModelLike, ::Type{T}) where T

Returns a `LazyBridgeOptimizer` bridging `model` for every bridge defined in
this package and for the coefficient type `T`.
"""
function full_bridge_optimizer(model::MOI.ModelLike, ::Type{T}) where T
    cache = MOIU.UniversalFallback(AllBridgedConstraints{T}())
    bridged_model = LazyBridgeOptimizer(model, cache)
    add_bridge(bridged_model, GreaterToLessBridge{T})
    add_bridge(bridged_model, LessToGreaterBridge{T})
    add_bridge(bridged_model, NonnegToNonposBridge{T})
    add_bridge(bridged_model, NonposToNonnegBridge{T})
    add_bridge(bridged_model, ScalarizeBridge{T})
    add_bridge(bridged_model, VectorizeBridge{T})
    add_bridge(bridged_model, ScalarSlackBridge{T})
    add_bridge(bridged_model, VectorSlackBridge{T})
    add_bridge(bridged_model, ScalarFunctionizeBridge{T})
    add_bridge(bridged_model, VectorFunctionizeBridge{T})
    add_bridge(bridged_model, SplitIntervalBridge{T})
    add_bridge(bridged_model, QuadtoSOCBridge{T})
    add_bridge(bridged_model, GeoMeanBridge{T})
    add_bridge(bridged_model, SquarePSDBridge{T})
    add_bridge(bridged_model, LogDetBridge{T})
    add_bridge(bridged_model, RootDetBridge{T})
    add_bridge(bridged_model, RSOCBridge{T})
    add_bridge(bridged_model, RSOCtoPSDBridge{T})
    add_bridge(bridged_model, SOCtoPSDBridge{T})
    return bridged_model
end

include("flip_sign_bridge.jl")
@bridge GreaterToLess GreaterToLessBridge () (MOI.GreaterThan,) () () (MOI.SingleVariable,) (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction) () ()
@bridge LessToGreater LessToGreaterBridge () (MOI.LessThan,) () () (MOI.SingleVariable,) (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction) () ()
@bridge NonnegToNonpos NonnegToNonposBridge () () (MOI.Nonnegatives,) () () () (MOI.VectorOfVariables,) (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction)
@bridge NonposToNonneg NonposToNonnegBridge () () (MOI.Nonpositives,) () () () (MOI.VectorOfVariables,) (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction)
include("vectorizebridge.jl")
@bridge Vectorize VectorizeBridge () (MOI.EqualTo, MOI.LessThan, MOI.GreaterThan,) () () (MOI.SingleVariable,) (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction) () ()
include("scalarizebridge.jl")
@bridge Scalarize ScalarizeBridge () () (MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives) () () () (MOI.VectorOfVariables,) (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction)
include("slackbridge.jl")
@bridge ScalarSlack ScalarSlackBridge () (MOI.Interval, MOI.LessThan, MOI.GreaterThan) () () () (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction) () ()
@bridge(VectorSlack, VectorSlackBridge,  (), (),
    (MOI.Nonnegatives, MOI.Nonpositives, MOI.SecondOrderCone,
    MOI.RotatedSecondOrderCone, MOI.GeometricMeanCone,
    MOI.PositiveSemidefiniteConeSquare, MOI.PositiveSemidefiniteConeTriangle, MOI.LogDetConeTriangle,
    MOI.RootDetConeTriangle),
    (MOI.PowerCone, MOI.DualPowerCone, MOI.SOS1, MOI.SOS2), (), (), (),
    (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction)
    )
include("functionize_bridge.jl")
@bridge ScalarFunctionize ScalarFunctionizeBridge () (MOI.Interval, MOI.LessThan, MOI.GreaterThan) () () (MOI.SingleVariable,) () () ()
@bridge(VectorFunctionize, VectorFunctionizeBridge,  (), (),
    (MOI.Nonnegatives, MOI.Nonpositives, MOI.SecondOrderCone,
    MOI.RotatedSecondOrderCone, MOI.GeometricMeanCone,
    MOI.PositiveSemidefiniteConeSquare, MOI.PositiveSemidefiniteConeTriangle, MOI.LogDetConeTriangle,
    MOI.RootDetConeTriangle),
    (MOI.PowerCone, MOI.DualPowerCone, MOI.SOS1, MOI.SOS2), (), (),
    (MOI.VectorOfVariables,), ()
    )
include("intervalbridge.jl")
@bridge SplitInterval SplitIntervalBridge () (MOI.Interval,) () () (MOI.SingleVariable,) (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction) () ()
include("rsocbridge.jl")
@bridge RSOC RSOCBridge () () (MOI.RotatedSecondOrderCone,) () () () (MOI.VectorOfVariables,) (MOI.VectorAffineFunction,)
include("quadtosocbridge.jl")
@bridge QuadtoSOC QuadtoSOCBridge () (MOI.LessThan, MOI.GreaterThan) () () () (MOI.ScalarQuadraticFunction,) () ()
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
