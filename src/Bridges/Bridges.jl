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

"""
    full_bridge_optimizer(model::MOI.ModelLike, ::Type{T}) where T

Returns a `LazyBridgeOptimizer` bridging `model` for every bridge defined in
this package and for the coefficient type `T`.
"""
function full_bridge_optimizer(model::MOI.ModelLike, ::Type{T}) where T
    bridged_model = LazyBridgeOptimizer(model)
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
    add_bridge(bridged_model, SquareBridge{T})
    add_bridge(bridged_model, LogDetBridge{T})
    add_bridge(bridged_model, RootDetBridge{T})
    add_bridge(bridged_model, RSOCBridge{T})
    add_bridge(bridged_model, RSOCtoPSDBridge{T})
    add_bridge(bridged_model, SOCtoPSDBridge{T})
    add_bridge(bridged_model, MOIB.IndicatorActiveOnFalseBridge{T})
    return bridged_model
end

include("flip_sign_bridge.jl")
const GreaterToLess{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{GreaterToLessBridge{T}, OT}
const LessToGreater{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{LessToGreaterBridge{T}, OT}
const NonnegToNonpos{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{NonnegToNonposBridge{T}, OT}
const NonposToNonneg{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{NonposToNonnegBridge{T}, OT}
include("vectorizebridge.jl")
const Vectorize{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{VectorizeBridge{T}, OT}
include("scalarizebridge.jl")
const Scalarize{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{ScalarizeBridge{T}, OT}
include("slackbridge.jl")
const ScalarSlack{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{ScalarSlackBridge{T}, OT}
const VectorSlack{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{VectorSlackBridge{T}, OT}
include("functionize_bridge.jl")
const ScalarFunctionize{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{ScalarFunctionizeBridge{T}, OT}
const VectorFunctionize{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{VectorFunctionizeBridge{T}, OT}
include("intervalbridge.jl")
const SplitInterval{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{SplitIntervalBridge{T}, OT}
include("rsocbridge.jl")
const RSOC{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{RSOCBridge{T}, OT}
include("quadtosocbridge.jl")
const QuadtoSOC{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{QuadtoSOCBridge{T}, OT}
include("geomeanbridge.jl")
const GeoMean{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{GeoMeanBridge{T}, OT}
include("square_bridge.jl")
const Square{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{SquareBridge{T}, OT}
include("detbridge.jl")
const LogDet{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{LogDetBridge{T}, OT}
const RootDet{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{RootDetBridge{T}, OT}
include("soctopsdbridge.jl")
const SOCtoPSD{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{SOCtoPSDBridge{T}, OT}
const RSOCtoPSD{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{RSOCtoPSDBridge{T}, OT}

include("indicatorbridge.jl")

end # module
