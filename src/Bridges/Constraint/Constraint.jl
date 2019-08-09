module Constraint

using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities
const MOIB = MOI.Bridges

const CI = MOI.ConstraintIndex

# Definition of a constraint bridge
include("bridge.jl")

# Mapping between constraint indices and bridges
include("map.jl")

# Bridge optimizer bridging a specific constraint bridge
include("single_bridge_optimizer.jl")

# Constraint bridges
include("flip_sign.jl")
const GreaterToLess{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{GreaterToLessBridge{T}, OT}
const LessToGreater{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{LessToGreaterBridge{T}, OT}
const NonnegToNonpos{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{NonnegToNonposBridge{T}, OT}
const NonposToNonneg{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{NonposToNonnegBridge{T}, OT}
include("vectorize.jl")
const Vectorize{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{VectorizeBridge{T}, OT}
include("scalarize.jl")
const Scalarize{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{ScalarizeBridge{T}, OT}
include("slack.jl")
const ScalarSlack{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{ScalarSlackBridge{T}, OT}
const VectorSlack{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{VectorSlackBridge{T}, OT}
include("functionize.jl")
const ScalarFunctionize{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{ScalarFunctionizeBridge{T}, OT}
const VectorFunctionize{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{VectorFunctionizeBridge{T}, OT}
include("interval.jl")
const SplitInterval{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{SplitIntervalBridge{T}, OT}
include("rsoc.jl")
const RSOC{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{RSOCBridge{T}, OT}
include("quad_to_soc.jl")
const QuadtoSOC{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{QuadtoSOCBridge{T}, OT}
include("geomean.jl")
const GeoMean{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{GeoMeanBridge{T}, OT}
include("square.jl")
const Square{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{SquareBridge{T}, OT}
include("det.jl")
const LogDet{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{LogDetBridge{T}, OT}
const RootDet{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{RootDetBridge{T}, OT}
include("soc_to_psd.jl")
const SOCtoPSD{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{SOCtoPSDBridge{T}, OT}
const RSOCtoPSD{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{RSOCtoPSDBridge{T}, OT}
include("indicator.jl")

"""
    add_all_bridges(bridged_model, T::Type)

Add all bridges defined in the `Bridges.Constraint` submodule to
`bridged_model`. The coefficient type used is `T`.
"""
function add_all_bridges(bridged_model, T::Type)
    MOIB.add_bridge(bridged_model, GreaterToLessBridge{T})
    MOIB.add_bridge(bridged_model, LessToGreaterBridge{T})
    MOIB.add_bridge(bridged_model, NonnegToNonposBridge{T})
    MOIB.add_bridge(bridged_model, NonposToNonnegBridge{T})
    MOIB.add_bridge(bridged_model, ScalarizeBridge{T})
    MOIB.add_bridge(bridged_model, VectorizeBridge{T})
    MOIB.add_bridge(bridged_model, ScalarSlackBridge{T})
    MOIB.add_bridge(bridged_model, VectorSlackBridge{T})
    MOIB.add_bridge(bridged_model, ScalarFunctionizeBridge{T})
    MOIB.add_bridge(bridged_model, VectorFunctionizeBridge{T})
    MOIB.add_bridge(bridged_model, SplitIntervalBridge{T})
    MOIB.add_bridge(bridged_model, QuadtoSOCBridge{T})
    MOIB.add_bridge(bridged_model, GeoMeanBridge{T})
    MOIB.add_bridge(bridged_model, SquareBridge{T})
    MOIB.add_bridge(bridged_model, LogDetBridge{T})
    MOIB.add_bridge(bridged_model, RootDetBridge{T})
    MOIB.add_bridge(bridged_model, RSOCBridge{T})
    MOIB.add_bridge(bridged_model, RSOCtoPSDBridge{T})
    MOIB.add_bridge(bridged_model, SOCtoPSDBridge{T})
    MOIB.add_bridge(bridged_model, IndicatorActiveOnFalseBridge{T})
    return
end

end
