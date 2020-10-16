module Constraint

using OrderedCollections # for OrderedDict in Map

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

# TODO(odow): the compiler in Julia <= 1.2 (and in later versions unless
# fixed) gets stuck compiling add_constrained_variable for some inputs. This
# method seemed necessary to fix it.
# See https://github.com/JuliaLang/julia/issues/32167 for more.
function MOI.Bridges.Constraint.bridge_constraint(BridgeType, b, f, s)
    throw(MOI.UnsupportedConstraint{typeof(f), typeof(s)}())
end

# Constraint bridges
# Function conversion bridges
include("function_conversion.jl")
# Transformation between a set S1 and a set S2 such that A*S1 = S2 for some linear map A
include("set_map.jl")

include("vectorize.jl")
const Vectorize{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{VectorizeBridge{T}, OT}
include("scalarize.jl")
const Scalarize{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{ScalarizeBridge{T}, OT}
include("slack.jl")
const ScalarSlack{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{ScalarSlackBridge{T}, OT}
const VectorSlack{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{VectorSlackBridge{T}, OT}
include("interval.jl")
const SplitInterval{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{SplitIntervalBridge{T}, OT}
include("quad_to_soc.jl")
const QuadtoSOC{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{QuadtoSOCBridge{T}, OT}
include("soc_to_nonconvex_quad.jl") # do not add these bridges by default
const SOCtoNonConvexQuad{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{SOCtoNonConvexQuadBridge{T}, OT}
const RSOCtoNonConvexQuad{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{RSOCtoNonConvexQuadBridge{T}, OT}
include("norm_to_lp.jl")
const NormInfinity{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{NormInfinityBridge{T}, OT}
const NormOne{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{NormOneBridge{T}, OT}
include("geomean.jl")
const GeoMean{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{GeoMeanBridge{T}, OT}
include("geomean_to_exp.jl")
const GeoMeantoExp{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{GeoMeantoExpBridge{T}, OT}
include("relentr_to_exp.jl")
const RelativeEntropy{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{RelativeEntropyBridge{T}, OT}
include("norm_spec_nuc_to_psd.jl")
const NormSpectral{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{NormSpectralBridge{T}, OT}
const NormNuclear{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{NormNuclearBridge{T}, OT}
include("square.jl")
const Square{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{SquareBridge{T}, OT}
include("det.jl")
const LogDet{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{LogDetBridge{T}, OT}
const RootDet{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{RootDetBridge{T}, OT}
include("soc_to_psd.jl")
const SOCtoPSD{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{SOCtoPSDBridge{T}, OT}
const RSOCtoPSD{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{RSOCtoPSDBridge{T}, OT}
include("indicator_activate_on_zero.jl")
include("indicator_sos.jl")
const IndicatortoSOS1{T, BC <: MOI.AbstractScalarSet, MaybeBC} = SingleBridgeOptimizer{IndicatorSOS1Bridge{T, BC, MaybeBC}}
include("semi_to_binary.jl")
const SemiToBinary{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{SemiToBinaryBridge{T}, OT}
include("zero_one.jl")
const ZeroOne{T, OT<:MOI.ModelLike} = SingleBridgeOptimizer{ZeroOneBridge{T}, OT}

"""
    add_all_bridges(bridged_model, ::Type{T})

Add all bridges defined in the `Bridges.Constraint` submodule to
`bridged_model`. The coefficient type used is `T`.
"""
function add_all_bridges(bridged_model, ::Type{T}) where {T}
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
    # We do not add `(R)SOCtoNonConvexQuad` because it starts with a convex
    # conic constraint and generate a non-convex constraint (in the QCP
    # interpretation).
    MOIB.add_bridge(bridged_model, NormInfinityBridge{T})
    MOIB.add_bridge(bridged_model, NormOneBridge{T})
    MOIB.add_bridge(bridged_model, GeoMeanBridge{T})
    MOIB.add_bridge(bridged_model, GeoMeantoExpBridge{T})
    MOIB.add_bridge(bridged_model, RelativeEntropyBridge{T})
    MOIB.add_bridge(bridged_model, NormSpectralBridge{T})
    MOIB.add_bridge(bridged_model, NormNuclearBridge{T})
    MOIB.add_bridge(bridged_model, SquareBridge{T})
    MOIB.add_bridge(bridged_model, LogDetBridge{T})
    MOIB.add_bridge(bridged_model, RootDetBridge{T})
    MOIB.add_bridge(bridged_model, RSOCBridge{T})
    MOIB.add_bridge(bridged_model, SOCRBridge{T})
    # We do not add `SOCtoPSDBridge` as transforming the `SOC` to `RSOC` and
    # then to `PSD` produces a smaller SDP constraint.
    MOIB.add_bridge(bridged_model, RSOCtoPSDBridge{T})
    MOIB.add_bridge(bridged_model, IndicatorActiveOnFalseBridge{T})
    MOIB.add_bridge(bridged_model, IndicatorSOS1Bridge{T})
    MOIB.add_bridge(bridged_model, SemiToBinaryBridge{T})
    MOIB.add_bridge(bridged_model, ZeroOneBridge{T})
    return
end

end
