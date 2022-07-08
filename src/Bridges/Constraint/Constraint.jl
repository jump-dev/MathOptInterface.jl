# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module Constraint

import LinearAlgebra
import MutableArithmetics
const MA = MutableArithmetics
import MathOptInterface
import OrderedCollections: OrderedDict, OrderedSet
import SparseArrays

const MOI = MathOptInterface

include("bridge.jl")
include("map.jl")
include("set_map.jl")
include("single_bridge_optimizer.jl")

include("bridges/all_different.jl")
include("bridges/bin_packing.jl")
include("bridges/circuit.jl")
include("bridges/count_at_least.jl")
include("bridges/count_belongs.jl")
include("bridges/count_distinct.jl")
include("bridges/count_greater_than.jl")
include("bridges/det.jl")
include("bridges/flip_sign.jl")
include("bridges/functionize.jl")
include("bridges/geomean_to_relentr.jl")
include("bridges/geomean.jl")
include("bridges/indicator_activate_on_zero.jl")
include("bridges/indicator_sos.jl")
include("bridges/interval.jl")
include("bridges/split_zeros.jl")
include("bridges/ltgt_to_interval.jl")
include("bridges/norm_infinity.jl")
include("bridges/norm_one.jl")
include("bridges/norm_spec_nuc_to_psd.jl")
include("bridges/quad_to_soc.jl")
include("bridges/relentr_to_exp.jl")
include("bridges/rsoc_soc.jl")
include("bridges/scalarize.jl")
include("bridges/semi_to_binary.jl")
include("bridges/slack.jl")
include("bridges/soc_rsoc.jl")
include("bridges/soc_to_nonconvex_quad.jl") # do not add these bridges by default
include("bridges/soc_to_psd.jl")
include("bridges/square.jl")
include("bridges/table.jl")
include("bridges/vectorize.jl")
include("bridges/zero_one.jl")

"""
    add_all_bridges(bridged_model, ::Type{T}) where {T}

Add all bridges defined in the `Bridges.Constraint` submodule to
`bridged_model`. The coefficient type used is `T`.
"""
function add_all_bridges(bridged_model, ::Type{T}) where {T}
    if T <: AbstractFloat
        MOI.Bridges.add_bridge(bridged_model, GreaterToIntervalBridge{T})
        MOI.Bridges.add_bridge(bridged_model, LessToIntervalBridge{T})
    end
    MOI.Bridges.add_bridge(bridged_model, GreaterToLessBridge{T})
    MOI.Bridges.add_bridge(bridged_model, LessToGreaterBridge{T})
    MOI.Bridges.add_bridge(bridged_model, NonnegToNonposBridge{T})
    MOI.Bridges.add_bridge(bridged_model, NonposToNonnegBridge{T})
    MOI.Bridges.add_bridge(bridged_model, ScalarizeBridge{T})
    MOI.Bridges.add_bridge(bridged_model, VectorizeBridge{T})
    MOI.Bridges.add_bridge(bridged_model, ScalarSlackBridge{T})
    MOI.Bridges.add_bridge(bridged_model, VectorSlackBridge{T})
    MOI.Bridges.add_bridge(bridged_model, ScalarFunctionizeBridge{T})
    MOI.Bridges.add_bridge(bridged_model, VectorFunctionizeBridge{T})
    MOI.Bridges.add_bridge(bridged_model, SplitIntervalBridge{T})
    MOI.Bridges.add_bridge(bridged_model, SplitZerosBridge{T})
    MOI.Bridges.add_bridge(bridged_model, QuadtoSOCBridge{T})
    # We do not add `(R)SOCtoNonConvexQuad` because it starts with a convex
    # conic constraint and generate a non-convex constraint (in the QCP
    # interpretation).
    MOI.Bridges.add_bridge(bridged_model, NormInfinityBridge{T})
    MOI.Bridges.add_bridge(bridged_model, NormOneBridge{T})
    MOI.Bridges.add_bridge(bridged_model, GeoMeantoRelEntrBridge{T})
    MOI.Bridges.add_bridge(bridged_model, GeoMeanBridge{T})
    MOI.Bridges.add_bridge(bridged_model, RelativeEntropyBridge{T})
    MOI.Bridges.add_bridge(bridged_model, NormSpectralBridge{T})
    MOI.Bridges.add_bridge(bridged_model, NormNuclearBridge{T})
    MOI.Bridges.add_bridge(bridged_model, SquareBridge{T})
    MOI.Bridges.add_bridge(bridged_model, LogDetBridge{T})
    MOI.Bridges.add_bridge(bridged_model, RootDetBridge{T})
    MOI.Bridges.add_bridge(bridged_model, RSOCtoSOCBridge{T})
    MOI.Bridges.add_bridge(bridged_model, SOCtoRSOCBridge{T})
    # We do not add `SOCtoPSDBridge` as transforming the `SOC` to `RSOC` and
    # then to `PSD` produces a smaller SDP constraint.
    # MOI.Bridges.add_bridge(bridged_model, SOCtoPSDBridge{T})
    MOI.Bridges.add_bridge(bridged_model, RSOCtoPSDBridge{T})
    MOI.Bridges.add_bridge(bridged_model, IndicatorActiveOnFalseBridge{T})
    MOI.Bridges.add_bridge(bridged_model, IndicatorSOS1Bridge{T})
    MOI.Bridges.add_bridge(bridged_model, SemiToBinaryBridge{T})
    MOI.Bridges.add_bridge(bridged_model, ZeroOneBridge{T})
    # Constraint programming bridges
    MOI.Bridges.add_bridge(bridged_model, AllDifferentToCountDistinctBridge{T})
    MOI.Bridges.add_bridge(bridged_model, BinPackingToMILPBridge{T})
    MOI.Bridges.add_bridge(bridged_model, CircuitToMILPBridge{T})
    MOI.Bridges.add_bridge(bridged_model, CountAtLeastToCountBelongsBridge{T})
    MOI.Bridges.add_bridge(bridged_model, CountBelongsToMILPBridge{T})
    MOI.Bridges.add_bridge(bridged_model, CountDistinctToMILPBridge{T})
    MOI.Bridges.add_bridge(bridged_model, CountGreaterThanToMILPBridge{T})
    MOI.Bridges.add_bridge(bridged_model, TableToMILPBridge{T})
    return
end

end
