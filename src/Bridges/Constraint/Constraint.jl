# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module Constraint

import LinearAlgebra
import MathOptInterface
import OrderedCollections: OrderedDict
import SparseArrays

const MOI = MathOptInterface
const MOIU = MOI.Utilities
const MOIB = MOI.Bridges

include("bridge.jl")
include("function_conversion.jl")
include("map.jl")
include("set_map.jl")
include("single_bridge_optimizer.jl")

include("bridges/det.jl")
include("bridges/flip_sign.jl")
include("bridges/functionize.jl")
include("bridges/geomean_to_relentr.jl")
include("bridges/geomean.jl")
include("bridges/indicator_activate_on_zero.jl")
include("bridges/indicator_sos.jl")
include("bridges/interval.jl")
include("bridges/ltgt_to_interval.jl")
include("bridges/norm_spec_nuc_to_psd.jl")
include("bridges/norm_to_lp.jl")
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
    MOI.Bridges.add_bridge(bridged_model, RSOCtoPSDBridge{T})
    MOI.Bridges.add_bridge(bridged_model, IndicatorActiveOnFalseBridge{T})
    MOI.Bridges.add_bridge(bridged_model, IndicatorSOS1Bridge{T})
    MOI.Bridges.add_bridge(bridged_model, SemiToBinaryBridge{T})
    MOI.Bridges.add_bridge(bridged_model, ZeroOneBridge{T})
    return
end

end
