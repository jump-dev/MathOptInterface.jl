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

# TODO(odow): the compiler in Julia <= 1.2 (and in later versions unless
# fixed) gets stuck compiling add_constrained_variable for some inputs. This
# method seemed necessary to fix it.
# See https://github.com/JuliaLang/julia/issues/32167 for more.
function MOI.Bridges.Constraint.bridge_constraint(BridgeType, b, f, s)
    return throw(MOI.UnsupportedConstraint{typeof(f),typeof(s)}())
end

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
        MOIB.add_bridge(bridged_model, GreaterToIntervalBridge{T})
        MOIB.add_bridge(bridged_model, LessToIntervalBridge{T})
    end
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
    MOIB.add_bridge(bridged_model, GeoMeantoRelEntrBridge{T})
    MOIB.add_bridge(bridged_model, GeoMeanBridge{T})
    MOIB.add_bridge(bridged_model, RelativeEntropyBridge{T})
    MOIB.add_bridge(bridged_model, NormSpectralBridge{T})
    MOIB.add_bridge(bridged_model, NormNuclearBridge{T})
    MOIB.add_bridge(bridged_model, SquareBridge{T})
    MOIB.add_bridge(bridged_model, LogDetBridge{T})
    MOIB.add_bridge(bridged_model, RootDetBridge{T})
    MOIB.add_bridge(bridged_model, RSOCtoSOCBridge{T})
    MOIB.add_bridge(bridged_model, SOCtoRSOCBridge{T})
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
