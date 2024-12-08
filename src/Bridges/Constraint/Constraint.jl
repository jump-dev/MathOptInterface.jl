# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module Constraint

import LinearAlgebra
import MutableArithmetics as MA
import MathOptInterface as MOI
import OrderedCollections: OrderedDict, OrderedSet
import SparseArrays

include("bridge.jl")
include("map.jl")
include("set_map.jl")
include("single_bridge_optimizer.jl")

for filename in readdir(joinpath(@__DIR__, "bridges"); join = true)
    include(filename)
end

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
    MOI.Bridges.add_bridge(bridged_model, ToScalarQuadraticBridge{T})
    MOI.Bridges.add_bridge(bridged_model, ToVectorQuadraticBridge{T})
    MOI.Bridges.add_bridge(bridged_model, ToScalarNonlinearBridge{T})
    MOI.Bridges.add_bridge(bridged_model, SplitHyperRectangleBridge{T})
    MOI.Bridges.add_bridge(bridged_model, SplitIntervalBridge{T})
    MOI.Bridges.add_bridge(bridged_model, SplitComplexEqualToBridge{T})
    MOI.Bridges.add_bridge(bridged_model, SplitComplexZerosBridge{T})
    MOI.Bridges.add_bridge(bridged_model, QuadtoSOCBridge{T})
    MOI.Bridges.add_bridge(bridged_model, SOCtoNonConvexQuadBridge{T})
    MOI.Bridges.add_bridge(bridged_model, RSOCtoNonConvexQuadBridge{T})
    MOI.Bridges.add_bridge(bridged_model, NormInfinityBridge{T})
    MOI.Bridges.add_bridge(bridged_model, NormOneBridge{T})
    MOI.Bridges.add_bridge(bridged_model, GeoMeantoRelEntrBridge{T})
    MOI.Bridges.add_bridge(bridged_model, GeoMeanBridge{T})
    MOI.Bridges.add_bridge(bridged_model, GeoMeanToPowerBridge{T})
    MOI.Bridges.add_bridge(bridged_model, NormToPowerBridge{T})
    MOI.Bridges.add_bridge(bridged_model, NormOneConeToNormConeBridge{T})
    MOI.Bridges.add_bridge(bridged_model, SecondOrderConeToNormConeBridge{T})
    MOI.Bridges.add_bridge(bridged_model, NormInfinityConeToNormConeBridge{T})
    MOI.Bridges.add_bridge(
        bridged_model,
        ComplexNormInfinityToSecondOrderConeBridge{T},
    )
    MOI.Bridges.add_bridge(bridged_model, RelativeEntropyBridge{T})
    MOI.Bridges.add_bridge(bridged_model, NormSpectralBridge{T})
    MOI.Bridges.add_bridge(bridged_model, NormNuclearBridge{T})
    MOI.Bridges.add_bridge(bridged_model, HermitianToSymmetricPSDBridge{T})
    MOI.Bridges.add_bridge(bridged_model, SquareBridge{T})
    MOI.Bridges.add_bridge(bridged_model, SetDotScalingBridge{T})
    MOI.Bridges.add_bridge(bridged_model, SetDotInverseScalingBridge{T})
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
    MOI.Bridges.add_bridge(bridged_model, IndicatorLessToGreaterThanBridge{T})
    MOI.Bridges.add_bridge(bridged_model, IndicatorGreaterToLessThanBridge{T})
    MOI.Bridges.add_bridge(bridged_model, SemiToBinaryBridge{T})
    MOI.Bridges.add_bridge(bridged_model, ZeroOneBridge{T})
    MOI.Bridges.add_bridge(bridged_model, IntegerToZeroOneBridge{T})
    MOI.Bridges.add_bridge(bridged_model, InequalityToComplementsBridge{T})
    # Do not add by default
    # MOI.Bridges.add_bridge(bridged_model, NumberConversionBridge{T})
    # Constraint programming bridges
    MOI.Bridges.add_bridge(bridged_model, AllDifferentToCountDistinctBridge{T})
    MOI.Bridges.add_bridge(
        bridged_model,
        ReifiedAllDifferentToCountDistinctBridge{T},
    )
    MOI.Bridges.add_bridge(bridged_model, BinPackingToMILPBridge{T})
    MOI.Bridges.add_bridge(bridged_model, CircuitToMILPBridge{T})
    MOI.Bridges.add_bridge(bridged_model, CountAtLeastToCountBelongsBridge{T})
    MOI.Bridges.add_bridge(bridged_model, CountBelongsToMILPBridge{T})
    MOI.Bridges.add_bridge(bridged_model, CountDistinctToMILPBridge{T})
    MOI.Bridges.add_bridge(bridged_model, ReifiedCountDistinctToMILPBridge{T})
    MOI.Bridges.add_bridge(bridged_model, CountGreaterThanToMILPBridge{T})
    MOI.Bridges.add_bridge(bridged_model, TableToMILPBridge{T})
    MOI.Bridges.add_bridge(bridged_model, SOS1ToMILPBridge{T})
    MOI.Bridges.add_bridge(bridged_model, SOS2ToMILPBridge{T})
    MOI.Bridges.add_bridge(bridged_model, IndicatorToMILPBridge{T})
    return
end

end
