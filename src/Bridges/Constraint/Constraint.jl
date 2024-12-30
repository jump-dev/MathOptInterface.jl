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
    add_all_bridges(model, ::Type{T}) where {T}

Add all bridges defined in the `Bridges.Constraint` submodule to `model`.

The coefficient type used is `T`.
"""
function add_all_bridges(model, ::Type{T}) where {T}
    if T <: AbstractFloat
        MOI.Bridges.add_bridge(model, GreaterToIntervalBridge{T})
        MOI.Bridges.add_bridge(model, LessToIntervalBridge{T})
    end
    MOI.Bridges.add_bridge(model, GreaterToLessBridge{T})
    MOI.Bridges.add_bridge(model, LessToGreaterBridge{T})
    MOI.Bridges.add_bridge(model, NonnegToNonposBridge{T})
    MOI.Bridges.add_bridge(model, NonposToNonnegBridge{T})
    MOI.Bridges.add_bridge(model, ScalarizeBridge{T})
    MOI.Bridges.add_bridge(model, VectorizeBridge{T})
    MOI.Bridges.add_bridge(model, ScalarSlackBridge{T})
    MOI.Bridges.add_bridge(model, VectorSlackBridge{T})
    MOI.Bridges.add_bridge(model, ScalarFunctionizeBridge{T})
    MOI.Bridges.add_bridge(model, VectorFunctionizeBridge{T})
    MOI.Bridges.add_bridge(model, ToScalarQuadraticBridge{T})
    MOI.Bridges.add_bridge(model, ToVectorQuadraticBridge{T})
    MOI.Bridges.add_bridge(model, ToScalarNonlinearBridge{T})
    MOI.Bridges.add_bridge(model, SplitHyperRectangleBridge{T})
    MOI.Bridges.add_bridge(model, SplitIntervalBridge{T})
    MOI.Bridges.add_bridge(model, SplitComplexEqualToBridge{T})
    MOI.Bridges.add_bridge(model, SplitComplexZerosBridge{T})
    MOI.Bridges.add_bridge(model, QuadtoSOCBridge{T})
    # We do not add `(R)SOCtoNonConvexQuad` because it starts with a convex
    # conic constraint and generate a non-convex constraint (in the QCP
    # interpretation).
    MOI.Bridges.add_bridge(model, NormInfinityBridge{T})
    MOI.Bridges.add_bridge(model, NormOneBridge{T})
    MOI.Bridges.add_bridge(model, GeoMeantoRelEntrBridge{T})
    MOI.Bridges.add_bridge(model, GeoMeanBridge{T})
    MOI.Bridges.add_bridge(model, GeoMeanToPowerBridge{T})
    MOI.Bridges.add_bridge(model, NormToPowerBridge{T})
    MOI.Bridges.add_bridge(model, NormOneConeToNormConeBridge{T})
    MOI.Bridges.add_bridge(model, SecondOrderConeToNormConeBridge{T})
    MOI.Bridges.add_bridge(model, NormInfinityConeToNormConeBridge{T})
    MOI.Bridges.add_bridge(model, ComplexNormInfinityToSecondOrderConeBridge{T})
    MOI.Bridges.add_bridge(model, RelativeEntropyBridge{T})
    MOI.Bridges.add_bridge(model, NormSpectralBridge{T})
    MOI.Bridges.add_bridge(model, NormNuclearBridge{T})
    MOI.Bridges.add_bridge(model, HermitianToSymmetricPSDBridge{T})
    MOI.Bridges.add_bridge(model, SquareBridge{T})
    MOI.Bridges.add_bridge(model, SetDotScalingBridge{T})
    MOI.Bridges.add_bridge(model, SetDotInverseScalingBridge{T})
    MOI.Bridges.add_bridge(model, LogDetBridge{T})
    MOI.Bridges.add_bridge(model, RootDetBridge{T})
    MOI.Bridges.add_bridge(model, RSOCtoSOCBridge{T})
    MOI.Bridges.add_bridge(model, SOCtoRSOCBridge{T})
    # We do not add `SOCtoPSDBridge` as transforming the `SOC` to `RSOC` and
    # then to `PSD` produces a smaller SDP constraint.
    # MOI.Bridges.add_bridge(model, SOCtoPSDBridge{T})
    MOI.Bridges.add_bridge(model, RSOCtoPSDBridge{T})
    MOI.Bridges.add_bridge(model, IndicatorActiveOnFalseBridge{T})
    MOI.Bridges.add_bridge(model, IndicatorSOS1Bridge{T})
    MOI.Bridges.add_bridge(model, IndicatorLessToGreaterThanBridge{T})
    MOI.Bridges.add_bridge(model, IndicatorGreaterToLessThanBridge{T})
    MOI.Bridges.add_bridge(model, SemiToBinaryBridge{T})
    MOI.Bridges.add_bridge(model, ZeroOneBridge{T})
    MOI.Bridges.add_bridge(model, IntegerToZeroOneBridge{T})
    MOI.Bridges.add_bridge(model, InequalityToComplementsBridge{T})
    # Do not add by default
    # MOI.Bridges.add_bridge(model, NumberConversionBridge{T})
    # Constraint programming bridges
    MOI.Bridges.add_bridge(model, AllDifferentToCountDistinctBridge{T})
    MOI.Bridges.add_bridge(model, ReifiedAllDifferentToCountDistinctBridge{T})
    MOI.Bridges.add_bridge(model, BinPackingToMILPBridge{T})
    MOI.Bridges.add_bridge(model, CircuitToMILPBridge{T})
    MOI.Bridges.add_bridge(model, CountAtLeastToCountBelongsBridge{T})
    MOI.Bridges.add_bridge(model, CountBelongsToMILPBridge{T})
    MOI.Bridges.add_bridge(model, CountDistinctToMILPBridge{T})
    MOI.Bridges.add_bridge(model, ReifiedCountDistinctToMILPBridge{T})
    MOI.Bridges.add_bridge(model, CountGreaterThanToMILPBridge{T})
    MOI.Bridges.add_bridge(model, TableToMILPBridge{T})
    MOI.Bridges.add_bridge(model, SOS1ToMILPBridge{T})
    MOI.Bridges.add_bridge(model, SOS2ToMILPBridge{T})
    MOI.Bridges.add_bridge(model, IndicatorToMILPBridge{T})
    MOI.Bridges.add_bridge(
        model,
        ExponentialConeToScalarNonlinearFunctionBridge{T},
    )
    return
end

end
