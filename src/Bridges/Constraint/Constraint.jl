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
    if endswith(filename, ".jl")
        include(filename)
    end
end

"""
    add_all_bridges(model, ::Type{T}) where {T}

Add all bridges defined in the `Bridges.Constraint` submodule to `model`.

The coefficient type used is `T`.
"""
function add_all_bridges(model, ::Type{T}) where {T}
    MOI.Bridges.add_bridge(model, AllDifferentToCountDistinctBridge{T})
    MOI.Bridges.add_bridge(model, BinPackingToMILPBridge{T})
    MOI.Bridges.add_bridge(model, CircuitToMILPBridge{T})
    MOI.Bridges.add_bridge(model, ComplexNormInfinityToSecondOrderConeBridge{T})
    MOI.Bridges.add_bridge(model, CountAtLeastToCountBelongsBridge{T})
    MOI.Bridges.add_bridge(model, CountBelongsToMILPBridge{T})
    MOI.Bridges.add_bridge(model, CountDistinctToMILPBridge{T})
    MOI.Bridges.add_bridge(model, CountGreaterThanToMILPBridge{T})
    # * ExponentialConeToScalarNonlinearFunctionBridge{T}
    #      This bridge is not added by default because it starts with a convex
    #      conic constraint and adds a nonlinear constraint that local NLP
    #      solvers like Ipopt can struggle with because of log(x) when x is 0.
    #      In addition, the bridge does not support ConstraintDual.
    #  * FunctionConversionBridge{T}
    #      This bridge is not added because, even though it is not abstract, it
    #      is highly parameterized, and parameterized versions such as
    #      ScalarFunctionizeBridge are added.
    MOI.Bridges.add_bridge(model, GeoMeanBridge{T})
    MOI.Bridges.add_bridge(model, GeoMeanToPowerBridge{T})
    MOI.Bridges.add_bridge(model, GeoMeantoRelEntrBridge{T})
    if T <: AbstractFloat  # See note in docstring of AbstractToIntervalBridge
        MOI.Bridges.add_bridge(model, GreaterToIntervalBridge{T})
    end
    MOI.Bridges.add_bridge(model, GreaterToLessBridge{T})
    MOI.Bridges.add_bridge(model, HermitianToSymmetricPSDBridge{T})
    MOI.Bridges.add_bridge(model, HermitianToComplexSymmetricBridge{T})
    MOI.Bridges.add_bridge(model, IndicatorActiveOnFalseBridge{T})
    MOI.Bridges.add_bridge(model, IndicatorGreaterToLessThanBridge{T})
    MOI.Bridges.add_bridge(model, IndicatorLessToGreaterThanBridge{T})
    #  * IndicatorSetMapBridge{T}
    #      This bridge is not added because, even though it is not abstract, it
    #      is highly parameterized, and parameterized versions such as
    #      IndicatorGreaterToLessThanBridge are added.
    MOI.Bridges.add_bridge(model, IndicatorSOS1Bridge{T})
    MOI.Bridges.add_bridge(model, IndicatorToMILPBridge{T})
    #   * InequalityToComplementsBridge{T}
    #       This bridge is not added because of a bug in Convex.jl:
    #       https://github.com/jump-dev/Convex.jl/blob/ca5324217575af263bfeee20b3e0526bed051887/src/MOI_wrapper.jl#L119-L133
    #       It is also really useful only to PATHSolver.jl, which could add this
    #       to MOI.ListOfRequiredBridges.
    MOI.Bridges.add_bridge(model, IntegerToZeroOneBridge{T})
    MOI.Bridges.add_bridge(model, LessToGreaterBridge{T})
    if T <: AbstractFloat  # See note in docstring of AbstractToIntervalBridge
        MOI.Bridges.add_bridge(model, LessToIntervalBridge{T})
    end
    MOI.Bridges.add_bridge(model, LogDetBridge{T})
    MOI.Bridges.add_bridge(model, NonnegToNonposBridge{T})
    MOI.Bridges.add_bridge(model, NonposToNonnegBridge{T})
    MOI.Bridges.add_bridge(model, NormInfinityBridge{T})
    MOI.Bridges.add_bridge(model, NormInfinityConeToNormConeBridge{T})
    MOI.Bridges.add_bridge(model, NormNuclearBridge{T})
    MOI.Bridges.add_bridge(model, NormOneBridge{T})
    MOI.Bridges.add_bridge(model, NormOneConeToNormConeBridge{T})
    #  * NormSpecialCaseBridge{T}
    #      This bridge is not added because, even though it is not abstract, it
    #      is highly parameterized, and parameterized versions such as
    #      NormOneConeToNormConeBridge are added.
    MOI.Bridges.add_bridge(model, NormSpectralBridge{T})
    MOI.Bridges.add_bridge(model, NormToPowerBridge{T})
    #  * NumberConversionBridge{T}
    #      This bridge is not added by default because it would silently enable
    #      models with mixed precision. In most cases, this is a bug in the
    #      user's code, so we leave this bridge as opt-in.
    MOI.Bridges.add_bridge(model, QuadtoSOCBridge{T})
    MOI.Bridges.add_bridge(model, ReifiedAllDifferentToCountDistinctBridge{T})
    MOI.Bridges.add_bridge(model, ReifiedCountDistinctToMILPBridge{T})
    MOI.Bridges.add_bridge(model, RelativeEntropyBridge{T})
    MOI.Bridges.add_bridge(model, RootDetBridge{T})
    #  * RSOCtoNonConvexQuadBridge{T}
    #      This bridge is not added by default because it starts with a convex
    #      conic constraint and generate a non-convex constraint (in the QCP
    #      interpretation).
    MOI.Bridges.add_bridge(model, RSOCtoPSDBridge{T})
    MOI.Bridges.add_bridge(model, RSOCtoSOCBridge{T})
    MOI.Bridges.add_bridge(model, ScalarFunctionizeBridge{T})
    MOI.Bridges.add_bridge(model, ScalarizeBridge{T})
    MOI.Bridges.add_bridge(model, ScalarSlackBridge{T})
    MOI.Bridges.add_bridge(model, SecondOrderConeToNormConeBridge{T})
    MOI.Bridges.add_bridge(model, SemiToBinaryBridge{T})
    #  * SetConversionBridge{T}
    #      This bridge is not added because, even though it is not abstract, it
    #      is highly parameterized, and it intended for use by MOI extensions.
    MOI.Bridges.add_bridge(model, SetDotInverseScalingBridge{T})
    MOI.Bridges.add_bridge(model, SetDotScalingBridge{T})
    #  * SOCtoNonConvexQuadBridge{T}
    #      This bridge is not added by default because it starts with a convex
    #      conic constraint and generate a non-convex constraint (in the QCP
    #      interpretation).
    #  * SOCtoPSDBridge{T}
    #      This bridge is not added because transforming the `SOC` to `RSOC` and
    #      then to `PSD` produces a smaller SDP constraint. `RSOCtoPSDBridge` is
    #      added by default.
    MOI.Bridges.add_bridge(model, SOCtoRSOCBridge{T})
    MOI.Bridges.add_bridge(model, SOS1ToMILPBridge{T})
    MOI.Bridges.add_bridge(model, SOS2ToMILPBridge{T})
    MOI.Bridges.add_bridge(model, SplitComplexEqualToBridge{T})
    MOI.Bridges.add_bridge(model, SplitComplexZerosBridge{T})
    MOI.Bridges.add_bridge(model, SplitHyperRectangleBridge{T})
    MOI.Bridges.add_bridge(model, SplitIntervalBridge{T})
    MOI.Bridges.add_bridge(model, SquareBridge{T})
    MOI.Bridges.add_bridge(model, TableToMILPBridge{T})
    MOI.Bridges.add_bridge(model, ToScalarNonlinearBridge{T})
    MOI.Bridges.add_bridge(model, ToScalarQuadraticBridge{T})
    MOI.Bridges.add_bridge(model, ToVectorQuadraticBridge{T})
    MOI.Bridges.add_bridge(model, VectorFunctionizeBridge{T})
    MOI.Bridges.add_bridge(model, VectorizeBridge{T})
    MOI.Bridges.add_bridge(model, IntervalToHyperRectangleBridge{T})
    MOI.Bridges.add_bridge(model, VectorSlackBridge{T})
    MOI.Bridges.add_bridge(model, ZeroOneBridge{T})
    return
end

end
