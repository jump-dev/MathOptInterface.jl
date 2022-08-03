```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# List of bridges

This section describes the [`Bridges.AbstractBridge`](@ref)s that are
implemented in MathOptInterface.

## [Constraint bridges](@id constraint_bridges_ref)

These bridges are subtyptes of [`Bridges.Constraint.AbstractBridge`](@ref).

```@docs
Bridges.Constraint.GreaterToIntervalBridge
Bridges.Constraint.LessToIntervalBridge
Bridges.Constraint.GreaterToLessBridge
Bridges.Constraint.LessToGreaterBridge
Bridges.Constraint.NonnegToNonposBridge
Bridges.Constraint.NonposToNonnegBridge
Bridges.Constraint.VectorizeBridge
Bridges.Constraint.ScalarizeBridge
Bridges.Constraint.ScalarSlackBridge
Bridges.Constraint.VectorSlackBridge
Bridges.Constraint.ScalarFunctionizeBridge
Bridges.Constraint.VectorFunctionizeBridge
Bridges.Constraint.SplitComplexEqualToBridge
Bridges.Constraint.SplitComplexZerosBridge
Bridges.Constraint.SplitIntervalBridge
Bridges.Constraint.SOCtoRSOCBridge
Bridges.Constraint.RSOCtoSOCBridge
Bridges.Constraint.SOCtoNonConvexQuadBridge
Bridges.Constraint.RSOCtoNonConvexQuadBridge
Bridges.Constraint.QuadtoSOCBridge
Bridges.Constraint.SOCtoPSDBridge
Bridges.Constraint.RSOCtoPSDBridge
Bridges.Constraint.NormInfinityBridge
Bridges.Constraint.NormOneBridge
Bridges.Constraint.GeoMeantoRelEntrBridge
Bridges.Constraint.GeoMeanBridge
Bridges.Constraint.RelativeEntropyBridge
Bridges.Constraint.NormSpectralBridge
Bridges.Constraint.NormNuclearBridge
Bridges.Constraint.SquareBridge
Bridges.Constraint.RootDetBridge
Bridges.Constraint.LogDetBridge
Bridges.Constraint.IndicatorActiveOnFalseBridge
Bridges.Constraint.IndicatorGreaterToLessThanBridge
Bridges.Constraint.IndicatorLessToGreaterThanBridge
Bridges.Constraint.IndicatorSOS1Bridge
Bridges.Constraint.SemiToBinaryBridge
Bridges.Constraint.ZeroOneBridge
Bridges.Constraint.AllDifferentToCountDistinctBridge
Bridges.Constraint.BinPackingToMILPBridge
Bridges.Constraint.CircuitToMILPBridge
Bridges.Constraint.CountAtLeastToCountBelongsBridge
Bridges.Constraint.CountBelongsToMILPBridge
Bridges.Constraint.CountDistinctToMILPBridge
Bridges.Constraint.CountGreaterThanToMILPBridge
Bridges.Constraint.TableToMILPBridge
```

## [Objective bridges](@id objective_bridges_ref)

These bridges are subtyptes of [`Bridges.Objective.AbstractBridge`](@ref).

```@docs
Bridges.Objective.FunctionizeBridge
Bridges.Objective.SlackBridge
```

## [Variable bridges](@id variable_bridges_ref)

These bridges are subtyptes of [`Bridges.Variable.AbstractBridge`](@ref).

```@docs
Bridges.Variable.FreeBridge
Bridges.Variable.NonposToNonnegBridge
Bridges.Variable.RSOCtoPSDBridge
Bridges.Variable.RSOCtoSOCBridge
Bridges.Variable.SOCtoRSOCBridge
Bridges.Variable.VectorizeBridge
Bridges.Variable.ZerosBridge
Bridges.Variable.HermitianToSymmetricPSDBridge
```
