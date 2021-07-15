```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface

    # For compatibility with both Julia 1.0.5 and 1.5.2
    # Upon the Julia LTS version becoming 1.6, these imports could be dropped,
    # and all ScalarAffineTerm and VariableIndex instances in doctests below
    # could be replaced with MOI.ScalarAffineTerm and MOI.VariableIndex
    # Check discussion at PR 1184: https://github.com/jump-dev/MathOptInterface.jl/pull/1184#discussion_r515300914
    import MathOptInterface.ScalarAffineTerm
    import MathOptInterface.VariableIndex
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Bridges

```@docs
Bridges.AbstractBridge
Bridges.AbstractBridgeOptimizer
Bridges.LazyBridgeOptimizer
Bridges.add_bridge
Bridges.remove_bridge
Bridges.has_bridge
Bridges.full_bridge_optimizer
Bridges.debug_supports_constraint
Bridges.debug_supports
Bridges.bridged_variable_function
Bridges.unbridged_variable_function
```

## Constraint bridges

```@docs
Bridges.Constraint.AbstractBridge
Bridges.Constraint.SingleBridgeOptimizer
Bridges.Constraint.add_all_bridges
```

### [SetMap bridges](@id constraint_set_map)

```@docs
Bridges.Variable.SetMapBridge
Bridges.Constraint.SetMapBridge
Bridges.map_set
Bridges.inverse_map_set
Bridges.map_function
Bridges.inverse_map_function
Bridges.adjoint_map_function
Bridges.inverse_adjoint_map_function
```

### [Bridges implemented](@id constraint_bridges_ref)

```@docs
Bridges.Constraint.GreaterToIntervalBridge
Bridges.Constraint.GreaterToLessBridge
Bridges.Constraint.LessToIntervalBridge
Bridges.Constraint.LessToGreaterBridge
Bridges.Constraint.NonnegToNonposBridge
Bridges.Constraint.NonposToNonnegBridge
Bridges.Constraint.VectorizeBridge
Bridges.Constraint.ScalarizeBridge
Bridges.Constraint.ScalarSlackBridge
Bridges.Constraint.VectorSlackBridge
Bridges.Constraint.ScalarFunctionizeBridge
Bridges.Constraint.VectorFunctionizeBridge
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
Bridges.Constraint.IndicatorSOS1Bridge
Bridges.Constraint.SemiToBinaryBridge
Bridges.Constraint.ZeroOneBridge
```

## [Variable bridges](@id ref_variable_bridges)

```@docs
Bridges.Variable.AbstractBridge
Bridges.Variable.SingleBridgeOptimizer
Bridges.Variable.add_all_bridges
```

### [Bridges implemented](@id variable_bridges_ref)

```@docs
Bridges.Variable.ZerosBridge
Bridges.Variable.FreeBridge
Bridges.Variable.NonposToNonnegBridge
Bridges.Variable.VectorizeBridge
Bridges.Variable.SOCtoRSOCBridge
Bridges.Variable.RSOCtoSOCBridge
Bridges.Variable.RSOCtoPSDBridge
```

## Objective bridges

```@docs
Bridges.Objective.AbstractBridge
Bridges.Objective.SingleBridgeOptimizer
Bridges.Objective.add_all_bridges
```

### [Bridges implemented](@id objective_bridges_ref)

```@docs
Bridges.Objective.SlackBridge
Bridges.Objective.FunctionizeBridge
```
