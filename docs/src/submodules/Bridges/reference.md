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
Bridges.bridged_variable_function
Bridges.unbridged_variable_function
Bridges.bridged_function
Bridges.supports_constraint_bridges
Bridges.recursive_model
```

## LazyBridgeOptimizer API

```@docs
Bridges.LazyBridgeOptimizer
Bridges.full_bridge_optimizer
Bridges.ListOfNonstandardBridges
Bridges.add_bridge
Bridges.remove_bridge
Bridges.has_bridge
Bridges.debug_supports_constraint
Bridges.debug_supports
```

## [SetMap API](@id constraint_set_map)

```@docs
Bridges.map_set
Bridges.inverse_map_set
Bridges.map_function
Bridges.inverse_map_function
Bridges.adjoint_map_function
Bridges.inverse_adjoint_map_function
```

## Constraint bridge API

```@docs
Bridges.Constraint.AbstractBridge
Bridges.Constraint.AbstractFunctionConversionBridge
Bridges.Constraint.SingleBridgeOptimizer
Bridges.Constraint.add_all_bridges
Bridges.Constraint.FlipSignBridge
Bridges.Constraint.AbstractToIntervalBridge
Bridges.Constraint.SetMapBridge
```

## [Constraint bridges implemented](@id constraint_bridges_ref)

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

## [Variable bridge API](@id ref_variable_bridges)

```@docs
Bridges.Variable.AbstractBridge
Bridges.Variable.SingleBridgeOptimizer
Bridges.Variable.add_all_bridges
Bridges.Variable.FlipSignBridge
Bridges.Variable.SetMapBridge
Bridges.Variable.unbridged_map
```

## [Variable bridges implemented](@id variable_bridges_ref)

```@docs
Bridges.Variable.ZerosBridge
Bridges.Variable.FreeBridge
Bridges.Variable.NonposToNonnegBridge
Bridges.Variable.VectorizeBridge
Bridges.Variable.SOCtoRSOCBridge
Bridges.Variable.RSOCtoSOCBridge
Bridges.Variable.RSOCtoPSDBridge
```

## Objective bridge API

```@docs
Bridges.Objective.AbstractBridge
Bridges.Objective.SingleBridgeOptimizer
Bridges.Objective.add_all_bridges
```

### [Objective bridges implemented](@id objective_bridges_ref)

```@docs
Bridges.Objective.SlackBridge
Bridges.Objective.FunctionizeBridge
```

### Bridging graph API

```@docs
Bridges.Graph
Bridges.VariableNode
Bridges.ConstraintNode
Bridges.ObjectiveNode
Bridges.Edge
Bridges.ObjectiveEdge
Bridges.add_node
Bridges.add_edge
Bridges.set_variable_constraint_node
Bridges.bridge_index
Bridges.is_variable_edge_best
```
