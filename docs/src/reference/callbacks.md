```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    import MathOptInterface as MOI
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Callbacks

```@docs
AbstractCallback
AbstractSubmittable
submit
```

## Attributes

```@docs
CallbackNodeStatus
CallbackNodeStatusCode
CallbackVariablePrimal
```

## Lazy constraints

```@docs
LazyConstraintCallback
LazyConstraint
```

## User cuts

```@docs
UserCutCallback
UserCut
```

## Heuristic solutions

```@docs
HeuristicCallback
HeuristicSolutionStatus
HeuristicSolution
```
