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
CallbackVariablePrimal
CallbackNodeStatusCode
CALLBACK_NODE_STATUS_INTEGER
CALLBACK_NODE_STATUS_FRACTIONAL
CALLBACK_NODE_STATUS_UNKNOWN
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
HeuristicSolution
HeuristicSolutionStatus
HEURISTIC_SOLUTION_ACCEPTED
HEURISTIC_SOLUTION_REJECTED
HEURISTIC_SOLUTION_UNKNOWN
```
