```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Models

## Attribute interface

```@docs
is_set_by_optimize
is_copyable
get
get!
set
supports
attribute_value_type
```

## Model interface

```@docs
ModelLike
is_empty
empty!
write_to_file
read_from_file
supports_incremental_interface
copy_to
IndexMap
```

## Model attributes

```@docs
AbstractModelAttribute
Name
ObjectiveFunction
ObjectiveFunctionType
ObjectiveSense
NumberOfVariables
ListOfVariableIndices
ListOfConstraintTypesPresent
NumberOfConstraints
ListOfConstraintIndices
ListOfOptimizerAttributesSet
ListOfModelAttributesSet
ListOfVariableAttributesSet
ListOfConstraintAttributesSet
```

## Optimizer interface

```@docs
AbstractOptimizer
OptimizerWithAttributes
optimize!
instantiate
```

## Optimizer attributes

```@docs
AbstractOptimizerAttribute
SolverName
Silent
TimeLimitSec
RawOptimizerAttribute
NumberOfThreads
RawSolver
```

List of attributes useful for optimizers

```@docs
TerminationStatus
TerminationStatusCode
PrimalStatus
DualStatus
ResultStatusCode
RawStatusString
ResultCount
ObjectiveValue
DualObjectiveValue
ObjectiveBound
RelativeGap
SolveTimeSec
SimplexIterations
BarrierIterations
NodeCount
```

### Conflict Status

```@docs
compute_conflict!
ConflictStatus
ConflictStatusCode
ConstraintConflictStatus
ConflictParticipationStatusCode
```
