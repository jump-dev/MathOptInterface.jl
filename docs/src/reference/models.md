```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    import MathOptInterface as MOI
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
OptimizationSense
MIN_SENSE
MAX_SENSE
FEASIBILITY_SENSE
NumberOfVariables
ListOfVariableIndices
ListOfConstraintTypesPresent
NumberOfConstraints
ListOfConstraintIndices
ListOfOptimizerAttributesSet
ListOfModelAttributesSet
ListOfVariableAttributesSet
ListOfVariablesWithAttributeSet
ListOfConstraintAttributesSet
ListOfConstraintsWithAttributeSet
UserDefinedFunction
ListOfSupportedNonlinearOperators
```

## Optimizer interface

```@docs
AbstractOptimizer
OptimizerWithAttributes
optimize!
optimize!(::ModelLike, ::ModelLike)
instantiate
default_cache
```

## Optimizer attributes

```@docs
AbstractOptimizerAttribute
SolverName
SolverVersion
Silent
TimeLimitSec
ObjectiveLimit
SolutionLimit
NodeLimit
RawOptimizerAttribute
NumberOfThreads
RawSolver
AbsoluteGapTolerance
RelativeGapTolerance
AutomaticDifferentiationBackend
```

List of attributes useful for optimizers

```@docs
TerminationStatus
TerminationStatusCode
OPTIMIZE_NOT_CALLED
OPTIMAL
INFEASIBLE
DUAL_INFEASIBLE
LOCALLY_SOLVED
LOCALLY_INFEASIBLE
INFEASIBLE_OR_UNBOUNDED
ALMOST_OPTIMAL
ALMOST_INFEASIBLE
ALMOST_DUAL_INFEASIBLE
ALMOST_LOCALLY_SOLVED
ITERATION_LIMIT
TIME_LIMIT
NODE_LIMIT
SOLUTION_LIMIT
MEMORY_LIMIT
OBJECTIVE_LIMIT
NORM_LIMIT
OTHER_LIMIT
SLOW_PROGRESS
NUMERICAL_ERROR
INVALID_MODEL
INVALID_OPTION
INTERRUPTED
OTHER_ERROR
PrimalStatus
DualStatus
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

### ResultStatusCode

```@docs
ResultStatusCode
NO_SOLUTION
FEASIBLE_POINT
NEARLY_FEASIBLE_POINT
INFEASIBLE_POINT
INFEASIBILITY_CERTIFICATE
NEARLY_INFEASIBILITY_CERTIFICATE
REDUCTION_CERTIFICATE
NEARLY_REDUCTION_CERTIFICATE
UNKNOWN_RESULT_STATUS
OTHER_RESULT_STATUS
```

### Conflict Status

```@docs
compute_conflict!
ConflictStatus
ConflictStatusCode
COMPUTE_CONFLICT_NOT_CALLED
NO_CONFLICT_EXISTS
NO_CONFLICT_FOUND
CONFLICT_FOUND
ConflictCount
ConstraintConflictStatus
ConflictParticipationStatusCode
NOT_IN_CONFLICT
IN_CONFLICT
MAYBE_IN_CONFLICT
```
