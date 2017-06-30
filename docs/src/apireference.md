```@meta
CurrentModule = MathOptInterface
```

# API

Some introduction to API. List basic standalone methods.

```@docs
AbstractModel
AbstractSolver
```

```@docs
Model
optimize!
freemodel!
```

## Variables

```@docs
VariableReference
candelete(::AbstractModel,::VariableReference)
isvalid(::AbstractModel,::VariableReference)
delete!(::AbstractModel,::VariableReference)
addvariables!
addvariable!
```

## Objectives

How to add and set objectives.
```@docs
setobjective!
modifyobjective!
getobjectiveconstant
getobjectiveaffine
```

## Constraints

How to add and modify constraints.
```@docs
VariablewiseConstraintReference
AffineConstraintReference
QuadraticConstraintReference
candelete(::AbstractModel,::ConstraintReference)
isvalid(::AbstractModel,::ConstraintReference)
delete!(::AbstractModel,::ConstraintReference)
addconstraint!
modifyconstraint!
getconstraintconstant
getconstraintaffine
getconstraintquadratic
```

## Sets

List of sets.
```@docs
AbstractSet
Reals
Zeros
NonNegatives
NonPositives
GreaterThan
LessThan
Interval
SecondOrderCone
ExponentialCone
DualExponentialCone
PowerCone
DualPowerCone
PositiveSemidefiniteConeTriangle
PositiveSemidefiniteConeScaled
Integers
ZeroOne
SOS1
SOS2
```

Functions for getting and setting properties of sets.
```@docs
dimension
```

## Attributes

### Solver or Model Attributes

List of solver or model attributes.
```@docs
ReturnsDuals
SupportsAddConstraintAfterSolve
SupportsDeleteConstraint
SupportsAddVariableAfterSolver
SupportsQuadraticObjective
SupportsConicThroughQuadratic
ObjectiveValue
ObjectiveBound
RelativeGap
SolveTime
Sense
SimplexIterations
BarrierIterations
NodeCount
RawSolver
ResultCount
NumberOfVariables
NumberOfVariablewiseConstraints
NumberOfAffineConstraints
NumberOfQuadraticConstraints
SupportsVariablewiseConstraint
SupportsAffineConstraint
SupportsQuadraticConstraint
TerminationStatus
PrimalStatus
DualStatus
```

Functions for getting and setting model or solver attributes.
```@docs
AbstractSolverOrModelAttribute
AbstractVariableAttribute
AbstractConstraintAttribute
cangetattribute
getattribute
getattribute!
cansetattribute
setattribute!
```

### Variable Attributes

List of attributes associated with variables. Calls to `getattribute` and `setattribute!` should include as an argument a single `VariableReference` or a vector of `VariableReference` objects.
```@docs
VariablePrimalStart
VariablePrimal
VariableBasisStatus
```

### Constraint Attributes

List of attributes associated with constraints. Calls to `getattribute` and `setattribute!` should include as an argument a single `ConstraintReference` or a vector of `ConstriaintReference{T}` objects.
```@docs
ConstraintPrimalStart
ConstraintDualStart
ConstraintPrimal
ConstraintDual
ConstraintBasisStatus
```

## Status Codes

### Termination Status

The `TerminationStatus` attribute is meant to explain the reason why the solver stopped executing. The value of the attribute is of type `TerminationStatusCode`.
```@docs
TerminationStatusCode
```

### Result Status

The `PrimalStatus` and `DualStatus` attributes are meant to explain how to interpret the result returned by the solver. The value of the attributes are of type `ResultStatusCode`.
```@docs
ResultStatusCode
```

### Basis Status

```@docs
BasisStatusCode
```
