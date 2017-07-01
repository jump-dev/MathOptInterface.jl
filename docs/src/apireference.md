```@meta
CurrentModule = MathOptInterface
```

# API

[Some introduction to API. List basic standalone methods.]

## Attributes

List of attribute categories.

```@docs
AbstractSolverAttribute
AbstractInstanceAttribute
AbstractVariableAttribute
AbstractConstraintAttribute
```

Functions for getting and setting attributes.

```@docs
cangetattribute
getattribute
getattribute!
cansetattribute
setattribute!
```

## Solver

```@docs
AbstractSolver
```

List of solver attributes

```@docs
ReturnsDuals
SupportsAddConstraintAfterSolve
SupportsDeleteConstraint
SupportsAddVariableAfterSolve
SupportsQuadraticObjective
SupportsConicThroughQuadratic
```

## Instance

```@docs
AbstractInstance
```

```@docs
Instance
optimize!
freeinstance!
```

List of instance attributes

```@docs
RawSolver
Sense
NumberOfVariables
NumberOfVariablewiseConstraints
NumberOfAffineConstraints
NumberOfQuadraticConstraints
ResultCount
ObjectiveValue
ObjectiveBound
RelativeGap
SolveTime
SimplexIterations
BarrierIterations
NodeCount
TerminationStatus
PrimalStatus
DualStatus
```

### Termination Status

The `TerminationStatus` attribute indicates why the solver stopped executing.
The value of the attribute is of type `TerminationStatusCode`.

```@docs
TerminationStatusCode
```

### Result Status

The `PrimalStatus` and `DualStatus` attributes indicate how to interpret the result returned by the solver.
The value of the attribute is of type `ResultStatusCode`.

```@docs
ResultStatusCode
```

### Basis Status

[what? BasisStatus]

```@docs
BasisStatusCode
```

## Sets

List of recognized sets.

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

## Variables

Variable references and functions for adding and deleting variables.

[attribute that points to the (scalar) variable domain??? eg GreaterThan, NonNegatives, ZeroOne, SemiInteger]
```@docs
VariableReference
candelete(::AbstractInstance,::VariableReference)
isvalid(::AbstractInstance,::VariableReference)
delete!(::AbstractInstance,::VariableReference)
addvariables!
addvariable!
```

List of attributes associated with variables. [category AbstractVariableAttribute]
Calls to `getattribute` and `setattribute!` should include as an argument a single `VariableReference` or a vector of `VariableReference` objects.

```@docs
VariablePrimalStart
VariablePrimal
VariableBasisStatus
```

## Objectives

Functions for adding and modifying objectives.

```@docs
setobjective!
modifyobjective!
getobjectiveconstant
getobjectiveaffine
```

## Constraints

Constraint references and functions for adding, modifying, and removing constraints.

```@docs
VariablewiseConstraintReference
AffineConstraintReference
QuadraticConstraintReference
candelete(::AbstractInstance,::ConstraintReference)
isvalid(::AbstractInstance,::ConstraintReference)
delete!(::AbstractInstance,::ConstraintReference)
addconstraint!
modifyconstraint!
getconstraintconstant
getconstraintaffine
getconstraintquadratic
```

List of attributes associated with constraints. [category AbstractConstraintAttribute]
Calls to `getattribute` and `setattribute!` should include as an argument a single `ConstraintReference` or a vector of `ConstraintReference{T}` objects.

[why is ConstraintBasisStatus under constraint attributes but below we have a basis status attribute separately??]
```@docs
ConstraintPrimalStart
ConstraintDualStart
ConstraintPrimal
ConstraintDual
ConstraintBasisStatus
```
