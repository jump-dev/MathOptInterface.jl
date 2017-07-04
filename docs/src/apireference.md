```@meta
CurrentModule = MathOptInterface
```

# API

[Some introduction to API. List basic standalone methods.]

## Attributes

List of attribute categories.

```@docs
AbstractSolverAttribute
AbstractSolverInstanceAttribute
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
supportsproblem
```

List of solver attributes

```@docs
ReturnsDuals
SupportsAddConstraintAfterSolve
SupportsDeleteConstraint
SupportsAddVariableAfterSolve
SupportsConicThroughQuadratic
```

## Solver Instance

```@docs
AbstractSolverInstance
```

```@docs
SolverInstance
optimize!
free!
```

List of solver instance attributes

```@docs
RawSolver
Sense
NumberOfVariables
NumberOfConstraints
ListOfPresentConstraints
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

## Variables and Constraints

### Basis Status

The `BasisStatus` attribute of a variable or constraint describes its status with respect to a basis, if one is known.
The value of the attribute is of type `BasisStatusCode`.

```@docs
BasisStatusCode
```

### Variables

Variable references and functions for adding and deleting variables.

[attribute that points to the (scalar) variable domain??? eg GreaterThan, NonNegatives, ZeroOne, SemiInteger]
```@docs
VariableReference
candelete(::AbstractSolverInstance,::VariableReference)
isvalid(::AbstractSolverInstance,::VariableReference)
delete!(::AbstractSolverInstance,::VariableReference)
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

### Constraints

Constraint references and functions for adding, modifying, and removing constraints.

```@docs
ConstraintReference
candelete(::AbstractSolverInstance,::ConstraintReference)
isvalid(::AbstractSolverInstance,::ConstraintReference)
delete!(::AbstractSolverInstance,::ConstraintReference)
addconstraint!
modifyconstraint!
getconstraintconstant
getconstraintaffine
getconstraintquadratic
```

List of attributes associated with constraints. [category AbstractConstraintAttribute]
Calls to `getattribute` and `setattribute!` should include as an argument a single `ConstraintReference` or a vector of `ConstraintReference{T}` objects.

```@docs
ConstraintPrimalStart
ConstraintDualStart
ConstraintPrimal
ConstraintDual
ConstraintBasisStatus
```

## Functions

List of recognized functions.
```@docs
AbstractFunction
ScalarVariablewiseFunction
VectorVariablewiseFunction
ScalarAffineFunction
VectorAffineFunction
ScalarQuadraticFunction
VectorQuadraticFunction
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

## Objectives

Functions for adding and modifying objectives.

```@docs
setobjective!
modifyobjective!
getobjectiveconstant
getobjectiveaffine
```
