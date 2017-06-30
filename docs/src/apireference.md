```@meta
CurrentModule = MathOptInterface
```

# API

[Some introduction to API. List basic standalone methods.]

## Attributes

List of attribute categories.

```@docs
AbstractSolverOrModelAttribute
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

## Solver and Model

[separate solver and model?]

```@docs
AbstractModel
AbstractSolver
```

```@docs
Model
optimize!
freemodel!
```

List of solver or model attributes. [category AbstractSolverOrModelAttribute]

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

### Termination Status

The ``TerminationStatus`` attribute indicates why the solver stopped executing.
The value of the attribute is of type ``TerminationStatusCode``.

```@docs
TerminationStatusCode
```

### Result Status

The ``PrimalStatus`` and ``DualStatus`` attributes indicate how to interpret the result returned by the solver.
The value of the attribute is of type ``ResultStatusCode``.

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
candelete(::AbstractModel,::VariableReference)
isvalid(::AbstractModel,::VariableReference)
delete!(::AbstractModel,::VariableReference)
addvariables!
addvariable!
```

List of attributes associated with variables. [category AbstractVariableAttribute]
Calls to ``getattribute`` and ``setattribute!`` should include as an argument a single ``VariableReference`` or a vector of ``VariableReference`` objects.

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
candelete(::AbstractModel,::ConstraintReference)
isvalid(::AbstractModel,::ConstraintReference)
delete!(::AbstractModel,::ConstraintReference)
addconstraint!
modifyconstraint!
getconstraintconstant
getconstraintaffine
getconstraintquadratic
```

List of attributes associated with constraints. [category AbstractConstraintAttribute]
Calls to ``getattribute`` and ``setattribute!`` should include as an argument a single ``ConstraintReference`` or a vector of ``ConstraintReference{T}`` objects.

[why is ConstraintBasisStatus under constraint attributes but below we have a basis status attribute separately??]
```@docs
ConstraintPrimalStart
ConstraintDualStart
ConstraintPrimal
ConstraintDual
ConstraintBasisStatus
```
