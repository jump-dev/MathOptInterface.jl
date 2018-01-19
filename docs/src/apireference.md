```@meta
CurrentModule = MathOptInterface
```

# API Reference

[Some introduction to API. List basic standalone methods.]

## Attributes

List of attribute categories.

```@docs
AbstractInstanceAttribute
AbstractVariableAttribute
AbstractConstraintAttribute
```

Functions for getting and setting attributes.

```@docs
canget
get
get!
canset
set!
supports
```

## Instance

```@docs
AbstractInstance
AbstractStandaloneInstance
AbstractSolverInstance
write
read!
```

Copying

```@docs
copy!
CopyResult
CopyStatusCode
mustcopy
```

List of instance attributes

```@docs
Name
ObjectiveSense
NumberOfVariables
ListOfVariableIndices
ListOfConstraints
NumberOfConstraints
ListOfConstraintIndices
ListOfInstanceAttributesSet
ListOfVariableAttributesSet
ListOfConstraintAttributesSet
```

There are no attributes specific to a standalone instance.

## Solver instance

```@docs
optimize!
free!
```

List of solver instance attributes


```@docs
RawSolver
ResultCount
ObjectiveFunction
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

### Index types

```@docs
VariableIndex
ConstraintIndex
candelete
isvalid
delete!(::AbstractSolverInstance,::Index)
```

### Variables

Functions for adding variables. For deleting, see index types section.

```@docs
addvariables!
addvariable!
```

List of attributes associated with variables. [category AbstractVariableAttribute]
Calls to `get` and `set!` should include as an argument a single `VariableIndex` or a vector of `VariableIndex` objects.

```@docs
VariableName
VariablePrimalStart
VariablePrimal
VariableBasisStatus
```

### Constraints

Functions for adding and modifying constraints.

```@docs
isvalid(::AbstractSolverInstance,::ConstraintIndex)
canaddconstraint
addconstraint!
addconstraints!
modifyconstraint!
canmodifyconstraint
transformconstraint!
cantransformconstraint
supportsconstraint
```

List of attributes associated with constraints. [category AbstractConstraintAttribute]
Calls to `get` and `set!` should include as an argument a single `ConstraintIndex` or a vector of `ConstraintIndex{F,S}` objects.

```@docs
ConstraintName
ConstraintPrimalStart
ConstraintDualStart
ConstraintPrimal
ConstraintDual
ConstraintBasisStatus
ConstraintFunction
ConstraintSet
```

## Functions and function modifications

List of recognized functions.
```@docs
AbstractFunction
SingleVariable
VectorOfVariables
ScalarAffineFunction
VectorAffineFunction
ScalarQuadraticFunction
VectorQuadraticFunction
```

List of function modifications.
```@docs
ScalarConstantChange
VectorConstantChange
ScalarCoefficientChange
MultirowChange
```

## Sets

List of recognized sets.

```@docs
AbstractSet
Reals
Zeros
Nonnegatives
Nonpositives
GreaterThan
LessThan
EqualTo
Interval
SecondOrderCone
RotatedSecondOrderCone
GeometricMeanCone
ExponentialCone
DualExponentialCone
PowerCone
DualPowerCone
PositiveSemidefiniteConeTriangle
PositiveSemidefiniteConeSquare
LogDetConeTriangle
LogDetConeSquare
RootDetConeTriangle
RootDetConeSquare
Integer
ZeroOne
Semicontinuous
Semiinteger
SOS1
SOS2
```

Functions for getting and setting properties of sets.

```@docs
dimension
```

## Objective modifications

Functions for modifying objective functions. Use `ObjectiveFunction` and `ObjectiveSense` to set and query the objective function.

```@docs
modifyobjective!
canmodifyobjective
```
