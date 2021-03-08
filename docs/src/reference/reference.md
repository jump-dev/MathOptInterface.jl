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

# API Reference

[Some introduction to API. List basic standalone methods.]

## Attributes

List of attribute categories.

```@docs
AbstractOptimizerAttribute
AbstractModelAttribute
AbstractVariableAttribute
AbstractConstraintAttribute
```

Attributes can be set in different ways:

* it is either set when the model is created like [`SolverName`](@ref) and
  [`RawSolver`](@ref),
* or explicitly when the model is copied like [`ObjectiveSense`](@ref),
* or implicitly, e.g., [`NumberOfVariables`](@ref) is implicitly set by
  [`add_variable`](@ref) and [`ConstraintFunction`](@ref) is implicitly set by
  [`add_constraint`](@ref).
* or it is set to contain the result of the optimization during
  [`optimize!`](@ref) like [`VariablePrimal`](@ref).

The following functions allow to distinguish between some of these different
categories:
```@docs
is_set_by_optimize
is_copyable
```

Functions for getting and setting attributes.

```@docs
get
get!
set
supports
```

### Submit

Objects may be submitted to an optimizer using [`submit`](@ref).
```@docs
AbstractSubmittable
submit
```

List of submittables

```@docs
LazyConstraint
HeuristicSolutionStatus
HeuristicSolution
UserCut
```

## Model Interface

```@docs
ModelLike
is_empty
empty!
write_to_file
read_from_file
```

Copying

```@docs
copy_to
```

List of model attributes

```@docs
Name
ObjectiveSense
NumberOfVariables
ListOfVariableIndices
ListOfConstraints
NumberOfConstraints
ListOfConstraintIndices
ListOfOptimizerAttributesSet
ListOfModelAttributesSet
ListOfVariableAttributesSet
ListOfConstraintAttributesSet
```

## Optimizers

```@docs
AbstractOptimizer
optimize!
OptimizerWithAttributes
instantiate
```

List of optimizers attributes

```@docs
SolverName
Silent
TimeLimitSec
RawParameter
NumberOfThreads
```

List of attributes useful for optimizers


```@docs
RawSolver
ResultCount
ObjectiveFunction
ObjectiveFunctionType
ObjectiveValue
DualObjectiveValue
ObjectiveBound
RelativeGap
SolveTime
SimplexIterations
BarrierIterations
NodeCount
TerminationStatus
RawStatusString
PrimalStatus
DualStatus
```

Attributes relating to solver callbacks:

```@docs
AbstractCallback
LazyConstraintCallback
HeuristicCallback
UserCutCallback
CallbackNodeStatus
CallbackNodeStatusCode
CallbackVariablePrimal
```
### Termination Status

The `TerminationStatus` attribute indicates why the optimizer stopped executing.
The value of the attribute is of type `TerminationStatusCode`.

```@docs
TerminationStatusCode
```

### Conflict Status

The `ConflictStatus` attribute indicates why the conflict finder stopped executing.
The value of the attribute is of type `ConflictStatusCode`.

```@docs
compute_conflict!
ConflictStatus
ConflictStatusCode
ConstraintConflictStatus
ConflictParticipationStatusCode
```

### Result Status

The `PrimalStatus` and `DualStatus` attributes indicate how to interpret the result returned by the solver.
The value of the attribute is of type `ResultStatusCode`.

```@docs
ResultStatusCode
```

## Variables and Constraints

### Basis Status

The `BasisStatus` attribute of a constraint describes its status with respect to
a basis, if one is known. The value of the attribute is of type
`BasisStatusCode`.

```@docs
BasisStatusCode
```

### Index types

```@docs
VariableIndex
ConstraintIndex
is_valid
throw_if_not_valid
delete(::ModelLike, ::Index)
delete(::ModelLike, ::Vector{<:Index})
```

### Variables

*Free variables* are the variables created with [`add_variable`](@ref) or
[`add_variables`](@ref) while *constrained variables* are
the variables created with [`add_constrained_variable`](@ref) or
[`add_constrained_variables`](@ref). Adding constrained variables instead of
constraining free variables with [`add_constraint`](@ref) allows
[variable bridges](@ref variable_bridges) to be used.
Note further that free variables that are constrained with
[`add_constraint`](@ref) may be copied by [`copy_to`](@ref) with
[`add_constrained_variable`](@ref) or [`add_constrained_variables`](@ref) by the
[`Utilities.CachingOptimizer`](@ref).
More precisely, the attributes do not distinguish constraints on variables
created with `add_constrained_variable(s)` or `add_variable(s)`/`add_constraint`.
When the model is copied, if a variable is constrained in several sets,
the implementation of [`copy_to`](@ref) can determine whether it is added
using [`add_variable`](@ref) or [`add_constrained_variable`](@ref) with one
of the sets. The rest of the constraints on the variables are added
with [`add_constraint`](@ref). For deleting, see [Index types](@ref).

```@docs
add_variable
add_variables
add_constrained_variable
add_constrained_variables
supports_add_constrained_variable
supports_add_constrained_variables
```

List of attributes associated with variables. [category AbstractVariableAttribute]
Calls to `get` and `set` should include as an argument a single `VariableIndex` or a vector of `VariableIndex` objects.

```@docs
VariableName
VariablePrimalStart
VariablePrimal
```

### Constraints

Functions for adding and modifying constraints.

```@docs
is_valid(::ModelLike,::ConstraintIndex)
add_constraint
add_constraints
transform
supports_constraint
```

List of attributes associated with constraints. [category AbstractConstraintAttribute]
Calls to `get` and `set` should include as an argument a single `ConstraintIndex` or a vector of `ConstraintIndex{F,S}` objects.

```@docs
ConstraintName
ConstraintPrimalStart
ConstraintDualStart
ConstraintPrimal
ConstraintDual
ConstraintBasisStatus
ConstraintFunction
CanonicalConstraintFunction
ConstraintSet
```

Note that setting the [`ConstraintFunction`](@ref) of a [`SingleVariable`]
constraint is not allowed:
```@docs
SettingSingleVariableFunctionNotAllowed
```

## Functions and function modifications

List of recognized functions.
```@docs
AbstractFunction
AbstractVectorFunction
SingleVariable
VectorOfVariables
ScalarAffineTerm
ScalarAffineFunction
VectorAffineTerm
VectorAffineFunction
ScalarQuadraticTerm
ScalarQuadraticFunction
VectorQuadraticTerm
VectorQuadraticFunction
```

Functions for getting and setting properties of functions.

```@docs
output_dimension
constant(f::Union{ScalarAffineFunction, ScalarQuadraticFunction})
constant(f::Union{VectorAffineFunction, VectorQuadraticFunction})
constant(f::SingleVariable, ::DataType)
constant(f::VectorOfVariables, T::DataType)
```

## Sets

All sets are subtypes of [`AbstractSet`](@ref) and they should either be scalar
or vector sets.
```@docs
AbstractSet
AbstractScalarSet
AbstractVectorSet
```

Functions for getting properties of sets.
```@docs
dimension
dual_set
dual_set_type
constant(s::EqualTo)
supports_dimension_update
update_dimension
```

### Scalar sets

List of recognized scalar sets.
```@docs
GreaterThan
LessThan
EqualTo
Interval
Integer
ZeroOne
Semicontinuous
Semiinteger
```

### Vector sets

List of recognized vector sets.
```@docs
Reals
Zeros
Nonnegatives
Nonpositives
NormInfinityCone
NormOneCone
SecondOrderCone
RotatedSecondOrderCone
GeometricMeanCone
ExponentialCone
DualExponentialCone
PowerCone
DualPowerCone
RelativeEntropyCone
NormSpectralCone
NormNuclearCone
SOS1
SOS2
IndicatorSet
Complements
```

### Matrix sets

Matrix sets are vectorized in order to be subtypes of
[`AbstractVectorSet`](@ref). For sets of symmetric matrices, storing both the
`(i, j)` and `(j, i)` elements is redundant so there exists the
[`AbstractSymmetricMatrixSetTriangle`](@ref) set to represent only the
vectorization of the upper triangular part of the matrix. When the matrix
of expressions constrained to be in the set is not symmetric and hence
the `(i, j)` and `(j, i)` elements should be constrained to be symmetric,
the [`AbstractSymmetricMatrixSetSquare`](@ref) set can be used. The
[`Bridges.Constraint.SquareBridge`](@ref) can transform a set from the square
form to the [`triangular_form`](@ref) by adding appropriate constraints if
the `(i, j)` and `(j, i)` expressions are different.
```@docs
AbstractSymmetricMatrixSetTriangle
AbstractSymmetricMatrixSetSquare
side_dimension
triangular_form
```
List of recognized matrix sets.
```@docs
PositiveSemidefiniteConeTriangle
PositiveSemidefiniteConeSquare
LogDetConeTriangle
LogDetConeSquare
RootDetConeTriangle
RootDetConeSquare
```

## Modifications

Functions for modifying objective and constraint functions.

```@docs
modify
AbstractFunctionModification
ScalarConstantChange
VectorConstantChange
ScalarCoefficientChange
MultirowChange
```

## Nonlinear programming (NLP)

### Attributes

```@docs
NLPBlock
NLPBoundsPair
NLPBlockData
NLPBlockDual
NLPBlockDualStart
```
### NLP evaluator methods

```@docs
AbstractNLPEvaluator
initialize
features_available
eval_objective
eval_constraint
eval_objective_gradient
jacobian_structure
hessian_lagrangian_structure
eval_constraint_jacobian
eval_constraint_jacobian_product
eval_constraint_jacobian_transpose_product
eval_hessian_lagrangian
eval_hessian_lagrangian_product
objective_expr
constraint_expr
```

## Errors

When an MOI call fails on a model, precise errors should be thrown when possible
instead of simply calling `error` with a message. The docstrings for the
respective methods describe the errors that the implementation should thrown in
certain situations. This error-reporting system allows code to distinguish
between internal errors (that should be shown to the user) and unsupported
operations which may have automatic workarounds.

When an invalid index is used in an MOI call, an [`InvalidIndex`](@ref) should
be thrown:
```@docs
InvalidIndex
```

As discussed in [JuMP mapping](@ref), for scalar constraint with a nonzero
function constant, a [`ScalarFunctionConstantNotZero`](@ref) exception may be
thrown:
```@docs
ScalarFunctionConstantNotZero
```

Some [`SingleVariable`](@ref) constraints cannot be combined on the same
variable:
```@docs
LowerBoundAlreadySet
UpperBoundAlreadySet
```

As discussed in [`AbstractCallback`](@ref), trying to [`get`](@ref) attributes
inside a callback may throw:
```@docs
OptimizeInProgress
```

Trying to submit the wrong type of [`AbstractSubmittable`](@ref) inside an
[`AbstractCallback`](@ref) (e.g., a [`UserCut`](@ref) inside a
[`LazyConstraintCallback`](@ref)) will throw:
```@docs
InvalidCallbackUsage
```

The rest of the errors defined in MOI fall in two categories represented by the
following two abstract types:
```@docs
UnsupportedError
NotAllowedError
```

The different [`UnsupportedError`](@ref) and [`NotAllowedError`](@ref) are the
following errors:
```@docs
UnsupportedAttribute
SetAttributeNotAllowed
AddVariableNotAllowed
UnsupportedConstraint
AddConstraintNotAllowed
ModifyConstraintNotAllowed
ModifyObjectiveNotAllowed
DeleteNotAllowed
UnsupportedSubmittable
SubmitNotAllowed
```
