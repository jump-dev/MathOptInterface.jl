```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
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

### Fallbacks

The value of some attributes can be inferred from the value of other
attributes. For instance, the value of [`ObjectiveValue`](@ref) can be computed
using [`ObjectiveFunction`](@ref) and [`VariablePrimal`](@ref). When a solver
gives access to the objective value, it is better to return this value but
otherwise, [`Utilities.get_fallback`](@ref) can be used.
```julia
function MOI.get(optimizer::Optimizer, attr::MOI.ObjectiveValue)
    return MOI.Utilities.get_fallback(optimizer, attr)
end
```

```@docs
Utilities.get_fallback
```

### Submit

Objects may be submitted to an optimizer using [`submit`](@ref).
```@docs
AbstractSubmittable
submit
```


## Model Interface

```@docs
ModelLike
isempty
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
```

List of optimizers attributes

```@docs
SolverName
Silent
TimeLimitSec
RawParameter
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

### Termination Status

The `TerminationStatus` attribute indicates why the optimizer stopped executing.
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
```

### Variables

*Free variables* are the variables created with [`add_variable`](@ref) or
[`add_variables`](@ref) while *constrained variables* are
the variables created with [`add_constrained_variable`](@ref) or
[`add_constrained_variables`](@ref). Adding constrained variables instead of
constraining free variables with [`add_constraint`](@ref) allows Variable
bridges to be used.
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
SecondOrderCone
RotatedSecondOrderCone
GeometricMeanCone
ExponentialCone
DualExponentialCone
PowerCone
DualPowerCone
SOS1
SOS2
IndicatorSet
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

## Models

[`Utilities.Model`](@ref) provides an implementation of a [`ModelLike`](@ref)
that efficiently supports all functions and sets defined within MOI. However,
given the extensibility of MOI, this might not over all use cases.

[`Utilities.UniversalFallback`](@ref) is a layer that sits on top of any
`ModelLike` and provides non-specialized (slower) fallbacks for constraints and
attributes that the underlying `ModeLike` does not support.

For advanced use cases that need efficient support for functions and sets
defined outside of MOI (but still known at compile time), we provide the
[`Utilities.@model`](@ref) macro.

```@docs
Utilities.Model
Utilities.UniversalFallback
Utilities.@model
```

## Bridges

Bridges can be used for automatic reformulation of a certain constraint type into equivalent constraints.
```@docs
Bridges.AbstractBridge
Bridges.Constraint.AbstractBridge
Bridges.AbstractBridgeOptimizer
Bridges.Constraint.SingleBridgeOptimizer
Bridges.LazyBridgeOptimizer
Bridges.add_bridge
```

Below is the list of bridges implemented in this package.
```@docs
Bridges.Constraint.GreaterToLessBridge
Bridges.Constraint.LessToGreaterBridge
Bridges.Constraint.NonnegToNonposBridge
Bridges.Constraint.NonposToNonnegBridge
Bridges.Constraint.VectorizeBridge
Bridges.Constraint.ScalarizeBridge
Bridges.Constraint.ScalarSlackBridge
Bridges.Constraint.VectorSlackBridge
Bridges.Constraint.ScalarFunctionizeBridge
Bridges.Constraint.VectorFunctionizeBridge
Bridges.Constraint.SplitIntervalBridge
Bridges.Constraint.RSOCBridge
Bridges.Constraint.QuadtoSOCBridge
Bridges.Constraint.GeoMeanBridge
Bridges.Constraint.SquareBridge
Bridges.Constraint.RootDetBridge
Bridges.Constraint.LogDetBridge
Bridges.Constraint.SOCtoPSDBridge
Bridges.Constraint.RSOCtoPSDBridge
Bridges.Constraint.IndicatorActiveOnFalseBridge
```
For each bridge defined in this package, a corresponding bridge optimizer is available with the same name without the "Bridge" suffix, e.g., `SplitInterval` is an `SingleBridgeOptimizer` for the `SplitIntervalBridge`.
Moreover, a `LazyBridgeOptimizer` with all the bridges defined in this package can be obtained with
```@docs
Bridges.full_bridge_optimizer
```

### Bridge interface

A bridge should implement the following functions to be usable by a bridge optimizer:
```@docs
supports_constraint(::Type{<:Bridges.Constraint.AbstractBridge}, ::Type{<:AbstractFunction}, ::Type{<:AbstractSet})
Bridges.Constraint.concrete_bridge_type
Bridges.Constraint.bridge_constraint
Bridges.added_constraint_types
```

When querying the [`NumberOfVariables`](@ref), [`NumberOfConstraints`](@ref)
and [`ListOfConstraintIndices`](@ref), the variables and constraints created
by the bridges in the underlying model are hidden by the bridge optimizer.
For this purpose, the bridge should provide access to the variables and
constraints it has creates by implemented the following methods of
[`get`](@ref):
```@docs
get(::Bridges.Constraint.AbstractBridge, ::NumberOfVariables)
get(::Bridges.Constraint.AbstractBridge, ::NumberOfConstraints)
get(::Bridges.Constraint.AbstractBridge, ::ListOfConstraintIndices)
```

## Copy utilities

The following utilities can be used to implement [`copy_to`](@ref). See
[Implementing copy](@ref) for more details.

```@docs
Utilities.automatic_copy_to
Utilities.default_copy_to
Utilities.supports_default_copy_to
```

### Allocate-Load API

The Allocate-Load API allows solvers that do not support loading the problem
incrementally to implement [`copy_to`](@ref) in a way that still allows
transformations to be applied in the copy between the cache and the
model if the transformations are implemented as MOI layers implementing the
Allocate-Load API, see [Implementing copy](@ref) for more details.

Loading a model using the Allocate-Load interface consists of two passes
through the model data:
1) the allocate pass where the model typically records the necessary information
   about the constraints and attributes such as their number and size.
   This information may be used by the solver to allocate datastructures of
   appropriate size.
2) the load pass where the model typically loads the constraint and attribute
   data to the model.

The description above only gives a suggestion of what to achieve in each pass.
In fact the exact same constraint and attribute data is provided to each pass,
so an implementation of the Allocate-Load API is free to do whatever is more
convenient in each pass.

The main difference between each pass, apart from the fact that one is executed
before the other during a copy, is that the allocate pass needs to create and
return new variable and constraint indices, while during the load pass the
appropriate constraint indices are provided.

The Allocate-Load API is **not** meant to be used outside a copy operation,
that is, the interface is not meant to be used to create new constraints with
[`Utilities.allocate_constraint`](@ref) followed by
[`Utilities.load_constraint`](@ref) after a solve.
This means that the order in which the different functions of the API are
called is fixed by [`Utilities.allocate_load`](@ref) and models implementing the
API can rely on the fact that functions will be called in this order. That is,
it can be assumed that the different functions will the called in the following
order:
1) [`Utilities.allocate_variables`](@ref)
2) [`Utilities.allocate`](@ref) and [`Utilities.allocate_constraint`](@ref)
3) [`Utilities.load_variables`](@ref)
4) [`Utilities.load`](@ref) and [`Utilities.load_constraint`](@ref)

```@docs
Utilities.allocate_load
Utilities.supports_allocate_load
Utilities.allocate_variables
Utilities.allocate
Utilities.allocate_constraint
Utilities.load_variables
Utilities.load
Utilities.load_constraint
```

### Caching optimizer

Some solvers do not support incremental definition of optimization
models. Nevertheless, you are still able to build incrementally an optimization
model with such solvers. MathOptInterface provides a utility,
[`Utilities.CachingOptimizer`](@ref), that will store in a [`ModelLike`](@ref)
the optimization model during its incremental definition. Once the
model is completely defined, the `CachingOptimizer` specifies all problem
information to the underlying solver, all at once.

The function [`Utilities.state`](@ref) allows to query the state
of the optimizer cached inside a `CachingOptimizer`. The state
could be:
* `NO_OPTIMIZER`, if no optimizer is attached;
* `EMPTY_OPTIMIZER`, if the attached optimizer is empty;
* `ATTACHED_OPTIMIZER`, if the attached optimizer is synchronized with the
  cached model defined in `CachingOptimizer`.

The following methods modify the state of the attached optimizer:
* [`Utilities.attach_optimizer`](@ref) attachs a new `optimizer`
  to a `cached_optimizer` with state `EMPTY_OPTIMIZER`.
  The state of `cached_optimizer` is set to `ATTACHED_OPTIMIZER` after the call.
* [`Utilities.drop_optimizer`](@ref) drops the underlying `optimizer`
  from `cached_optimizer`, without emptying it. The state of `cached_optimizer`
  is set to `NO_OPTIMIZER` after the call.
* [`Utilities.reset_optimizer`](@ref) empties `optimizer` inside
  `cached_optimizer`, without droping it. The state of `cached_optimizer`
  is set to `EMPTY_OPTIMIZER` after the call.

The way to operate a `CachingOptimizer` depends whether the mode
is set to `AUTOMATIC` or to `MANUAL`.
* In `MANUAL` mode, the state of the `CachingOptimizer` changes only
  if the methods [`Utilities.attach_optimizer`](@ref),
  [`Utilities.reset_optimizer`](@ref) or [`Utilities.drop_optimizer`](@ref)
  are being called. Any unattended operation results in an error.
* In `AUTOMATIC` mode, the state of the `CachingOptimizer` changes when
  necessary. Any modification not supported by the solver (e.g. dropping
  a constraint) results in a drop to the state `EMPTY_OPTIMIZER`.

When calling [`Utilities.attach_optimizer`](@ref), the `CachingOptimizer` copies
the cached model to the optimizer with [`MathOptInterface.copy_to`](@ref).
We refer to [Implementing copy](@ref) for more details.

```@docs
Utilities.CachingOptimizer
Utilities.attach_optimizer
Utilities.reset_optimizer
Utilities.drop_optimizer
Utilities.state
Utilities.mode
```

## Function utilities

The following utilities are available for functions:
```@docs
Utilities.eval_variables
Utilities.map_indices
Utilities.substitute_variables
Utilities.remove_variable
Utilities.all_coefficients
Utilities.unsafe_add
Utilities.isapprox_zero
Utilities.modify_function
```

The following functions can be used to canonicalize a function:
```@docs
Utilities.is_canonical
Utilities.canonical
Utilities.canonicalize!
```

The following functions can be used to manipulate functions with basic algebra:
```@docs
Utilities.scalar_type
Utilities.promote_operation
Utilities.operate
Utilities.operate!
Utilities.operate_output_index!
Utilities.vectorize
```

## Constraint utilities

The following utilities are available for moving the function constant to the
set for scalar constraints:
```@docs
Utilities.shift_constant
Utilities.normalize_and_add_constraint
Utilities.normalize_constant
```

## Benchmarks

Functions to help benchmark the performance of solver wrappers. See
[Benchmarking](@ref) for more details.

```@docs
Benchmarks.suite
Benchmarks.create_baseline
Benchmarks.compare_against_baseline
```
