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
NumberOfThreads
AbstractCallback
LazyConstraintCallback
HeuristicCallback
UserCutCallback
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
```

List of attributes associated with variables. [category AbstractVariableAttribute]
Calls to `get` and `set` should include as an argument a single `VariableIndex` or a vector of `VariableIndex` objects.

```@docs
VariableName
VariablePrimalStart
VariablePrimal
CallbackVariablePrimal
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

Bridges can be used for automatic reformulation of constrained variables (i.e.
variables added with [`add_constrained_variable`](@ref)/[`add_constrained_variables`](@ref))
or constraints into equivalent formulations using constrained variables and
constraints of different types. There are two important concepts to distinguish:
* [`Bridges.AbstractBridge`](@ref)s are recipes implementing a specific
  reformulation. Bridges are not directly subtypes of
  [`Bridges.AbstractBridge`](@ref), they are either
  [`Bridges.Variable.AbstractBridge`](@ref) or
  [`Bridges.Constraint.AbstractBridge`](@ref).
* [`Bridges.AbstractBridgeOptimizer`](@ref) is a layer that can be applied to
  another [`ModelLike`](@ref) to apply the reformulation. The
  [`Bridges.LazyBridgeOptimizer`](@ref) automatically chooses the appropriate
  bridges to use when a constrained variable or constraint is not supported
  by using the list of bridges that were added to it by
  [`Bridges.add_bridge`](@ref). [`Bridges.full_bridge_optimizer`](@ref) wraps a
  model in a [`Bridges.LazyBridgeOptimizer`](@ref) where all the bridges defined
  in MOI are added. This is the recommended way to use bridges in the
  [Testing guideline](@ref), and JuMP automatically calls
  [`Bridges.full_bridge_optimizer`](@ref) when attaching an optimizer.

```@docs
Bridges.AbstractBridge
Bridges.AbstractBridgeOptimizer
Bridges.LazyBridgeOptimizer
Bridges.add_bridge
Bridges.full_bridge_optimizer
```

### [Variable bridges](@id variable_bridges)

When variables are added, either free with
[`add_variable`](@ref)/[`add_variables`](@ref),
or constrained with
[`add_constrained_variable`](@ref)/[`add_constrained_variables`](@ref),
variable bridges allow to return *bridged variables* that do not correspond to
variables of the underlying model. These variables are parametrized by
variables of the underlying model and this parametrization can be obtained with
[`Bridges.bridged_variable_function`](@ref). Similarly, the variables of the
underlying model that were created by the bridge can be expressed in terms of
the bridged variables and this expression can be obtained with
[`Bridges.unbridged_variable_function`](@ref).
For instance, consider a model bridged by the
[`Bridges.Variable.VectorizeBridge`](@ref):
```jldoctest bridged_variable_function
model = MOI.Utilities.Model{Float64}()
bridged_model = MOI.Bridges.Variable.Vectorize{Float64}(model)
bridged_variable, bridged_constraint = MOI.add_constrained_variable(bridged_model, MOI.GreaterThan(1.0))

# output

(MOI.VariableIndex(-1), MOI.ConstraintIndex{MOI.SingleVariable,MOI.GreaterThan{Float64}}(-1))
```
The constrained variable in `MOI.GreaterThan(1.0)` returned is a bridged
variable as its index in negative. In `model`, a constrained variable in
`MOI.Nonnegatives` is created:
```jldoctest bridged_variable_function
inner_variables = MOI.get(model, MOI.ListOfVariableIndices())

# output

1-element Array{MOI.VariableIndex,1}:
 MOI.VariableIndex(1)
```
In the functions used for adding constraints or setting the objective to
`bridged_model`, `bridged_variable` is substituted for `inner_variables[1]` plus
1:
```jldoctest bridged_variable_function
MOI.Bridges.bridged_variable_function(bridged_model, bridged_variable)

# output

MOI.ScalarAffineFunction{Float64}(MOI.ScalarAffineTerm{Float64}[ScalarAffineTerm{Float64}(1.0, VariableIndex(1))], 1.0)
```
When getting [`ConstraintFunction`](@ref) or [`ObjectiveFunction`](@ref),
`inner_variables[1]` is substituted for `bridged_variable` minus 1:
```jldoctest bridged_variable_function
MOI.Bridges.unbridged_variable_function(bridged_model, inner_variables[1])

# output

MOI.ScalarAffineFunction{Float64}(MOI.ScalarAffineTerm{Float64}[ScalarAffineTerm{Float64}(1.0, VariableIndex(-1))], -1.0)
```


!!! note
    A notable exception is with [`Bridges.Variable.ZerosBridge`](@ref) where no
    variable is created in the underlying model as the variables are simply
    transformed to zeros. When this bridge is used, it is not possible to
    recover functions with bridged variables from functions of the inner
    model. Consider for instance that we create two zero variables:
    ```jldoctest cannot_unbridge_zero
    model = MOI.Utilities.Model{Float64}()
    bridged_model = MOI.Bridges.Variable.Zeros{Float64}(model)
    bridged_variables, bridged_constraint = MOI.add_constrained_variables(bridged_model, MOI.Zeros(2))

    # output

    (MOI.VariableIndex[VariableIndex(-1), VariableIndex(-2)], MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Zeros}(-1))
    ```
    Consider the following functions in the variables of `bridged_model`:
    ```jldoctest cannot_unbridge_zero
    func = MOI.Utilities.operate(+, Float64, MOI.SingleVariable.(bridged_variables)...)

    # output

    MOI.ScalarAffineFunction{Float64}(MOI.ScalarAffineTerm{Float64}[ScalarAffineTerm{Float64}(1.0, VariableIndex(-1)), ScalarAffineTerm{Float64}(1.0, VariableIndex(-2))], 0.0)
    ```
    We can obtain the equivalent function in the variables of `model` as follows:
    ```jldoctest cannot_unbridge_zero
    inner_func = MOI.Bridges.bridged_function(bridged_model, func)

    # output

    MOI.ScalarAffineFunction{Float64}(MOI.ScalarAffineTerm{Float64}[], 0.0)
    ```
    However, it's not possible to invert this operation. Indeed, since the
    bridged variables are substituted for zeros, we cannot deduce whether
    they were present in the initial function.
    ```jldoctest cannot_unbridge_zero; filter = r"Stacktrace:.*"s
    MOI.Bridges.unbridged_function(bridged_model, inner_func)

    # output

    ERROR: Cannot unbridge function because some variables are bridged by variable bridges that do not support reverse mapping, e.g., `ZerosBridge`.
    Stacktrace:
     [1] error(::String, ::String, ::String) at ./error.jl:42
     [2] throw_if_cannot_unbridge at /home/blegat/.julia/dev/MathOptInterface/src/Bridges/Variable/map.jl:343 [inlined]
     [3] unbridged_function(::MOI.Bridges.Variable.SingleBridgeOptimizer{MOI.Bridges.Variable.ZerosBridge{Float64},MOI.Utilities.Model{Float64}}, ::MOI.ScalarAffineFunction{Float64}) at /home/blegat/.julia/dev/MOI/src/Bridges/bridge_optimizer.jl:920
     [4] top-level scope at none:0
    ```

```@docs
Bridges.Variable.AbstractBridge
Bridges.bridged_variable_function
Bridges.unbridged_variable_function
```

Below is the list of variable bridges implemented in this package.
```@docs
Bridges.Variable.ZerosBridge
Bridges.Variable.FreeBridge
Bridges.Variable.NonposToNonnegBridge
Bridges.Variable.VectorizeBridge
Bridges.Variable.SOCtoRSOCBridge
Bridges.Variable.RSOCtoSOCBridge
Bridges.Variable.RSOCtoPSDBridge
```

For each bridge defined in this package, a corresponding
[`Bridges.Variable.SingleBridgeOptimizer`](@ref) is available with the same
name without the "Bridge" suffix, e.g., `SplitInterval` is a
`SingleBridgeOptimizer` for the `SplitIntervalBridge`. Moreover, they are all
added in the [`Bridges.LazyBridgeOptimizer`](@ref) returned by
[`Bridges.full_bridge_optimizer`](@ref) as it calls
[`Bridges.Variable.add_all_bridges`](@ref).
```@docs
Bridges.Variable.SingleBridgeOptimizer
Bridges.Variable.add_all_bridges
```

### Constraint bridges

When constraints are added with [`add_constraint`](@ref), constraint bridges
allow to return *bridged constraints* that do not correspond to
constraints of the underlying model. These constraints were enforced by an
equivalent formulation that added constraints (and possibly also variables) in
the underlying model.
For instance, consider a model bridged by the
[`Bridges.Constraint.SplitIntervalBridge`](@ref):
```jldoctest split_interval
model = MOI.Utilities.Model{Float64}()
bridged_model = MOI.Bridges.Constraint.SplitInterval{Float64}(model)
x, y = MOI.add_variables(bridged_model, 2)
func = MOI.Utilities.operate(+, Float64, MOI.SingleVariable(x), MOI.SingleVariable(y))
c = MOI.add_constraint(bridged_model, func, MOI.Interval(1.0, 2.0))

# output

MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.Interval{Float64}}(1)
```
We can see the constraint was bridged to two constraints, one for each bound,
in the inner model.
```jldoctest split_interval
MOI.get(model, MOI.ListOfConstraints())

# output

2-element Array{Tuple{DataType,DataType},1}:
 (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})
 (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
```
However, `bridged_model` transparently hides these constraints and creates the
illusion that an interval constraint was created.
```jldoctest split_interval
MOI.get(bridged_model, MOI.ListOfConstraints())

# output

1-element Array{Tuple{DataType,DataType},1}:
 (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64})
```
It is nevertheless possible to differentiate this constraint from a constraint
added to the inner model by asking whether it is bridged:
```jldoctest split_interval
MOI.Bridges.is_bridged(bridged_model, c)

# output

true
```

```@docs
Bridges.Constraint.AbstractBridge
```

Below is the list of constraint bridges implemented in this package.
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
Bridges.Constraint.SOCRBridge
Bridges.Constraint.QuadtoSOCBridge
Bridges.Constraint.GeoMeanBridge
Bridges.Constraint.SquareBridge
Bridges.Constraint.RootDetBridge
Bridges.Constraint.LogDetBridge
Bridges.Constraint.SOCtoPSDBridge
Bridges.Constraint.RSOCtoPSDBridge
Bridges.Constraint.IndicatorActiveOnFalseBridge
```
For each bridge defined in this package, a corresponding
[`Bridges.Constraint.SingleBridgeOptimizer`](@ref) is available with the same
name without the "Bridge" suffix, e.g., `SplitInterval` is a
`SingleBridgeOptimizer` for the `SplitIntervalBridge`. Moreover, they are all
added in the [`Bridges.LazyBridgeOptimizer`](@ref) returned by
[`Bridges.full_bridge_optimizer`](@ref) as it calls
[`Bridges.Constraint.add_all_bridges`](@ref).
```@docs
Bridges.Constraint.SingleBridgeOptimizer
Bridges.Constraint.add_all_bridges
```

### Objective bridges

When an objective is set with [`set`](@ref), objective bridges
allow to set a *bridged objective* to the underlying model that do not
correspond to the objective set by the user. This equivalent formulation may add
constraints (and possibly also variables) in the underlying model in addition
to setting an objective function.

```@docs
Bridges.Objective.AbstractBridge
```

Below is the list of objective bridges implemented in this package.
```@docs
Bridges.Objective.SlackBridge
Bridges.Objective.FunctionizeBridge
```
For each bridge defined in this package, a corresponding
[`Bridges.Objective.SingleBridgeOptimizer`](@ref) is available with the same
name without the "Bridge" suffix, e.g., `Slack` is a `SingleBridgeOptimizer`
for the `SlackBridge`. Moreover, they are all added in the
[`Bridges.LazyBridgeOptimizer`](@ref) returned by
[`Bridges.full_bridge_optimizer`](@ref) as it calls
[`Bridges.Objective.add_all_bridges`](@ref).
```@docs
Bridges.Objective.SingleBridgeOptimizer
Bridges.Objective.add_all_bridges
```

### Bridge interface

A bridge should implement the following functions to be usable by a bridge optimizer:
```@docs
Bridges.added_constrained_variable_types
Bridges.added_constraint_types
```
Additionally, variable bridges should implement:
```@docs
Bridges.Variable.supports_constrained_variable
Bridges.Variable.concrete_bridge_type
Bridges.Variable.bridge_constrained_variable
```
constraint bridges should implement:
```@docs
supports_constraint(::Type{<:Bridges.Constraint.AbstractBridge}, ::Type{<:AbstractFunction}, ::Type{<:AbstractSet})
Bridges.Constraint.concrete_bridge_type
Bridges.Constraint.bridge_constraint
```
and objective bridges should implement:
```@docs
Bridges.set_objective_function_type
Bridges.Objective.concrete_bridge_type
Bridges.Objective.bridge_objective
```

When querying the [`NumberOfVariables`](@ref), [`NumberOfConstraints`](@ref)
and [`ListOfConstraintIndices`](@ref), the variables and constraints created
by the bridges in the underlying model are hidden by the bridge optimizer.
For this purpose, the bridge should provide access to the variables and
constraints it has creates by implemented the following methods of
[`get`](@ref):
```@docs
get(::Bridges.Constraint.AbstractBridge, ::NumberOfVariables)
get(::Bridges.Constraint.AbstractBridge, ::ListOfVariableIndices)
get(::Bridges.AbstractBridge, ::NumberOfConstraints)
get(::Bridges.AbstractBridge, ::ListOfConstraintIndices)
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
the cached model to the optimizer with [`copy_to`](@ref).
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

The following utility identifies those constraints imposing bounds on a given
variable, and returns those bound values:
```@docs
Utilities.get_bounds
```

## Benchmarks

Functions to help benchmark the performance of solver wrappers. See
[Benchmarking](@ref) for more details.

```@docs
Benchmarks.suite
Benchmarks.create_baseline
Benchmarks.compare_against_baseline
```
