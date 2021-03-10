```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Basic usage

!!! note
    MOI does not export functions, but for brevity we often omit qualifying
    names with the MOI module. Best practice is to have
    ```julia
    using MathOptInterface
    const MOI = MathOptInterface
    ```
    and prefix all MOI methods with `MOI.` in user code. If a name is also
    available in base Julia, we always explicitly use the module prefix, for
    example, with `MOI.get`.

## The ModelLike and AbstractOptimizer APIs

The most significant part of MOI is the definition of the **model API** that is
used to specify an instance of an optimization problem (e.g., by adding
variables and constraints). Objects that implement the model API should inherit
from the [`ModelLike`](@ref) abstract type.

Notably missing from the model API is the method to solve an optimization
problem. `ModelLike` objects may store an instance (e.g., in memory or backed by
a file format) without being linked to a particular solver. In addition to the
model API, MOI defines [`AbstractOptimizer`](@ref).

*Optimizers* (or solvers) implement the model API (inheriting from `ModelLike`)
and additionally provide methods to solve the model.

!!! info
    Throughout the rest of the manual, `model` is used as a generic `ModelLike`,
    and `optimizer` is used as a generic `AbstractOptimizer`.

Models are constructed by
* adding variables using [`add_variable`](@ref) (or [`add_variables`](@ref)),
  see [Add a variable](@ref);
* setting an objective sense and function using [`set`](@ref),
  see [Setting an objective](@ref);
* and adding constraints using [`add_constraint`](@ref) (or
  [`add_constraints`](@ref)), see [Constraints](@ref).

The way the problem is solved by the optimimizer is controlled by
[`AbstractOptimizerAttribute`](@ref)s, see [Solver-specific attributes](@ref).

## Functions

MOI defines six functions as listed in the definition of the [Standard form problem](@ref).
The simplest function is [`SingleVariable`](@ref), defined as:
```julia
struct SingleVariable <: AbstractFunction
    variable::VariableIndex
end
```

If `v` is a [`VariableIndex`](@ref) object, then `SingleVariable(v)` is simply
the scalar-valued function from the complete set of variables in a model that
returns the value of variable `v`. One may also call this function a coordinate
projection, which is more useful for defining constraints than as an objective
function.

A more interesting function is [`ScalarAffineFunction`](@ref), defined as:
```julia
struct ScalarAffineFunction{T} <: AbstractScalarFunction
    terms::Vector{ScalarAffineTerm{T}}
    constant::T
end
```

The [`ScalarAffineTerm`](@ref) struct defines a variable-coefficient pair:
```julia
struct ScalarAffineTerm{T}
    coefficient::T
    variable_index::VariableIndex
end
```

If `x` is a vector of `VariableIndex` objects, then
```julia
MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([5.0, -2.3], [x[1], x[2]]), 1.0)
```
represents the function ``5 x_1 - 2.3 x_2 + 1``.

!!! note
    `MOI.ScalarAffineTerm.([5.0, -2.3], [x[1], x[2]])` is a shortcut for
    `[MOI.ScalarAffineTerm(5.0, x[1]), MOI.ScalarAffineTerm(-2.3, x[2])]`. This
    is Julia's broadcast syntax in action, and is used quite often.

### Setting an objective

Objective functions are assigned to a model by setting the [`ObjectiveFunction`](@ref)
attribute. The [`ObjectiveSense`](@ref) attribute is used for setting the
optimization sense. For example,
```julia
x = MOI.add_variables(model, 2)
MOI.set(
    model,
    MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.([5.0, -2.3], [x[1], x[2]]), 1.0),
    )
MOI.set(model, MOI.ObjectiveSense(), MIN_SENSE)
```
sets the objective to the function just discussed in the minimization sense.

See [Functions and function modifications](@ref) for the complete list of
functions.

## Solving and retrieving the results

Once an optimizer is loaded with the objective function and all of the
constraints, we can ask the solver to solve the model by calling
[`optimize!`](@ref).
```julia
MOI.optimize!(optimizer)
```
