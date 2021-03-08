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
Utilities.filter_variables
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
Utilities.supports_shift_constant
Utilities.normalize_and_add_constraint
Utilities.normalize_constant
```

The following utility identifies those constraints imposing bounds on a given
variable, and returns those bound values:
```@docs
Utilities.get_bounds
```

The following utilities are useful when working with symmetric matrix cones.
```@docs
Utilities.is_diagonal_vectorized_index
Utilities.side_dimension_for_vectorized_dimension
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
