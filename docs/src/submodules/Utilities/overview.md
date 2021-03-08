```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# The Utilities submodule

## Model

[`Utilities.Model`](@ref) provides an implementation of a [`ModelLike`](@ref)
that efficiently supports all functions and sets defined within MOI. However,
given the extensibility of MOI, this might not cover all use cases.

[`Utilities.UniversalFallback`](@ref) is a layer that sits on top of any
`ModelLike` and provides non-specialized (slower) fallbacks for constraints and
attributes that the underlying `ModeLike` does not support.

For advanced use cases that need efficient support for functions and sets
defined outside of MOI (but still known at compile time), we provide the
[`Utilities.@model`](@ref) macro.

## CachingOptimizer

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

## Copy utilities

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
