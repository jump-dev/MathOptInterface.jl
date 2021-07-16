```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# The Utilities submodule

The Utilities submodule provides a variety of functionality for managing
`MOI.ModelLike` objects.

## Utilities.Model

[`Utilities.Model`](@ref) provides an implementation of a [`ModelLike`](@ref)
that efficiently supports all functions and sets defined within MOI. However,
given the extensibility of MOI, this might not cover all use cases.

Create a model as follows:
```jldoctest
julia> model = MOI.Utilities.Model{Float64}()
MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}
```

## Utilities.UniversalFallback

[`Utilities.UniversalFallback`](@ref) is a layer that sits on top of any
[`ModelLike`](@ref) and provides non-specialized (slower) fallbacks for
constraints and attributes that the underlying [`ModelLike`](@ref) does not
support.

For example, [`Utilities.Model`](@ref) doesn't support some variable attributes
like [`VariablePrimalStart`](@ref), so JuMP uses a combination of Universal
fallback and [`Utilities.Model`](@ref) as a generic problem cache:
```jldoctest
julia> model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
MOIU.UniversalFallback{MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}}
fallback for MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}
```

!!! warning
    Adding a UniversalFallback means that your `model` will now support all
    constraints, even if the inner-model does not! This can lead to unexpected
    behavior.

## Utilities.@model

For advanced use cases that need efficient support for functions and sets
defined outside of MOI (but still known at compile time), we provide the
[`Utilities.@model`](@ref) macro.

The `@model` macro takes a name (for a new type, which must not exist yet),
eight tuples specifying the types of constraints that are supported, and then a
`Bool` indicating the type should be a subtype of `MOI.AbstractOptimizer` (if
`true`) or `MOI.ModelLike` (if `false`).

The eight tuples are in the following order:
 1. Un-typed scalar sets, e.g., [`Integer`](@ref)
 2. Typed scalar sets, e.g., [`LessThan`](@ref)
 3. Un-typed vector sets, e.g., [`Nonnegatives`](@ref)
 4. Typed vector sets, e.g., [`PowerCone`](@ref)
 5. Un-typed scalar functions, e.g., [`SingleVariable`](@ref)
 6. Typed scalar functions, e.g., [`ScalarAffineFunction`](@ref)
 7. Un-typed vector functions, e.g., [`VectorOfVariables`](@ref)
 8. Typed vector functions, e.g., [`VectorAffineFunction`](@ref)

The tuples can contain more than one element. Typed-sets should be speficied
without their type parameter, i.e., `MOI.LessThan`, not `MOI.LessThan{Float64}`.

Here is an example:
```jldoctest
julia> MOI.Utilities.@model(
           MyNewModel,
           (MOI.Integer,),                  # Un-typed scalar sets
           (MOI.GreaterThan,),              # Typed scalar sets
           (MOI.Nonnegatives,),             # Un-typed vector sets
           (MOI.PowerCone,),                # Typed vector sets
           (MOI.SingleVariable,),           # Un-typed scalar functions
           (MOI.ScalarAffineFunction,),     # Typed scalar functions
           (MOI.VectorOfVariables,),        # Un-typed vector functions
           (MOI.VectorAffineFunction,),     # Typed vector functions
           true,                            # <:MOI.AbstractOptimizer?
       )
MathOptInterface.Utilities.GenericOptimizer{T,MyNewModelFunctionConstraints{T}} where T

julia> model = MyNewModel{Float64}()
MOIU.GenericOptimizer{Float64,MyNewModelFunctionConstraints{Float64}}
```

!!! warning
    `MyNewModel` supports every `SingleVariable`-in-Set constraint, as well as
    [`SingleVariable`](@ref), [`ScalarAffineFunction`](@ref), and
    [`ScalarQuadraticFunction`](@ref) objective functions. Implement
    `MOI.supports` as needed to forbid constraint and objective function
    combinations.

As another example, [PATHSolver](https://github.com/chkwon/PATHSolver.jl), which
only supports [`VectorAffineFunction`](@ref)-in-[`Complements`](@ref)
defines its optimizer as:
```jldoctest pathoptimizer
julia> MOI.Utilities.@model(
           PathOptimizer,
           (),  # Scalar sets
           (),  # Typed scalar sets
           (MOI.Complements,),  # Vector sets
           (),  # Typed vector sets
           (),  # Scalar functions
           (),  # Typed scalar functions
           (),  # Vector functions
           (MOI.VectorAffineFunction,),  # Typed vector functions
           true,  # is_optimizer
       )
MathOptInterface.Utilities.GenericOptimizer{T,MathOptInterface.Utilities.VectorOfConstraints{MathOptInterface.VectorAffineFunction{T},MathOptInterface.Complements}} where T
```
However, `PathOptimizer` does not support some `SingleVariable`-in-Set
constraints, so we must explicitly define:
```jldoctest pathoptimizer
julia> function MOI.supports_constraint(
           ::PathOptimizer,
           ::Type{MOI.SingleVariable},
           ::Type{Union{<:MOI.Semiinteger,MOI.Semicontinuous,MOI.ZeroOne,MOI.Integer}}
       )
           return false
       end
```

Finally, PATH doesn't support an objective function, so we need to add:
```jldoctest pathoptimizer
julia> MOI.supports(::PathOptimizer, ::MOI.ObjectiveFunction) = false
```

!!! warning
    This macro creates a new type, so it must be called from the top-level of a
    module, e.g., it cannot be called from inside a function.

## Utilities.CachingOptimizer

A [`Utilities.CachingOptimizer`] is an MOI layer that abstracts the difference
between solvers that support incremental modification (e.g., they support adding
variables one-by-one), and solvers that require the entire problem in a single
API call (e.g., they only accept the `A`, `b` and `c` matrices of a linear
program).

It has two parts:
 1. A cache, where the model can be built and modified incrementally
 2. An optimizer, which is used to solve the problem

```jldoctest pathoptimizer
julia> model = MOI.Utilities.CachingOptimizer(
           MOI.Utilities.Model{Float64}(),
           PathOptimizer{Float64}(),
       )
MOIU.CachingOptimizer{MOIU.GenericOptimizer{Float64,MOIU.VectorOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Complements}},MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}}
in state EMPTY_OPTIMIZER
in mode AUTOMATIC
with model cache MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}
with optimizer MOIU.GenericOptimizer{Float64,MOIU.VectorOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Complements}}
```

A [`Utilities.CachingOptimizer`](@ref) may be in one of three possible states:

* `NO_OPTIMIZER`: The CachingOptimizer does not have any optimizer.
* `EMPTY_OPTIMIZER`: The CachingOptimizer has an empty optimizer, and it is not
  synchronized with the cached model. Modifications are forwarded to the cache,
  but _not_ to the optimizer.
* `ATTACHED_OPTIMIZER`: The CachingOptimizer has an optimizer, and it is
  synchronized with the cached model. Modifications are forwarded to the
  optimizer. If the optimizer does not support modifications, and error will be
  thrown.

Use [`Utilities.attach_optimizer`](@ref) to go from `EMPTY_OPTIMIZER` to
`ATTACHED_OPTIMIZER`:
```jldoctest pathoptimizer
julia> MOI.Utilities.attach_optimizer(model)

julia> model
MOIU.CachingOptimizer{MOIU.GenericOptimizer{Float64,MOIU.VectorOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Complements}},MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}}
in state ATTACHED_OPTIMIZER
in mode AUTOMATIC
with model cache MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}
with optimizer MOIU.GenericOptimizer{Float64,MOIU.VectorOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Complements}}
```

!!! info
    You must be in `ATTACHED_OPTIMIZER` to use [`optimize!`](@ref).

Use [`Utilities.reset_optimizer`](@ref) to go from `ATTACHED_OPTIMIZER` to
`EMPTY_OPTIMIZER`:
```jldoctest pathoptimizer
julia> MOI.Utilities.reset_optimizer(model)

julia> model
MOIU.CachingOptimizer{MOIU.GenericOptimizer{Float64,MOIU.VectorOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Complements}},MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}}
in state EMPTY_OPTIMIZER
in mode AUTOMATIC
with model cache MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}
with optimizer MOIU.GenericOptimizer{Float64,MOIU.VectorOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Complements}}
```

!!! info
    Calling `MOI.empty!(model)` also resets the state to `EMPTY_OPTIMIZER`.
    So after emptying a model, the modification will only be applied to the
    cache.

Use [`Utilities.drop_optimizer`](@ref) to go from any state to `NO_OPTIMIZER`:
```jldoctest pathoptimizer
julia> MOI.Utilities.drop_optimizer(model)

julia> model
MOIU.CachingOptimizer{MOIU.GenericOptimizer{Float64,MOIU.VectorOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Complements}},MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}}
in state NO_OPTIMIZER
in mode AUTOMATIC
with model cache MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}
with optimizer nothing
```

Pass an empty optimizer to [`Utilities.reset_optimizer`](@ref) to go from
`NO_OPTIMIZER` to `EMPTY_OPTIMIZER`:
```jldoctest pathoptimizer
julia> MOI.Utilities.reset_optimizer(model, PathOptimizer{Float64}())

julia> model
MOIU.CachingOptimizer{MOIU.GenericOptimizer{Float64,MOIU.VectorOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Complements}},MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}}
in state EMPTY_OPTIMIZER
in mode AUTOMATIC
with model cache MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}
with optimizer MOIU.GenericOptimizer{Float64,MOIU.VectorOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Complements}}
```

Deciding when to attach and reset the optimizer is tedious, and you will often
write code like this:
```julia
try
    # modification
catch
    MOI.Utilities.reset_optimizer(model)
    # Re-try modification
end
```

To make this easier, [`Utilities.CachingOptimizer`](@ref) has two modes of
operation:

* `AUTOMATIC`: The `CachingOptimizer` changes its state when necessary.
  Attempting to add a constraint or perform a modification not supported by the
  optimizer results in a drop to `EMPTY_OPTIMIZER` mode.
* `MANUAL`: The user must change the state of the `CachingOptimizer`. Attempting
  to perform an operation in the incorrect state results in an error.

By default, `AUTOMATIC` mode is chosen. However, you can create a
`CachingOptimizer` in `MANUAL` mode as follows:
```jldoctest pathoptimizer
julia> model = MOI.Utilities.CachingOptimizer(
           MOI.Utilities.Model{Float64}(),
           MOI.Utilities.MANUAL,
       )
MOIU.CachingOptimizer{MOI.AbstractOptimizer,MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}}
in state NO_OPTIMIZER
in mode MANUAL
with model cache MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}
with optimizer nothing

julia> MOI.Utilities.reset_optimizer(model, PathOptimizer{Float64}())

julia> model
MOIU.CachingOptimizer{MOI.AbstractOptimizer,MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}}
in state EMPTY_OPTIMIZER
in mode MANUAL
with model cache MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}
with optimizer MOIU.GenericOptimizer{Float64,MOIU.VectorOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Complements}}
```

## Printing

Use `print` to print the formulation of the model.
```jldoctest utilities_print
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model)
MathOptInterface.VariableIndex(1)

julia> MOI.set(model, MOI.VariableName(), x, "x_var")

julia> f = MOI.SingleVariable(x)
MathOptInterface.SingleVariable(MathOptInterface.VariableIndex(1))

julia> MOI.add_constraint(model, f, MOI.ZeroOne())
MathOptInterface.ConstraintIndex{MathOptInterface.SingleVariable,MathOptInterface.ZeroOne}(1)

julia> MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)

julia> MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)

julia> print(model)
Maximize SingleVariable:
 x_var

Subject to:

SingleVariable-in-ZeroOne
 x_var ∈ {0, 1}
```

Use [`Utilities.latex_formulation`](@Ref) to display the model in LaTeX form:
```jldoctest utilities_print
julia> MOI.Utilities.latex_formulation(model)
$$ \begin{aligned}
\max\quad & x\_var \\
\text{Subject to}\\
 & \text{SingleVariable-in-ZeroOne} \\
 & x\_var \in \{0, 1\} \\
\end{aligned} $$
```

!!! tip
    In IJulia, calling `print` or ending a cell with
    [`Utilities.latex_formulation`](@ref) will render the model in LaTex.

## Allocate-Load

The Allocate-Load API allows solvers that do not support loading the problem
incrementally to implement [`copy_to`](@ref) in a way that still allows
transformations to be applied in the copy between the cache and the
model if the transformations are implemented as MOI layers implementing the
Allocate-Load API.

Loading a model using the Allocate-Load interface consists of two passes
through the model data:
1) the _allocate_ pass where the model typically records the necessary
   information about the constraints and attributes such as their number and
   size. This information may be used by the solver to allocate datastructures
   of appropriate size.
2) the _load_ pass where the model typically loads the constraint and attribute
   data to the model.

The description above only gives a suggestion of what to achieve in each pass.
In fact the exact same constraint and attribute data is provided to each pass,
so an implementation of the Allocate-Load API is free to do whatever is more
convenient in each pass.

The main difference between each pass, apart from the fact that one is executed
before the other during a copy, is that the allocate pass needs to create and
return new variable and constraint indices, while during the load pass the
appropriate constraint indices are provided.

If you choose to implement the Allocate-Load API, implement the following
functions, which will be called in order:

**Allocate**

 * [`Utilities.allocate_variables`](@ref)
 * [`Utilities.allocate_constrained_variable`](@ref)
 * [`Utilities.allocate_constrained_variables`](@ref)
 * [`Utilities.allocate`](@ref)
 * [`Utilities.allocate_constraint`](@ref)

**Load**

 * [`Utilities.load_variables`](@ref)
 * [`Utilities.load_constrained_variable`](@ref)
 * [`Utilities.load_constrained_variables`](@ref)
 * [`Utilities.load`](@ref)
 * [`Utilities.load_constraint`](@ref)

Note that the `_constrained_variable` functions are optional, and are only
needed if the solver requires variables be constrained on creation.

You must also implement:
```julia
function MOI.copy_to(dest::Optimizer, src::MOI.ModelLike; kwargs...)
    return MOI.Utilities.automatic_copy_to(dest, src; kwargs...)
end

function MOI.Utilities.supports_allocate_load(
    model::Optimizer,
    copy_names::Bool,
)
    # If you support names...
    return true
    # Otherwise...
    return !copy_names
end
```
See [`Utilities.supports_allocate_load`](@ref) for more details.

!!! warning
    The Allocate-Load API should **not** be used outside [`copy_to`](@ref).

## Utilities.MatrixOfConstraints

The constraints of [`Utilities.Model`](@ref) are stored as a vector of tuples
of function and set in a `Utilities.VectorOfConstraints`. Other representations
can be used by parametrizing the type [`Utilities.GenericModel`](@ref)
(resp. [`Utilities.GenericOptimizer`](@ref)). For instance, if all
non-`SingleVariable` constraints are affine, the coefficients of all the
constraints can be stored in a single sparse matrix using
[`Utilities.MatrixOfConstraints`](@ref). The constraints storage can even be
customized up to a point where it exactly matches the storage of the solver of
interest, in which case [`copy_to`](@ref) can be implemented for the solver by
calling [`copy_to`](@ref) to this custom model.

For instance, [Clp](https://github.com/jump-dev/Clp.jl) defines the following
model
```julia
MOI.Utilities.@product_of_scalar_sets(LP, MOI.EqualTo{T}, MOI.LessThan{T}, MOI.GreaterThan{T})
const Model = MOI.Utilities.GenericModel{
    Float64,
    MOI.Utilities.MatrixOfConstraints{
        Float64,
        MOI.Utilities.MutableSparseMatrixCSC{Float64,Cint,MOI.Utilities.ZeroBasedIndexing},
        MOI.Utilities.MatrixBounds{Float64},
        LP{Float64},
    },
}
```

The [`copy_to`](@ref) operation can now be implemented as follows (assuming that
the `Model` definition above is in the `Clp` module so that it can be referred
to as `Model`, to be distinguished with [`Utilities.Model`](@ref)):
```julia
function _copy_to(
    dest::Optimizer,
    src::Model
)
    @assert MOI.is_empty(dest)
    A = src.constraints.coefficients
    row_bounds = src.constraints.constants
    Clp_loadProblem(
        dest,
        A.n,
        A.m,
        A.colptr,
        A.rowval,
        A.nzval,
        src.lower_bound,
        src.upper_bound,
        # (...) objective vector (omitted),
        row_bounds.lower,
        row_bounds.upper,
    )
    # Set objective sense and constant (omitted)
    return
end

function MOI.copy_to(
    dest::Optimizer,
    src::Model;
    copy_names::Bool = false
)
    _copy_to(dest, src)
    return MOI.Utilities.identity_index_map(src)
end

function MOI.copy_to(
    dest::Optimizer,
    src::MOI.Utilities.UniversalFallback{Model};
    copy_names::Bool = false
)
    # Copy attributes from `src` to `dest` and error in case any unsupported
    # constraints or attributes are set in `UniversalFallback`.
    return MOI.copy_to(dest, src.model)
end

function MOI.copy_to(
    dest::Optimizer,
    src::MOI.ModelLike;
    copy_names::Bool = false
)
    model = Model()
    index_map = MOI.copy_to(model, src)
    _copy_to(dest, model)
    return index_map
end
```

## Fallbacks

The value of some attributes can be inferred from the value of other
attributes.

For example, the value of [`ObjectiveValue`](@ref) can be computed using
[`ObjectiveFunction`](@ref) and [`VariablePrimal`](@ref).

When a solver gives direct access to an attribute, it is better to return this
value. However, if this is not the case, [`Utilities.get_fallback`](@ref) can be
used instead. For example:
```julia
function MOI.get(model::Optimizer, attr::MOI.ObjectiveFunction)
    return MOI.Utilities.get_fallback(model, attr)
end
```
