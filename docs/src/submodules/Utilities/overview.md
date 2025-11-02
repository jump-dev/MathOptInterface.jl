```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    import MathOptInterface as MOI
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# The Utilities submodule

The Utilities submodule provides a variety of functions and datastructures for
managing `MOI.ModelLike` objects.

## [Utilities.Model](@id overview_utilities_model)

[`Utilities.Model`](@ref) provides an implementation of a [`ModelLike`](@ref)
that efficiently supports all functions and sets defined within MOI. However,
given the extensibility of MOI, this might not cover all use cases.

Create a model as follows:
```jldoctest
julia> model = MOI.Utilities.Model{Float64}()
MOIU.Model{Float64}
├ ObjectiveSense: FEASIBILITY_SENSE
├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
├ NumberOfVariables: 0
└ NumberOfConstraints: 0
```

## [Utilities.UniversalFallback](@id overview_utilities_universal_fallback)

[`Utilities.UniversalFallback`](@ref) is a layer that sits on top of any
[`ModelLike`](@ref) and provides non-specialized (slower) fallbacks for
constraints and attributes that the underlying [`ModelLike`](@ref) does not
support.

For example, [`Utilities.Model`](@ref) doesn't support some variable attributes
like [`VariablePrimalStart`](@ref), so JuMP uses a combination of Universal
fallback and [`Utilities.Model`](@ref) as a generic problem cache:
```jldoctest
julia> model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
MOIU.UniversalFallback{MOIU.Model{Float64}}
├ ObjectiveSense: FEASIBILITY_SENSE
├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
├ NumberOfVariables: 0
└ NumberOfConstraints: 0
```

!!! warning
    Adding a UniversalFallback means that your `model` will now support all
    constraints, even if the inner-model does not. This can lead to unexpected
    behavior.

## [Utilities.@model](@id overview_model_macro)

For advanced use cases that need efficient support for functions and sets
defined outside of MOI (but still known at compile time), we provide the
[`Utilities.@model`](@ref) macro.

The `@model` macro takes a name (for a new type, which must not exist yet),
eight tuples specifying the types of constraints that are supported, and then a
`Bool` indicating the type is a subtype of `MOI.AbstractOptimizer` (if
`true`) or `MOI.ModelLike` (if `false`).

The eight tuples are in the following order:
 1. Un-typed scalar sets, for example, [`Integer`](@ref)
 2. Typed scalar sets, for example, [`LessThan`](@ref)
 3. Un-typed vector sets, for example, [`Nonnegatives`](@ref)
 4. Typed vector sets, for example, [`PowerCone`](@ref)
 5. Un-typed scalar functions, for example, [`VariableIndex`](@ref)
 6. Typed scalar functions, for example, [`ScalarAffineFunction`](@ref)
 7. Un-typed vector functions, for example, [`VectorOfVariables`](@ref)
 8. Typed vector functions, for example, [`VectorAffineFunction`](@ref)

The tuples can contain more than one element. Typed-sets must be specified
without their type parameter, for example, `MOI.LessThan`, not
`MOI.LessThan{Float64}`.

Here is an example:
```jldoctest
julia> MOI.Utilities.@model(
           MyNewModel,
           (MOI.Integer,),                  # Un-typed scalar sets
           (MOI.GreaterThan,),              # Typed scalar sets
           (MOI.Nonnegatives,),             # Un-typed vector sets
           (MOI.PowerCone,),                # Typed vector sets
           (MOI.VariableIndex,),            # Un-typed scalar functions
           (MOI.ScalarAffineFunction,),     # Typed scalar functions
           (MOI.VectorOfVariables,),        # Un-typed vector functions
           (MOI.VectorAffineFunction,),     # Typed vector functions
           true,                            # <:MOI.AbstractOptimizer?
       )
MathOptInterface.Utilities.GenericOptimizer{T, MathOptInterface.Utilities.ObjectiveContainer{T}, MathOptInterface.Utilities.VariablesContainer{T}, MyNewModelFunctionConstraints{T}} where T

julia> model = MyNewModel{Float64}()
MOIU.GenericOptimizer{Float64, MOIU.ObjectiveContainer{Float64}, MOIU.VariablesContainer{Float64}, MyNewModelFunctionConstraints{Float64}}
├ ObjectiveSense: FEASIBILITY_SENSE
├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
├ NumberOfVariables: 0
└ NumberOfConstraints: 0
```

!!! warning
    `MyNewModel` supports every `VariableIndex`-in-Set constraint, as well as
    [`VariableIndex`](@ref), [`ScalarAffineFunction`](@ref), and
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
MathOptInterface.Utilities.GenericOptimizer{T, MathOptInterface.Utilities.ObjectiveContainer{T}, MathOptInterface.Utilities.VariablesContainer{T}, MathOptInterface.Utilities.VectorOfConstraints{MathOptInterface.VectorAffineFunction{T}, MathOptInterface.Complements}} where T
```
However, `PathOptimizer` does not support some `VariableIndex`-in-Set
constraints, so we must explicitly define:
```jldoctest pathoptimizer
julia> function MOI.supports_constraint(
           ::PathOptimizer,
           ::Type{MOI.VariableIndex},
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
    module, for example, it cannot be called from inside a function.

## Utilities.CachingOptimizer

A [`Utilities.CachingOptimizer`] is an MOI layer that abstracts the difference
between solvers that support incremental modification (for example, they support
adding variables one-by-one), and solvers that require the entire problem in a
single API call (for example, they only accept the `A`, `b` and `c` matrices of
a linear program).

It has two parts:
 1. A cache, where the model can be built and modified incrementally
 2. An optimizer, which is used to solve the problem

```jldoctest pathoptimizer
julia> model = MOI.Utilities.CachingOptimizer(
           MOI.Utilities.Model{Float64}(),
           PathOptimizer{Float64}(),
       )
MOIU.CachingOptimizer
├ state: EMPTY_OPTIMIZER
├ mode: AUTOMATIC
├ model_cache: MOIU.Model{Float64}
│ ├ ObjectiveSense: FEASIBILITY_SENSE
│ ├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
│ ├ NumberOfVariables: 0
│ └ NumberOfConstraints: 0
└ optimizer: MOIU.GenericOptimizer{Float64, MOIU.ObjectiveContainer{Float64}, MOIU.VariablesContainer{Float64}, MOIU.VectorOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.Complements}}
  ├ ObjectiveSense: FEASIBILITY_SENSE
  ├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
  ├ NumberOfVariables: 0
  └ NumberOfConstraints: 0
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

julia> MOI.Utilities.state(model)
ATTACHED_OPTIMIZER::CachingOptimizerState = 2
```

!!! info
    You must be in `ATTACHED_OPTIMIZER` to use [`optimize!`](@ref).

Use [`Utilities.reset_optimizer`](@ref) to go from `ATTACHED_OPTIMIZER` to
`EMPTY_OPTIMIZER`:
```jldoctest pathoptimizer
julia> MOI.Utilities.reset_optimizer(model)

julia> MOI.Utilities.state(model)
EMPTY_OPTIMIZER::CachingOptimizerState = 1
```

!!! info
    Calling `MOI.empty!(model)` also resets the state to `EMPTY_OPTIMIZER`.
    So after emptying a model, the modification will only be applied to the
    cache.

Use [`Utilities.drop_optimizer`](@ref) to go from any state to `NO_OPTIMIZER`:
```jldoctest pathoptimizer
julia> MOI.Utilities.drop_optimizer(model)

julia> MOI.Utilities.state(model)
NO_OPTIMIZER::CachingOptimizerState = 0

julia> model
MOIU.CachingOptimizer
├ state: NO_OPTIMIZER
├ mode: AUTOMATIC
├ model_cache: MOIU.Model{Float64}
│ ├ ObjectiveSense: FEASIBILITY_SENSE
│ ├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
│ ├ NumberOfVariables: 0
│ └ NumberOfConstraints: 0
└ optimizer: nothing
```

Pass an empty optimizer to [`Utilities.reset_optimizer`](@ref) to go from
`NO_OPTIMIZER` to `EMPTY_OPTIMIZER`:
```jldoctest pathoptimizer
julia> MOI.Utilities.reset_optimizer(model, PathOptimizer{Float64}())

julia> MOI.Utilities.state(model)
EMPTY_OPTIMIZER::CachingOptimizerState = 1

julia> model
MOIU.CachingOptimizer
├ state: EMPTY_OPTIMIZER
├ mode: AUTOMATIC
├ model_cache: MOIU.Model{Float64}
│ ├ ObjectiveSense: FEASIBILITY_SENSE
│ ├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
│ ├ NumberOfVariables: 0
│ └ NumberOfConstraints: 0
└ optimizer: MOIU.GenericOptimizer{Float64, MOIU.ObjectiveContainer{Float64}, MOIU.VariablesContainer{Float64}, MOIU.VectorOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.Complements}}
  ├ ObjectiveSense: FEASIBILITY_SENSE
  ├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
  ├ NumberOfVariables: 0
  └ NumberOfConstraints: 0
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
MOIU.CachingOptimizer
├ state: NO_OPTIMIZER
├ mode: MANUAL
├ model_cache: MOIU.Model{Float64}
│ ├ ObjectiveSense: FEASIBILITY_SENSE
│ ├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
│ ├ NumberOfVariables: 0
│ └ NumberOfConstraints: 0
└ optimizer: nothing

julia> MOI.Utilities.reset_optimizer(model, PathOptimizer{Float64}())

julia> model
MOIU.CachingOptimizer
├ state: EMPTY_OPTIMIZER
├ mode: MANUAL
├ model_cache: MOIU.Model{Float64}
│ ├ ObjectiveSense: FEASIBILITY_SENSE
│ ├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
│ ├ NumberOfVariables: 0
│ └ NumberOfConstraints: 0
└ optimizer: MOIU.GenericOptimizer{Float64, MOIU.ObjectiveContainer{Float64}, MOIU.VariablesContainer{Float64}, MOIU.VectorOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.Complements}}
  ├ ObjectiveSense: FEASIBILITY_SENSE
  ├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
  ├ NumberOfVariables: 0
  └ NumberOfConstraints: 0
```

## Printing

Use `print` to print the formulation of the model.
```jldoctest utilities_print
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> MOI.set(model, MOI.VariableName(), x, "x_var")

julia> MOI.add_constraint(model, x, MOI.ZeroOne())
MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.ZeroOne}(1)

julia> MOI.set(model, MOI.ObjectiveFunction{typeof(x)}(), x)

julia> MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)

julia> print(model)
Maximize VariableIndex:
 x_var

Subject to:

VariableIndex-in-ZeroOne
 x_var ∈ {0, 1}
```

Use [`Utilities.latex_formulation`](@ref) to display the model in LaTeX form:
```jldoctest utilities_print
julia> MOI.Utilities.latex_formulation(model)
$$ \begin{aligned}
\max\quad & x\_var \\
\text{Subject to}\\
 & \text{VariableIndex-in-ZeroOne} \\
 & x\_var \in \{0, 1\} \\
\end{aligned} $$
```

!!! tip
    In IJulia, calling `print` or ending a cell with
    [`Utilities.latex_formulation`](@ref) will render the model in LaTeX.

## Utilities.PenaltyRelaxation

Pass [`Utilities.PenaltyRelaxation`](@ref) to [`modify`](@ref) to relax the
problem by adding penalized slack variables to the constraints. This is helpful
when debugging sources of infeasible models.

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model);

julia> MOI.set(model, MOI.VariableName(), x, "x")

julia> c = MOI.add_constraint(model, 1.0 * x, MOI.LessThan(2.0));

julia> map = MOI.modify(model, MOI.Utilities.PenaltyRelaxation(Dict(c => 2.0)));

julia> print(model)
Minimize ScalarAffineFunction{Float64}:
 0.0 + 2.0 v[2]

Subject to:

ScalarAffineFunction{Float64}-in-LessThan{Float64}
 0.0 + 1.0 x - 1.0 v[2] <= 2.0

VariableIndex-in-GreaterThan{Float64}
 v[2] >= 0.0

julia> map[c]
0.0 + 1.0 MOI.VariableIndex(2)
```

You can also modify a single constraint using [`Utilities.ScalarPenaltyRelaxation`](@ref):
```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model);

julia> MOI.set(model, MOI.VariableName(), x, "x")

julia> c = MOI.add_constraint(model, 1.0 * x, MOI.LessThan(2.0));

julia> f = MOI.modify(model, c, MOI.Utilities.ScalarPenaltyRelaxation(2.0));

julia> print(model)
Minimize ScalarAffineFunction{Float64}:
 0.0 + 2.0 v[2]

Subject to:

ScalarAffineFunction{Float64}-in-LessThan{Float64}
 0.0 + 1.0 x - 1.0 v[2] <= 2.0

VariableIndex-in-GreaterThan{Float64}
 v[2] >= 0.0

julia> f
0.0 + 1.0 MOI.VariableIndex(2)
```

## Utilities.MatrixOfConstraints

The constraints of [`Utilities.Model`](@ref) are stored as a vector of tuples
of function and set in a [`Utilities.VectorOfConstraints`](@ref).

Other representations can be used by parameterizing the type
[`Utilities.GenericModel`](@ref) (resp. [`Utilities.GenericOptimizer`](@ref)).

For example, if all non-`VariableIndex` constraints are affine, the coefficients
of all the constraints can be stored in a single sparse matrix using
[`Utilities.MatrixOfConstraints`](@ref).

The constraints storage can even be customized up to a point where it exactly
matches the storage of the solver of interest, in which case [`copy_to`](@ref)
can be implemented for the solver by calling [`copy_to`](@ref) to this custom
model.

For example, [Clp.jl](https://github.com/jump-dev/Clp.jl) defines the following
model:
```jldoctest matrixofconstraints
julia> MOI.Utilities.@product_of_sets(
           SupportedSets,
           MOI.EqualTo{T},
           MOI.LessThan{T},
           MOI.GreaterThan{T},
           MOI.Interval{T},
       );

julia> const OptimizerCache = MOI.Utilities.GenericModel{
           Float64,
           MOI.Utilities.ObjectiveContainer{Float64},
           MOI.Utilities.VariablesContainer{Float64},
           MOI.Utilities.MatrixOfConstraints{
               Float64,
               MOI.Utilities.MutableSparseMatrixCSC{
                   # The data type of the coefficients
                   Float64,
                   # The data type of the variable indices
                   Cint,
                   # Can also be MOI.Utilities.OneBasedIndexing
                   MOI.Utilities.ZeroBasedIndexing,
               },
               MOI.Utilities.Hyperrectangle{Float64},
               SupportedSets{Float64},
           },
       };
```
Given the input model:
```jldoctest matrixofconstraints
julia> src = MOI.Utilities.Model{Float64}();

julia> MOI.Utilities.loadfromstring!(
           src,
           """
           variables: x, y, z
           maxobjective: x + 2.0 * y + -3.1 * z
           x + y <= 1.0
           2.0 * y >= 3.0
           -4.0 * x + z == 5.0
           x in Interval(0.0, 1.0)
           y <= 10.0
           z == 5.0
           """,
       )
```

We can construct a new cached model and copy `src` to it:
```jldoctest matrixofconstraints
julia> dest = OptimizerCache();

julia> index_map = MOI.copy_to(dest, src);
```

From `dest`, we can access the `A` matrix in sparse matrix form:
```jldoctest matrixofconstraints
julia> A = dest.constraints.coefficients;

julia> A.n
3

julia> A.m
3

julia> A.colptr
4-element Vector{Int32}:
 0
 2
 4
 5

julia> A.rowval
5-element Vector{Int32}:
 0
 1
 1
 2
 0

julia> A.nzval
5-element Vector{Float64}:
 -4.0
  1.0
  1.0
  2.0
  1.0
```
The lower and upper row bounds:
```jldoctest matrixofconstraints
julia> row_bounds = dest.constraints.constants;

julia> row_bounds.lower
3-element Vector{Float64}:
   5.0
 -Inf
   3.0

julia> row_bounds.upper
3-element Vector{Float64}:
  5.0
  1.0
 Inf
```
The lower and upper variable bounds:
```jldoctest matrixofconstraints
julia> dest.variables.lower
3-element Vector{Float64}:
   0.0
 -Inf
   5.0

julia> dest.variables.upper
3-element Vector{Float64}:
  1.0
 10.0
  5.0
```
Because of larger variations between solvers, the objective can be queried using
the standard MOI methods:
```jldoctest matrixofconstraints
julia> MOI.get(dest, MOI.ObjectiveSense())
MAX_SENSE::OptimizationSense = 1

julia> F = MOI.get(dest, MOI.ObjectiveFunctionType())
MathOptInterface.ScalarAffineFunction{Float64}

julia> F = MOI.get(dest, MOI.ObjectiveFunction{F}())
0.0 + 1.0 MOI.VariableIndex(1) + 2.0 MOI.VariableIndex(2) - 3.1 MOI.VariableIndex(3)
```

Thus, Clp.jl implements [`copy_to`](@ref) methods similar to the following:
```julia
# This method copies from the cache to the `Clp.Optimizer` object.
function MOI.copy_to(dest::Optimizer, src::OptimizerCache)
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
    return MOI.Utilities.identity_index_map(src)
end

# This method copies from an arbitrary model to the optimizer, by the
# intermediate `OptimizerCache` representation.
function MOI.copy_to(dest::Optimizer, src::MOI.ModelLike)
    cache = OptimizerCache()
    index_map = MOI.copy_to(cache, src)
    MOI.copy_to(dest, cache)
    return index_map
end

# This is a special method that gets called in some cases when `OptimizerCache`
# is used as the backing data structure in a `MOI.Utilities.CachingOptimizer`.
# It is needed for performance, but not correctness.
function MOI.copy_to(
    dest::Optimizer,
    src::MOI.Utilities.UniversalFallback{OptimizerCache},
)
    MOI.Utilities.throw_unsupported(src)
    return MOI.copy_to(dest, src.model)
end
```

!!! tip
    For other examples of [`Utilities.MatrixOfConstraints`](@ref), see:
     * [Cbc.jl](https://github.com/jump-dev/Cbc.jl)
     * [ECOS.jl](https://github.com/jump-dev/ECOS.jl)
     * [SCS.jl](https://github.com/jump-dev/SCS.jl)

## ModelFilter

Utilities provides [`Utilities.ModelFilter`](@ref) as a useful tool to copy a
subset of a model. For example, given an infeasible model, we can copy the
irreducible infeasible subsystem (for models implementing
[`ConstraintConflictStatus`](@ref)) as follows:
```julia
my_filter(::Any) = true
function my_filter(ci::MOI.ConstraintIndex)
    status = MOI.get(dest, MOI.ConstraintConflictStatus(), ci)
    return status != MOI.NOT_IN_CONFLICT
end
filtered_src = MOI.Utilities.ModelFilter(my_filter, src)
index_map = MOI.copy_to(dest, filtered_src)
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

## DoubleDicts

When writing MOI interfaces, we often need to handle situations in which we map
[`ConstraintIndex`](@ref)s to different values. For example, to a string
for [`ConstraintName`](@ref).

One option is to use a dictionary like `Dict{MOI.ConstraintIndex,String}`.
However, this incurs a performance cost because the key is not a concrete type.

The DoubleDicts submodule helps this situation by providing two types main
types [`Utilities.DoubleDicts.DoubleDict`](@ref) and
[`Utilities.DoubleDicts.IndexDoubleDict`](@ref). These types act like normal
dictionaries, but internally they use more efficient dictionaries specialized to
the type of the function-set pair.

The most common usage of a `DoubleDict` is in the `index_map` returned by
[`copy_to`](@ref). Performance can be improved, by using a function barrier.
That is, instead of code like:
```julia
index_map = MOI.copy_to(dest, src)
for (F, S) in MOI.get(src, MOI.ListOfConstraintTypesPresent())
    for ci in MOI.get(src, MOI.ListOfConstraintIndices{F,S}())
        dest_ci = index_map[ci]
        # ...
    end
end
```
use instead:
```julia
function function_barrier(
    dest,
    src,
    index_map::MOI.Utilities.DoubleDicts.IndexDoubleDictInner{F,S},
) where {F,S}
    for ci in MOI.get(src, MOI.ListOfConstraintIndices{F,S}())
        dest_ci = index_map[ci]
        # ...
    end
    return
end

index_map = MOI.copy_to(dest, src)
for (F, S) in MOI.get(src, MOI.ListOfConstraintTypesPresent())
    function_barrier(dest, src, index_map[F, S])
end
```
