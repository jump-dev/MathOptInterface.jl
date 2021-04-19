```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Implementing a solver interface

This guide outlines the basic steps to implement an interface to 
MathOptInterface for a new solver. 

!!! warning
    Implementing an interface to MathOptInterface for a new solver is a lot of 
    work. Before starting, we recommend that you join the 
    [Developer chatroom](https://gitter.im/JuliaOpt/JuMP-dev) and explain a 
    little bit about the solver you are wrapping. If you have questions that are
    not answered by this guide, please ask them in the [Developer chatroom](https://gitter.im/JuliaOpt/JuMP-dev).

## Deciding if MathOptInterface is right for you

The first step in writing a wrapper is to decide whether implementing an 
interface is the right thing to do. 

MathOptInterface is an abstraction layer for unifying _constrained_ mathematical 
optimization solvers. If your solver doesn't fit in the category, i.e., it
implements a derivative-free algorithm for unconstrained objective functions, 
MathOptInterface may not be the right tool for the job. 

!!! tip
    If you're not sure whether you ssould write an interface, ask in the 
    [Developer chatroom](https://gitter.im/JuliaOpt/JuMP-dev).

## Find a similar solver already wrapped

The next step is to find (if possible) a similar solver that is already wrapped.
Although not strictly necessary, this will be a good place to look for 
inspiration when implementing your wrapper.

The [JuMP documentation](https://jump.dev/JuMP.jl/stable/installation/#Supported-solvers)
has a good list of solvers, along with the problem classes they support. 

!!! tip
    If you're not sure which solver is most similar, ask in the 
    [Developer chatroom](https://gitter.im/JuliaOpt/JuMP-dev).

## Create a low-level interface

### Wrapping solvers written in Julia

If your solver is written in Julia, there's nothing to do here! Go to the next 
section.

### Solvers written in C

Julia is well suited to wrapping solvers written in C.

!!! warning
    This is _not_ true for C++.

Before writing a MathOptInterface wrapper, there are a few extra steps.

#### Create a JLL

If the C code is publicly available under an open-source license, create a 
JLL package via [Yggdrasil](https://github.com/JuliaPackaging/Yggdrasil). The 
easiest way to do this is to copy an existing solver. Good examples to follow
are the [COIN-OR solvers](https://github.com/JuliaPackaging/Yggdrasil/tree/master/C/Coin-OR).

!!! warning
    Building the solver via Yggdrasil is non-trivial. please ask the 
    [Developer chatroom](https://gitter.im/JuliaOpt/JuMP-dev) for help.

If the code is commercial or not publicly available, the user will need to
manually install the solver. See [Gurobi.jl](https://github.com/jump-dev/Gurobi.jl)
or [CPLEX.jl](https://github.com/jump-dev/CPLEX.jl) for examples of how to 
structure this.

#### Use Clang.jl to wrap the C API

The next step is to use [Clang.jl](https://github.com/JuliaInterop/Clang.jl) to 
automatically wrap the C API. The easiest way to do this is to follow an 
example. Good examples to follow are 
[Cbc.jl](https://github.com/jump-dev/Cbc.jl/blob/master/scripts/clang.jl) and
[HiGHS.jl](https://github.com/jump-dev/HiGHS.jl/blob/master/gen/gen.jl).

Sometimes, you will need to make manual modifications to the resulting files.

### Solvers written in other languages

Ask the [Developer chatroom](https://gitter.im/JuliaOpt/JuMP-dev) for advice.
You may be able to use on of the JuliaInterop packages to call out to the 
solver.

For example, [SeDuMi.jl](https://github.com/jump-dev/SeDuMi.jl) uses 
[MATLAB.jl](https://github.com/JuliaInterop/MATLAB.jl) to call the SeDuMi solver
written in MATLAB.

## Structuring the package

Structure your wrapper as a Julia package.

MOI solver interfaces may be in the same package as the solver itself (either 
the C wrapper if the solver is accessible through C, or the Julia code if the 
solver is written in Julia, for example). 

!!! note
    The JuMP [core contributors](https://jump.dev/pages/governance/#core-contributors)
    request that you do not use "JuMP" in the name of your package without prior
    consent.

The guideline for naming the file containing the MOI wrapper is 
`src/MOI_wrapper.jl` and `test/MOI_wrapper.jl` for the tests. 

If the MOI wrapper implementation is spread in several files, they should be 
stored in a `src/MOI_wrapper` folder and included by a 
`src/MOI_wrapper/MOI_wrapper.jl` file.

For example:
```
/gen
    gen.jl  # Code to wrap the C API
/src
    NewSolver.jl
    /gen
        libnewsolver_api.jl
        libnewsolver_common.jl
    /MOI_wrapper
        MOI_wrapper.jl
        other_files.jl
/test
    runtests.jl
    /MOI_wrapper
        MOI_wrapper.jl
```

!!! tip
    For the info on how to structure the tests, see 
    [How to test a solver](@ref).

## The `Optimizer` object

The first object to create is a subtype of `AbstractOptimizer`. By convention, 
these optimizers should not be exported and should be named 
`PackageName.Optimizer`.

```julia
struct Optimizer <: MOI.AbstractOptimizer
    # Fields go here
end
```

### Optimizer objects for C solvers

!!! warning 
    This section is important if you wrap a solver written in C.

Wrapping a solver written in C will require the use of pointers. **Never pass 
the pointer directly to a low-level function.** Instead, store the pointer as a 
field in your `Optimizer`, and implement `Base.cconvert` and 
`Base.unsafe_convert`.

```julia
struct Optimizer <: MOI.AbstractOptimizer
    ptr::Ptr{Cvoid}
end

Base.cconvert(::Type{Ptr{Cvoid}}, model::Optimizer) = model
Base.unsafe_convert(::Type{Ptr{Cvoid}}, model::Optimizer) = model.ptr
```

Then, pass `model` instead of `model.ptr` to low-level functions that expect the
model pointer.

### Implement methods for `Optimizer`

Now that we have an `Optimizer`, we need to implement a few basic methods.

* `Base.show(::IO, ::Optimizer)` 
    Overload `show` to print a nice string when some prints your model.
* [`empty!`](@ref) and [`is_empty`](@ref)

### Implement Optimizer attributes

* [`SolverName`](@ref)
* [`Name`](@ref)
* [`Silent`](@ref)
* [`TimeLimitSec`](@ref)
* [`RawParameter`](@ref)

## The first big decision: incremental modifications?

The first big decision you face is whether to support incremental modification.

Incremental modification means that you can add variables and constraints 
one-by-one without needing to rebuild the entire problem, and you can modify 
the problem data after an [`optimize!`](@ref) call. Supporting incremental 
modification means implementing functions like [`add_variable`](@ref) and 
[`add_constraint`](@ref).

The alternative is to accept the problem data in a single [`copy_to`](@ref) 
function call, afterwhich it cannot be modified.

Good examples of solvers supporting incremental modification are MILP solvers
like [GLPK.jl](https://github.com/jump-dev/GLPK.jl) and 
[Gurobi.jl](https://github.com/jump-dev/Gurobi.jl). Examples of [`copy_to`](@ref)
solvers are [AmplNLWriter.jl](https://github.com/jump-dev/AmplNLWriter.jl) and
[SCS.jl](https://github.com/jump-dev/SCS.jl)

It is possible to implement both approaches, but you should probably start with
one for simplicity.

!!! tip
    Only support incremental modification if your solver has native support for 
    it.

In general, supporting incremental modification is more work, and it usually 
some extra book-keeping. However, it provides a more efficient interface to the 
solver, particularly if the problem is going to be resolved multiple times with
small modifications. 

Moreover, once you've implemented incremental modification, it's usually not 
much extra work to add a [`copy_to`](@ref) interface. The converse is not true.

## Solver-specific attributes

Solver-specific attributes should be specified by creating an
[`AbstractOptimizerAttribute`](@ref). For example, inside `MyPackage`, we could
add the following:
```julia
struct PrintLevel <: MOI.AbstractOptimizerAttribute end
function MOI.set(model::Optimizer, ::PrintLevel, level::Int)
    # ... set the print level ...
end
```
Then, the user can write:
```julia
model = MyPackage.Optimizer()
MOI.set(model, MyPackage.PrintLevel(), 0)
```

## Supported constrained variables and constraints

The solver interface should only implement support for variables
constrained on creation (see
[`add_constrained_variable`](@ref)/[`add_constrained_variables`](@ref)) or
constraints that directly map to a structure exploited by the solver algorithm.
There is no need to add support for additional types, this is handled by
[The Bridges submodule](@ref). Furthermore, this allows
[`supports_constraint`](@ref) to indicate which types are exploited by the
solver and hence allows layers such as [`Bridges.LazyBridgeOptimizer`](@ref)
to accurately select the most appropriate transformations.

As [`add_constrained_variable`](@ref) (resp. [`add_constrained_variables`](@ref))
falls back to [`add_variable`](@ref) (resp. [`add_variables`](@ref)) followed by
[`add_constraint`](@ref), there is no need to implement this function
if `model` does not require that variables be constrained when they are created.
However, if `model` requires that variables be constrained when they're created,
then it should only implement [`add_constrained_variable`](@ref) and not
[`add_variable`](@ref) nor [`add_constraint`](@ref) for
[`SingleVariable`](@ref)-in-`typeof(set)`. In addition, it should implement
`supports_add_constrained_variables(::Optimizer, ::Type{Reals})` and return
`false` so that these variables are bridged, see
[`supports_add_constrained_variables`](@ref).

## Handling duplicate coefficients

Solvers should expect that functions such as `ScalarAffineFunction` and
`VectorQuadraticFunction` may contain duplicate coefficents, for example,
`ScalarAffineFunction([ScalarAffineTerm(x, 1), ScalarAffineTerm(x, 1)], 0.0)`.
These duplicate terms can be aggregated by calling
[`Utilities.canonical`](@ref).

```jldoctest; setup = :(using MathOptInterface)
x = MathOptInterface.VariableIndex(1)
term = MathOptInterface.ScalarAffineTerm(1, x)
func = MathOptInterface.ScalarAffineFunction([term, term], 0)
func_canon = MathOptInterface.Utilities.canonical(func)
func_canon ≈ MathOptInterface.ScalarAffineFunction(
    [MathOptInterface.ScalarAffineTerm(2, x)], 0)

# output

true
```

## Implementing copy

Avoid storing extra copies of the problem when possible. This means that solver
wrappers should not use [`Utilities.CachingOptimizer`](@ref) as part of the
wrapper. Instead, do one of the following to load the problem (assuming the
solver wrapper type is called `Optimizer`):

* If the solver supports loading the problem incrementally, implement
  [`add_variable`](@ref), [`add_constraint`](@ref) for supported constraints and
  [`set`](@ref) for supported attributes and add:
  ```julia
  function MOI.copy_to(dest::Optimizer, src::MOI.ModelLike; kws...)
      return MOI.Utilities.automatic_copy_to(dest, src; kws...)
  end
  ```
  with
  ```julia
  MOI.Utilities.supports_default_copy_to(model::Optimizer, copy_names::Bool) = true
  ```
  or
  ```julia
  MOI.Utilities.supports_default_copy_to(model::Optimizer, copy_names::Bool) = !copy_names
  ```
  depending on whether the solver support names; see
  [`Utilities.supports_default_copy_to`](@ref) for more details.
* If the solver does not support loading the problem incrementally, do not
  implement [`add_variable`](@ref) and [`add_constraint`](@ref) as implementing
  them would require caching the problem. Let users or JuMP decide whether to
  use a `CachingOptimizer` instead. Write either a custom implementation of
  [`copy_to`](@ref) or implement the [Allocate-Load API](@ref). If you choose to
  implement the Allocate-Load API,
  do
  ```julia
  function MOI.copy_to(dest::Optimizer, src::MOI.ModelLike; kws...)
      return MOI.Utilities.automatic_copy_to(dest, src; kws...)
  end
  ```
  with
  ```julia
  MOI.Utilities.supports_allocate_load(model::Optimizer, copy_names::Bool) = true
  ```
  or
  ```julia
  MOI.Utilities.supports_allocate_load(model::Optimizer, copy_names::Bool) = !copy_names
  ```
  depending on whether the solver support names; see
  [`Utilities.supports_allocate_load`](@ref) for more details.

  Note that even if both writing a custom implementation of [`copy_to`](@ref)
  and implementing the [Allocate-Load API](@ref) requires the user to copy the
  model from a cache, the [Allocate-Load API](@ref) allows MOI layers to be
  added between the cache and the solver which allows transformations to be
  applied without the need for additional caching. For instance, with the
  proposed [Light bridges](https://github.com/jump-dev/MathOptInterface.jl/issues/523),
  no cache will be needed to store the bridged model when bridges are used by
  JuMP so implementing the [Allocate-Load API](@ref) will allow JuMP to use only
  one cache instead of two.

## JuMP mapping

MOI defines a very general interface, with multiple possible ways to describe
the same constraint.

This is considered a feature, not a bug.

MOI is designed to make it possible to experiment with alternative
representations of an optimization problem at both the solving and modeling
level.

When implementing an interface, it is important to keep in mind that the way the
user can express problems in JuMP is not directly limited by the constraints
which a solver supports via MOI as JuMP performs automatic reformulation
via [The Bridges submodule](@ref).

Therefore, we recommend to only support the constraint types that directly map
to a structure exploited by the solver algorithm.

The following bullet points show examples of how JuMP constraints are translated
into MOI function-set pairs:

 - `@constraint(m, 2x + y <= 10)` becomes `ScalarAffineFunction`-in-`LessThan`
 - `@constraint(m, 2x + y >= 10)` becomes `ScalarAffineFunction`-in-`GreaterThan`
 - `@constraint(m, 2x + y == 10)` becomes `ScalarAffineFunction`-in-`EqualTo`
 - `@constraint(m, 0 <= 2x + y <= 10)` becomes `ScalarAffineFunction`-in-`Interval`
 - `@constraint(m, 2x + y in ArbitrarySet())` becomes
   `ScalarAffineFunction`-in-`ArbitrarySet`.

Variable bounds are handled in a similar fashion:

 - `@variable(m, x <= 1)` becomes `SingleVariable`-in-`LessThan`
 - `@variable(m, x >= 1)` becomes `SingleVariable`-in-`GreaterThan`

One notable difference is that a variable with an upper and lower bound is
translated into two constraints, rather than an interval. i.e.:

 - `@variable(m, 0 <= x <= 1)` becomes `SingleVariable`-in-`LessThan` *and*
    `SingleVariable`-in-`GreaterThan`.

Solvers are not expected to support `AbstractScalarFunction` in `GreaterThan`,
`LessThan`, `EqualTo`, or `Interval` with a nonzero constant in the function.
Constants in the affine function should instead be moved into the parameters of
the corresponding sets. The [`ScalarFunctionConstantNotZero`](@ref) exception
may be thrown in this case.

## Column Generation

There is no special interface for column generation. If the solver has a special
API for setting coefficients in existing constraints when adding a new variable,
it is possible to queue modifications and new variables and then call the
solver's API once all of the new coefficients are known.

## Problem data

All data passed to the solver should be copied immediately to internal data
structures. Solvers may not modify any input vectors and should assume that
input vectors may be modified by users in the future. This applies, for example,
to the `terms` vector in `ScalarAffineFunction`. Vectors returned to the user,
e.g., via `ObjectiveFunction` or `ConstraintFunction` attributes, should not be
modified by the solver afterwards. The in-place version of `get!` can be used by
users to avoid extra copies in this case.

## Statuses

Solver wrappers should document how the low-level statuses map to the MOI
statuses. Statuses like `NEARLY_FEASIBLE_POINT` and `INFEASIBLE_POINT`, are
designed to be used when the solver explicitly indicates that relaxed tolerances
are satisfied or the returned point is infeasible, respectively.
