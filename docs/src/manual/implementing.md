```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Implementing a solver interface

[The interface is designed for multiple dispatch, e.g., attributes, combinations
of sets and functions.]

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
which a solver supports via MOI as JuMP performs automatic reformulation](@ref)
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

## Naming

MOI solver interfaces may be in the same package as the solver itself (either
the C wrapper if the solver is accessible through C, or the Julia code if the
solver is written in Julia, for example). The guideline for naming the file
containing the MOI wrapper is `src/MOI_wrapper.jl` and `test/MOI_wrapper.jl` for
the tests. If the MOI wrapper implementation is spread in several files, they
should be stored in a `src/MOI_wrapper` folder and included by a
`src/MOI_wrapper/MOI_wrapper.jl` file.

By convention, optimizers should not be exported and should be named
`PackageName.Optimizer`. For example, `CPLEX.Optimizer`, `Gurobi.Optimizer`, and
`Xpress.Optimizer`.

## Testing guideline

The skeleton below can be used for the wrapper test file of a solver named
`FooBar`.
```julia
using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.Test
const MOIU = MOI.Utilities
const MOIB = MOI.Bridges

import FooBar
const OPTIMIZER_CONSTRUCTOR = MOI.OptimizerWithAttributes(
    FooBar.Optimizer, MOI.Silent() => true
)
const OPTIMIZER = MOI.instantiate(OPTIMIZER_CONSTRUCTOR)

@testset "SolverName" begin
    @test MOI.get(OPTIMIZER, MOI.SolverName()) == "FooBar"
end

@testset "supports_default_copy_to" begin
    @test MOIU.supports_default_copy_to(OPTIMIZER, false)
    # Use `@test !...` if names are not supported
    @test MOIU.supports_default_copy_to(OPTIMIZER, true)
end

const BRIDGED = MOI.instantiate(
    OPTIMIZER_CONSTRUCTOR, with_bridge_type = Float64
)
const CONFIG = MOIT.TestConfig(atol=1e-6, rtol=1e-6)

@testset "Unit" begin
    # Test all the functions included in dictionary `MOI.Test.unittests`,
    # except functions "number_threads" and "solve_qcp_edge_cases."
    MOIT.unittest(
        BRIDGED,
        CONFIG,
        ["number_threads", "solve_qcp_edge_cases"]
    )
end

@testset "Modification" begin
    MOIT.modificationtest(BRIDGED, CONFIG)
end

@testset "Continuous Linear" begin
    MOIT.contlineartest(BRIDGED, CONFIG)
end

@testset "Continuous Conic" begin
    MOIT.contlineartest(BRIDGED, CONFIG)
end

@testset "Integer Conic" begin
    MOIT.intconictest(BRIDGED, CONFIG)
end
```

Test functions like `MOI.Test.unittest` and `MOI.Test.modificationtest` are
wrappers around corresponding dictionaries `MOI.Test.unittests` and
`MOI.Test.modificationtests`. The keys of each dictionary (strings describing
the test) map to functions that take two arguments: an optimizer and a
`MOI.Test.TestConfig` object. Exclude tests by passing a vector of strings
corresponding to the test keys you want to exclude as the third positional
argument to the test function (e.g., `MOI.Test.unittest`).

Print a list of all keys using `println.(keys(MOI.Test.unittests))`

The optimizer `BRIDGED` constructed with [`instantiate`](@ref)
automatically bridges constraints that are not supported by `OPTIMIZER`
using the bridges listed in [Bridges](@ref). It is recommended for an
implementation of MOI to only support constraints that are natively supported
by the solver and let bridges transform the constraint to the appropriate form.
For this reason it is expected that tests may not pass if `OPTIMIZER` is used
instead of `BRIDGED`.

To test that a specific problem can be solved without bridges, a specific test
can be run with `OPTIMIZER` instead of `BRIDGED`. For instance
```julia
@testset "Interval constraints" begin
    MOIT.linear10test(OPTIMIZER, CONFIG)
end
```
checks that `OPTIMIZER` implements support for
[`ScalarAffineFunction`](@ref)-in-[`Interval`](@ref).

If the wrapper does not support building the model incrementally (i.e. with
[`add_variable`](@ref) and [`add_constraint`](@ref)),
then [`Utilities.supports_default_copy_to`](@ref) can be replaced by
[`Utilities.supports_allocate_load`](@ref) if appropriate (see
[Implementing copy](@ref)).

