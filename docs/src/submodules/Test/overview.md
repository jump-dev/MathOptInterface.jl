```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# The Test submodule

The `Test` submodule provides tools to help solvers implement unit tests in
order to ensure they implement the MathOptInterface API correctly, and to check
for solver-correctness.

We use a centralized repository of tests, so that if we find a bug in one
solver, instead of adding a test to that particular repository, we add it here
so that all solvers can benefit.

## How to test a solver

The skeleton below can be used for the wrapper test file of a solver named
`FooBar`. Remove unnecessary tests as appropriate.

```julia
# ============================ /test/MOI_wrapper.jl ============================
module TestFoobar

import FooBar
using MathOptInterface
using Test

const MOI = MathOptInterface

const OPTIMIZER_CONSTRUCTOR = MOI.OptimizerWithAttributes(
    FooBar.Optimizer,
    MOI.Silent() => true
)
const OPTIMIZER = MOI.instantiate(OPTIMIZER_CONSTRUCTOR)

const BRIDGED = MOI.instantiate(
    OPTIMIZER_CONSTRUCTOR, with_bridge_type = Float64
)
const CONFIG = MOI.Test.TestConfig(
    # Modify tolerances as necessary.
    atol = 1e-6,
    rtol = 1e-6,
    # Set false if dual solutions are not generated
    duals = true,
    # Set false if infeasibility certificates are not generated
    infeas_certificates = true,
    # Use MOI.LOCALLY_SOLVED for local solvers.
    optimal_status = MOI.OPTIMAL,
    # Set true if basis information is available
    basis = false,
)

function test_SolverName()
    @test MOI.get(OPTIMIZER, MOI.SolverName()) == "FooBar"
end

function test_supports_default_copy_to()
    @test MOI.Utilities.supports_default_copy_to(OPTIMIZER, false)
    # Use `@test !...` if names are not supported
    @test MOI.Utilities.supports_default_copy_to(OPTIMIZER, true)
end

function test_unittest()
    # Test all the functions included in dictionary `MOI.Test.unittests`,
    # except functions "number_threads" and "solve_qcp_edge_cases."
    MOI.Test.unittest(
        BRIDGED,
        CONFIG,
        ["number_threads", "solve_qcp_edge_cases"]
    )
end

function test_modification()
    MOI.Test.modificationtest(BRIDGED, CONFIG)
end

function test_contlinear()
    MOI.Test.contlineartest(BRIDGED, CONFIG)
end

function test_contquadratictest()
    MOI.Test.contquadratictest(OPTIMIZER, CONFIG)
end

function test_contconic()
    MOI.Test.contlineartest(BRIDGED, CONFIG)
end

function test_intconic()
    MOI.Test.intconictest(BRIDGED, CONFIG)
end

function test_default_objective_test()
    MOI.Test.default_objective_test(OPTIMIZER)
end

function test_default_status_test()
    MOI.Test.default_status_test(OPTIMIZER)
end

function test_nametest()
    MOI.Test.nametest(OPTIMIZER)
end

function test_validtest()
    MOI.Test.validtest(OPTIMIZER)
end

function test_emptytest()
    MOI.Test.emptytest(OPTIMIZER)
end

function test_orderedindicestest()
    MOI.Test.orderedindicestest(OPTIMIZER)
end

function test_scalar_function_constant_not_zero()
    MOI.Test.scalar_function_constant_not_zero(OPTIMIZER)
end

# This function runs all functions in this module starting with `test_`.
function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
end

end # module TestFooBar

TestFooBar.runtests()
```

Test functions like `MOI.Test.unittest` and `MOI.Test.modificationtest` are
wrappers around corresponding dictionaries `MOI.Test.unittests` and
`MOI.Test.modificationtests`. Exclude tests by passing a vector of strings
corresponding to the test keys you want to exclude as the third positional
argument to the test function.

!!! tip
     Print a list of all keys using `println.(keys(MOI.Test.unittests))`

The optimizer `BRIDGED` constructed with [`instantiate`](@ref)
automatically bridges constraints that are not supported by `OPTIMIZER`
using the bridges listed in [Bridges](@ref). It is recommended for an
implementation of MOI to only support constraints that are natively supported
by the solver and let bridges transform the constraint to the appropriate form.
For this reason it is expected that tests may not pass if `OPTIMIZER` is used
instead of `BRIDGED`.

To test that a specific problem can be solved without bridges, a specific test
can be added with `OPTIMIZER` instead of `BRIDGED`. For example:
```julia
function test_interval_constraints()
    MOI.Test.linear10test(OPTIMIZER, CONFIG)
end
```
checks that `OPTIMIZER` implements support for
[`ScalarAffineFunction`](@ref)-in-[`Interval`](@ref).

## How to add a test

To give an example, ECOS errored calling [`optimize!`](@ref) twice in a row.
(See [ECOS.jl PR #72](https://github.com/jump-dev/ECOS.jl/pull/72).)

We could add a test to ECOS.jl, but that would only stop us from re-introducing
the bug to ECOS.jl in the future.

Instead, if we add a test to `MOI.Test`, then all solvers will also check that
they handle a double optimize call!

For this test, we care about correctness, rather than performance. therefore, we
don't expect solvers to efficiently decide that they have already solved the
problem, only that calling [`optimize!`](@ref) twice doesn't throw an error or
give the wrong answer.

To resolve this issue, follow these steps (tested on Julia v1.5):

1. Install the `MathOptInterface` julia package in `dev` mode
   ([ref](https://julialang.github.io/Pkg.jl/v1/managing-packages/#developing-1)):
   ```julia
   julia> ]
   (@v1.5) pkg> dev ECOS
   (@v1.5) pkg> dev MathOptInterface
   ```
2. From here on, proceed with making the following changes in the
   `~/.julia/dev/MathOptInterface` folder (or equivalent `dev` path on your
   machine).
3. Since the double-optimize error involves solving an optimization problem,
   add a new test to [src/Test/UnitTests/solve.jl](https://github.com/jump-dev/MathOptInterface.jl/blob/master/src/Test/UnitTests/solve.jl).
   The test should be something like
   ```julia
   function solve_twice(model::MOI.ModelLike, config::TestConfig)
       MOI.empty!(model)
       x = MOI.add_variable(model)
       c = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(1.0))
       MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
       MOI.set(model, MOI.ObjectiveFunction{MOI.SingleVariable}(), MOI.SingleVariable(x))
       if config.solve
           MOI.optimize!(model)
           MOI.optimize!(model)
           MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
           MOI.get(model, MOI.VariablePrimal(), x) == 1.0
       end
   end
   unittests["solve_twice"] = solve_twice
   ```
2. Add a test for the test you just wrote. (We test the tests!)
   a. Add the name of the test (`"solve_twice"`) to the end of the array in
      `MOI.Test.unittest(...)` ([link](https://github.com/jump-dev/MathOptInterface.jl/blob/7543afe4b5151cf36bbd18181c1bb5c83266ae2f/test/Test/unit.jl#L51-L52)).
    b. Add a test for the test towards the end of the "Unit Tests" test set
       ([link](https://github.com/jump-dev/MathOptInterface.jl/blob/7543afe4b5151cf36bbd18181c1bb5c83266ae2f/test/Test/unit.jl#L394)).
       The test should look something like
       ```julia
       @testset "solve_twice" begin
        MOI.Utilities.set_mock_optimize!(mock,
            (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0]),
            ),
            (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0]),
                )
            )
            MOI.Test.solve_twice(mock, config)
        end
        ```
        In the above `mock` is a `MOI.Utilities.MockOptimizer` that is defined
        tesearlier in the file. In this test, `MOI.Utilities.set_mock_optimize!` loads
        `mock` with two results. Each says that the
        [`TerminationStatus`](@ref) is `MOI.OPTIMAL`, that the
        [`PrimalStatus`](@ref) is `MOI.FEASIBLE_POINT`, and that there is one
        variable with a `MOI.VariableValue` or `1.0`
3. Run the tests:
   ```julia
   (@v1.5) pkg> test ECOS
   ```
4. Finally, commit the changes to git from `~/.julia/dev/MathOptInterface` and
   submit the PR for review.
