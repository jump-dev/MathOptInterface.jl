```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# [The Test submodule](@id test_module)

The `Test` submodule provides tools to help solvers implement unit tests in
order to ensure they implement the MathOptInterface API correctly, and to check
for solver-correctness.

We use a centralized repository of tests, so that if we find a bug in one
solver, instead of adding a test to that particular repository, we add it here
so that all solvers can benefit.

## How to test a solver

The skeleton below can be used for the wrapper test file of a solver named
`FooBar`.

```julia
# ============================ /test/MOI_wrapper.jl ============================
module TestFooBar

import FooBar
using MathOptInterface
using Test

const MOI = MathOptInterface

const OPTIMIZER = MOI.instantiate(
    MOI.OptimizerWithAttributes(FooBar.Optimizer, MOI.Silent() => true),
)

const BRIDGED = MOI.instantiate(
    MOI.OptimizerWithAttributes(FooBar.Optimizer, MOI.Silent() => true),
    with_bridge_type = Float64,
)

# See the docstring of MOI.Test.Config for other arguments.
const CONFIG = MOI.Test.Config(
    # Modify tolerances as necessary.
    atol = 1e-6,
    rtol = 1e-6,
    # Use MOI.LOCALLY_SOLVED for local solvers.
    optimal_status = MOI.OPTIMAL,
    # Pass attributes or MOI functions to `exclude` to skip tests that
    # rely on this functionality.
    exclude = Any[MOI.VariableName, MOI.delete],
)

"""
    runtests()

This function runs all functions in the this Module starting with `test_`.
"""
function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
end

"""
    test_runtests()

This function runs all the tests in MathOptInterface.Test.

Pass arguments to `exclude` to skip tests for functionality that is not
implemented or that your solver doesn't support.
"""
function test_runtests()
    MOI.Test.runtests(
        BRIDGED,
        CONFIG,
        exclude = [
            "test_attribute_NumberOfThreads",
            "test_quadratic_",
        ],
        # This argument is useful to prevent tests from failing on future
        # releases of MOI that add new tests. Don't let this number get too far
        # behind the current MOI release though! You should periodically check
        # for new tests in order to fix bugs and implement new features.
        exclude_tests_after = v"0.10.5",
    )
    return
end

"""
    test_SolverName()

You can also write new tests for solver-specific functionality. Write each new
test as a function with a name beginning with `test_`.
"""
function test_SolverName()
    @test MOI.get(FooBar.Optimizer(), MOI.SolverName()) == "FooBar"
    return
end

end # module TestFooBar

# This line at tne end of the file runs all the tests!
TestFooBar.runtests()
```

Then modify your `runtests.jl` file to include the `MOI_wrapper.jl` file:
```julia
# ============================ /test/runtests.jl ============================

using Test

@testset "MOI" begin
    include("test/MOI_wrapper.jl")
end
```

!!! info
    The optimizer `BRIDGED` constructed with [`instantiate`](@ref)
    automatically bridges constraints that are not supported by `OPTIMIZER`
    using the bridges listed in [Bridges](@ref). It is recommended for an
    implementation of MOI to only support constraints that are natively
    supported by the solver and let bridges transform the constraint to the
    appropriate form. For this reason it is expected that tests may not pass if
    `OPTIMIZER` is used instead of `BRIDGED`.

## How to debug a failing test

When writing a solver, it's likely that you will initially fail many tests!
Some failures will be bugs, but other failures you may choose to exclude.

There are two ways to exclude tests:

 - Exclude tests whose names contain a string using:
   ```julia
   MOI.Test.runtests(
       model,
       config;
       exclude = String["test_to_exclude", "test_conic_"],
   )
   ```
   This will exclude tests whose name contains either of the two strings
   provided.

 - Exclude tests which rely on specific functionality using:
   ```julia
   MOI.Test.Config(exclude = Any[MOI.VariableName, MOI.optimize!])
   ```
   This will exclude tests which use the `MOI.VariableName` attribute,
   or which call `MOI.optimize!`.

Each test that fails can be independently called as:
```julia
model = FooBar.Optimizer()
config = MOI.Test.Config()
MOI.empty!(model)
MOI.Test.test_category_name_that_failed(model, config)
```

You can look-up the source code of the test that failed by searching for it
in the `src/Test/test_category.jl` file.

!!! tip
    Each test function also has a docstring that explains what the test is
    for. Use `? MOI.Test.test_category_name_that_failed` from the REPL to
    read it.

## How to add a test

To detect bugs in solvers, we add new tests to `MOI.Test`.

As an example, ECOS errored calling [`optimize!`](@ref) twice in a row. (See
[ECOS.jl PR #72](https://github.com/jump-dev/ECOS.jl/pull/72).) We could add a
test to ECOS.jl, but that would only stop us from re-introducing the bug to
ECOS.jl in the future, but it would not catch other solvers in the ecosystem
with the same bug! Instead, if we add a test to `MOI.Test`, then all solvers
will also check that they handle a double optimize call!

For this test, we care about correctness, rather than performance. therefore, we
don't expect solvers to efficiently decide that they have already solved the
problem, only that calling [`optimize!`](@ref) twice doesn't throw an error or
give the wrong answer.

**Step 1**

Install the `MathOptInterface` julia package in `dev` mode
([ref](https://julialang.github.io/Pkg.jl/v1/managing-packages/#developing-1)):
```julia
julia> ]
(@v1.6) pkg> dev MathOptInterface
```

**Step 2**

From here on, proceed with making the following changes in the
`~/.julia/dev/MathOptInterface` folder (or equivalent `dev` path on your
machine).

**Step 3**

Since the double-optimize error involves solving an optimization problem,
add a new test to [src/Test/UnitTests/solve.jl](https://github.com/jump-dev/MathOptInterface.jl/blob/master/src/Test/UnitTests/solve.jl):
```julia
"""
    test_unit_optimize!_twice(model::MOI.ModelLike, config::Config)

Test that calling `MOI.optimize!` twice does not error.

This problem was first detected in ECOS.jl PR#72:
https://github.com/jump-dev/ECOS.jl/pull/72
"""
function test_unit_optimize!_twice(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    # Use the `@requires` macro to check conditions that the test function
    # requires in order to run. Models failing this `@requires` check will
    # silently skip the test.
    @requires MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{Float64},
    )
    @requires _supports(config, MOI.optimize!)
    # If needed, you can test that the model is empty at the start of the test.
    # You can assume that this will be the case for tests run via `runtests`.
    # User's calling tests individually need to call `MOI.empty!` themselves.
    @test MOI.is_empty(model)
    # Create a simple model. Try to make this as simple as possible so that the
    # majority of solvers can run the test.
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.GreaterThan(one(T)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.VariableIndex}(),
        x,
    )
    # The main component of the test: does calling `optimize!` twice error?
    MOI.optimize!(model)
    MOI.optimize!(model)
    # Check we have a solution.
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    # There is a three-argument version of `Base.isapprox` for checking
    # approximate equality based on the tolerances defined in `config`:
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), x), one(T), config)
    # For code-style, these tests should always `return` `nothing`.
    return
end
```

!!! info
    Make sure the function is agnoistic to the number type `T`! Don't assume it
    is a `Float64` capable solver!

We also need to write a test for the test. Place this function immediately below
the test you just wrote in the same file:
```julia
function setup_test(
    ::typeof(test_unit_optimize!_twice),
    model::MOI.Utilities.MockOptimizer,
    ::Config,
)
    MOI.Utilities.set_mock_optimize!(
        model,
        (mock::MOI.Utilities.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0]),
        ),
    )
    return
end
```

Finally, you also need to implement [`Test.version_added`](@ref). If we added
this test when the latest released version of MOI was `v0.10.5`, define:
```julia
version_added(::typeof(test_unit_optimize!_twice)) = v"0.10.6"
```

**Step 6**

Commit the changes to git from `~/.julia/dev/MathOptInterface` and
submit the PR for review.

!!! tip
    If you need help writing a test, [open an issue on GitHub](https://github.com/jump-dev/MathOptInterface.jl/issues/new),
    or ask the [Developer Chatroom](https://gitter.im/JuliaOpt/JuMP.jl)
