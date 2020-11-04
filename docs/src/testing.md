# Testing

All solvers use the tests in this repository as extra correctness tests for themselves.
If we find a bug in one solver, instead of adding a test to that particular repository, we add it here so that all solvers can benefit.
All supported solvers are tested on travis with https://github.com/blegat/SolverTests.

## Example of adding a test

To give an example, ECOS errored calling `optimize!(model); optimize!(model)` ([ECOS.jl PR #72](https://github.com/jump-dev/ECOS.jl/pull/72)).
We could add a test to ECOS.jl, but that would only stop us from re-introducing the bug to ECOS.jl in the future.
Instead if we add a test here, then all solvers (e.g., SCS.jl, Gurobi.jl, Mosek.jl, ...) will also check that they handle a double optimize call!

For this test, we care about correctness, rather than performance.
We don't expect solvers to efficiently decide that they have already solved the problem,
only that calling `optimize!` twice doesn't throw an error or give the wrong answer.

To resolve this issue, follow these steps (tested on Julia v1.5):

1. Install the `MathOptInterface` julia package in `dev` mode ([ref](https://julialang.github.io/Pkg.jl/v1/managing-packages/#developing-1)):

```julia
julia> ]
(@v1.5) pkg> dev ECOS
(@v1.5) pkg> dev MathOptInterface
```

2. From here on, proceed with making the following changes in the `~/.julia/dev/MathOptInterface` folder (or equivalent `dev` path on your machine)
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

2. Add a test for the test you just wrote (We test the tests!)

    a. Add the name of the test ("solve_twice") to the end of the array in `MOIT.unittest(...)` ([link](https://github.com/jump-dev/MathOptInterface.jl/blob/7543afe4b5151cf36bbd18181c1bb5c83266ae2f/test/Test/unit.jl#L51-L52)).

    b. Add a test for the test towards the end of the "Unit Tests" test set ([link](https://github.com/jump-dev/MathOptInterface.jl/blob/7543afe4b5151cf36bbd18181c1bb5c83266ae2f/test/Test/unit.jl#L394)). The test should look something like

```julia
@testset "solve_twice" begin
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0]),
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0]),
        )
    )
    MOIT.solve_twice(mock, config)
end
```

In the above `mock` is a `MOI.Utilities.MockOptimizer` that is defined earlier in the file.
In this test, `MOIU.set_mock_optimize!` loads `mock` with two results. Each says
that the [`MOI.TerminationStatus`](@ref) is `MOI.OPTIMAL`, that the
[`MOI.PrimalStatus`](@ref) is `MOI.FEASIBLE_POINT`, and that there is one
variable with a `MOI.VariableValue` or `1.0`.

3. Run the tests:

```
(@v1.5) pkg> test ECOS
```

4. Finally, commit the changes to git from `~/.julia/dev/MathOptInterface`. Use a branch name of the format `initials/issueNumber_issueShortTitle`, and submit the PR for review.
