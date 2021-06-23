module Test

using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

using Test

struct Config{T<:Real}
    atol::T
    rtol::T
    solve::Bool
    query_number_of_constraints::Bool
    query::Bool
    modify_lhs::Bool
    duals::Bool
    dual_objective_value::Bool
    infeas_certificates::Bool
    optimal_status::MOI.TerminationStatusCode
    basis::Bool

    """
        Config{T}(;
            atol::Real = Base.rtoldefault(T),
            rtol::Real = Base.rtoldefault(T),
            solve::Bool = true,
            query_number_of_constraints::Bool = true,
            query::Bool = true,
            modify_lhs::Bool = true,
            duals::Bool = true,
            dual_objective_value::Bool = duals,
            infeas_certificates::Bool = true,
            optimal_status = MOI.OPTIMAL,
            basis::Bool = false,
        )

    Return an object that is used to configure various tests.

    ## Keywords

     * `atol::Real = Base.rtoldefault(T)`: Control the absolute tolerance used
        when comparing solutions.
     * `rtol::Real = Base.rtoldefault(T)`: Control the relative tolerance used
        when comparing solutions.
     * `solve::Bool = true`: Set to `false` to skip tests requiring a call to
       [`MOI.optimize!`](@ref)
     * `query_number_of_constraints::Bool = true`: Set to `false` to skip tests
       requiring a call to [`MOI.NumberOfConstraints`](@ref).
     * `query::Bool = true`: Set to `false` to skip tests requiring a call to
       [`MOI.get`](@ref) for [`MOI.ConstraintFunction`](@ref) and
       [`MOI.ConstraintSet`](@ref)
     * `modify_lhs::Bool = true`:
     * `duals::Bool = true`: Set to `false` to skip tests querying
       [`MOI.ConstraintDual`](@ref).
     * `dual_objective_value::Bool = duals`: Set to `false` to skip tests
       querying [`MOI.DualObjectiveValue`](@ref).
     * `infeas_certificates::Bool = true`: Set to `false` to skip tests querying
       primal and dual infeasibility certificates.
     * `optimal_status = MOI.OPTIMAL`: Set to `MOI.LOCALLY_SOLVED` if the solver
       cannot prove global optimality.
     * `basis::Bool = false`: Set to `true` if the solver supports
       [`MOI.ConstraintBasisStatus`](@ref) and [`MOI.VariableBasisStatus`](@ref).
    """
    function Config{T}(;
        atol::Real = Base.rtoldefault(T),
        rtol::Real = Base.rtoldefault(T),
        solve::Bool = true,
        query_number_of_constraints::Bool = true,
        query::Bool = true,
        modify_lhs::Bool = true,
        duals::Bool = true,
        dual_objective_value::Bool = duals,
        infeas_certificates::Bool = true,
        optimal_status = MOI.OPTIMAL,
        basis::Bool = false,
    ) where {T<:Real}
        return new(
            atol,
            rtol,
            solve,
            query_number_of_constraints,
            query,
            modify_lhs,
            duals,
            dual_objective_value,
            infeas_certificates,
            optimal_status,
            basis,
        )
    end
    Config(; kwargs...) = Config{Float64}(; kwargs...)
end

@deprecate TestConfig Config

"""
    setup_test(::typeof(f), model::MOI.ModelLike, config::Config)

Overload this method to modify `model` before running the test function `f` on
`model` with `config`.

This function should either return `nothing`, or return a function which, when
called with zero arguments, undoes the setup to return the model to its
previous state.

This is most useful when writing new tests of the tests for MOI, but can also be
used to set test-specific tolerances, etc.

See also: [`runtests`](@ref)

## Example

```julia
function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_VariablePrimalStart_partial),
    mock::MOIU.MockOptimizer,
    ::MOI.Test.Config,
)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 0.0]),
    )
    mock.eval_variable_constraint_dual = false

    function reset_function()
        mock.eval_variable_constraint_dual = true
        return
    end
    return reset_function
end
```
"""
setup_test(::Any, ::MOI.ModelLike, ::Config) = nothing

"""
    runtests(
        model::MOI.ModelLike,
        config::Config;
        include::Vector{String} = String[],
        exclude::Vector{String} = String[],
    )

Run all tests in `MathOptInterface.Test` on `model`.

## Configuration arguments

 * `config` is a [`Test.Config`](@ref) object that can be used to modify the
   behavior of tests.
 * If `include` is not empty, only run tests that contain an element from
   `include` in their name.
 * If `exclude` is not empty, skip tests that contain an element from `exclude`
   in their name.
 * `exclude` takes priority over `include`.

See also: [`setup_test`](@ref).

## Example

```julia
config = MathOptInterface.Test.Config()
MathOptInterface.Test.runtests(
    model,
    config;
    include = ["test_linear_"],
    exclude = ["VariablePrimalStart"],
)
```
"""
function runtests(
    model::MOI.ModelLike,
    config::Config;
    include::Vector{String} = String[],
    exclude::Vector{String} = String[],
)
    for name_sym in names(@__MODULE__; all = true)
        name = string(name_sym)
        if !startswith(name, "test_")
            continue  # All test functions start with test_
        elseif !isempty(include) && !any(s -> occursin(s, name), include)
            continue
        elseif !isempty(exclude) && any(s -> occursin(s, name), exclude)
            continue
        end
        @testset "$(name)" begin
            test_function = getfield(@__MODULE__, name_sym)
            tear_down = setup_test(test_function, model, config)
            test_function(model, config)
            if tear_down !== nothing
                tear_down()
            end
        end
    end
end

"""
    _test_model_solution(
        model::MOI.ModelLike,
        config::Config;
        objective_value   = nothing,
        variable_primal   = nothing,
        constraint_primal = nothing,
        constraint_dual   = nothing,
    )

Solve, and then test, various aspects of a model.

First, check that `TerminationStatus == MOI.OPTIMAL`.

If `objective_value` is not nothing, check that the attribute `ObjectiveValue()`
is approximately `objective_value`.

If `variable_primal` is not nothing, check that the attribute  `PrimalStatus` is
`MOI.FEASIBLE_POINT`. Then for each `(index, value)` in `variable_primal`, check
that the primal value of the variable `index` is approximately `value`.

If `constraint_primal` is not nothing, check that the attribute  `PrimalStatus` is
`MOI.FEASIBLE_POINT`. Then for each `(index, value)` in `constraint_primal`, check
that the primal value of the constraint `index` is approximately `value`.

Finally, if `config.duals = true`, and if `constraint_dual` is not nothing,
check that the attribute  `DualStatus` is `MOI.FEASIBLE_POINT`. Then for each
`(index, value)` in `constraint_dual`, check that the dual of the constraint
`index` is approximately `value`.

### Example

```julia
MOIU.loadfromstring!(model, \"\"\"
    variables: x
    minobjective: 2.0x + 1.0
    c: x >= 1.0
\"\"\")
x = MOI.get(model, MOI.VariableIndex, "x")
c = MOI.get(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{Float64}}, "c")
_test_model_solution(
    model,
    config;
    objective_value   = 3.0,
    variable_primal   = [(x, 1.0)],
    constraint_primal = [(c, 1.0)],
    constraint_dual   = [(c, 2.0)],
)
```
"""
function _test_model_solution(
    model,
    config;
    objective_value = nothing,
    variable_primal = nothing,
    constraint_primal = nothing,
    constraint_dual = nothing,
)
    if !config.solve
        return
    end
    atol, rtol = config.atol, config.rtol
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    if objective_value !== nothing
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ objective_value atol = atol rtol =
            rtol
    end
    if variable_primal !== nothing
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        for (index, solution_value) in variable_primal
            @test MOI.get(model, MOI.VariablePrimal(), index) ≈ solution_value atol =
                atol rtol = rtol
        end
    end
    if constraint_primal !== nothing
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        for (index, solution_value) in constraint_primal
            @test MOI.get(model, MOI.ConstraintPrimal(), index) ≈ solution_value atol =
                atol rtol = rtol
        end
    end
    if config.duals
        if constraint_dual !== nothing
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            for (index, solution_value) in constraint_dual
                @test MOI.get(model, MOI.ConstraintDual(), index) ≈
                      solution_value atol = atol rtol = rtol
            end
        end
    end
    return
end

macro moitestset(setname, subsets = false)
    testname = Symbol(string(setname) * "test")
    testdict = Symbol(string(testname) * "s")
    if subsets
        runtest = :(f(model, config, exclude))
    else
        runtest = :(f(model, config))
    end
    return esc(
        :(
            function $testname(
                model::$MOI.ModelLike,
                config::$MOI.Test.Config,
                exclude::Vector{String} = String[],
            )
                for (name, f) in $testdict
                    if name in exclude
                        continue
                    end
                    @testset "$name" begin
                        $runtest
                    end
                end
            end
        ),
    )
end

include("modellike.jl")
include("contlinear.jl")
include("contconic.jl")
include("contquadratic.jl")
include("intlinear.jl")
include("intconic.jl")
include("nlp.jl")

include("UnitTests/attributes.jl")
include("UnitTests/basic_constraint_tests.jl")
include("UnitTests/constraints.jl")
include("UnitTests/modifications.jl")
include("UnitTests/objectives.jl")
include("UnitTests/solve.jl")
include("UnitTests/variables.jl")

end # module
