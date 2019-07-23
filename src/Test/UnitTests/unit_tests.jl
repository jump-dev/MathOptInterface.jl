#=
    These tests aim to minimally test each expected feature in MOI, in addition
    to the full end-to-end tests in contlinear.jl etc
=#

const unittests = Dict{String, Function}()

"""
    test_model_solution(model::MOI.ModelLike, config::TestConfig;
        objective_value   = nothing,
        variable_primal   = nothing,
        constraint_primal = nothing,
        constraint_dual   = nothing
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

    MOIU.loadfromstring!(model, \"\"\"
        variables: x
        minobjective: 2.0x + 1.0
        c: x >= 1.0
    \"\"\")
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{Float64}}, "c")
    test_model_solution(model, config;
        objective_value   = 3.0,
        variable_primal   = [(x, 1.0)],
        constraint_primal = [(c, 1.0)],
        constraint_dual   = [(c, 2.0)]
    )

"""
function test_model_solution(model, config;
        objective_value   = nothing,
        variable_primal   = nothing,
        constraint_primal = nothing,
        constraint_dual   = nothing
    )
    config.solve || return
    atol, rtol = config.atol, config.rtol
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    if objective_value != nothing
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ objective_value atol=atol rtol=rtol
    end
    if variable_primal != nothing
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        for (index, solution_value) in variable_primal
            @test MOI.get(model, MOI.VariablePrimal(), index) ≈ solution_value atol=atol rtol=rtol
        end
    end
    if constraint_primal != nothing
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        for (index, solution_value) in constraint_primal
            @test MOI.get(model, MOI.ConstraintPrimal(), index) ≈ solution_value atol=atol rtol=rtol
        end
    end
    if config.duals
        if constraint_dual != nothing
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            for (index, solution_value) in constraint_dual
                @test MOI.get(model, MOI.ConstraintDual(), index) ≈ solution_value atol=atol rtol=rtol
            end
        end
    end
end

include("variables.jl")
include("objectives.jl")
include("constraints.jl")
include("basic_constraint_tests.jl")
include("modifications.jl")
include("solve.jl")
include("attributes.jl")

@moitestset unit
