"""
    test_Integer_SecondOrderCone(model::MOI.ModelLike, config::Config)

Run an integration test on the problem:
```
 min    - 2y - 1z
s.t.   x           == 1
      [x,  y,   z] in SecondOrderCone()
           y,   z  in ZeroOne()
```
"""
function test_Integer_SecondOrderCone(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    @test MOI.supports_incremental_interface(model, false) #=copy_names=#
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Zeros,
    )
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.ZeroOne)
    @test MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.SecondOrderCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x, y, z = MOI.add_variables(model, 3)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([-2.0, -1.0], [y, z]),
            0.0,
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    ceq = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
            [-1.0],
        ),
        MOI.Zeros(1),
    )
    csoc = MOI.add_constraint(
        model,
        MOI.VectorOfVariables([x, y, z]),
        MOI.SecondOrderCone(3),
    )
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Zeros,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SecondOrderCone}(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{Float64}, MOI.Zeros) in loc
    @test (MOI.VectorOfVariables, MOI.SecondOrderCone) in loc
    bin1 = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.ZeroOne())
    # We test this after the creation of every `SingleVariable` constraint
    # to ensure a good coverage of corner cases.
    @test bin1.value == y.value
    bin2 = MOI.add_constraint(model, MOI.SingleVariable(z), MOI.ZeroOne())
    @test bin2.value == z.value
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -2 atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 1 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 1 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), z) ≈ 0 atol = atol rtol =
            rtol
    end
end

function setup_test(
    ::typeof(test_Integer_SecondOrderCone),
    model::MOI.Utilities.MockOptimizer,
    config::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1.0, 0.0]),
    )
    return
end
