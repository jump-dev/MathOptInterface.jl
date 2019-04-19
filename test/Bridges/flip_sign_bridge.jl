mock = MOIU.MockOptimizer(SimpleModel{Float64}())
config = MOIT.TestConfig()
config_with_basis = MOIT.TestConfig(basis = true)

@testset "GreaterToLess" begin
    bridged_mock = MOIB.GreaterToLess{Float64}(mock)

    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, -100]))
    MOIT.linear6test(bridged_mock, config)

    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}()))
    test_delete_bridge(bridged_mock, ci, 2,
                       ((MOI.ScalarAffineFunction{Float64},
                         MOI.LessThan{Float64}, 1),))
end

@testset "LessToGreater" begin
    bridged_mock = MOIB.LessToGreater{Float64}(mock)

    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
            MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [1.0]),
            MOI.FEASIBLE_POINT,
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [1.0]
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
            MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [2.0]),
            MOI.FEASIBLE_POINT,
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [1.0]
        )
    )
    MOIT.solve_set_scalaraffine_lessthan(bridged_mock, config)

    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
            MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [1.0]),
            MOI.FEASIBLE_POINT,
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [1.0]
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
            MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [0.5]),
            MOI.FEASIBLE_POINT,
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [0.5]
        )
    )
    MOIT.solve_coef_scalaraffine_lessthan(bridged_mock, config)

    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}}()))
    test_delete_bridge(bridged_mock, ci, 1,
                       ((MOI.ScalarAffineFunction{Float64},
                         MOI.GreaterThan{Float64}, 0),))
end

@testset "NonnegToNonpos" begin
    bridged_mock = MOIB.NonnegToNonpos{Float64}(mock)

    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, -100]))
    MOIT.linear7test(bridged_mock, config)

    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives}()))
    test_delete_bridge(bridged_mock, ci, 2,
                       ((MOI.VectorAffineFunction{Float64},
                         MOI.Nonpositives, 1),))
end

@testset "NonposToNonneg" begin
    bridged_mock = MOIB.NonposToNonneg{Float64}(mock)

    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, -100]))
    MOIT.linear7test(bridged_mock, config)

    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
            MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [0.0, 0.0])
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
            MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [1.0, 0.75])
        )
    )
    MOIT.solve_const_vectoraffine_nonpos(bridged_mock, config)

    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [0.5])
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [0.25])
        )
    )
    MOIT.solve_multirow_vectoraffine_nonpos(bridged_mock, config)

    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Nonpositives}()))
    test_delete_bridge(bridged_mock, ci, 1,
                       ((MOI.VectorAffineFunction{Float64},
                         MOI.Nonnegatives, 0),))
end
