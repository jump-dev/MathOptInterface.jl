@testset "Basic Constraint Tests" begin
    mock   = MOIU.MockOptimizer(Model{Float64}())
    config = MOIT.TestConfig()
    MOIT.basic_constraint_tests(mock, config)
end

@testset "Unit Tests" begin
    mock = MOIU.MockOptimizer(Model{Float64}())
    config = MOIT.TestConfig()
    MOIT.unittest(mock, config, [
        "solve_blank_obj",
        "solve_constant_obj",
        "solve_singlevariable_obj",
        "solve_with_lowerbound",
        "solve_with_upperbound",
        "solve_affine_lessthan",
        "solve_affine_greaterthan",
        "solve_affine_equalto",
        "solve_affine_interval",
        "solve_qp_edge_cases",
        "solve_qcp_edge_cases",
        "solve_affine_deletion_edge_cases"
        ])

    @testset "solve_blank_obj" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success,
                (MOI.FeasiblePoint, [1]),
                MOI.FeasiblePoint,
                    (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [0.0]
            )
        )
        MOIT.solve_blank_obj(mock, config)
    end
    @testset "solve_constant_obj" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success,
                (MOI.FeasiblePoint, [1]),
                MOI.FeasiblePoint,
                    (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [2.0]
            )
        )
        MOIT.solve_constant_obj(mock, config)
    end
    @testset "solve_singlevariable_obj" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success,
                (MOI.FeasiblePoint, [1]),
                MOI.FeasiblePoint,
                    (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [1.0]
            )
        )
        MOIT.solve_singlevariable_obj(mock, config)
    end
    @testset "solve_with_lowerbound" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success,
                (MOI.FeasiblePoint, [1]),
                MOI.FeasiblePoint,
                    (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [2.0],
                    (MOI.SingleVariable, MOI.LessThan{Float64})    => [0.0]
            )
        )
        MOIT.solve_with_lowerbound(mock, config)
    end
    @testset "solve_with_upperbound" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success,
                (MOI.FeasiblePoint, [1]),
                MOI.FeasiblePoint,
                    (MOI.SingleVariable, MOI.LessThan{Float64})    => [-2.0],
                    (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [0.0]
            )
        )
        MOIT.solve_with_upperbound(mock, config)
    end
    @testset "solve_affine_lessthan" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success,
                (MOI.FeasiblePoint, [0.5]),
                MOI.FeasiblePoint,
                    (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-0.5]
            )
        )
        MOIT.solve_affine_lessthan(mock, config)
    end
    @testset "solve_affine_greaterthan" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success,
                (MOI.FeasiblePoint, [0.5]),
                MOI.FeasiblePoint,
                    (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [0.5]
            )
        )
        MOIT.solve_affine_greaterthan(mock, config)
    end
    @testset "solve_affine_equalto" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success,
                (MOI.FeasiblePoint, [0.5]),
                MOI.FeasiblePoint,
                    (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) => [0.5]
            )
        )
        MOIT.solve_affine_equalto(mock, config)
    end
    @testset "solve_affine_interval" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success,
                (MOI.FeasiblePoint, [2.0]),
                MOI.FeasiblePoint,
                    (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) => [-1.5]
            )
        )
        MOIT.solve_affine_interval(mock, config)
    end

    @testset "solve_qcp_edge_cases" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success,
                (MOI.FeasiblePoint, [0.5, 0.5])
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success,
                (MOI.FeasiblePoint, [0.5, (√13 - 1)/4])
            )
        )
        MOIT.solve_qcp_edge_cases(mock, config)
    end

    @testset "solve_qp_edge_cases" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success,
                (MOI.FeasiblePoint, [1.0, 2.0])
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success,
                (MOI.FeasiblePoint, [1.0, 2.0])
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success,
                (MOI.FeasiblePoint, [1.0, 2.0])
            )
        )
        MOIT.solve_qp_edge_cases(mock, config)
    end

    @testset "solve_affine_deletion_edge_cases" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success, (MOI.FeasiblePoint, [0.0])
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success, (MOI.FeasiblePoint, [0.0])
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success, (MOI.FeasiblePoint, [1.0])
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success, (MOI.FeasiblePoint, [1.0])
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success, (MOI.FeasiblePoint, [2.0])
            )
        )
        MOIT.solve_affine_deletion_edge_cases(mock, config)
    end
end

@testset "modifications" begin
    mock   = MOIU.MockOptimizer(Model{Float64}())
    config = MOIT.TestConfig()
    @testset "solve_set_singlevariable_lessthan" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success, (MOI.FeasiblePoint, [1.0]),
                MOI.FeasiblePoint,
                    (MOI.SingleVariable, MOI.LessThan{Float64}) => [-1.0]
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success, (MOI.FeasiblePoint, [2.0]),
                MOI.FeasiblePoint,
                    (MOI.SingleVariable, MOI.LessThan{Float64}) => [-1.0]
            )
        )
        MOIT.solve_set_singlevariable_lessthan(mock, config)
    end
    @testset "solve_set_scalaraffine_lessthan" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success, (MOI.FeasiblePoint, [1.0]),
                MOI.FeasiblePoint,
                    (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1.0]
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success, (MOI.FeasiblePoint, [2.0]),
                MOI.FeasiblePoint,
                    (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1.0]
            )
        )
        MOIT.solve_set_scalaraffine_lessthan(mock, config)
    end
    @testset "solve_coef_scalaraffine_lessthan" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success, (MOI.FeasiblePoint, [1.0]),
                MOI.FeasiblePoint,
                    (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1.0]
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success, (MOI.FeasiblePoint, [0.5]),
                MOI.FeasiblePoint,
                    (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-0.5]
            )
        )
        MOIT.solve_coef_scalaraffine_lessthan(mock, config)
    end
    @testset "solve_const_vectoraffine_nonpos" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success, (MOI.FeasiblePoint, [0.0, 0.0])
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success, (MOI.FeasiblePoint, [1.0, 0.75])
            )
        )
        MOIT.solve_const_vectoraffine_nonpos(mock, config)
    end
    @testset "solve_const_scalar_objective" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success, (MOI.FeasiblePoint, [1.0])
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success, (MOI.FeasiblePoint, [1.0])
            )
        )
        MOIT.solve_const_scalar_objective(mock, config)
    end
    @testset "solve_coef_scalar_objective" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success, (MOI.FeasiblePoint, [1.0])
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.Success, (MOI.FeasiblePoint, [1.0])
            )
        )
        MOIT.solve_coef_scalar_objective(mock, config)
    end
end
