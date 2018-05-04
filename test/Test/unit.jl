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
        "solve_affine_interval"
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
end
