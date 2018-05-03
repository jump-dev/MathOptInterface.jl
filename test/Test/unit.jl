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

    # @testset "solve_blank_obj" begin
    #     MOIU.set_mock_optimize!(mock,
    #         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
    #             MOI.Success,
    #             (MOI.FeasiblePoint, [1]),
    #             MOI.FeasiblePoint,
    #                 (MOI.SingleVariable, MOI.GreaterThan{Float64}) => 0.0
    #         )
    #     )
    #     MOIT.solve_blank_obj(mock, config)
    # end
    # @testset "solve_constant_obj" begin
    #     MOIU.set_mock_optimize!(mock,
    #         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
    #             MOI.Success,
    #             (MOI.FeasiblePoint, [1]),
    #             MOI.FeasiblePoint,
    #                 (MOI.SingleVariable, MOI.GreaterThan{Float64}) => 2.0
    #         )
    #     )
    #     MOIT.solve_constant_obj(mock, config)
    # end
    # @testset "solve_singlevariable_obj" begin
    #     MOIU.set_mock_optimize!(mock,
    #         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
    #             MOI.Success,
    #             (MOI.FeasiblePoint, [1]),
    #             MOI.FeasiblePoint,
    #                 (MOI.SingleVariable, MOI.GreaterThan{Float64}) => 1.0
    #         )
    #     )
    #     MOIT.solve_singlevariable_obj(mock, config)
    # end
    # @testset "solve_with_lowerbound" begin
    #     MOIU.set_mock_optimize!(mock,
    #         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
    #             MOI.Success,
    #             (MOI.FeasiblePoint, [1]),
    #             MOI.FeasiblePoint,
    #                 (MOI.SingleVariable, MOI.LessThan{Float64})    => -2.0,
    #                 (MOI.SingleVariable, MOI.GreaterThan{Float64}) =>  0.0
    #         )
    #     )
    #     MOIT.solve_with_lowerbound(mock, config)
    # end
    # @testset "solve_with_upperbound" begin
    #     MOIU.set_mock_optimize!(mock,
    #         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
    #             MOI.Success,
    #             (MOI.FeasiblePoint, [1]),
    #             MOI.FeasiblePoint,
    #                 (MOI.SingleVariable, MOI.GreaterThan{Float64}) => 2.0,
    #                 (MOI.SingleVariable, MOI.LessThan{Float64})    => 0.0
    #         )
    #     )
    #     MOIT.solve_with_upperbound(mock, config)
    # end
end
