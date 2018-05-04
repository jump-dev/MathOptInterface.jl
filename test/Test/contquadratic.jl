@testset "Continuous Quadratic" begin
    mock = MOIU.MockOptimizer(Model{Float64}())
    config = MOIT.TestConfig()

    @testset "QP" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [4/7, 3/7, 6/7]))
        MOIT.qp1test(mock, config)
        MOIT.qp2test(mock, config)
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/4, 3/4]),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 0.0]))
        MOIT.qp3test(mock, config)
    end
    @testset "QCP" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/2, 7/4], MOI.FeasiblePoint))
        MOIT.qcp1test(mock, config)
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [√2], MOI.FeasiblePoint))
        MOIT.qcp2test(mock, config)
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [√2], MOI.FeasiblePoint))
        MOIT.qcp3test(mock, config)
    end
    @testset "SOCP" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/2, 1/2, 1/√2]))
        MOIT.socp1test(mock, config)
    end
end
