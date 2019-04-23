@testset "Integer Linear" begin
    mock = MOIU.MockOptimizer(Model{Float64}())
    config = MOIT.TestConfig()

    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 20.0)
            MOIU.mock_optimize!(mock, [4, 5, 1])
        end)
    MOIT.int1test(mock, config)
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 1, 2]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 1, 2]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 3.0, 12.0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0.0, 0.0, 2.0, 2.0, 0.0, 2.0, 0.0, 0.0, 6.0, 24.0]))
    MOIT.int2test(mock, config)
    # FIXME [1, 0...] is not the correct optimal solution but it passes the test
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0; zeros(10)]))
    MOIT.int3test(mock, config)
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 0, 0, 1, 1]))
    MOIT.knapsacktest(mock, config)
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.25, 8.75, 0., 1.])
    )
    MOIT.indicator_test1(mock, config)
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2.0, 8.0, 1., 0.])
    )
    MOIT.indicator_test2(mock, config)
end
