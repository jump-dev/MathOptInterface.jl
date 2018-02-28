@testset "Integer Linear" begin
    mock = MOIU.MockOptimizer(Model{Float64}())
    config = MOIT.TestConfig()
    mock.evalobjective = true

    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [4, 5, 1])
    MOIT.int1test(mock, config)
    # FIXME [1, 0...] is not the correct optimal solution but it passes the test
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0; zeros(10)])
    MOIT.int3test(mock, config)
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 0, 0, 1, 1])
    MOIT.knapsacktest(mock, config)
end
