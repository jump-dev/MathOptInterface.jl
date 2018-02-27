@testset "Integer Linear" begin
    optimizer = MOIU.MockOptimizer(Model{Float64}())
    config = MOIT.TestConfig()
    optimizer.evalobjective = true

    optimizer.optimize! = (optimizer::MOIU.MockOptimizer) -> MOIU.mock_optimize!(optimizer, [4, 5, 1])
    MOIT.int1test(optimizer, config)
    # FIXME [1, 0...] is not the correct optimal solution but it passes the test
    optimizer.optimize! = (optimizer::MOIU.MockOptimizer) -> MOIU.mock_optimize!(optimizer, [1.0; zeros(10)])
    MOIT.int3test(optimizer, config)
    optimizer.optimize! = (optimizer::MOIU.MockOptimizer) -> MOIU.mock_optimize!(optimizer, [1, 0, 0, 1, 1])
    MOIT.knapsacktest(optimizer, config)
end
