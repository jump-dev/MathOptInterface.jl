@testset "Integer Conic" begin
    optimizer = MOIU.MockOptimizer(Model{Float64}())
    config = MOIT.TestConfig()
    optimizer.evalobjective = true

    @testset "SOC" begin
        optimizer.optimize! = (optimizer::MOIU.MockOptimizer) -> MOIU.mock_optimize!(optimizer, [1.0, 1.0, 0.0])
        MOIT.intsoc1test(optimizer, config)
    end
end
