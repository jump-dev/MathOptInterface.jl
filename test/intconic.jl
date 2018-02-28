@testset "Integer Conic" begin
    mock = MOIU.MockOptimizer(Model{Float64}())
    config = MOIT.TestConfig()
    mock.evalobjective = true

    @testset "SOC" begin
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1.0, 0.0])
        MOIT.intsoc1test(mock, config)
    end
end
