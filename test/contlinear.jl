@testset "Mock optimizer continuous linear tests" begin
    optimizer = MOIU.MockOptimizer(ModelForMock{Float64}())
    config = MOIT.TestConfig(solve=false)
    MOIT.contlineartest(optimizer, config)
end
