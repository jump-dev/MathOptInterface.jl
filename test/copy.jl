@testset "Copy test" begin
    @testset "Default copy" begin
        model = Model{Float64}()
        MOIT.failcopytestc(model)
        MOIT.failcopytestia(model)
        MOIT.failcopytestva(model)
        MOIT.failcopytestca(model)
        MOIT.copytest(model, Model{Float64}())
    end
    @testset "Allocate-Load copy" begin
        mock = MOIU.MockOptimizer(Model{Float64}(), needs_allocate_load=true)
        MOIT.failcopytestc(mock)
        MOIT.failcopytestia(mock)
        MOIT.failcopytestva(mock)
        MOIT.failcopytestca(mock)
        MOIT.copytest(mock, Model{Float64}())
    end
end
