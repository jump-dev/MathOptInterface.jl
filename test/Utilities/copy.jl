@testset "Copy test" begin
    @testset "AUTOMATIC copy" begin
        src = DummyModel()
        dest = DummyModel()
        @test_throws ErrorException MOIU.automatic_copy_to(dest, src)
        try
            @test_throws ErrorException MOIU.automatic_copy_to(dest, src)
        catch err
            @test sprint(showerror, err) == "Model DummyModel does not" *
            " support copy with names."
        end
    end
    @testset "Default copy" begin
        @test !MOIU.supports_default_copy_to(DummyModel(), false)
        @test !MOIU.supports_default_copy_to(DummyModel(), true)
        model = Model{Float64}()
        MOIT.failcopytestc(model)
        MOIT.failcopytestia(model)
        MOIT.failcopytestva(model)
        MOIT.failcopytestca(model)
        MOIT.copytest(model, Model{Float64}())
    end
    @testset "Allocate-Load copy" begin
        @test !MOIU.supports_allocate_load(DummyModel(), false)
        @test !MOIU.supports_allocate_load(DummyModel(), true)
        mock = MOIU.MockOptimizer(Model{Float64}(), needs_allocate_load=true)
        MOIT.failcopytestc(mock)
        MOIT.failcopytestia(mock)
        MOIT.failcopytestva(mock)
        MOIT.failcopytestca(mock)
        MOIT.copytest(mock, Model{Float64}())
    end
end
