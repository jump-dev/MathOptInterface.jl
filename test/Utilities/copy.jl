using Test
import MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.Test
const MOIU = MOI.Utilities

include("../dummy.jl")

@testset "AUTOMATIC" begin
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
@testset "Default" begin
    @test !MOIU.supports_default_copy_to(DummyModel(), false)
    @test !MOIU.supports_default_copy_to(DummyModel(), true)
    model = MOIU.Model{Float64}()
    MOIT.failcopytestc(model)
    MOIT.failcopytestia(model)
    MOIT.failcopytestva(model)
    MOIT.failcopytestca(model)
    MOIT.copytest(model, MOIU.Model{Float64}())
end
@testset "Allocate-Load" begin
    @test !MOIU.supports_allocate_load(DummyModel(), false)
    @test !MOIU.supports_allocate_load(DummyModel(), true)
    mock = MOIU.MockOptimizer(MOIU.Model{Float64}(), needs_allocate_load=true)
    MOIT.failcopytestc(mock)
    MOIT.failcopytestia(mock)
    MOIT.failcopytestva(mock)
    MOIT.failcopytestca(mock)
    MOIT.copytest(mock, MOIU.Model{Float64}())
end
