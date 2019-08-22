using Test
import MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.Test
const MOIU = MOI.Utilities

include("../dummy.jl")

remove_moi(x::String) = replace(x, "MathOptInterface." => "")
function compare_without_moi(x::String, y::String)
    @test remove_moi(x) == remove_moi(y)
end

@testset "IndexMap" begin
    map = MOIU.IndexMap()
    @test length(map) == 0
    compare_without_moi(sprint(show, map), "Utilities.IndexMap()")
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    cx = MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integer}(1)
    cy = MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integer}(2)
    map = MOIU.IndexMap(Dict(x => y), Dict(cx => cy))
    @test length(map) == 2
	compare_without_moi(sprint(show, map), "Utilities.IndexMap($x=>$y,Pair{ConstraintIndex,ConstraintIndex}($cx, $cy))")
end

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
