using Test
import MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.DeprecatedTest
const MOU = MOI.Utilities

@testset "Start value attributes and nothing" begin
    model = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
    MOI.DeprecatedTest.start_values_unset_test(
        model,
        MOI.DeprecatedTest.Config(),
    )
end

@testset "Failed copy" begin
    include("../dummy.jl")
    model = DummyModelWithAdd()
    MOIT.failcopytestc(model)
    MOIT.failcopytestia(model)
    MOIT.failcopytestva(model)
    MOIT.failcopytestca(model)
end
