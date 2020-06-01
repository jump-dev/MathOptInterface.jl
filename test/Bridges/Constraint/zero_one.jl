using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges
const MOIBC = MathOptInterface.Bridges.Constraint

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.TestConfig()

@testset "ZeroOne" begin
    bridged_mock = MOIBC.ZeroOne{Float64}(mock)

    bridge_type = MOIBC.ZeroOneBridge{Float64}
    @test MOI.supports_constraint(bridge_type, MOI.SingleVariable, MOI.ZeroOne)
    @test MOIBC.concrete_bridge_type(bridge_type,
                                     MOI.SingleVariable,
                                     MOI.ZeroOne) == bridge_type

    @test MOI.supports(bridged_mock, MOI.ConstraintPrimalStart(), bridge_type)

    MOIT.basic_constraint_tests(bridged_mock, config, include = [(MOI.SingleVariable, MOI.ZeroOne)])
end
