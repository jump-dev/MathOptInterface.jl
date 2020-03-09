using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges
const MOIBC = MathOptInterface.Bridges.Constraint

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.TestConfig(duals = false)

@testset "RSOCtoNonConvexQuad" begin

    @test MOIBC.RSOCtoNonConvexQuadBridge{Float64} == MOIBC.concrete_bridge_type(
        MOIBC.RSOCtoNonConvexQuadBridge{Float64},
        MOI.VectorOfVariables,
        MOI.RotatedSecondOrderCone)
    @test MOI.supports_constraint(MOIBC.RSOCtoNonConvexQuadBridge{Float64},
                                  MOI.VectorOfVariables,
                                  MOI.RotatedSecondOrderCone)
    @test !MOI.supports_constraint(MOIBC.RSOCtoNonConvexQuadBridge{Float64},
                                   MOI.ScalarAffineFunction{Float64},
                                   MOI.RotatedSecondOrderCone)

    bridged_mock = MOIB.Constraint.RSOCtoNonConvexQuad{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock, config,
        include = [(F, S)
                   for F in [MOI.VectorOfVariables]
                   for S in [MOI.RotatedSecondOrderCone]])

    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0.5, 1.0, 1/√2, 1/√2])
    MOIT.rotatedsoc1vtest(bridged_mock, config)

    ci = first(MOI.get(bridged_mock,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,
                                    MOI.RotatedSecondOrderCone}()))

    test_delete_bridge(bridged_mock, ci, 4,
                    (
                        (MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}, 0),
                        (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),
                    ))
end

@testset "SOCtoNonConvexQuad" begin

    @test MOIBC.SOCtoNonConvexQuadBridge{Float64} == MOIBC.concrete_bridge_type(
        MOIBC.SOCtoNonConvexQuadBridge{Float64},
        MOI.VectorOfVariables,
        MOI.SecondOrderCone)
    @test MOI.supports_constraint(MOIBC.SOCtoNonConvexQuadBridge{Float64},
                                  MOI.VectorOfVariables,
                                  MOI.SecondOrderCone)
    @test !MOI.supports_constraint(MOIBC.SOCtoNonConvexQuadBridge{Float64},
                                   MOI.ScalarAffineFunction{Float64},
                                   MOI.SecondOrderCone)

    bridged_mock = MOIB.Constraint.SOCtoNonConvexQuad{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock, config,
        include = [(F, S)
                   for F in [MOI.VectorOfVariables]
                   for S in [MOI.SecondOrderCone]])

    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1/√2, 1/√2])
    MOIT.soc1vtest(bridged_mock, config)

    ci = first(MOI.get(bridged_mock,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,
                                    MOI.SecondOrderCone}()))

    test_delete_bridge(bridged_mock, ci, 3,
                       (
                           (MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}, 0),
                           (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),
                       ))
end
