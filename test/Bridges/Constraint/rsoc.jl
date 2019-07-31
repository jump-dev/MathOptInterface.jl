using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.TestConfig()

@testset "RSOC" begin
    bridged_mock = MOIB.Constraint.RSOC{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock, config,
        include = [(F, S)
                   for F in [MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64},
                             MOI.VectorQuadraticFunction{Float64}]
                   for S in [MOI.RotatedSecondOrderCone]])

    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0.5, 1.0, 1/√2, 1/√2],
                          (MOI.SingleVariable,                MOI.EqualTo{Float64}) => [-√2, -1/√2],
                          (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone)  => [[3/2, 1/2, -1.0, -1.0]])
    MOIT.rotatedsoc1vtest(bridged_mock, config)
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/√2, 1/√2],
                          (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone)  => [[3/2, 1/2, -1.0, -1.0]])
    MOIT.rotatedsoc1ftest(bridged_mock, config)
    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone}()))
    test_delete_bridge(bridged_mock, ci, 2, ((MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone, 0),))
end
