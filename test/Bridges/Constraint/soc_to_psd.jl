using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.TestConfig()

@testset "SOCtoPSD" begin
    bridged_mock = MOIB.Constraint.SOCtoPSD{Float64}(mock)
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1/√2, 1/√2],
                          (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle) => [[√2/2, -1/2, √2/4, -1/2, √2/4, √2/4]],
                          (MOI.VectorAffineFunction{Float64}, MOI.Zeros)                            => [[-√2]])
    MOIT.soc1vtest(bridged_mock, config)
    MOIT.soc1ftest(bridged_mock, config)
    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone}()))
    test_delete_bridge(bridged_mock, ci, 3, ((MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0),))
end

@testset "RSOCtoPSD" begin
    bridged_mock = MOIB.Constraint.RSOCtoPSD{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock, config,
        include = [(F, S)
                   for F in [MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64}]
                   for S in [MOI.RotatedSecondOrderCone]])

    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0.5, 1.0, 1/√2, 1/√2],
                          (MOI.SingleVariable,                MOI.EqualTo{Float64})       => [-√2, -1/√2],
                          (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle) => [[√2, -1/2, √2/8, -1/2, √2/8, √2/8]])
    MOIT.rotatedsoc1vtest(bridged_mock, config)
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/√2, 1/√2],
                          (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle) => [[√2, -1/2, √2/8, -1/2, √2/8, √2/8]])
    MOIT.rotatedsoc1ftest(bridged_mock, config)
    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone}()))
    test_delete_bridge(bridged_mock, ci, 2, ((MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0),))
end
