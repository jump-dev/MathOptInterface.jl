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
    bridged_mock = MOIB.Constraint.SOCR{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock, config,
        include = [(F, S)
                   for F in [MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64},
                             MOI.VectorQuadraticFunction{Float64}]
                   for S in [MOI.SecondOrderCone]])

    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1/√2, 1/√2],
                          (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone) => [[1 - 1/√2, 1 + 1/√2, -1]],
                          (MOI.VectorAffineFunction{Float64}, MOI.Zeros)           => [[-√2]])
    MOIT.soc1vtest(bridged_mock, config)
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1/√2, 1/√2],
                          (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone) => [[1 - 1/√2, 1 + 1/√2, -1]],
                          (MOI.VectorAffineFunction{Float64}, MOI.Zeros)           => [[-√2]])
    MOIT.soc1ftest(bridged_mock, config)
    ci = first(MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64},
                                    MOI.SecondOrderCone}()))
    test_delete_bridge(bridged_mock, ci, 3,
                       ((MOI.VectorAffineFunction{Float64},
                         MOI.RotatedSecondOrderCone, 0),))
end
