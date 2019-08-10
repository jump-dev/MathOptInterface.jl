using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.TestConfig()

@testset "NormInfinity" begin
    bridged_mock = MOIB.Constraint.NormInfinity{Float64}(mock)
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 0.5, 1.0],
        (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[0.0, 1.0, 0.0, 0.0]],
        (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[-1], [-1]])
    MOIT.norminf1vtest(bridged_mock, config)
    MOIT.norminf1ftest(bridged_mock, config)
    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.NormInfinityCone}()))
    test_delete_bridge(bridged_mock, ci, 3, ((MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives, 0),))
end

@testset "NormOne" begin
    bridged_mock = MOIB.Constraint.NormOne{Float64}(mock)
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 0.5, 0.5, 0.5, 0.5],
        (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[1.0, 1.0, 0.0, 0.0]],
        (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [[1.0]],
        (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[-1], [0]])
    MOIT.normone1vtest(bridged_mock, config)
    MOIT.normone1ftest(bridged_mock, config)
    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.NormOneCone}()))
    test_delete_bridge(bridged_mock, ci, 3, (
        (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),
        (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives, 0)))
end
