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
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 0.5, 1.0])
    bridged_mock = MOIB.Constraint.NormInfinity{Float64}(mock)
    MOIT.norminf1vtest(bridged_mock, config)
    MOIT.norminf1ftest(bridged_mock, config)
    # Dual is not yet implemented for NormInfinity bridge
    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.NormInfinityCone}()))
    test_delete_bridge(bridged_mock, ci, 3, ((MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives, 0),))
end

@testset "NormOne" begin
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 0.5, 0.5, 0.5, 0.5])
    bridged_mock = MOIB.Constraint.NormOne{Float64}(mock)
    MOIT.normone1vtest(bridged_mock, config)
    MOIT.normone1ftest(bridged_mock, config)
    # Dual is not yet implemented for NormOne bridge
    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.NormOneCone}()))
    test_delete_bridge(bridged_mock, ci, 3, ((MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}, 2), (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives, 0)))
end
