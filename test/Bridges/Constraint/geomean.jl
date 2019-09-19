using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.TestConfig()

@testset "GeoMean" begin
    bridged_mock = MOIB.Constraint.GeoMean{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock, config,
        include = [(F, S)
                   for F in [MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64},
                             MOI.VectorQuadraticFunction{Float64}]
                   for S in [MOI.GeometricMeanCone]])

    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [ones(4); 2; √2; √2])
    MOIT.geomean1vtest(bridged_mock, config)
    MOIT.geomean1ftest(bridged_mock, config)
    # Dual is not yet implemented for GeoMean bridge
    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}()))
    test_delete_bridge(bridged_mock, ci, 4, ((MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0),
                                            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64},      1)))
end
