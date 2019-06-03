@testset "GeoMean" begin
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [ones(4); 2; √2; √2])
    bridged_mock = MOIB.Constraint.GeoMean{Float64}(mock)
    MOIT.geomean1vtest(bridged_mock, config)
    MOIT.geomean1ftest(bridged_mock, config)
    # Dual is not yet implemented for GeoMean bridge
    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}()))
    test_delete_bridge(bridged_mock, ci, 4, ((MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0),
                                            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64},      1)))
end
