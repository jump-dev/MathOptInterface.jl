@testset "LogDet" begin
    bridged_mock = MOIB.LogDet{Float64}(mock)
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 1, 0, 1, 1, 0, 1, 0, 0, 1])
    MOIT.logdett1vtest(bridged_mock, config)
    MOIT.logdett1ftest(bridged_mock, config)
    # Dual is not yet implemented for LogDet bridge
    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.LogDetConeTriangle}()))
    test_delete_bridge(bridged_mock, ci, 5, ((MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone, 0), (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0)))
end

@testset "RootDet" begin
    bridged_mock = MOIB.RootDet{Float64}(mock)
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 1, 0, 1, 1, 0, 1])
    MOIT.rootdett1vtest(bridged_mock, config)
    MOIT.rootdett1ftest(bridged_mock, config)
    # Dual is not yet implemented for RootDet bridge
    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.RootDetConeTriangle}()))
    test_delete_bridge(bridged_mock, ci, 4, ((MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0),
                                            (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone, 0),
                                            (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0)))
end
