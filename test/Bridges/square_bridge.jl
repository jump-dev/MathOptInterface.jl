@testset "SquarePSD" begin
    bridged_mock = MOIB.SquarePSD{Float64}(mock)
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, ones(4),
                          (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})                 => [2, 2])
    MOIT.psds0vtest(bridged_mock, config)
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, ones(4),
                          (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle) => [[1, -1, 1]],
                          (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})                 => [2, 2])
    MOIT.psds0ftest(bridged_mock, config)
    ci = first(MOI.get(bridged_mock,
                       MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64},
                                                   MOI.PositiveSemidefiniteConeSquare}()))
    test_delete_bridge(bridged_mock, ci, 4,
                       ((MOI.VectorAffineFunction{Float64},
                         MOI.PositiveSemidefiniteConeTriangle, 0),
                        (MOI.ScalarAffineFunction{Float64},
                         MOI.EqualTo{Float64}, 1)))
end
