using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.TestConfig()

bridged_mock = MOIB.Objective.Functionize{Float64}(mock)

@testset "solve_singlevariable_obj" begin
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, [1.0], MOI.FEASIBLE_POINT),
    )
    MOIT.solve_singlevariable_obj(bridged_mock, config)
    @test MOI.get(mock, MOI.ObjectiveFunctionType()) ==
          MOI.ScalarAffineFunction{Float64}
    @test MOI.get(bridged_mock, MOI.ObjectiveFunctionType()) ==
          MOI.SingleVariable
    @test MOI.get(mock, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    @test MOI.get(bridged_mock, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    vis = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
    func = MOI.SingleVariable(vis[1])
    @test MOI.get(
        mock,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    ) ≈ convert(MOI.ScalarAffineFunction{Float64}, func)
    @test MOI.get(bridged_mock, MOI.ObjectiveFunction{MOI.SingleVariable}()) ==
          func
    MOI.set(bridged_mock, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(mock, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    @test MOI.get(bridged_mock, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    test_delete_objective(bridged_mock, 1, tuple())
end

# Tests that the `ObjectiveValue` attribute passed has the correct
# `result_index`.
@testset "solve_result_index" begin
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0]),
            MOI.FEASIBLE_POINT,
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [1.0],
        ),
    )
    MOIT.solve_result_index(bridged_mock, config)
end
