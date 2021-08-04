module TestObjectiveFunctionize

using Test

using MathOptInterface
const MOI = MathOptInterface

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

include("../utilities.jl")

function test_solve_singlevariable_obj()
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    bridged_mock = MOI.Bridges.Objective.Functionize{Float64}(mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [1.0], MOI.FEASIBLE_POINT),
    )
    MOI.Test.test_objective_ObjectiveFunction_SingleVariable(
        bridged_mock,
        MOI.Test.Config(),
    )
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
    _test_delete_objective(bridged_mock, 1, tuple())
    return
end

function test_solve_result_index()
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    bridged_mock = MOI.Bridges.Objective.Functionize{Float64}(mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0]),
            MOI.FEASIBLE_POINT,
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [1.0],
        ),
    )
    return MOI.Test.test_solve_result_index(bridged_mock, MOI.Test.Config())
end

end  # module

TestObjectiveFunctionize.runtests()
