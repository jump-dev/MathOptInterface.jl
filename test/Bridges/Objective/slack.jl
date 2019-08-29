using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.TestConfig()

bridged_mock = MOIB.Objective.Slack{Float64}(mock)

@testset "Set objective before sense" begin
    err = ErrorException(
        "Set `MOI.ObjectiveSense` before `MOI.ObjectiveFunction` when" *
        " using `MOI.Bridges.Objective.SlackBridge`."
    )
    F = MOI.ScalarAffineFunction{Float64}
    @test_throws err MOI.set(bridged_mock, MOI.ObjectiveFunction{F}(), zero(F))
end

@testset "solve_qp_edge_cases" begin
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 2.0])
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 2.0])
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 2.0])
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 2.0])
        )
    )
    MOIT.solve_qp_edge_cases(mock, config)
end
