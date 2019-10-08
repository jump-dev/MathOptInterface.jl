using Test

import MathOptInterface
const MOI = MathOptInterface

@testset "complementarity" begin
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    )
    config = MOI.Test.TestConfig()

    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock) -> MOI.Utilities.mock_optimize!(
            mock, MOI.LOCALLY_SOLVED, [2.8, 0.0, 0.8, 1.2]
        )
    )
    MOI.Test.test_linear_mcp(mock, config)
end
