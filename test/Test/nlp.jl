using Test

import MathOptInterface
const MOI = MathOptInterface

@testset "mixed_complementarity" begin
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    )
    config = MOI.Test.TestConfig(optimal_status = MOI.LOCALLY_SOLVED)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock) -> MOI.Utilities.mock_optimize!(
            mock, config.optimal_status, [2.8, 0.0, 0.8, 1.2]
        )
    )
    MOI.Test.mixed_complementaritytest(mock, config)
end

@testset "math_program_complementarity_constraints" begin
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    )
    config = MOI.Test.TestConfig(optimal_status = MOI.LOCALLY_SOLVED)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock) -> MOI.Utilities.mock_optimize!(
            mock, config.optimal_status, [1.0, 0.0, 3.5, 0.0, 0.0, 0.0, 3.0, 6.0]
        )
    )
    MOI.Test.math_program_complementarity_constraintstest(mock, config)
end
