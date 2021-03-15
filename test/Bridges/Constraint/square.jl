using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.TestConfig()

@testset "Square" begin
    bridged_mock = MOIB.Constraint.Square{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock,
        config,
        include = [
            (F, S) for F in [
                MOI.VectorOfVariables,
                MOI.VectorAffineFunction{Float64},
                MOI.VectorQuadraticFunction{Float64},
            ] for S in [MOI.PositiveSemidefiniteConeSquare]
        ],
    )

    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(4),
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [2, 2],
        )
    MOIT.psds0vtest(bridged_mock, config)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(4),
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [[1, -1, 1]],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [2, 2],
        )
    MOIT.psds0ftest(bridged_mock, config)
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeSquare,
            }(),
        ),
    )
    test_delete_bridge(
        bridged_mock,
        ci,
        4,
        (
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
                0,
            ),
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}, 1),
        ),
    )
end
