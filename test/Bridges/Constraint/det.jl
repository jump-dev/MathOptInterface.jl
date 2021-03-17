using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.TestConfig()

@testset "LogDet" begin
    bridged_mock = MOIB.Constraint.LogDet{Float64}(mock)

    @testset "logdet1test" begin
        var_primal = [0, 1, 0, 1, 1, 0, 1, 0, 0, 1]
        exp_duals = [[-1, -1, 1], [-1, -1, 1]]
        psd_dual = [1, 0, 1, -1, 0, 1, 0, -1, 0, 1]
        mock.optimize! =
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                var_primal,
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1],
                (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                    [[1, 1]],
                (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone) =>
                    exp_duals,
                (
                    MOI.VectorAffineFunction{Float64},
                    MOI.PositiveSemidefiniteConeTriangle,
                ) => [psd_dual],
            )

        MOIT.logdett1vtest(bridged_mock, config)
        MOIT.logdett1ftest(bridged_mock, config)

        # set primal/dual start is not yet implemented for LogDet bridge
        ci = first(
            MOI.get(
                bridged_mock,
                MOI.ListOfConstraintIndices{
                    MOI.VectorAffineFunction{Float64},
                    MOI.LogDetConeTriangle,
                }(),
            ),
        )
        test_delete_bridge(
            bridged_mock,
            ci,
            5,
            (
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 0),
                (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone, 0),
                (
                    MOI.VectorAffineFunction{Float64},
                    MOI.PositiveSemidefiniteConeTriangle,
                    0,
                ),
            ),
        )
    end

    @testset "logdet2test" begin
        var_primal =
            [log(5), 3, 2, 2 / 3, 1, 1 / 3, 2.5, log(3), log(2 / 3), log(2.5)]
        exp_duals = [
            [-1, log(3) - 1, 1 / 3],
            [-1, log(2 / 3) - 1, 3 / 2],
            [-1, log(2.5) - 1, 0.4],
        ]
        psd_dual = [
            1,
            -1,
            1.6,
            0,
            -0.2,
            0.4,
            -1 / 3,
            0,
            0,
            1 / 3,
            1,
            -1.5,
            0,
            0,
            1.5,
            0,
            0.2,
            -0.4,
            0,
            0,
            0.4,
        ]
        mock.optimize! =
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                var_primal,
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1.0],
                (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone) =>
                    exp_duals,
                (
                    MOI.VectorAffineFunction{Float64},
                    MOI.PositiveSemidefiniteConeTriangle,
                ) => [psd_dual],
            )

        MOIT.logdett2test(bridged_mock, config)

        # set primal/dual start is not yet implemented for LogDet bridge
        ci = first(
            MOI.get(
                bridged_mock,
                MOI.ListOfConstraintIndices{
                    MOI.VectorAffineFunction{Float64},
                    MOI.LogDetConeTriangle,
                }(),
            ),
        )
        test_delete_bridge(
            bridged_mock,
            ci,
            1,
            (
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 0),
                (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone, 0),
                (
                    MOI.VectorAffineFunction{Float64},
                    MOI.PositiveSemidefiniteConeTriangle,
                    0,
                ),
            ),
        )
    end
end

@testset "RootDet" begin
    bridged_mock = MOIB.Constraint.RootDet{Float64}(mock)

    @testset "rootdet1test" begin
        var_primal = [1, 1, 0, 1, 1, 0, 1]
        geomean_dual = [-1, 0.5, 0.5]
        psd_dual = [0.5, 0, 0.5, -0.5, 0, 0.5, 0, -0.5, 0, 0.5]
        mock.optimize! =
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                var_primal,
                (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                    [[0.5, 0.5]],
                (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone) => [geomean_dual],
                (
                    MOI.VectorAffineFunction{Float64},
                    MOI.PositiveSemidefiniteConeTriangle,
                ) => [psd_dual],
            )

        MOIT.rootdett1vtest(bridged_mock, config)
        MOIT.rootdett1ftest(bridged_mock, config)

        # set primal/dual start is not yet implemented for RootDet bridge
        ci = first(
            MOI.get(
                bridged_mock,
                MOI.ListOfConstraintIndices{
                    MOI.VectorAffineFunction{Float64},
                    MOI.RootDetConeTriangle,
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
                    MOI.RotatedSecondOrderCone,
                    0,
                ),
                (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone, 0),
                (
                    MOI.VectorAffineFunction{Float64},
                    MOI.PositiveSemidefiniteConeTriangle,
                    0,
                ),
            ),
        )
    end

    @testset "rootdet2test" begin
        var_primal = [5^inv(3), 3, 2, 2 / 3, 1, 1 / 3, 2.5]
        multiplier = 5^inv(3) / 3
        geomean_dual = vcat(-1, multiplier * [inv(3), 1.5, 0.4])
        psd_dual =
            multiplier * [
                1,
                -1,
                1.6,
                0,
                -0.2,
                0.4,
                -1 / 3,
                0,
                0,
                1 / 3,
                1,
                -1.5,
                0,
                0,
                1.5,
                0,
                0.2,
                -0.4,
                0,
                0,
                0.4,
            ]
        mock.optimize! =
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                var_primal,
                (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone) => [geomean_dual],
                (
                    MOI.VectorAffineFunction{Float64},
                    MOI.PositiveSemidefiniteConeTriangle,
                ) => [psd_dual],
            )

        MOIT.rootdett2test(bridged_mock, config)

        # set primal/dual start is not yet implemented for RootDet bridge
        ci = first(
            MOI.get(
                bridged_mock,
                MOI.ListOfConstraintIndices{
                    MOI.VectorAffineFunction{Float64},
                    MOI.RootDetConeTriangle,
                }(),
            ),
        )
        test_delete_bridge(
            bridged_mock,
            ci,
            1,
            (
                (
                    MOI.VectorAffineFunction{Float64},
                    MOI.RotatedSecondOrderCone,
                    0,
                ),
                (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone, 0),
                (
                    MOI.VectorAffineFunction{Float64},
                    MOI.PositiveSemidefiniteConeTriangle,
                    0,
                ),
            ),
        )
    end
end
