using Test
import MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.Test
const MOIU = MOI.Utilities

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.Config()

@testset "SOC" begin
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 1 / √2, 1 / √2],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[-√2]],
        )
    MOIT.soc1vtest(mock, config)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 1 / √2, 1 / √2],
            (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone) =>
                [[√2, -1, -1]],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[-√2]],
        )
    MOIT.soc1ftest(mock, config)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [-1 / √2, 1 / √2, 1.0],
            (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone) =>
                [[√2, 1, -1]],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[√2]],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                [[1.0]],
        )
    MOIT.soc2ntest(mock, config)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [-1 / √2, 1 / √2, 1.0],
            (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone) =>
                [[√2, 1, -1]],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[√2]],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) =>
                [[-1.0]],
        )
    MOIT.soc2ptest(mock, config)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            MOI.INFEASIBLE_POINT,
            MOI.INFEASIBILITY_CERTIFICATE,
        )
    MOIT.soc3test(mock, config)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 2 / √5, 1 / √5, 2 / √5, 1 / √5],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) =>
                [[-√5, -2.0, -1.0]],
        )
    MOIT.soc4test(mock, config)
end

@testset "RSOC" begin
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0.5, 1.0, 1 / √2, 1 / √2],
            (MOI.SingleVariable, MOI.EqualTo{Float64}) => [-√2, -1 / √2],
            (MOI.VectorOfVariables, MOI.RotatedSecondOrderCone) =>
                [[√2, 1 / √2, -1.0, -1.0]],
        )
    # double variable bounds on a and b variables
    mock.eval_variable_constraint_dual = false
    MOIT.rotatedsoc1vtest(mock, config)
    mock.eval_variable_constraint_dual = true
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1 / √2, 1 / √2],
            (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone) => [[√2, 1 / √2, -1.0, -1.0]],
        )
    MOIT.rotatedsoc1ftest(mock, config)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            tuple(),
            (MOI.SingleVariable, MOI.LessThan{Float64}) => [-1],
            (MOI.SingleVariable, MOI.EqualTo{Float64}) => [-1],
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [1],
            (MOI.VectorOfVariables, MOI.RotatedSecondOrderCone) =>
                [[1, 1, -1]],
        )
    # double variable bounds on x, y, z variables
    mock.eval_variable_constraint_dual = false
    MOIT.rotatedsoc2test(mock, config)
    mock.eval_variable_constraint_dual = true
    n = 2
    ub = 3.0
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0; zeros(n - 1); ub; √ub; ones(2)],
            (MOI.SingleVariable, MOI.EqualTo{Float64}) =>
                [-√ub / 4, -√ub / 4],
            (MOI.VectorOfVariables, MOI.Nonnegatives) => [zeros(n)],
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [0.0],
            (MOI.SingleVariable, MOI.LessThan{Float64}) => [-1 / (2 * √ub)],
            (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone) => [
#! format:off
# TODO(odow): formatting incorrectly modifies this line.
                [√ub / (2 * √2); √ub / (2 * √2); -√ub / 2; zeros(n - 1)],
#! format:on
                [√ub / √2, 1 / √(2 * ub), -1.0],
            ],
        )
    # double variable bounds on u
    mock.eval_variable_constraint_dual = false
    MOIT.rotatedsoc3test(mock, config)
    mock.eval_variable_constraint_dual = true

    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(4),
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1.0],
            (MOI.VectorOfVariables, MOI.RotatedSecondOrderCone) =>
                [[1.0, 1.0, -1.0, -1.0]],
        )
    MOIT.rotatedsoc4test(mock, config)
end

@testset "GeoMean" begin
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(4),
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-inv(3)],
        )
    MOIT.geomean1vtest(mock, config)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(4),
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-inv(3)],
            (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone) =>
                [vcat(-1.0, fill(inv(3), 3))],
        )
    MOIT.geomean1ftest(mock, config)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(10),
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                fill(-inv(9), 9),
        )
    MOIT.geomean2vtest(mock, config)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(10),
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                fill(-inv(9), 9),
            (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone) =>
                [vcat(-1.0, fill(inv(9), 9))],
        )
    MOIT.geomean2ftest(mock, config)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [2.0, 2.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-2.0],
        )
    MOIT.geomean3vtest(mock, config)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [2.0, 2.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-2.0],
            (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone) =>
                [[-2.0, 2.0]],
        )
    MOIT.geomean3ftest(mock, config)
end

@testset "PSD" begin
    # PSD0
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(3),
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [2],
        )
    MOIT.psdt0vtest(mock, config)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(3),
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [[1.0, -1.0, 1.0]],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [2],
        )
    MOIT.psdt0ftest(mock, config)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(4),
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [2],
        )
    MOIT.psds0vtest(mock, config)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(4),
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeSquare,
            ) => [[1.0, -2.0, 0.0, 1.0]],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [2],
        )
    MOIT.psds0ftest(mock, config)
    # PSD1
    δ = √(1 + (3 * √2 + 2) * √(-116 * √2 + 166) / 14) / 2
    ε = √((1 - 2 * (√2 - 1) * δ^2) / (2 - √2))
    y2 = 1 - ε * δ
    y1 = 1 - √2 * y2
    obj = y1 + y2 / 2
    k = -2 * δ / ε
    x2 = ((3 - 2obj) * (2 + k^2) - 4) / (4 * (2 + k^2) - 4 * √2)
    α = √(3 - 2obj - 4x2) / 2
    β = k * α
    Xv = [α^2, α * β, β^2, α^2, α * β, α^2]
    xv = [√2 * x2, x2, x2]
    cX0 = 1 + (√2 - 1) * y2
    cX1 = 1 - y2
    cX2 = -y2
    cXv = [cX0, cX1, cX0, cX2, cX1, cX0]
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [Xv; xv],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [y1, y2],
        )
    MOIT.psdt1vtest(mock, config)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [Xv; xv],
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [cXv],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [y1, y2],
        )
    MOIT.psdt1ftest(mock, config)
    Xv = [α^2, α * β, α^2, α * β, β^2, α * β, α^2, α * β, α^2]
    cXv = [cX0, cX1, cX2, cX1, cX0, cX1, cX2, cX1, cX0]
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [Xv; xv],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [y1, y2],
        )
    MOIT.psds1vtest(mock, config)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [Xv; xv],
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeSquare,
            ) => [cXv],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [y1, y2],
        )
    MOIT.psds1ftest(mock, config)
    # PSD2
    η = 10.0
    α = 0.8
    δ = 0.9
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [2η / 3, 0, η / 3, 0, 0, 0, η * δ * (1 - 1 / √3) / 2],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [δ * (1 - 1 / √3) / 2],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) => [[
                0,
                -α / √3 + δ / (2 * √6) * (2 * √2 - 1),
                0,
                -3δ * (1 - 1 / √3) / 8,
                -3δ * (1 - 1 / √3) / 8,
                -δ * (3 - 2 * √3 + 1 / √3) / 8,
            ]],
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [[(1 - 1 / √3) / 2, 1 / √6, (1 + 1 / √3) / 2]],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [0],
        )
    MOIT.psdt2test(mock, config)
    # PSD3
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(1),
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [[2, -1, 2, -1, -1, 2] / 6],
        )
    MOIT.psdt3test(mock, config)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(1),
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeSquare,
            ) => [[1, 0, 0, -1, 1, 0, -1, -1, 1] / 3],
        )
    MOIT.psds3test(mock, config)
end
