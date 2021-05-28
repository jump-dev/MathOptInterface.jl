using Test
import MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.Test
const MOIU = MOI.Utilities

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.Config()

@testset "QP" begin
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [4 / 7, 3 / 7, 6 / 7],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [5 / 7, 6 / 7],
        ),
    )
    MOIT.qp1test(mock, config)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [4 / 7, 3 / 7, 6 / 7],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [5 / 7, 6 / 7],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [4 / 7, 3 / 7, 6 / 7],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [10 / 7, 12 / 7],
        ),
    )
    MOIT.qp2test(mock, config)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1 / 4, 3 / 4],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [11 / 4],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 0.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [-2.0],
        ),
    )
    MOIT.qp3test(mock, config)
end
@testset "QCP" begin
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1 / 2, 7 / 4],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                [zeros(2)],
            (MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1.0],
        ),
    )
    MOIT.qcp1test(mock, config)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [√2],
            (MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1 / (2 * √2)],
        ),
    )
    MOIT.qcp2test(mock, config)
    MOIT.qcp3test(mock, config)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 1.0],
            (MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1 / 3],
        ),
    )
    MOIT.qcp4test(mock, config)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 1.0],
            (MOI.ScalarQuadraticFunction{Float64}, MOI.GreaterThan{Float64}) => [1 / 3],
        ),
    )
    MOIT.qcp5test(mock, config)
end
@testset "Non-convex QCP" begin
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, [4.0, 1.0], MOI.FEASIBLE_POINT),
    )
    MOIT.ncqcp1test(mock, config)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, [2.0, 2.0], MOI.FEASIBLE_POINT),
    )
    MOIT.ncqcp2test(mock, config)
end
@testset "SOCP" begin
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1 / 2, 1 / 2, 1 / √2],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [1 / √2],
            (MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1 / √2],
        ),
    )
    MOIT.socp1test(mock, config)
end
