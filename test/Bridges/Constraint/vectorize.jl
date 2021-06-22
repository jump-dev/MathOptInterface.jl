using Test

import MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include(joinpath(dirname(@__DIR__), "utilities.jl"))

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.Config()

@testset "Vectorize" begin
    bridged_mock = MOI.Bridges.Constraint.Vectorize{Float64}(mock)

    MOIT.scalar_function_constant_not_zero(bridged_mock)

    MOIT.basic_constraint_tests(
        bridged_mock,
        config,
        include = Iterators.product(
            [
                MOI.SingleVariable,
                MOI.ScalarAffineFunction{Float64},
                # TODO: add when operate(vcat, ...) is implemented for quadratic
                # MOI.ScalarQuadraticFunction{Float64},
            ],
            [
                MOI.EqualTo{Float64},
                MOI.GreaterThan{Float64},
                MOI.LessThan{Float64},
            ],
        ),
    )

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1, 0],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) => [[-1]],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                [[0], [1]],
        ),
    )
    MOIT.test_linear_integration_2(bridged_mock, config)

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, -100]),
    )
    MOIT.test_linear_LessThan_and_GreaterThan(bridged_mock, config)

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [4 / 3, 4 / 3]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [4, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2]),
    )
    MOIT.test_linear_integration_modification(mock, config)

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, -100]),
    )
    MOIT.test_linear_modify_GreaterThan_and_LessThan_constraints(mock, config)

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0, 1 / 2, 1],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) =>
                [[-1], [-2]],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                [[2], [0], [0]],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) => [[-1]],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[0]],
        ),
    )
    # linear14 has double variable bounds for the z variable
    mock.eval_variable_constraint_dual = false
    MOIT.test_linear_integration_delete_variables(bridged_mock, config)
    mock.eval_variable_constraint_dual = true

    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(3),
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[2]],
        )
    MOIT.psdt0vtest(bridged_mock, config)

    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{Float64},
                MOI.EqualTo{Float64},
            }(),
        ),
    )

    @testset "$attr" for attr in [
        MOI.ConstraintPrimalStart(),
        MOI.ConstraintDualStart(),
    ]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        MOI.set(bridged_mock, attr, ci, 2.0)
        @test MOI.get(bridged_mock, attr, ci) == 2.0
    end

    test_delete_bridge(
        bridged_mock,
        ci,
        3,
        ((MOI.VectorAffineFunction{Float64}, MOI.Zeros, 0),),
    )
end
