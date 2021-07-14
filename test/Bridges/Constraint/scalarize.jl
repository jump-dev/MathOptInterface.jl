using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.DeprecatedTest
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.Config()

@testset "Scalarize" begin
    bridged_mock = MOIB.Constraint.Scalarize{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock,
        config,
        include = [
            (F, S) for F in [
                MOI.VectorOfVariables,
                MOI.VectorAffineFunction{Float64},
                MOI.VectorQuadraticFunction{Float64},
            ] for S in [MOI.Nonnegatives, MOI.Nonpositives, MOI.Zeros]
        ],
    )

    # VectorOfVariables-in-Nonnegatives
    # VectorAffineFunction-in-Zeros
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 0.0, 2.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [-3, -1],
        )
    MOIT.lin1vtest(bridged_mock, config)
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.Zeros,
            }(),
        ),
    )
    _test_delete_bridge(
        bridged_mock,
        ci,
        3,
        (
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}, 0),
            (MOI.SingleVariable, MOI.GreaterThan{Float64}, 0),
        ),
    )
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorOfVariables,
                MOI.Nonnegatives,
            }(),
        ),
    )
    func = MOI.get(bridged_mock, MOI.ConstraintFunction(), ci)
    MOI.delete(bridged_mock, func.variables[2])
    new_func = MOI.VectorOfVariables(func.variables[[1, 3]])
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) == new_func
    @test MOI.get(bridged_mock, MOI.ConstraintSet(), ci) == MOI.Nonnegatives(2)

    @testset "$attr" for attr in [
        MOI.ConstraintPrimalStart(),
        MOI.ConstraintDualStart(),
    ]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        values = [1.0, 2.0]
        MOI.set(bridged_mock, attr, ci, values)
        @test MOI.get(bridged_mock, attr, ci) == values
    end

    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        (
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}, 0),
            (MOI.SingleVariable, MOI.GreaterThan{Float64}, 0),
        ),
    )

    # VectorAffineFunction-in-Nonnegatives
    # VectorAffineFunction-in-Zeros
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 0.0, 2.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [0, 2, 0],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [-3, -1],
        )
    MOIT.lin1ftest(bridged_mock, config)

    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.Zeros,
            }(),
        ),
    )

    @testset "$attr" for attr in [
        MOI.ConstraintPrimalStart(),
        MOI.ConstraintDualStart(),
    ]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        values = [1.0, -2.0]
        MOI.set(bridged_mock, attr, ci, values)
        @test MOI.get(bridged_mock, attr, ci) == values
    end

    _test_delete_bridge(
        bridged_mock,
        ci,
        3,
        (
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}, 0),
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),
        ),
    )

    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.Nonnegatives,
            }(),
        ),
    )

    @testset "$attr" for attr in [
        MOI.ConstraintPrimalStart(),
        MOI.ConstraintDualStart(),
    ]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        values = [1.0, -2.0, 3.0]
        MOI.set(bridged_mock, attr, ci, values)
        @test MOI.get(bridged_mock, attr, ci) == values
    end

    _test_delete_bridge(
        bridged_mock,
        ci,
        3,
        (
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}, 0),
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),
        ),
    )

    # Test setting VectorAffineFunction with nonzero constants
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0.0, 0.0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 0.75]),
    )
    MOIT.solve_func_vectoraffine_nonneg(bridged_mock, config)

    # VectorOfVariables-in-Nonnegatives
    # VectorOfVariables-in-Nonpositives
    # VectorOfVariables-in-Zeros
    # VectorAffineFunction-in-Zeros
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [-4, -3, 16, 0],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [7, 2, -4],
        )
    MOIT.lin2vtest(bridged_mock, config)

    # VectorAffineFunction-in-Nonnegatives
    # VectorAffineFunction-in-Nonpositives
    # VectorAffineFunction-in-Zeros
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [-4, -3, 16, 0],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [0],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [7, 2, -4, 7],
        )
    MOIT.lin2ftest(bridged_mock, config)

    @testset "constraint_ConstraintPrimalStart" begin
        MOI.Test.test_constraint_ConstraintPrimalStart(
            bridged_mock,
            MOI.Test.Config(),
        )
        MOI.Test.test_constraint_ConstraintDualStart(
            bridged_mock,
            MOI.Test.Config(),
        )
    end
end
