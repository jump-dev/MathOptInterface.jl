using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.DeprecatedTest
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.Config()

@testset "GreaterToLess" begin
    bridged_mock = MOIB.Constraint.GreaterToLess{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock,
        config,
        include = [
            (F, S) for F in [
                MOI.SingleVariable,
                MOI.ScalarAffineFunction{Float64},
                MOI.ScalarQuadraticFunction{Float64},
            ] for S in [MOI.GreaterThan{Float64}]
        ],
    )

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0.0, 0.0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100.0, 0.0]),
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, [100.0, -100.0]),
    )
    MOIT.linear6test(bridged_mock, config)

    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{Float64},
                MOI.GreaterThan{Float64},
            }(),
        ),
    )

    @testset "$attr" for attr in [
        MOI.ConstraintPrimalStart(),
        MOI.ConstraintDualStart(),
    ]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        MOI.set(bridged_mock, attr, ci, 2.0)
        @test MOI.get(bridged_mock, attr, ci) ≈ 2.0
    end

    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        ((MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 1),),
    )
end

@testset "LessToGreater" begin
    bridged_mock = MOIB.Constraint.LessToGreater{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock,
        config,
        include = [
            (F, S) for F in [
                MOI.SingleVariable,
                MOI.ScalarAffineFunction{Float64},
                MOI.ScalarQuadraticFunction{Float64},
            ] for S in [MOI.LessThan{Float64}]
        ],
    )

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [1.0],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [2.0]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [1.0],
        ),
    )
    MOIT.solve_set_scalaraffine_lessthan(bridged_mock, config)

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [1.0],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.5]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [0.5],
        ),
    )
    MOIT.solve_coef_scalaraffine_lessthan(bridged_mock, config)

    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{Float64},
                MOI.LessThan{Float64},
            }(),
        ),
    )

    @testset "$attr" for attr in [
        MOI.ConstraintPrimalStart(),
        MOI.ConstraintDualStart(),
    ]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        MOI.set(bridged_mock, attr, ci, 2.0)
        @test MOI.get(bridged_mock, attr, ci) ≈ 2.0
    end

    _test_delete_bridge(
        bridged_mock,
        ci,
        1,
        ((MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),),
    )
end

@testset "NonnegToNonpos" begin
    bridged_mock = MOIB.Constraint.NonnegToNonpos{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock,
        config,
        include = [
            (F, S) for F in [
                MOI.VectorOfVariables,
                MOI.VectorAffineFunction{Float64},
                MOI.VectorQuadraticFunction{Float64},
            ] for S in [MOI.NonnegativeCone]
        ],
    )

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0.0, 0.0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100.0, 0.0]),
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, [100.0, -100.0]),
    )
    MOIT.linear7test(bridged_mock, config)

    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.NonnegativeCone,
            }(),
        ),
    )
    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        ((MOI.VectorAffineFunction{Float64}, MOI.Nonpositives, 1),),
    )

    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 0.0, 2.0],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) =>
                [[0.0, -2.0, 0.0]],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) =>
                [[-3.0, -1.0]],
        )
    MOIT.lin1vtest(bridged_mock, config)
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorOfVariables,
                MOI.NonnegativeCone,
            }(),
        ),
    )
    func = MOI.get(bridged_mock, MOI.ConstraintFunction(), ci)
    MOI.delete(bridged_mock, func.variables[2])
    new_func = MOI.VectorOfVariables(func.variables[[1, 3]])
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) == new_func
    @test MOI.get(bridged_mock, MOI.ConstraintSet(), ci) == MOI.NonnegativeCone(2)

    @testset "$attr" for attr in [
        MOI.ConstraintPrimalStart(),
        MOI.ConstraintDualStart(),
    ]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        MOI.set(bridged_mock, attr, ci, [1.0, 2.0])
        @test MOI.get(bridged_mock, attr, ci) ≈ [1.0, 2.0]
    end

    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        ((MOI.VectorAffineFunction{Float64}, MOI.Nonpositives, 0),),
    )
end

@testset "NonposToNonneg" begin
    bridged_mock = MOIB.Constraint.NonposToNonneg{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock,
        config,
        include = [
            (F, S) for F in [
                MOI.VectorOfVariables,
                MOI.VectorAffineFunction{Float64},
                MOI.VectorQuadraticFunction{Float64},
            ] for S in [MOI.Nonpositives]
        ],
    )

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0.0, 0.0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100.0, 0.0]),
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, [100.0, -100.0]),
    )
    MOIT.linear7test(bridged_mock, config)

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.0, 0.0]),
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 0.75]),
        ),
    )
    MOIT.solve_const_vectoraffine_nonpos(bridged_mock, config)

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [0.5])),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.25]),
        ),
    )
    MOIT.solve_multirow_vectoraffine_nonpos(bridged_mock, config)

    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.Nonpositives,
            }(),
        ),
    )

    @testset "$attr" for attr in [
        MOI.ConstraintPrimalStart(),
        MOI.ConstraintDualStart(),
    ]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        MOI.set(bridged_mock, attr, ci, [1.0, 2.0])
        @test MOI.get(bridged_mock, attr, ci) ≈ [1.0, 2.0]
    end

    _test_delete_bridge(
        bridged_mock,
        ci,
        1,
        ((MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone, 0),),
    )
end
