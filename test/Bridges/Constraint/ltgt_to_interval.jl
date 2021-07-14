# These tests are mostly copies of the flip_sign.jl tests for GreaterToLess

using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.DeprecatedTest
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.Config()

@testset "GreaterToInterval" begin
    bridged_mock = MOIB.Constraint.GreaterToInterval{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock,
        config,
        include = [
            (F, S) for F in [
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
        ((MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}, 0),),
    )
end

@testset "LessToInterval" begin
    bridged_mock = MOIB.Constraint.LessToInterval{Float64}(mock)

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
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [-1.0],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [2.0]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [-1.0],
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
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [-1.0],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.5]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [-0.5],
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
        ((MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}, 0),),
    )
end

# Define a dummy optimizer that only supports intervals
# and use it in the below unmocked test
mutable struct Optimizer <: MOI.AbstractOptimizer
    function Optimizer()
        return new()
    end
end

MOI.get(model::Optimizer, ::MOI.SolverName) = "OnlyIntervalOptimizer"

function MOI.supports_constraint(
    ::Optimizer,
    ::Type{MOI.ScalarAffineFunction{Float64}},
    ::Type{MOI.Interval{Float64}},
)
    return true
end

@testset "GreaterOrLessToInterval_unmocked" begin
    # model supports Interval but not LessThan or GreaterThan
    model = Optimizer()
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.Interval{Float64},
    )
    @test !MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    @test !MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.GreaterThan{Float64},
    )

    # bridged model supports all
    bridged = MOIB.Constraint.GreaterToInterval{Float64}(
        MOIB.Constraint.LessToInterval{Float64}(model),
    )
    @test MOI.supports_constraint(
        bridged,
        MOI.ScalarAffineFunction{Float64},
        MOI.Interval{Float64},
    )
    @test MOI.supports_constraint(
        bridged,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    @test MOI.supports_constraint(
        bridged,
        MOI.ScalarAffineFunction{Float64},
        MOI.GreaterThan{Float64},
    )

    # bridged model with Bridges.full_bridge_optimizer
    bridged2 = MOIB.full_bridge_optimizer(model, Float64)
    @test MOI.supports_constraint(
        bridged2,
        MOI.ScalarAffineFunction{Float64},
        MOI.Interval{Float64},
    )
    @test MOI.supports_constraint(
        bridged2,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    @test MOI.supports_constraint(
        bridged2,
        MOI.ScalarAffineFunction{Float64},
        MOI.GreaterThan{Float64},
    )
end
