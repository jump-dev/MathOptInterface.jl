using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.DeprecatedTest
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.Config()
config_with_basis = MOIT.Config(basis = true)

@testset "Split" begin
    T = Float64
    bridged_mock = MOIB.Constraint.SplitInterval{T}(mock)
    MOIT.basic_constraint_tests(
        bridged_mock,
        config,
        include = [
            (MOI.SingleVariable, MOI.Interval{T}),
            (MOI.ScalarAffineFunction{T}, MOI.Interval{T}),
            (MOI.ScalarQuadraticFunction{T}, MOI.Interval{T}),
            (MOI.SingleVariable, MOI.EqualTo{T}),
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}),
            (MOI.ScalarQuadraticFunction{T}, MOI.EqualTo{T}),
            (MOI.VectorOfVariables, MOI.Zeros),
            (MOI.VectorAffineFunction{T}, MOI.Zeros),
            (MOI.VectorQuadraticFunction{T}, MOI.Zeros),
        ],
    )
end

@testset "Interval" begin
    bridged_mock = MOIB.Constraint.SplitInterval{Float64}(mock)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [5.0, 5.0],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.BASIC],
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [MOI.NONBASIC],
            ],
            variable_basis_status = [MOI.BASIC, MOI.BASIC],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [2.5, 2.5],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.NONBASIC],
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [MOI.BASIC],
            ],
            variable_basis_status = [MOI.BASIC, MOI.BASIC],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [1],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [0],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 1.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [1],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [0],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.NONBASIC],
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [MOI.BASIC],
            ],
            variable_basis_status = [MOI.BASIC, MOI.BASIC],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [6.0, 6.0],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.BASIC],
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [MOI.NONBASIC],
            ],
            variable_basis_status = [MOI.BASIC, MOI.BASIC],
        ),
    )
    MOIT.linear10test(bridged_mock, config_with_basis)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0.0, 0.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.BASIC],
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [MOI.BASIC],
            ],
            variable_basis_status = [
                MOI.NONBASIC_AT_LOWER,
                MOI.NONBASIC_AT_LOWER,
            ],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [0],
        ),
    )
    MOIT.linear10btest(bridged_mock, config_with_basis)

    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{Float64},
                MOI.Interval{Float64},
            }(),
        ),
    )
    vis = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
    newf =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, -1.0], vis), 0.0)
    MOI.set(bridged_mock, MOI.ConstraintFunction(), ci, newf)
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) ≈ newf

    MOI.modify(bridged_mock, ci, MOI.ScalarCoefficientChange(vis[2], 1.0))
    modified_f =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(ones(2), vis), 0.0)
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) ≈ modified_f

    @testset "$attr" for attr in [
        MOI.ConstraintPrimalStart(),
        MOI.ConstraintDualStart(),
    ]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        MOI.set(bridged_mock, attr, ci, 2.0)
        @test MOI.get(bridged_mock, attr, ci) == 2.0
        bridge = MOIB.bridge(bridged_mock, ci)
        if attr isa MOI.ConstraintPrimalStart
            @test MOI.get(bridged_mock, attr, bridge.lower) == 2.0
            @test MOI.get(bridged_mock, attr, bridge.upper) == 2.0
        else
            @test MOI.get(bridged_mock, attr, bridge.lower) == 2.0
            @test MOI.get(bridged_mock, attr, bridge.upper) == 0.0
        end
        MOI.set(bridged_mock, attr, ci, -2.0)
        @test MOI.get(bridged_mock, attr, ci) == -2.0
        if attr isa MOI.ConstraintPrimalStart
            @test MOI.get(bridged_mock, attr, bridge.lower) == -2.0
            @test MOI.get(bridged_mock, attr, bridge.upper) == -2.0
        else
            @test MOI.get(bridged_mock, attr, bridge.lower) == 0.0
            @test MOI.get(bridged_mock, attr, bridge.upper) == -2.0
        end
    end

    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        (
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 0),
        ),
    )
end
@testset "EqualTo{$T}" for T in [Float64, Int]
    bridged_mock = MOIB.Constraint.SplitInterval{T}(mock)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [one(T), one(T)],
            (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}) =>
                zeros(T, 2),
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => zeros(T, 1),
        ),
    )
    MOIT.linear13test(bridged_mock, MOIT.Config{T}())

    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{T},
                MOI.EqualTo{T},
            }(),
        ),
    )

    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        (
            (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}, 1),
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}, 0),
        ),
    )
end
@testset "Zeros" begin
    bridged_mock = MOIB.Constraint.SplitInterval{Float64}(mock)
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 0.0, 2.0],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                [[0.0, 0.0]],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) =>
                [[-3, -1]],
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
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives, 0),
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives, 0),
        ),
    )
end
