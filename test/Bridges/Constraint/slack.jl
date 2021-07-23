module TestConstraintSlack

using Test

using MathOptInterface
const MOI = MathOptInterface

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

include("../utilities.jl")

function test_scalar_slack()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.ScalarSlack{Float64}(mock)

    x = MOI.add_variable(bridged_mock)
    y = MOI.add_variable(bridged_mock)
    f = MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm{Float64}.([1.0, 2.0], [x, y]),
        0.0,
    )
    ci = MOI.add_constraint(bridged_mock, f, MOI.GreaterThan(0.0))
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) ≈ f
    newf = MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm{Float64}.([2.0, 1.0], [x, y]),
        0.0,
    )
    MOI.set(bridged_mock, MOI.ConstraintFunction(), ci, newf)
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) ≈ newf
    @test MOI.get(bridged_mock, MOI.ConstraintSet(), ci) == MOI.GreaterThan(0.0)
    MOI.set(bridged_mock, MOI.ConstraintSet(), ci, MOI.GreaterThan(1.0))
    @test MOI.get(bridged_mock, MOI.ConstraintSet(), ci) == MOI.GreaterThan(1.0)
    MOI.modify(bridged_mock, ci, MOI.ScalarConstantChange{Float64}(1.0))
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) ≈
          MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm{Float64}.([2.0, 1.0], [x, y]),
        1.0,
    )
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        MOI.set(bridged_mock, attr, ci, 2.0)
        @test MOI.get(bridged_mock, attr, ci) == 2.0
        bridge = MOI.Bridges.bridge(bridged_mock, ci)
        if attr isa MOI.ConstraintPrimalStart
            @test MOI.get(mock, MOI.VariablePrimalStart(), bridge.slack) == 2.0
            @test MOI.get(mock, attr, bridge.equality) == 0.0
        else
            @test MOI.get(mock, attr, bridge.equality) == 2.0
        end
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        (
            (MOI.SingleVariable, MOI.GreaterThan{Float64}, 0),
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}, 0),
        ),
    )
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_ScalarAffineFunction_GreaterThan",
            "test_basic_ScalarAffineFunction_LessThan",
            "test_basic_ScalarQuadraticFunction_GreaterThan",
            "test_basic_ScalarQuadraticFunction_LessThan",
        ],
    )
    MOI.empty!(bridged_mock)
    # There are extra variables due to the bridge
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1, 0, 1],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                    [MOI.NONBASIC],
            ],
            variable_basis_status = [
                MOI.BASIC,
                MOI.NONBASIC_AT_LOWER,
                MOI.NONBASIC_AT_UPPER,
            ],
        ),
    )
    config = MOI.Test.Config(
        exclude = Any[MOI.ConstraintDual, MOI.DualObjectiveValue],
    )
    MOI.Test.test_linear_integration_2(bridged_mock, config)
    c1 = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Float64},
            MOI.LessThan{Float64},
        }(),
    )
    @test length(c1) == 1
    @test MOI.get(bridged_mock, MOI.ConstraintBasisStatus(), c1[]) ==
          MOI.NONBASIC
    MOI.empty!(bridged_mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [1.0, 1.0, 2.0, 2.0]),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [0.5, 0.5, 1.0, 1.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [1, 0],
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [1],
            (MOI.SingleVariable, MOI.LessThan{Float64}) => [0],
        ),
    )
    MOI.Test.test_linear_transform(bridged_mock, config)
    c1 = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Float64},
            MOI.GreaterThan{Float64},
        }(),
    )
    @test length(c1) == 1
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), c1[]) ≈ 1.0
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), c1[]) ≈ 1.0
    c2 = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Float64},
            MOI.LessThan{Float64},
        }(),
    )
    @test length(c2) == 1
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), c2[]) ≈ 1.0
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), c2[]) ≈ 0.0
    loc = MOI.get(bridged_mock, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) in loc
    @test (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) in loc
    loc = MOI.get(mock, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 3
    @test (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) in loc
    @test (MOI.SingleVariable, MOI.LessThan{Float64}) in loc
    @test (MOI.SingleVariable, MOI.GreaterThan{Float64}) in loc
    for T in [Int, Float64], S in [MOI.GreaterThan{T}, MOI.GreaterThan{T}]
        for F in [MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}]
            @test MOI.Bridges.added_constraint_types(
                MOI.Bridges.Constraint.ScalarSlackBridge{T,F,S},
            ) == [(F, MOI.EqualTo{T})]
        end
    end
    return
end

function test_vector_slack()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.VectorSlack{Float64}(mock)

    x = MOI.add_variable(bridged_mock)
    y = MOI.add_variable(bridged_mock)
    f = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(1, MOI.ScalarAffineTerm.([1.0, 2.0], [x, y])),
        [0.0],
    )
    ci = MOI.add_constraint(bridged_mock, f, MOI.Nonpositives(1))
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) ≈ f
    newf = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(1, MOI.ScalarAffineTerm.([2.0, 1.0], [x, y])),
        [0.0],
    )
    MOI.set(bridged_mock, MOI.ConstraintFunction(), ci, newf)
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) ≈ newf
    @test MOI.get(bridged_mock, MOI.ConstraintSet(), ci) == MOI.Nonpositives(1)
    MOI.modify(bridged_mock, ci, MOI.VectorConstantChange([1.0]))
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) ≈
          MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(1, MOI.ScalarAffineTerm.([2.0, 1.0], [x, y])),
        [1.0],
    )
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        MOI.set(bridged_mock, attr, ci, [2.0])
        @test MOI.get(bridged_mock, attr, ci) == [2.0]
        bridge = MOI.Bridges.bridge(bridged_mock, ci)
        if attr isa MOI.ConstraintPrimalStart
            @test MOI.get(mock, MOI.VariablePrimalStart(), bridge.slack) ==
                  [2.0]
            @test MOI.get(mock, attr, bridge.equality) == [0.0]
        else
            @test MOI.get(mock, attr, bridge.equality) == [2.0]
        end
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        (
            (MOI.VectorOfVariables, MOI.Nonpositives, 0),
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros, 0),
        ),
    )

    fp = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(
            [1, 2, 3],
            MOI.ScalarAffineTerm.([1.0, 2.0, 3.0], [x, y, y]),
        ),
        [0.0, 0.0, 0.0],
    )
    cp = MOI.add_constraint(bridged_mock, fp, MOI.PowerCone(0.1))
    @test MOI.get(bridged_mock, MOI.ConstraintSet(), cp) == MOI.PowerCone(0.1)
    MOI.set(bridged_mock, MOI.ConstraintSet(), cp, MOI.PowerCone(0.2))
    @test MOI.get(bridged_mock, MOI.ConstraintSet(), cp) == MOI.PowerCone(0.2)
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_VectorAffineFunction_Nonnegatives",
            "test_basic_VectorAffineFunction_Nonpositives",
            "test_basic_VectorQuadraticFunction_Nonnegatives",
            "test_basic_VectorQuadraticFunction_Nonpositives",
        ],
    )
    MOI.empty!(bridged_mock)
    # There are extra variables due to the bridge
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [0, 0, 0, 0]),
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [100, 0, 100, 0]),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [100, -100, 100, -100],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) =>
                [[1.0], [1.0]],
            (MOI.VectorOfVariables, MOI.Nonnegatives) => [[1.0]],
            (MOI.VectorOfVariables, MOI.Nonpositives) => [[1.0]],
        ),
    )
    MOI.Test.test_linear_VectorAffineFunction(bridged_mock, config)
    c1 = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.Nonnegatives,
        }(),
    )
    @test length(c1) == 1
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), c1[]) ≈ [100.0]
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), c1[]) ≈ [1.0]
    c2 = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.Nonpositives,
        }(),
    )
    @test length(c2) == 1
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), c2[]) ≈ [-100.0]
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), c2[]) ≈ [1.0]
    loc = MOI.get(bridged_mock, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) in loc
    @test (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) in loc
    loc = MOI.get(mock, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 3
    @test (MOI.VectorAffineFunction{Float64}, MOI.Zeros) in loc
    @test (MOI.VectorOfVariables, MOI.Nonnegatives) in loc
    @test (MOI.VectorOfVariables, MOI.Nonpositives) in loc
    for T in [Int, Float64], S in [MOI.Nonnegatives, MOI.Nonpositives]
        for F in [MOI.VectorAffineFunction{T}, MOI.VectorQuadraticFunction{T}]
            @test MOI.Bridges.added_constraint_types(
                MOI.Bridges.Constraint.VectorSlackBridge{T,F,S},
            ) == [(F, MOI.Zeros)]
        end
    end
    return
end

end  # module

TestConstraintSlack.runtests()
