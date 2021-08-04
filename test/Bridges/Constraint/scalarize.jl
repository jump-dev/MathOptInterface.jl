module TestConstraintScalarize

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

function test_scalarize()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.Scalarize{Float64}(mock)
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_$(F)_$(S)" for F in [
                "VectorOfVariables",
                "VectorAffineFunction",
                "VectorQuadraticFunction",
            ] for S in ["Nonnegatives", "Nonpositives", "Zeros"]
        ],
    )
    MOI.empty!(bridged_mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1.0, 0.0, 2.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [-3, -1],
        )
    MOI.Test.test_conic_linear_VectorOfVariables(bridged_mock, config)
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
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
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
    MOI.empty!(bridged_mock)
    # VectorAffineFunction-in-Nonnegatives
    # VectorAffineFunction-in-Zeros
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1.0, 0.0, 2.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [0, 2, 0],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [-3, -1],
        )
    MOI.Test.test_conic_linear_VectorAffineFunction(bridged_mock, config)
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.Zeros,
            }(),
        ),
    )
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
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
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
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
    MOI.empty!(bridged_mock)
    # Test setting VectorAffineFunction with nonzero constants
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [0.0, 0.0]),
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [1.0, 0.75]),
    )
    MOI.Test.test_modification_func_vectoraffine_nonneg(bridged_mock, config)
    MOI.empty!(bridged_mock)
    # VectorOfVariables-in-Nonnegatives
    # VectorOfVariables-in-Nonpositives
    # VectorOfVariables-in-Zeros
    # VectorAffineFunction-in-Zeros
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [-4, -3, 16, 0],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [7, 2, -4],
        )
    MOI.Test.test_conic_linear_VectorOfVariables_2(bridged_mock, config)
    MOI.empty!(bridged_mock)
    # VectorAffineFunction-in-Nonnegatives
    # VectorAffineFunction-in-Nonpositives
    # VectorAffineFunction-in-Zeros
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [-4, -3, 16, 0],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [0],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [7, 2, -4, 7],
        )
    MOI.Test.test_conic_linear_VectorAffineFunction_2(bridged_mock, config)
    MOI.Test.test_constraint_ConstraintPrimalStart(
        bridged_mock,
        MOI.Test.Config(),
    )
    MOI.Test.test_constraint_ConstraintDualStart(
        bridged_mock,
        MOI.Test.Config(),
    )
    return
end

end  # module

TestConstraintScalarize.runtests()
