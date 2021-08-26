module TestConstraintVectorize

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

function test_ScalarFunctionConstantNotZero()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.Vectorize{Float64}(mock)
    config = MOI.Test.Config()
    MOI.Test.test_model_ScalarFunctionConstantNotZero(bridged_mock, config)
    return
end

function test_basic()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.Vectorize{Float64}(mock)
    config = MOI.Test.Config()
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_$(F)_$(S)" for
            F in ["VariableIndex", "ScalarAffineFunction"] for
            S in ["EqualTo", "GreaterThan", "LessThan"]
        ],
    )
    return
end

function test_linear_integration()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.Vectorize{Float64}(mock)
    config = MOI.Test.Config(exclude = Any[MOI.ConstraintBasisStatus])
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1, 0],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) => [[-1]],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                [[0], [1]],
        ),
    )
    MOI.Test.test_linear_integration_2(bridged_mock, config)
    return
end

function test_linear_LessThan_and_GreaterThan()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.Vectorize{Float64}(mock)
    config = MOI.Test.Config()
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [0, 0]),
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [100, 0]),
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [100, -100]),
    )
    return MOI.Test.test_linear_LessThan_and_GreaterThan(bridged_mock, config)
end

function test_linear_integration_modification()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.Vectorize{Float64}(mock)
    config = MOI.Test.Config()
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [4 / 3, 4 / 3]),
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [2, 0]),
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [4, 0]),
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [2]),
    )
    MOI.Test.test_linear_integration_modification(bridged_mock, config)
    return
end

function test_linear_modify_GreaterThan_and_LessThan_constraints()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.Vectorize{Float64}(mock)
    config = MOI.Test.Config()
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [0, 0]),
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [100, 0]),
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [100, -100]),
    )
    MOI.Test.test_linear_modify_GreaterThan_and_LessThan_constraints(
        bridged_mock,
        config,
    )
    return
end

function test_linear_integration_delete_variables()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.Vectorize{Float64}(mock)
    config = MOI.Test.Config(exclude = Any[MOI.ConstraintBasisStatus])
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [0, 1 / 2, 1],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) =>
                [[-1], [-2]],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                [[2], [0], [0]],
        ),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) => [[-1]],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[0]],
        ),
    )
    # test_linear_integration_delete_variables has double variable bounds for
    # the z variable
    mock.eval_variable_constraint_dual = false
    MOI.Test.test_linear_integration_delete_variables(bridged_mock, config)
    return
end

function test_conic_PositiveSemidefiniteConeTriangle_VectorOfVariables()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.Vectorize{Float64}(mock)
    config = MOI.Test.Config()
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            ones(3),
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[2]],
        )
    MOI.Test.test_conic_PositiveSemidefiniteConeTriangle_VectorOfVariables(
        bridged_mock,
        config,
    )
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{Float64},
                MOI.EqualTo{Float64},
            }(),
        ),
    )
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        MOI.set(bridged_mock, attr, ci, 2.0)
        @test MOI.get(bridged_mock, attr, ci) == 2.0
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        3,
        ((MOI.VectorAffineFunction{Float64}, MOI.Zeros, 0),),
    )
    return
end

end  # module

TestConstraintVectorize.runtests()
