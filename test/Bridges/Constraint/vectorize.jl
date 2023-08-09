# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintVectorize

using Test

import MathOptInterface as MOI

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

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.VectorizeBridge,
        """
        variables: x
        x >= 2.0
        """,
        """
        variables: x
        [-2.0 + 1.0 * x] in Nonnegatives(1)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.VectorizeBridge,
        """
        variables: x
        x <= -2.0
        """,
        """
        variables: x
        [2.0 + 1.0 * x] in Nonpositives(1)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.VectorizeBridge,
        """
        variables: x
        x == 1.1
        """,
        """
        variables: x
        [-1.1 + 1.0 * x] in Zeros(1)
        """,
    )
    return
end

MOI.Utilities.@model(
    Model2179,
    (),
    (MOI.GreaterThan, MOI.LessThan),
    (),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (),
    ()
 )

function test_unsupported_ScalarNonlinearFunction()
    model = MOI.instantiate(Model2179{Float64}; with_bridge_type = Float64)
    MOI.supports_constraint(
        model,
        MOI.ScalarNonlinearFunction,
        MOI.GreaterThan{Float64},
    )
    return
end

function test_VectorNonlinearFunction()
    # We can't use the standard runtests because ScalarNonlinearFunction does
    # not preserve f(x) ≈ (f(x) - g(x)) + g(x)
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Constraint.Vectorize{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarNonlinearFunction(:log, Any[x])
    c = MOI.add_constraint(model, f, MOI.EqualTo(1.0))
    F, S = MOI.VectorNonlinearFunction, MOI.Zeros
    indices = MOI.get(inner, MOI.ListOfConstraintIndices{F,S}())
    @test length(indices) == 1
    inner_variables = MOI.get(inner, MOI.ListOfVariableIndices())
    @test length(inner_variables) == 1
    y = inner_variables[1]
    g = MOI.ScalarNonlinearFunction(
        :-,
        Any[MOI.ScalarNonlinearFunction(:log, Any[x]), 1.0],
    )
    @test ≈(
        MOI.get(inner, MOI.ConstraintFunction(), indices[1]),
        MOI.VectorNonlinearFunction(Any[g]),
    )
    return
end

end  # module

TestConstraintVectorize.runtests()
