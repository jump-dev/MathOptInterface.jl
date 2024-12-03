# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintScalarize

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
            (MOI.VariableIndex, MOI.GreaterThan{Float64}, 0),
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
            (MOI.VariableIndex, MOI.GreaterThan{Float64}, 0),
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

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ScalarizeBridge,
        """
        variables: x
        [2.0 * x + -1.0] in Nonnegatives(1)
        [3.0 * x + 1.0] in Nonpositives(1)
        [4.0 * x + -5.0] in Zeros(1)
        """,
        """
        variables: x
        2.0 * x >= 1.0
        3.0 * x <= -1.0
        4.0 * x == 5.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ScalarizeBridge,
        """
        variables: x
        VectorNonlinearFunction([2.0 * x - 1.0]) in Nonnegatives(1)
        VectorNonlinearFunction([3.0 * x + 1.0]) in Nonpositives(1)
        VectorNonlinearFunction([4.0 * x - 5.0]) in Zeros(1)
        """,
        """
        variables: x
        ScalarNonlinearFunction(2.0 * x - 1.0) >= 0.0
        ScalarNonlinearFunction(3.0 * x + 1.0) <= 0.0
        ScalarNonlinearFunction(4.0 * x - 5.0) == 0.0
        """,
    )
    return
end

function test_VectorNonlinearFunction_mixed_type()
    # We can't use the standard runtests because ScalarNonlinearFunction does
    # not preserve f(x) ≈ (f(x) - g(x)) + g(x)
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Constraint.Scalarize{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarNonlinearFunction(:log, Any[x])
    g = MOI.VectorNonlinearFunction(Any[1.0, x, 2.0*x-1.0, f])
    c = MOI.add_constraint(model, g, MOI.Nonnegatives(4))
    F, S = MOI.ScalarNonlinearFunction, MOI.GreaterThan{Float64}
    indices = MOI.get(inner, MOI.ListOfConstraintIndices{F,S}())
    @test length(indices) == 4
    inner_variables = MOI.get(inner, MOI.ListOfVariableIndices())
    @test length(inner_variables) == 1
    y = inner_variables[1]
    out = convert.(MOI.ScalarNonlinearFunction, Any[1.0, y, 2.0*y-1.0])
    push!(out, MOI.ScalarNonlinearFunction(:log, Any[y]))
    for (input, output) in zip(indices, out)
        @test ≈(MOI.get(inner, MOI.ConstraintFunction(), input), output)
    end
    new_g = MOI.VectorNonlinearFunction(Any[f, 2.0*x-1.0, 1.0, x])
    MOI.set(model, MOI.ConstraintFunction(), c, new_g)
    out = vcat(
        MOI.ScalarNonlinearFunction(:log, Any[y]),
        convert.(MOI.ScalarNonlinearFunction, Any[2.0*y-1.0, 1.0, y]),
    )
    for (input, output) in zip(indices, out)
        @test ≈(MOI.get(inner, MOI.ConstraintFunction(), input), output)
    end
    return
end

end  # module

TestConstraintScalarize.runtests()
