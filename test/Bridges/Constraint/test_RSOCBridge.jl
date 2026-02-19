# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintRSOC

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

function test_RSOC()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.RSOC{Float64}(mock)
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_VectorOfVariables_RotatedSecondOrderCone",
            "test_basic_VectorAffineFunction_RotatedSecondOrderCone",
            "test_basic_VectorQuadraticFunction_RotatedSecondOrderCone",
        ],
    )
    MOI.empty!(bridged_mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [0.5, 1.0, 1 / √2, 1 / √2],
            (MOI.VariableIndex, MOI.EqualTo{Float64}) => [-√2, -1 / √2],
            (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone) =>
                [[3 / 2, 1 / 2, -1.0, -1.0]],
        )
    MOI.Test.test_conic_RotatedSecondOrderCone_VectorOfVariables(
        bridged_mock,
        config,
    )
    MOI.empty!(bridged_mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1 / √2, 1 / √2],
            (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone) =>
                [[3 / 2, 1 / 2, -1.0, -1.0]],
        )
    MOI.Test.test_conic_RotatedSecondOrderCone_VectorAffineFunction(
        bridged_mock,
        config,
    )
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.RotatedSecondOrderCone,
            }(),
        ),
    )
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value = [√2, 1 / √2, -1.0, -1.0]
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        ((MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone, 0),),
    )
    return
end

function test_SOCR()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.SOCR{Float64}(mock)
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_VectorOfVariables_SecondOrderCone",
            "test_basic_VectorAffineFunction_SecondOrderCone",
            "test_basic_VectorQuadraticFunction_SecondOrderCone",
        ],
    )
    MOI.empty!(bridged_mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1.0, 1 / √2, 1 / √2],
            (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone) => [[1 - 1 / √2, 1 + 1 / √2, -1]],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[-√2]],
        )
    MOI.Test.test_conic_SecondOrderCone_VectorOfVariables(bridged_mock, config)
    MOI.empty!(bridged_mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1.0, 1 / √2, 1 / √2],
            (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone) => [[1 - 1 / √2, 1 + 1 / √2, -1]],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[-√2]],
        )
    MOI.Test.test_conic_SecondOrderCone_VectorAffineFunction(
        bridged_mock,
        config,
    )
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.SecondOrderCone,
            }(),
        ),
    )
    @test !MOI.Utilities.is_canonical(
        MOI.get(bridged_mock, MOI.ConstraintFunction(), ci),
    )
    @test MOI.Utilities.is_canonical(
        MOI.get(bridged_mock, MOI.CanonicalConstraintFunction(), ci),
    )
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value = [√2, 1 / √2, -1.0, -1.0]
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        3,
        ((MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0),),
    )
    return
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SOCtoRSOCBridge,
        """
        variables: t, x, y
        [t, x, y] in SecondOrderCone(3)
        """,
        """
        variables: t, x, y
        [0.7071067811865475 * t + 0.7071067811865475 * x, 0.7071067811865475 * t + -0.7071067811865475 * x, y] in RotatedSecondOrderCone(3)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.RSOCtoSOCBridge,
        """
        variables: t, u, x
        [t, u, x] in RotatedSecondOrderCone(3)
        """,
        """
        variables: t, u, x
        [0.7071067811865475 * t + 0.7071067811865475 * u, 0.7071067811865475 * t + -0.7071067811865475 * u, x] in SecondOrderCone(3)
        """,
    )
    return
end

function test_dimension_mismatch_SOCR()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Constraint.SOCR{Float64}(inner)
    @test_throws(
        DimensionMismatch,
        MOI.add_constrained_variables(model, MOI.SecondOrderCone(1)),
    )
    return
end

function test_dimension_mismatch_RSOC()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Constraint.RSOC{Float64}(inner)
    @test_throws(
        DimensionMismatch,
        MOI.add_constrained_variables(model, MOI.RotatedSecondOrderCone(1)),
    )
    return
end

function test_map_function_SOC()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Constraint.SOCR{Float64}(inner)
    x = MOI.add_variables(model, 3)
    ci = MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.SecondOrderCone(3),
    )
    y = MOI.VectorNonlinearFunction([
        MOI.ScalarNonlinearFunction(:+, Any[x[i]]) for i in 1:3
    ])
    g = MOI.Bridges.map_function(model.map[ci], y)
    @test MOI.Utilities.eval_variables(xi -> abs(xi.value), model, g) ==
          [3 / sqrt(2), -1 / sqrt(2), 3]
    return
end

function test_map_function_RSOC()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Constraint.RSOC{Float64}(inner)
    x = MOI.add_variables(model, 3)
    ci = MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.RotatedSecondOrderCone(3),
    )
    y = MOI.VectorNonlinearFunction([
        MOI.ScalarNonlinearFunction(:+, Any[x[i]]) for i in 1:3
    ])
    g = MOI.Bridges.map_function(model.map[ci], y)
    @test MOI.Utilities.eval_variables(xi -> abs(xi.value), model, g) ==
          [3 / sqrt(2), -1 / sqrt(2), 3]
    return
end

end  # module

TestConstraintRSOC.runtests()
