# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintSOCtoPSD

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

function test_SOCtoPSD()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.SOCtoPSD{Float64}(mock)
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_VectorOfVariabless_SecondOrderCone",
            "test_basic_VectorAffineFunction_SecondOrderCone",
            "test_basic_VectorQuadraticFunction_SecondOrderCone",
        ],
    )
    MOI.empty!(bridged_mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1.0, 1 / √2, 1 / √2],
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [[√2 / 2, -1 / 2, √2 / 4, -1 / 2, √2 / 4, √2 / 4]],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[-√2]],
        )
    MOI.Test.test_conic_SecondOrderCone_VectorOfVariables(bridged_mock, config)
    MOI.empty!(bridged_mock)
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
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value = [√2, -1.0, -1.0]
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        3,
        ((
            MOI.VectorAffineFunction{Float64},
            MOI.PositiveSemidefiniteConeTriangle,
            0,
        ),),
    )
    return
end

function test_RSOCtoPSD()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.RSOCtoPSD{Float64}(mock)
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_VectorOfVariabless_RotatedSecondOrderCone",
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
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [[√2, -1 / 2, √2 / 8, -1 / 2, √2 / 8, √2 / 8]],
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
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [[√2, -1 / 2, √2 / 8, -1 / 2, √2 / 8, √2 / 8]],
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
        value = [√2, √2 / 4, -1.0, -1.0]
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        (
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
                0,
            ),
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives, 0),
        ),
    )
    return
end

function test_rsoc_to_psd_dimension_2()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Constraint.RSOCtoPSD{Float64}(inner)
    x = MOI.add_variables(model, 2)
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.RotatedSecondOrderCone(2),
    )
    @test MOI.get(
        inner,
        MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonnegatives}(),
    ) == 1
    return
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SOCtoPSDBridge,
        """
        variables: t, x, y
        [t, x, y] in SecondOrderCone(3)
        """,
        """
        variables: t, x, y
        [t, x, t, y, 0.0, t] in PositiveSemidefiniteConeTriangle(3)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.RSOCtoPSDBridge,
        """
        variables: t, u, x
        [t, u, x] in RotatedSecondOrderCone(3)
        """,
        """
        variables: t, u, x
        [t, x, 2u] in PositiveSemidefiniteConeTriangle(2)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.RSOCtoPSDBridge,
        """
        variables: t, u
        [t, u] in RotatedSecondOrderCone(2)
        """,
        """
        variables: t, u
        [t, u] in Nonnegatives(2)
        """,
    )
    return
end

function test_bridging_cost_SOCtoPSD()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Constraint.SOCtoPSD{Float64}(inner)
    x = MOI.add_variables(model, 3)
    c = MOI.add_constraint(model, x, MOI.SecondOrderCone(3))
    bridge = model.map[c]
    MOI.Bridges.bridging_cost(typeof(bridge)) == 10.0
    return
end

function test_bridging_cost_RSOCtoPSD()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Constraint.RSOCtoPSD{Float64}(inner)
    x = MOI.add_variables(model, 3)
    c = MOI.add_constraint(model, x, MOI.RotatedSecondOrderCone(3))
    bridge = model.map[c]
    MOI.Bridges.bridging_cost(typeof(bridge)) == 10.0
    return
end

end  # module

TestConstraintSOCtoPSD.runtests()
