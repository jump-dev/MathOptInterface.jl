# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintNormSpectral

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

function test_NormSpectral()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.NormSpectral{Float64}(mock)
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_VectorOfVariables_NormSpectralCone",
            "test_basic_VectorAffineFunction_NormSpectralCone",
            "test_basic_VectorQuadraticFunction_NormSpectralCone",
        ],
    )
    MOI.empty!(bridged_mock)
    inv6 = inv(6)
    rt3 = sqrt(3)
    invrt12 = inv(2 * rt3)
    psd_dual = [
        inv6,
        -invrt12,
        inv6,
        invrt12,
        -invrt12,
        inv6,
        0,
        0,
        0,
        0,
        -invrt12,
        invrt12,
        -invrt12,
        0,
        0.5,
    ]
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [rt3],
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [psd_dual],
        )
    MOI.Test.test_conic_NormSpectralCone(bridged_mock, config)
    var_names = ["t"]
    MOI.set(
        bridged_mock,
        MOI.VariableName(),
        MOI.get(bridged_mock, MOI.ListOfVariableIndices()),
        var_names,
    )
    psd = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.PositiveSemidefiniteConeTriangle,
        }(),
    )
    @test length(psd) == 1
    MOI.set(mock, MOI.ConstraintName(), psd[1], "psd")

    s = """
    variables: t
    psd: [t, 0.0, t, 0.0, 0.0, t, 1.0, 1.0, 0.0, t, 1.0, -1.0, 1.0, 0.0, t] in PositiveSemidefiniteConeTriangle(5)
    minobjective: t
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(mock, model, var_names, ["psd"])
    spec = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.NormSpectralCone,
        }(),
    )
    @test length(spec) == 1
    MOI.set(bridged_mock, MOI.ConstraintName(), spec[1], "spec")

    s = """
    variables: t
    spec: [t, 1.0, 1.0, 1.0, -1.0, 0.0, 1.0] in NormSpectralCone(2, 3)
    minobjective: t
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(bridged_mock, model, var_names, ["spec"])
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.NormSpectralCone,
            }(),
        ),
    )
    attr = MOI.ConstraintPrimalStart()
    @test MOI.supports(bridged_mock, attr, typeof(ci))
    value = Float64[4, 1, 1, 1, -1, 0, 1]
    MOI.set(bridged_mock, attr, ci, value)
    @test MOI.get(bridged_mock, attr, ci) ≈ value
    @test MOI.get(mock, attr, psd[1]) ==
          Float64[4, 0, 4, 0, 0, 4, 1, 1, 0, 4, 1, -1, 1, 0, 4]
    _test_delete_bridge(
        bridged_mock,
        ci,
        1,
        ((
            MOI.VectorAffineFunction{Float64},
            MOI.PositiveSemidefiniteConeTriangle,
            0,
        ),),
    )
    return
end

function test_NormNuclear()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.NormNuclear{Float64}(mock)
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_VectorOfVariables_NormNuclearCone",
            "test_basic_VectorAffineFunction_NormNuclearCone",
            "test_basic_VectorQuadraticFunction_NormNuclearCone",
        ],
    )
    MOI.empty!(bridged_mock)
    rt2 = sqrt(2)
    rt3 = sqrt(3)
    invrt2 = inv(rt2)
    invrt3 = inv(rt3)
    var_primal = [
        rt2 + rt3,
        invrt2 + invrt3,
        invrt2 - invrt3,
        invrt2 + invrt3,
        invrt3,
        -invrt3,
        invrt3,
        rt2,
        0,
        rt3,
    ]
    invrt8 = inv(sqrt(8))
    invrt12 = inv(sqrt(12))
    psd_dual = [
        0.5,
        0,
        0.5,
        0,
        0,
        0.5,
        -invrt8,
        -invrt8,
        0,
        invrt8,
        -invrt12,
        invrt12,
        -invrt12,
        0,
        0.5,
    ]
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            var_primal,
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [1.0],
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [psd_dual],
        )
    MOI.Test.test_conic_NormNuclearCone(bridged_mock, config)
    greater = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Float64},
            MOI.GreaterThan{Float64},
        }(),
    )
    psd = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.PositiveSemidefiniteConeTriangle,
        }(),
    )
    var_names =
        ["t", "U11", "U12", "U22", "U31", "U32", "U33", "V11", "V12", "V22"]
    MOI.set(
        mock,
        MOI.VariableName(),
        MOI.get(mock, MOI.ListOfVariableIndices()),
        var_names,
    )
    @test length(greater) == 1
    MOI.set(mock, MOI.ConstraintName(), greater[1], "greater")
    @test length(psd) == 1
    MOI.set(mock, MOI.ConstraintName(), psd[1], "psd")

    s = """
    variables: t, U11, U12, U22, U31, U32, U33, V11, V12, V22
    greater: t + -0.5U11 + -0.5U22 + -0.5U33 + -0.5V11 + -0.5V22 >= 0.0
    psd: [U11, U12, U22, U31, U32, U33, 1.0, 1.0, 0.0, V11, 1.0, -1.0, 1.0, V12, V22] in PositiveSemidefiniteConeTriangle(5)
    minobjective: t
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(mock, model, var_names, ["greater", "psd"])
    MOI.set(
        bridged_mock,
        MOI.VariableName(),
        MOI.get(bridged_mock, MOI.ListOfVariableIndices()),
        ["t"],
    )
    nuc = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.NormNuclearCone,
        }(),
    )
    @test length(nuc) == 1
    MOI.set(bridged_mock, MOI.ConstraintName(), nuc[1], "nuc")

    s = """
    variables: t
    nuc: [t, 1.0, 1.0, 1.0, -1.0, 0.0, 1.0] in NormNuclearCone(2, 3)
    minobjective: t
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(bridged_mock, model, ["t"], ["nuc"])
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.NormNuclearCone,
            }(),
        ),
    )
    attr = MOI.ConstraintDualStart()
    @test MOI.supports(bridged_mock, attr, typeof(ci))
    value = Float64[4, 1, 1, 1, -1, 0, 1]
    MOI.set(bridged_mock, attr, ci, value)
    @test MOI.get(bridged_mock, attr, ci) ≈ value
    @test MOI.get(mock, attr, greater[1]) == 4
    @test MOI.get(mock, attr, psd[1]) ==
          Float64[4, 0, 4, 0, 0, 4, 0.5, 0.5, 0, 4, 0.5, -0.5, 0.5, 0, 4]
    _test_delete_bridge(
        bridged_mock,
        ci,
        1,
        (
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
                0,
            ),
        ),
    )
    return
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.NormSpectralBridge,
        """
        variables: t, x11, x12, x21, x22
        [t, x11, x21, x12, x22] in NormSpectralCone(2, 2)
        """,
        """
        variables: t, x11, x12, x21, x22
        [t, 0, t, x11, x12, t, x21, x22, 0, t] in PositiveSemidefiniteConeTriangle(4)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.NormNuclearBridge,
        """
        variables: t, x11, x12, x21, x22
        [t, x11, x21, x12, x22] in NormNuclearCone(2, 2)
        """,
        """
        variables: t, x11, x12, x21, x22, u1, u2, u3, v1, v2, v3
        t + -0.5 * u1 + -0.5 * u3 + -0.5 * v1 + -0.5 * v3 >= 0.0
        [u1, u2, u3, x11, x12, v1, x21, x22, v2, v3] in PositiveSemidefiniteConeTriangle(4)
        """,
    )
    return
end

end  # module

TestConstraintNormSpectral.runtests()
