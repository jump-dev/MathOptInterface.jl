# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintNormInfinity

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

function test_NormInfinity()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.NormInfinity{Float64}(mock)
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_VectorOfVariables_NormInfinityCone",
            "test_basic_VectorAffineFunction_NormInfinityCone",
            "test_basic_VectorQuadraticFunction_NormInfinityCone",
            "test_basic_VectorNonlinearFunction_NormInfinityCone",
        ],
    )
    return
end

function test_NormInfinity_1()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.NormInfinity{Float64}(mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1.0, 0.5, 1.0],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                [[0.0, 1.0, 0.0, 0.0]],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[-1], [-1]],
        )
    MOI.Test.test_conic_NormInfinityCone_VectorOfVariables(bridged_mock, config)
    MOI.empty!(bridged_mock)
    MOI.Test.test_conic_NormInfinityCone_VectorAffineFunction(
        bridged_mock,
        config,
    )
    var_names = ["x", "y", "z"]
    MOI.set(
        mock,
        MOI.VariableName(),
        MOI.get(mock, MOI.ListOfVariableIndices()),
        var_names,
    )
    nonneg = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.Nonnegatives,
        }(),
    )
    @test length(nonneg) == 1
    MOI.set(mock, MOI.ConstraintName(), nonneg[1], "nonneg")
    zeros = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.Zeros,
        }(),
    )
    @test length(zeros) == 2
    MOI.set(mock, MOI.ConstraintName(), zeros[1], "x_eq")
    MOI.set(mock, MOI.ConstraintName(), zeros[2], "y_eq")

    s = """
    variables: x, y, z
    nonneg: [x + -1.0y, x + -1.0z, x + y, x + z] in Nonnegatives(4)
    x_eq: [-1.0 + x] in Zeros(1)
    y_eq: [-0.5 + y] in Zeros(1)
    maxobjective: y + z
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        mock,
        model,
        var_names,
        ["nonneg", "x_eq", "y_eq"],
    )
    MOI.set(
        bridged_mock,
        MOI.VariableName(),
        MOI.get(bridged_mock, MOI.ListOfVariableIndices()),
        var_names,
    )
    norminf = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.NormInfinityCone,
        }(),
    )
    @test length(norminf) == 1
    MOI.set(bridged_mock, MOI.ConstraintName(), norminf[1], "norminf")
    zeros = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.Zeros,
        }(),
    )
    @test length(zeros) == 2
    MOI.set(bridged_mock, MOI.ConstraintName(), zeros[1], "x_eq")
    MOI.set(bridged_mock, MOI.ConstraintName(), zeros[2], "y_eq")

    s = """
    variables: x, y, z
    norminf: [1.0x, y, z] in NormInfinityCone(3)
    x_eq: [-1.0 + x] in Zeros(1)
    y_eq: [-0.5 + y] in Zeros(1)
    maxobjective: y + z
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        bridged_mock,
        model,
        var_names,
        ["norminf", "x_eq", "y_eq"],
    )
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.NormInfinityCone,
            }(),
        ),
    )
    nonneg = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.Nonnegatives,
        }(),
    )
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value = [4.0, 1.0, -2.0]
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
        if attr isa MOI.ConstraintPrimalStart
            nonneg_value = Float64[3, 6, 5, 2]
        else
            nonneg_value = [0.25, 2.25, 1.25, 0.25]
        end
        @test MOI.get(mock, attr, nonneg[1]) ≈ nonneg_value
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        3,
        ((MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives, 0),),
    )
    return
end

function test_conic_NormInfinityCone_3()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.NormInfinity{Float64}(mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [2, -1, -1, -1],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                [vcat(fill(inv(3), 3), zeros(3)), fill(inv(3), 3)],
        )
    MOI.Test.test_conic_NormInfinityCone_3(bridged_mock, config)
    var_names = ["x", "y1", "y2", "y3"]
    MOI.set(
        mock,
        MOI.VariableName(),
        MOI.get(mock, MOI.ListOfVariableIndices()),
        var_names,
    )
    nonneg = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.Nonnegatives,
        }(),
    )
    @test length(nonneg) == 2
    MOI.set(mock, MOI.ConstraintName(), nonneg[1], "nonneg1")
    MOI.set(mock, MOI.ConstraintName(), nonneg[2], "nonneg2")

    s = """
    variables: x, y1, y2, y3
    nonneg1: [x + -1.0y1 + -3.0, x + -1.0y2 + -3.0, x + -1.0y3 + -3.0, x + y1 + 1.0, x + y2 + 1.0, x + y3 + 1.0] in Nonnegatives(6)
    nonneg2: [y1 + 1.0, y2 + 1.0, y3 + 1.0] in Nonnegatives(3)
    minobjective: x
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        mock,
        model,
        var_names,
        ["nonneg1", "nonneg2"],
    )
    MOI.set(
        bridged_mock,
        MOI.VariableName(),
        MOI.get(bridged_mock, MOI.ListOfVariableIndices()),
        var_names,
    )
    norminf = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.NormInfinityCone,
        }(),
    )
    @test length(norminf) == 1
    MOI.set(bridged_mock, MOI.ConstraintName(), norminf[1], "norminf")
    nonneg = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.Nonnegatives,
        }(),
    )
    @test length(nonneg) == 1
    MOI.set(bridged_mock, MOI.ConstraintName(), nonneg[1], "nonneg")

    s = """
    variables: x, y1, y2, y3
    norminf: [x + -1.0, y1 + 2.0, y2 + 2.0, y3 + 2.0] in NormInfinityCone(4)
    nonneg: [y1 + 1.0, y2 + 1.0, y3 + 1.0] in Nonnegatives(3)
    minobjective: x
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        bridged_mock,
        model,
        var_names,
        ["norminf", "nonneg"],
    )
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.NormInfinityCone,
            }(),
        ),
    )
    nonneg = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.Nonnegatives,
        }(),
    )
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value = Float64[5, 1, -2, 3]
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
        if attr isa MOI.ConstraintPrimalStart
            nonneg_value = Float64[4, 7, 2, 6, 3, 8]
        else
            nonneg_value = [-1, 11, -1, 5, -1, 17] / 6
        end
        @test MOI.get(mock, attr, nonneg[1]) ≈ nonneg_value
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        4,
        ((MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives, 1),),
    )
    return
end

function test_runtests_vector_of_variables()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.NormInfinityBridge,
        """
        variables: t, x
        [t, x] in NormInfinityCone(2)
        """,
        """
        variables: t, x
        [t + -1.0 * x, t + x] in Nonnegatives(2)
        """,
    )
    return
end

function test_runtests_vector_affine_function()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.NormInfinityBridge,
        """
        variables: t, x
        [t, 2.0 * x + 1.0] in NormInfinityCone(2)
        """,
        """
        variables: t, x
        [t + -2.0 * x + -1.0, t + 2.0 * x + 1.0] in Nonnegatives(2)
        """,
    )
    return
end

function test_NormInfinity_VectorNonlinearFunction()
    # We can't use the standard runtests because ScalarNonlinearFunction does
    # not preserve f(x) ≈ (f(x) - g(x)) + g(x)
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Constraint.NormInfinity{Float64}(inner)
    t = MOI.add_variable(model)
    x = MOI.add_variable(model)
    f = MOI.VectorNonlinearFunction([
        MOI.ScalarNonlinearFunction(:+, Any[t]),
        MOI.ScalarNonlinearFunction(:sin, Any[x]),
    ])
    c = MOI.add_constraint(model, f, MOI.NormInfinityCone(2))
    F, S = MOI.VectorNonlinearFunction, MOI.Nonnegatives
    indices = MOI.get(inner, MOI.ListOfConstraintIndices{F,S}())
    @test length(indices) == 1
    inner_variables = MOI.get(inner, MOI.ListOfVariableIndices())
    @test length(inner_variables) == 2
    u, v = inner_variables
    u_p = MOI.ScalarNonlinearFunction(:+, Any[u])
    v_sin = MOI.ScalarNonlinearFunction(:sin, Any[v])
    g = MOI.VectorNonlinearFunction([
        MOI.ScalarNonlinearFunction(
            :+,
            Any[MOI.ScalarNonlinearFunction(:-, Any[v_sin]), u_p],
        ),
        MOI.ScalarNonlinearFunction(:+, Any[v_sin, u_p]),
    ])
    @test ≈(MOI.get(inner, MOI.ConstraintFunction(), indices[1]), g)
    h = MOI.VectorNonlinearFunction([
        MOI.ScalarNonlinearFunction(:+, Any[t]),
        MOI.ScalarNonlinearFunction(:cos, Any[x]),
    ])
    MOI.set(model, MOI.ConstraintFunction(), c, h)
    v_cos = MOI.ScalarNonlinearFunction(:cos, Any[v])
    g_2 = MOI.VectorNonlinearFunction([
        MOI.ScalarNonlinearFunction(
            :+,
            Any[MOI.ScalarNonlinearFunction(:-, Any[v_cos]), u_p],
        ),
        MOI.ScalarNonlinearFunction(:+, Any[v_cos, u_p]),
    ])
    @test ≈(MOI.get(inner, MOI.ConstraintFunction(), indices[1]), g_2)
    return
end

end  # module

TestConstraintNormInfinity.runtests()
