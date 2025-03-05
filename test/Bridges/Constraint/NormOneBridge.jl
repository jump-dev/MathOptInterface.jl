# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintNormOne

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

function test_NormOne()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.NormOne{Float64}(mock)
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_VectorOfVariables_NormOneCone",
            "test_basic_VectorAffineFunction_NormOneCone",
            "test_basic_VectorQuadraticFunction_NormOneCone",
            # "test_basic_VectorNonlinearFunction_NormOneCone",
        ],
    )
    return
end

function test_conic_NormOneCone_VectorOfVariables()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.NormOne{Float64}(mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1.0, 0.5, 0.5, 0.5, 0.5],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                [[1.0, 1.0, 1.0, 0.0, 0.0]],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[-1], [0]],
        )
    MOI.Test.test_conic_NormOneCone_VectorOfVariables(bridged_mock, config)
    MOI.empty!(bridged_mock)
    MOI.Test.test_conic_NormOneCone_VectorAffineFunction(bridged_mock, config)
    var_names = ["x", "y", "z"]
    MOI.set(
        bridged_mock,
        MOI.VariableName(),
        MOI.get(bridged_mock, MOI.ListOfVariableIndices()),
        var_names,
    )
    nonneg = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.Nonnegatives,
        }(),
    )
    u, v = MOI.get(mock, MOI.ListOfVariableIndices())[4:5]
    MOI.set(mock, MOI.VariableName(), u, "u")
    MOI.set(mock, MOI.VariableName(), v, "v")
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
    variables: x, y, z, u, v
    nonneg: [x + -1.0u + -1.0v, u + -1.0y, v + -1.0z, u + y, v + z] in Nonnegatives(5)
    x_eq: [-1.0 + x] in Zeros(1)
    y_eq: [-0.5 + y] in Zeros(1)
    maxobjective: y + z
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        mock,
        model,
        [var_names; "u"; "v"],
        ["nonneg", "x_eq", "y_eq"],
    )
    normone = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.NormOneCone,
        }(),
    )
    @test length(normone) == 1
    MOI.set(bridged_mock, MOI.ConstraintName(), normone[1], "normone")
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
    normone: [1.0x, y, z] in NormOneCone(3)
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
        ["normone", "x_eq", "y_eq"],
    )
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.NormOneCone,
            }(),
        ),
    )
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value = [4.0, 1.0, -2.0]
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
        if attr isa MOI.ConstraintPrimalStart
            @test MOI.get(mock, MOI.VariablePrimalStart(), u) == 1
            @test MOI.get(mock, MOI.VariablePrimalStart(), v) == 2
            @test MOI.get(mock, attr, nonneg[1]) == Float64[1, 0, 4, 2, 0]
        else
            @test MOI.get(mock, attr, nonneg[1]) == Float64[4, 0, 2, 1, 0]
        end
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        3,
        ((MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives, 0),),
    )
    return
end

function test_conic_NormOneCone()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.NormOne{Float64}(mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [4, -1, -1, -1, 1, 1, 1],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                [vcat(ones(4), zeros(3)), ones(3)],
        )
    MOI.Test.test_conic_NormOneCone(bridged_mock, config)
    var_names = ["x", "y1", "y2", "y3"]
    var_names_all = vcat(var_names, "z1", "z2", "z3")
    MOI.set(
        mock,
        MOI.VariableName(),
        MOI.get(mock, MOI.ListOfVariableIndices()),
        var_names_all,
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
    variables: x, y1, y2, y3, z1, z2, z3
    nonneg1: [x + -1.0 + -1.0z1 + -1.0z2 + -1.0z3, z1 + -1.0y1 + -2.0, z2 + -1.0y2 + -2.0, z3 + -1.0y3 + -2.0, z1 + y1 + 2.0, z2 + y2 + 2.0, z3 + y3 + 2.0] in Nonnegatives(7)
    nonneg2: [y1 + 1.0, y2 + 1.0, y3 + 1.0] in Nonnegatives(3)
    minobjective: x
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        mock,
        model,
        var_names_all,
        ["nonneg1", "nonneg2"],
    )
    MOI.set(
        bridged_mock,
        MOI.VariableName(),
        MOI.get(bridged_mock, MOI.ListOfVariableIndices()),
        var_names,
    )
    normone = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.NormOneCone,
        }(),
    )
    @test length(normone) == 1
    MOI.set(bridged_mock, MOI.ConstraintName(), normone[1], "normone")
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
    normone: [x + -1.0, y1 + 2.0, y2 + 2.0, y3 + 2.0] in NormOneCone(4)
    nonneg: [y1 + 1.0, y2 + 1.0, y3 + 1.0] in Nonnegatives(3)
    minobjective: x
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        bridged_mock,
        model,
        var_names,
        ["normone", "nonneg"],
    )
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.NormOneCone,
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
        value = (
            attr isa MOI.ConstraintPrimalStart ? vcat(3, ones(3)) :
            vcat(1, fill(-1, 3))
        )
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
        nonneg_value = (
            attr isa MOI.ConstraintPrimalStart ?
            vcat(zeros(4), fill(2.0, 3)) : vcat(ones(4), zeros(3))
        )
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
        MOI.Bridges.Constraint.NormOneBridge,
        """
        variables: t, x
        [t, x] in NormOneCone(2)
        """,
        """
        variables: t, x, y
        [t + -1.0 * y, y + -1.0 * x, y + x] in Nonnegatives(3)
        """,
    )
    return
end

function test_runtests_vector_affine_function()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.NormOneBridge,
        """
        variables: t, x
        [t, 2.0 * x + 1.0] in NormOneCone(2)
        """,
        """
        variables: t, x, y
        [t + -1.0 * y, y + -2.0 * x + -1.0, y + 2.0 * x + 1.0] in Nonnegatives(3)
        """,
    )
    return
end

function test_NormOne_VectorNonlinearFunction()
    # We can't use the standard runtests because ScalarNonlinearFunction does
    # not preserve f(x) ≈ (f(x) - g(x)) + g(x)
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Constraint.NormOne{Float64}(inner)
    t = MOI.add_variable(model)
    x = MOI.add_variable(model)
    f = MOI.VectorNonlinearFunction([
        MOI.ScalarNonlinearFunction(:+, Any[t]),
        MOI.ScalarNonlinearFunction(:sin, Any[x]),
    ])
    c = MOI.add_constraint(model, f, MOI.NormOneCone(2))
    F, S = MOI.VectorNonlinearFunction, MOI.Nonnegatives
    indices = MOI.get(inner, MOI.ListOfConstraintIndices{F,S}())
    @test length(indices) == 1
    inner_variables = MOI.get(inner, MOI.ListOfVariableIndices())
    @test length(inner_variables) == 3
    u, v, w = inner_variables
    v_sin = MOI.ScalarNonlinearFunction(:sin, Any[v])
    g = MOI.VectorNonlinearFunction([
        MOI.ScalarNonlinearFunction(
            :-,
            Any[MOI.ScalarNonlinearFunction(:+, Any[u]), 0.0+1.0*w],
        ),
        MOI.ScalarNonlinearFunction(
            :+,
            Any[MOI.ScalarNonlinearFunction(:-, Any[v_sin]), w],
        ),
        MOI.ScalarNonlinearFunction(:+, Any[v_sin, w]),
    ])
    @test ≈(MOI.get(inner, MOI.ConstraintFunction(), indices[1]), g)
    return
end

end  # module

TestConstraintNormOne.runtests()
