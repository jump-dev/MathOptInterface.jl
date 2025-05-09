# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestVariableFlipSign

using Test

import MathOptInterface as MOI

include("../utilities.jl")

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

function test_NonposToNonneg()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Variable.NonposToNonneg{Float64}(mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [-4, 3, 16, 0],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[7, 2, -4]],
        )
    MOI.Test.test_conic_linear_VectorOfVariables_2(
        bridged_mock,
        MOI.Test.Config(),
    )
    @test MOI.get(mock, MOI.NumberOfVariables()) == 4
    @test MOI.get(bridged_mock, MOI.NumberOfVariables()) == 4
    # Variables are ordered
    #   x in R^1, y in R_-^1, z in R^1, s in R^1
    vis = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
    y = vis[2]
    @test y.value == -1

    @test MOI.supports(
        bridged_mock,
        MOI.VariablePrimalStart(),
        MOI.VariableIndex,
    )
    @test MOI.supports(
        bridged_mock,
        MOI.VariablePrimalStart(),
        typeof(MOI.Bridges.bridge(bridged_mock, y)),
    )
    MOI.set(bridged_mock, MOI.VariablePrimalStart(), y, 1.0)
    x, y_flipped, z, s = MOI.get(mock, MOI.ListOfVariableIndices())
    @test MOI.get(mock, MOI.VariablePrimalStart(), y_flipped) == -1
    @test MOI.get(bridged_mock, MOI.VariablePrimalStart(), y) == 1

    var_names = ["x", "y", "z", "w"]
    MOI.set(
        mock,
        MOI.VariableName(),
        MOI.get(mock, MOI.ListOfVariableIndices()),
        var_names,
    )
    con_w = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Zeros}(),
    )[1]
    con_yz = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Nonnegatives}(),
    )
    con_ex = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.Zeros,
        }(),
    )[1]

    MOI.set(mock, MOI.ConstraintName(), con_w, "cw")
    MOI.set(mock, MOI.ConstraintName(), con_yz[1], "cy")
    MOI.set(mock, MOI.ConstraintName(), con_yz[2], "cz")
    MOI.set(mock, MOI.ConstraintName(), con_ex, "cex")

    MOI.set(
        bridged_mock,
        MOI.VariableName(),
        MOI.get(bridged_mock, MOI.ListOfVariableIndices())[2],
        "v",
    )
    con_v = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Nonpositives}(),
    )[1]
    MOI.set(bridged_mock, MOI.ConstraintName(), con_v, "cv")
    s = """
    variables: x, y, z, w
    cw: [w] in Zeros(1)
    cy: [y] in Nonnegatives(1)
    cz: [z] in Nonnegatives(1)
    cex: [1*x + -1*w + 4.0, -1*y + 3.0, 1*x + 1*z + -12.0] in Zeros(3)
    minobjective: 3*x + -2*y + -4*z
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        mock,
        model,
        var_names,
        ["cw", "cy", "cz", "cex"],
    )
    s = """
    variables: x, z, w, v
    cv: [v] in Nonpositives(1)
    cw: [w] in Zeros(1)
    cz: [z] in Nonnegatives(1)
    cex: [1*x + -1*w + 4.0, 1*v + 3.0, 1*x + 1*z + -12.0] in Zeros(3)
    minobjective: 3*x + 2*v + -4*z
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        bridged_mock,
        model,
        ["x", "z", "w", "v"],
        ["cv", "cw", "cz", "cex"],
    )
    return
end

function test_conic_linear_INFEASIBLE_2()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Variable.NonposToNonneg{Float64}(mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            MOI.INFEASIBLE_POINT,
            MOI.INFEASIBILITY_CERTIFICATE,
        ),
    )
    MOI.Test.test_conic_linear_INFEASIBLE_2(bridged_mock, MOI.Test.Config())
    @test MOI.get(mock, MOI.NumberOfVariables()) == 1
    @test length(MOI.get(mock, MOI.ListOfVariableIndices())) == 1
    @test first(MOI.get(mock, MOI.ListOfVariableIndices())).value ≥ 0
    @test MOI.get(bridged_mock, MOI.NumberOfVariables()) == 1
    vis = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
    @test vis == [MOI.VariableIndex(-1)]
    _test_delete_bridged_variable(
        bridged_mock,
        vis[1],
        MOI.Nonpositives,
        1,
        ((MOI.VectorOfVariables, MOI.Nonnegatives, 0),),
    )
    return
end

function test_delete_in_vector()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Variable.NonposToNonneg{Float64}(mock)
    vis, _ = MOI.add_constrained_variables(bridged_mock, MOI.Nonpositives(4))
    _test_delete_bridged_variable(
        bridged_mock,
        vis[2],
        MOI.Nonpositives,
        4,
        ((MOI.VectorOfVariables, MOI.Nonnegatives, 0),),
        used_bridges = 0,
        used_constraints = 0,
    )
    return
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Variable.NonposToNonnegBridge,
        """
        constrainedvariable: [x, y] in Nonpositives(2)
        minobjective: x + y
        c: 2.0 * x + -3.0 * y <= 1.0
        """,
        """
        constrainedvariable: [x, y] in Nonnegatives(2)
        minobjective: -1.0 * x + -1.0 * y
        c: -2.0 * x + 3.0 * y <= 1.0
        """,
    )
    return
end

function test_adjoint_map_function()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Variable.NonposToNonneg{Float64}(inner)
    x, _ = MOI.add_constrained_variables(model, MOI.Nonpositives(1))
    @test MOI.Bridges.adjoint_map_function(model.map[only(x)], 1.23) == -1.23
    return
end

end  # module

TestVariableFlipSign.runtests()
