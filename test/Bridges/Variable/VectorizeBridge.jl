# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestVariableVectorize

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

function test_get_scalar_constraint()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Variable.Vectorize{Float64}(mock)
    x, cx = MOI.add_constrained_variable(bridged_mock, MOI.GreaterThan(1.0))
    func = 2.0 * x
    set = MOI.GreaterThan(5.0)
    err =
        MOI.ScalarFunctionConstantNotZero{Float64,typeof(func),typeof(set)}(1.0)
    @test_throws err MOI.add_constraint(bridged_mock, func + 1.0, set)

    c = MOI.add_constraint(bridged_mock, func, set)
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), c) ≈ func
    @test MOI.get(bridged_mock, MOI.ConstraintSet(), c) == set
    MOI.set(bridged_mock, MOI.ConstraintName(), c, "c")

    @testset "Mock model" begin
        MOI.set(
            mock,
            MOI.VariableName(),
            MOI.get(mock, MOI.ListOfVariableIndices()),
            ["y"],
        )
        MOI.set(
            mock,
            MOI.ConstraintName(),
            MOI.get(
                mock,
                MOI.ListOfConstraintIndices{
                    MOI.VectorOfVariables,
                    MOI.Nonnegatives,
                }(),
            ),
            ["cy"],
        )
        s = """
        variables: y
        cy: [y] in Nonnegatives(1)
        c: 2.0y >= 3.0
        """
        model = MOI.Utilities.Model{Float64}()
        MOI.Utilities.loadfromstring!(model, s)
        MOI.Test.util_test_models_equal(mock, model, ["y"], ["cy", "c"])
    end
    @testset "Bridged model" begin
        MOI.set(bridged_mock, MOI.VariableName(), x, "x")
        s = """
        variables: x
        x >= 1.0
        c: 2.0x >= 5.0
        """
        model = MOI.Utilities.Model{Float64}()
        MOI.Utilities.loadfromstring!(model, s)
        MOI.Test.util_test_models_equal(
            bridged_mock,
            model,
            ["x"],
            ["c"],
            [("x", MOI.GreaterThan{Float64}(1.0))],
        )
    end
end

function test_exp3_with_add_constrained_variable_y()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Variable.Vectorize{Float64}(mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [log(5), 0.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [0.0],
            (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone) =>
                [[-1.0, log(5) - 1, 1 / 5]],
        )

    MOI.empty!(bridged_mock)
    x = MOI.add_variable(bridged_mock)
    @test MOI.get(bridged_mock, MOI.NumberOfVariables()) == 1
    xc = MOI.add_constraint(bridged_mock, 2.0x, MOI.LessThan(4.0))
    y, yc = MOI.add_constrained_variable(bridged_mock, MOI.LessThan(5.0))
    @test yc.value == y.value == -1
    @test MOI.get(bridged_mock, MOI.NumberOfVariables()) == 2
    @test length(MOI.get(bridged_mock, MOI.ListOfVariableIndices())) == 2
    @test Set(MOI.get(bridged_mock, MOI.ListOfVariableIndices())) == Set([x, y])
    ec = MOI.add_constraint(
        bridged_mock,
        MOI.Utilities.operate(vcat, Float64, x, 1.0, y),
        MOI.ExponentialCone(),
    )

    MOI.optimize!(bridged_mock)
    @test MOI.get(bridged_mock, MOI.VariablePrimal(), x) ≈ log(5)
    @test MOI.get(bridged_mock, MOI.VariablePrimal(), y) ≈ 5.0
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), xc) ≈ 2log(5)
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), yc) ≈ 5
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), ec) ≈ [log(5), 1.0, 5.0]
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), xc) ≈ 0.0
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), yc) ≈ -1 / 5
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), ec) ≈
          [-1.0, log(5) - 1, 1 / 5]

    err = MOI.AddConstraintNotAllowed{MOI.VariableIndex,MOI.LessThan{Float64}}(
        "Cannot add two `VariableIndex`-in-`MathOptInterface.LessThan{Float64}`" *
        " on the same variable MOI.VariableIndex(-1).",
    )
    @test_throws err MOI.add_constraint(bridged_mock, y, MOI.LessThan(4.0))

    cis = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.ExponentialCone,
        }(),
    )
    @test length(cis) == 1

    err = ArgumentError(
        "Variable bridge of type `$(MOI.Bridges.Variable.VectorizeBridge{Float64,MOI.Nonpositives})`" *
        " does not support accessing the attribute `$(MOI.Test.UnknownVariableAttribute())`.",
    )
    @test_throws err MOI.get(
        bridged_mock,
        MOI.Test.UnknownVariableAttribute(),
        y,
    )
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.LessThan{Float64}}(y.value)
    attr = MOI.ConstraintSet()
    err = MOI.SetAttributeNotAllowed(
        attr,
        "The variable `MOI.VariableIndex(12345676)` is bridged by the `VectorizeBridge`.",
    )
    @test_throws err MOI.set(bridged_mock, attr, ci, MOI.LessThan(4.0))

    change = MOI.MultirowChange(y, [(3, 0.0)])
    message =
        "The change $change" *
        " contains variables bridged into a function with nonzero constant."
    err = MOI.ModifyConstraintNotAllowed(cis[1], change, message)
    @test_throws err MOI.modify(bridged_mock, cis[1], change)

    change = MOI.ScalarCoefficientChange(y, 0.0)
    attr = MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()
    message =
        "The change MathOptInterface.ScalarCoefficientChange{Float64}(MOI.VariableIndex(-1), 0.0)" *
        " contains variables bridged into a function with nonzero constant."
    err = MOI.ModifyObjectiveNotAllowed(change, message)
    @test_throws err MOI.modify(bridged_mock, attr, change)

    MOI.set(bridged_mock, MOI.VariableName(), x, "x")
    MOI.set(bridged_mock, MOI.ConstraintName(), xc, "xc")
    MOI.set(bridged_mock, MOI.ConstraintName(), ec, "ec")
    z = MOI.get(mock, MOI.ListOfVariableIndices())[2]

    MOI.set(mock, MOI.VariableName(), z, "z")
    MOI.set(
        mock,
        MOI.ConstraintName(),
        MOI.get(
            mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorOfVariables,
                MOI.Nonpositives,
            }(),
        ),
        ["zc"],
    )
    s = """
    variables: x, z
    zc: [z] in Nonpositives(1)
    xc: 2.0x <= 4.0
    ec: [x, 1.0, z + 5.0] in ExponentialCone()
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(mock, model, ["x", "z"], ["zc", "xc", "ec"])

    MOI.set(bridged_mock, MOI.VariableName(), y, "y")
    s = """
    variables: x, y
    y <= 5.0
    xc: 2.0x <= 4.0
    ec: [x, 1.0, y] in ExponentialCone()
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        bridged_mock,
        model,
        ["x", "y"],
        ["xc", "ec"],
        [("y", MOI.LessThan{Float64}(5.0))],
    )

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
    @test MOI.get(mock, MOI.VariablePrimalStart(), z) == -4
    @test MOI.get(bridged_mock, MOI.VariablePrimalStart(), y) == 1
    return
end

function test_delete_variable()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Variable.Vectorize{Float64}(inner)
    x, _ = MOI.add_constrained_variable(model, MOI.GreaterThan(1.0))
    @test_throws MOI.DeleteNotAllowed{MOI.VariableIndex} MOI.delete(model, x)
    return
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Variable.VectorizeBridge,
        """
        constrainedvariable: x in GreaterThan(2.0)
        minobjective: 1.0 * x
        c: 2.0 * x <= 1.0
        """,
        """
        constrainedvariable: [x] in Nonnegatives(1)
        minobjective: 2.0 + 1.0 * x
        c: 2.0 * x <= -3.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Variable.VectorizeBridge,
        """
        constrainedvariable: x in LessThan(2.0)
        minobjective: 1.0 * x
        c: 2.0 * x <= 1.0
        """,
        """
        constrainedvariable: [x] in Nonpositives(1)
        minobjective: 2.0 + 1.0 * x
        c: 2.0 * x <= -3.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Variable.VectorizeBridge,
        """
        constrainedvariable: x in EqualTo(2.0)
        minobjective: 1.0 * x
        c: 2.0 * x <= 1.0
        """,
        """
        constrainedvariable: [x] in Zeros(1)
        minobjective: 2.0 + 1.0 * x
        c: 2.0 * x <= -3.0
        """,
    )
    return
end

function test_list_of_constraint_indices()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Variable.Vectorize{Float64}(inner)
    x, _ = MOI.add_constrained_variable(model, MOI.EqualTo(1.0))
    attr = MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Zeros}()
    @test isempty(MOI.get(model, attr))
    return
end

function test_variable_primal_ray()
    inner = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    model = MOI.Bridges.Variable.Vectorize{Float64}(inner)
    x, _ = MOI.add_constrained_variable(model, MOI.EqualTo(1.0))
    MOI.set(inner, MOI.PrimalStatus(), MOI.INFEASIBILITY_CERTIFICATE)
    y = only(MOI.get(inner, MOI.ListOfVariableIndices()))
    MOI.set(inner, MOI.VariablePrimal(), y, 1.23)
    @test MOI.get(model, MOI.VariablePrimal(), x) == 1.23
    MOI.set(inner, MOI.PrimalStatus(), MOI.FEASIBLE_POINT)
    @test MOI.get(model, MOI.VariablePrimal(), x) == 2.23
    return
end

end  # module

TestVariableVectorize.runtests()
