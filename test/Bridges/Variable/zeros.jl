# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestVariableZeros

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

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Variable.ZerosBridge,
        model -> begin
            x, _ = MOI.add_constrained_variables(model, MOI.Zeros(2))
            MOI.add_constraint(
                model,
                1.0 * x[1] + 2.0 * x[2],
                MOI.EqualTo(3.0),
            )
        end,
        model -> begin
            MOI.add_constraint(
                model,
                zero(MOI.ScalarAffineFunction{Float64}),
                MOI.EqualTo(3.0),
            )
        end;
        cannot_unbridge = true,
    )
    return
end

function test_bridge_error_handler()
    for (err, flag) in (
        ErrorException("abc") => false,
        MOI.GetAttributeNotAllowed(MOI.ObjectiveSense()) => false,
        MOI.GetAttributeNotAllowed(MOI.ConstraintFunction()) => true,
    )
        @test_throws err try
            @assert false
        catch
            MOI.Bridges._runtests_error_handler(err, false)
        end
        if flag
            @test MOI.Bridges._runtests_error_handler(err, true) === nothing
        else
            @test_throws err try
                @assert false
            catch
                MOI.Bridges._runtests_error_handler(err, true)
            end
        end
    end
    return
end

function test_zeros()
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    bridged_mock = MOI.Bridges.Variable.Zeros{Float64}(mock)

    x, cx = MOI.add_constrained_variable(bridged_mock, MOI.GreaterThan(0.0))
    MOI.set(bridged_mock, MOI.VariableName(), x, "x")
    yz, cyz = MOI.add_constrained_variables(bridged_mock, MOI.Zeros(2))
    MOI.set(bridged_mock, MOI.VariableName(), yz, ["y", "z"])
    MOI.set(bridged_mock, MOI.ConstraintName(), cyz, "cyz")
    y, z = yz
    MOI.set(bridged_mock, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(bridged_mock, MOI.ObjectiveFunction{typeof(x)}(), x)
    @test MOI.get(bridged_mock, MOI.ObjectiveFunction{typeof(x)}()) == x

    # Test before adding affine constraints are affine expressions cannot be
    # unbridged when `Variable.ZerosBridge` is used.
    s = """
    variables: x, y, z
    x >= 0.0
    cyz: [y, z] in Zeros(2)
    minobjective: x
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        bridged_mock,
        model,
        ["x", "y", "z"],
        ["cyz"],
        [("x", MOI.GreaterThan{Float64}(0.0))],
    )

    c1, c2 = MOI.add_constraints(
        bridged_mock,
        [1.0y + 1.0z, 1.0x + 1.0y + 1.0z],
        [MOI.EqualTo(0.0), MOI.GreaterThan(1.0)],
    )
    MOI.set(bridged_mock, MOI.ConstraintName(), c1, "con1")
    MOI.set(bridged_mock, MOI.ConstraintName(), c2, "con2")
    MOI.set(bridged_mock, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    obj = 1.0x - 1.0y - 1.0z
    MOI.set(bridged_mock, MOI.ObjectiveFunction{typeof(obj)}(), obj)

    @test MOI.Bridges.Variable.unbridged_map(
        MOI.Bridges.bridge(bridged_mock, y),
        y,
        MOI.Bridges.IndexInVector(1),
    ) === nothing
    @test MOI.Bridges.Variable.unbridged_map(
        MOI.Bridges.bridge(bridged_mock, z),
        z,
        MOI.Bridges.IndexInVector(2),
    ) === nothing

    err = MOI.DeleteNotAllowed(
        cyz,
        "Cannot delete constraint index of bridged constrained variables. " *
        "Delete the scalar variable or the vector of variables instead.",
    )
    @test_throws err MOI.delete(bridged_mock, cyz)

    err = MOI.GetAttributeNotAllowed(
        MOI.ConstraintFunction(),
        "Cannot unbridge function because some variables are bridged by" *
        " variable bridges that do not support reverse mapping, for example," *
        " `ZerosBridge`.",
    )
    @test_throws err MOI.get(bridged_mock, MOI.ObjectiveFunction{typeof(obj)}())
    # With `c1`, the function does not contain any variable so it tests that it
    # also throws an error even if it never calls `variable_unbridged_function`
    @test_throws err MOI.get(bridged_mock, MOI.ConstraintFunction(), c1)
    @test_throws err MOI.get(bridged_mock, MOI.ConstraintFunction(), c2)

    err = ArgumentError(
        "Variable bridge of type `MathOptInterface.Bridges.Variable.ZerosBridge{Float64}`" *
        " does not support accessing the attribute `$(MOI.Test.UnknownVariableAttribute())`.",
    )
    @test_throws err MOI.get(
        bridged_mock,
        MOI.Test.UnknownVariableAttribute(),
        y,
    )

    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                0.0,
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                1.0,
        ),
    )
    MOI.optimize!(bridged_mock)
    @test MOI.get(bridged_mock, MOI.VariablePrimal(), x) == 1.0
    @test MOI.get(bridged_mock, MOI.VariablePrimal(), y) == 0.0
    @test MOI.get(bridged_mock, MOI.VariablePrimal(), z) == 0.0

    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), cyz) == zeros(2)

    @test MOI.get(bridged_mock, MOI.ConstraintDual(), cx) == 0.0
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), c1) == 0.0
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), c2) == 1.0
    attr = MOI.ConstraintDual()
    err = MOI.GetAttributeNotAllowed(
        attr,
        "Unable to query the dual of a variable bound that was reformulated " *
        "using `ZerosBridge`. This usually arises in conic solvers when a " *
        "variable is fixed to a value. As a work-around, instead of creating " *
        "a fixed variable using variable bounds like `p == 1`, add an affine " *
        "equality constraint like `1 * p == 1` (or `[1 * p - 1,] in Zeros(1)`).",
    )
    @test_throws err MOI.get(bridged_mock, attr, cyz)

    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), cyz).variables == yz
    @test MOI.get(mock, MOI.NumberOfVariables()) == 1
    @test MOI.get(mock, MOI.ListOfVariableIndices()) == [x]
    @test MOI.get(bridged_mock, MOI.NumberOfVariables()) == 3
    @test MOI.get(bridged_mock, MOI.ListOfVariableIndices()) == [x, y, z]
    @test MOI.get(
        mock,
        MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Zeros}(),
    ) == 0
    @test MOI.get(
        bridged_mock,
        MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Zeros}(),
    ) == 1
    @test MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Zeros}(),
    ) == [cyz]

    s = """
    variables: x
    x >= 0.0
    con1: 0.0 == 0.0
    con2: x + 0.0 >= 1.0
    minobjective: x
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        mock,
        model,
        ["x"],
        ["con1", "con2"],
        [("x", MOI.GreaterThan{Float64}(0.0))],
    )

    _test_delete_bridged_variables(
        bridged_mock,
        yz,
        MOI.Zeros,
        3,
        ((MOI.VariableIndex, MOI.GreaterThan{Float64}, 1),),
    )
    @test MOI.is_valid(bridged_mock, x)
    @test !MOI.is_valid(bridged_mock, y)
    @test !MOI.is_valid(bridged_mock, z)
    return
end

end  # module

TestVariableZeros.runtests()
