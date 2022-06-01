# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintZeroOne

using Test

using MathOptInterface
const MOI = MathOptInterface

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

function test_ZeroOne()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.ZeroOne{Float64}(mock)

    bridge_type = MOI.Bridges.Constraint.ZeroOneBridge{Float64}
    @test MOI.supports_constraint(bridge_type, MOI.VariableIndex, MOI.ZeroOne)
    @test MOI.Bridges.Constraint.concrete_bridge_type(
        bridge_type,
        MOI.VariableIndex,
        MOI.ZeroOne,
    ) == bridge_type
    MOI.Test.test_basic_VariableIndex_ZeroOne(bridged_mock, config)
    MOI.empty!(bridged_mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [1, 0, 0, 1, 1]),
    )
    MOI.Test.test_linear_integer_knapsack(bridged_mock, config)
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.ZeroOne}(),
        ),
    )
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), ci) == 1
    _test_delete_bridge(
        bridged_mock,
        ci,
        5,
        (
            (MOI.VariableIndex, MOI.Integer, 0),
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}, 0),
        ),
        num_bridged = 5,
    )
    MOI.empty!(bridged_mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 20.0)
            MOI.Utilities.mock_optimize!(mock, [4, 5, 1])
        end,
    )
    MOI.Test.test_linear_integer_integration(bridged_mock, config)
    MOI.empty!(bridged_mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [1.0; zeros(10)]),
    )
    MOI.Test.test_linear_integer_solve_twice(bridged_mock, config)
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.ZeroOne}(),
        ),
    )
    s = """
    variables: x, y
    y == 1.0
    x in ZeroOne()
    minobjective: x
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    sb = """
    variables: x, y
    y == 1.0
    x in Integer()
    c: 1.0 * x in Interval(0.0,1.0)
    minobjective: x
    """
    modelb = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(modelb, sb)
    MOI.empty!(bridged_mock)
    @test MOI.is_empty(bridged_mock)
    MOI.Utilities.loadfromstring!(bridged_mock, s)
    MOI.Test.util_test_models_equal(
        bridged_mock,
        model,
        ["x", "y"],
        String[],
        [("y", MOI.EqualTo{Float64}(1.0)), ("x", MOI.ZeroOne())],
    )
    F, S = MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}
    ci = first(MOI.get(mock, MOI.ListOfConstraintIndices{F,S}()))
    MOI.set(mock, MOI.ConstraintName(), ci, "c")
    MOI.Test.util_test_models_equal(
        mock,
        modelb,
        ["x", "y"],
        String["c"],
        [("y", MOI.EqualTo{Float64}(1.0)), ("x", MOI.Integer())],
    )
    return
end

function test_double_bounds()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    # These tests run, but don't actually evaluate any `@test`s because of
    # the exclude on `MOI.optimize!`. However, we only really care that the
    # models build without error.
    MOI.Test.runtests(
        MOI.Bridges.Constraint.ZeroOne{Float64}(mock),
        MOI.Test.Config(exclude = Any[MOI.optimize!]);
        include = ["test_constraint_ZeroOne_", "test_variable_solve_ZeroOne_"],
    )
    return
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ZeroOneBridge,
        """
        variables: x
        x in ZeroOne()
        """,
        """
        variables: x
        x in Integer()
        x in Interval(0.0, 1.0)
        """,
    )
    return
end

end  # module

TestConstraintZeroOne.runtests()
