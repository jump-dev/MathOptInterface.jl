# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintToInterval
# These tests are mostly copies of the flip_sign.jl tests for GreaterToLess

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

function test_GreaterToInterval()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.GreaterToInterval{Float64}(mock)
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_$(F)_GreaterThanThan" for F in
            ["VariableIndex", "ScalarAffineFunction", "ScalarQuadraticFunction"]
        ],
    )
    MOI.empty!(bridged_mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [0.0, 0.0]),
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [100.0, 0.0]),
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [100.0, -100.0]),
    )
    MOI.Test.test_linear_modify_GreaterThan_and_LessThan_constraints(
        bridged_mock,
        config,
    )
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{Float64},
                MOI.GreaterThan{Float64},
            }(),
        ),
    )
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        MOI.set(bridged_mock, attr, ci, 2.0)
        @test MOI.get(bridged_mock, attr, ci) ≈ 2.0
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        ((MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}, 0),),
    )
    return
end

function test_LessToInterval()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.LessToInterval{Float64}(mock)
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_$(F)_LessThan" for F in
            ["VariableIndex", "ScalarAffineFunction", "ScalarQuadraticFunction"]
        ],
    )
    MOI.empty!(bridged_mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [-1.0],
        ),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [2.0]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [-1.0],
        ),
    )
    MOI.Test.test_modification_set_scalaraffine_lessthan(bridged_mock, config)
    MOI.empty!(bridged_mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [-1.0],
        ),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.5]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [-0.5],
        ),
    )
    MOI.Test.test_modification_coef_scalaraffine_lessthan(bridged_mock, config)
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{Float64},
                MOI.LessThan{Float64},
            }(),
        ),
    )
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        MOI.set(bridged_mock, attr, ci, 2.0)
        @test MOI.get(bridged_mock, attr, ci) ≈ 2.0
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        1,
        ((MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}, 0),),
    )
    return
end

# Define a dummy optimizer that only supports intervals
# and use it in the below unmocked test
mutable struct Optimizer <: MOI.AbstractOptimizer end

MOI.get(model::Optimizer, ::MOI.SolverName) = "OnlyIntervalOptimizer"
MOI.get(model::Optimizer, ::MOI.SolverVersion) = "v0.1.0"

function MOI.supports_constraint(
    ::Optimizer,
    ::Type{MOI.ScalarAffineFunction{Float64}},
    ::Type{MOI.Interval{Float64}},
)
    return true
end

function test_GreaterOrLessToInterval_unmocked()
    # model supports Interval but not LessThan or GreaterThan
    model = Optimizer()
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.Interval{Float64},
    )
    @test !MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    @test !MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.GreaterThan{Float64},
    )

    # bridged model supports all
    bridged = MOI.Bridges.Constraint.GreaterToInterval{Float64}(
        MOI.Bridges.Constraint.LessToInterval{Float64}(model),
    )
    @test MOI.supports_constraint(
        bridged,
        MOI.ScalarAffineFunction{Float64},
        MOI.Interval{Float64},
    )
    @test MOI.supports_constraint(
        bridged,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    @test MOI.supports_constraint(
        bridged,
        MOI.ScalarAffineFunction{Float64},
        MOI.GreaterThan{Float64},
    )

    # bridged model with Bridges.full_bridge_optimizer
    bridged2 = MOI.Bridges.full_bridge_optimizer(model, Float64)
    @test MOI.supports_constraint(
        bridged2,
        MOI.ScalarAffineFunction{Float64},
        MOI.Interval{Float64},
    )
    @test MOI.supports_constraint(
        bridged2,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    @test MOI.supports_constraint(
        bridged2,
        MOI.ScalarAffineFunction{Float64},
        MOI.GreaterThan{Float64},
    )
    return
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.GreaterToIntervalBridge,
        """
        variables: x
        x >= 1.0
        """,
        """
        variables: x
        x in Interval(1.0, Inf)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.GreaterToIntervalBridge,
        """
        variables: x
        2.0 * x >= 1.0
        """,
        """
        variables: x
        2.0 * x in Interval(1.0, Inf)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.LessToIntervalBridge,
        """
        variables: x
        x <= 1.0
        """,
        """
        variables: x
        x in Interval(-Inf, 1.0)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.LessToIntervalBridge,
        """
        variables: x
        2.0 * x <= 1.0
        """,
        """
        variables: x
        2.0 * x in Interval(-Inf, 1.0)
        """,
    )
    return
end

end  # module

TestConstraintToInterval.runtests()
