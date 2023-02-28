# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestBridgesDebug

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

# This model is chosen because it has a number of properties that need bridging.

MOI.Utilities.@model(
    Model,
    (),
    (),
    (MOI.Reals, MOI.Zeros, MOI.Nonnegatives, MOI.RotatedSecondOrderCone),
    (),
    (),
    (),
    (MOI.VectorOfVariables,),
    (MOI.VectorAffineFunction,)
)

function MOI.supports_constraint(
    ::Model{T},
    ::Type{MOI.VariableIndex},
    ::Type{<:MOI.Utilities.SUPPORTED_VARIABLE_SCALAR_SETS{T}},
) where {T}
    return false
end

MOI.supports(::Model, ::MOI.ObjectiveFunction{MOI.VariableIndex}) = false

function MOI.supports(
    ::Model{T},
    ::MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}},
) where {T}
    return false
end

MOI.supports_add_constrained_variables(::Model, ::Type{MOI.Reals}) = false

MOI.supports_add_constrained_variables(::Model, ::Type{MOI.Nonnegatives}) = true

function test_print_active_bridges()
    model = MOI.Bridges.full_bridge_optimizer(Model{Float64}(), Float64)
    xs, _ = MOI.add_constrained_variables(model, MOI.Nonpositives(2))
    x = xs[1]
    MOI.add_constraint.(model, 1.0 * x, MOI.Interval(1.0, 2.0))
    MOI.add_constraint.(model, 1.0 * x, MOI.EqualTo(1.4))
    MOI.add_constraint.(model, 1.0 * x * x, MOI.LessThan(2.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = 1.0 * x * x
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    content = """
 * Unsupported objective: MOI.ScalarQuadraticFunction{Float64}
 |  bridged by:
 |   MOIB.Objective.SlackBridge{Float64, MOI.ScalarQuadraticFunction{Float64}, MOI.ScalarQuadraticFunction{Float64}}
 |  introduces:
 |   * Unsupported objective: MOI.VariableIndex
 |   |  bridged by:
 |   |   MOIB.Objective.FunctionizeBridge{Float64}
 |   |  introduces:
 |   |   * Supported objective: MOI.ScalarAffineFunction{Float64}
 |   * Unsupported constraint: MOI.ScalarQuadraticFunction{Float64}-in-MOI.LessThan{Float64}
 |   |  bridged by:
 |   |   MOIB.Constraint.QuadtoSOCBridge{Float64}
 |   |  introduces:
 |   |   * Supported constraint: MOI.VectorAffineFunction{Float64}-in-MOI.RotatedSecondOrderCone
 |   * Unsupported variable: MOI.Reals
 |   |  bridged by:
 |   |    MOIB.Variable.FreeBridge{Float64}
 |   |  introduces:
 |   |   * Supported variable: MOI.Nonnegatives
 * Unsupported constraint: MOI.ScalarAffineFunction{Float64}-in-MOI.EqualTo{Float64}
 |  bridged by:
 |   MOIB.Constraint.VectorizeBridge{Float64, MOI.VectorAffineFunction{Float64}, MOI.Zeros, MOI.ScalarAffineFunction{Float64}}
 |  introduces:
 |   * Supported constraint: MOI.VectorAffineFunction{Float64}-in-MOI.Zeros
 * Unsupported constraint: MOI.ScalarAffineFunction{Float64}-in-MOI.Interval{Float64}
 |  bridged by:
 |   MOIB.Constraint.SplitIntervalBridge{Float64, MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}, MOI.GreaterThan{Float64}, MOI.LessThan{Float64}}
 |  introduces:
 |   * Unsupported constraint: MOI.ScalarAffineFunction{Float64}-in-MOI.GreaterThan{Float64}
 |   |  bridged by:
 |   |   MOIB.Constraint.VectorizeBridge{Float64, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives, MOI.ScalarAffineFunction{Float64}}
 |   |  introduces:
 |   |   * Supported constraint: MOI.VectorAffineFunction{Float64}-in-MOI.Nonnegatives
 |   * Unsupported constraint: MOI.ScalarAffineFunction{Float64}-in-MOI.LessThan{Float64}
 |   |  bridged by:
 |   |   MOIB.Constraint.VectorizeBridge{Float64, MOI.VectorAffineFunction{Float64}, MOI.Nonpositives, MOI.ScalarAffineFunction{Float64}}
 |   |  introduces:
 |   |   * Unsupported constraint: MOI.VectorAffineFunction{Float64}-in-MOI.Nonpositives
 |   |   |  bridged by:
 |   |   |   MOIB.Constraint.NonposToNonnegBridge{Float64, MOI.VectorAffineFunction{Float64}, MOI.VectorAffineFunction{Float64}}
 |   |   |  introduces:
 |   |   |   * Supported constraint: MOI.VectorAffineFunction{Float64}-in-MOI.Nonnegatives
 * Unsupported constraint: MOI.ScalarQuadraticFunction{Float64}-in-MOI.LessThan{Float64}
 |  bridged by:
 |   MOIB.Constraint.QuadtoSOCBridge{Float64}
 |  introduces:
 |   * Supported constraint: MOI.VectorAffineFunction{Float64}-in-MOI.RotatedSecondOrderCone
 * Unsupported variable: MOI.Nonpositives
 |  bridged by:
 |    MOIB.Variable.NonposToNonnegBridge{Float64}
 |  introduces:
 |   * Supported variable: MOI.Nonnegatives
"""
    # Prints to stdout, but just check it doesn't error.
    mktemp() do _, io
        redirect_stdout(io) do
            return MOI.Bridges.print_active_bridges(model)
        end
        seekstart(io)
        @test read(io, String) == content
        return
    end
    @test sprint(MOI.Bridges.print_active_bridges, model) == content
    return
end

MOI.Utilities.@model(
    ParameterModel,
    (),
    (MOI.EqualTo,),
    (),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (),
    ()
)

function MOI.supports_constraint(
    ::ParameterModel,
    ::Type{MOI.VariableIndex},
    ::Type{<:MOI.Parameter},
)
    return false
end

function test_print_active_bridges_parameter()
    inner = ParameterModel{Float64}()
    model = MOI.Bridges.full_bridge_optimizer(inner, Float64)
    p, _ = MOI.add_constrained_variable(model, MOI.Parameter(2.5))
    @test sprint(MOI.Bridges.print_active_bridges, model) === """
 * Supported objective: MOI.ScalarAffineFunction{Float64}
 * Unsupported variable: MOI.Parameter{Float64}
 |  bridged by:
 |    MOIB.Variable.ParameterToEqualToBridge{Float64}
 |  introduces:
 |   * Supported variable: MOI.EqualTo{Float64}
"""
    return
end

end

TestBridgesDebug.runtests()
