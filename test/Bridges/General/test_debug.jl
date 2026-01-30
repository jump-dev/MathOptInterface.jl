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
 |  may introduce:
 |   * Unsupported objective: MOI.VariableIndex
 |   |  bridged by:
 |   |   MOIB.Objective.FunctionConversionBridge{Float64, MOI.ScalarAffineFunction{Float64}, MOI.VariableIndex}
 |   |  may introduce:
 |   |   * Supported objective: MOI.ScalarAffineFunction{Float64}
 |   * Unsupported constraint: MOI.ScalarQuadraticFunction{Float64}-in-MOI.GreaterThan{Float64}
 |   |  bridged by:
 |   |   MOIB.Constraint.QuadtoSOCBridge{Float64}
 |   |  may introduce:
 |   |   * Supported constraint: MOI.VectorAffineFunction{Float64}-in-MOI.RotatedSecondOrderCone
 |   * Unsupported constraint: MOI.ScalarQuadraticFunction{Float64}-in-MOI.LessThan{Float64}
 |   |  bridged by:
 |   |   MOIB.Constraint.QuadtoSOCBridge{Float64}
 |   |  may introduce:
 |   |   * Supported constraint: MOI.VectorAffineFunction{Float64}-in-MOI.RotatedSecondOrderCone
 |   * Unsupported variable: MOI.Reals
 |   |  bridged by:
 |   |    MOIB.Variable.FreeBridge{Float64}
 |   |  may introduce:
 |   |   * Supported variable: MOI.Nonnegatives
 * Unsupported constraint: MOI.ScalarAffineFunction{Float64}-in-MOI.EqualTo{Float64}
 |  bridged by:
 |   MOIB.Constraint.VectorizeBridge{Float64, MOI.VectorAffineFunction{Float64}, MOI.Zeros, MOI.ScalarAffineFunction{Float64}}
 |  may introduce:
 |   * Supported constraint: MOI.VectorAffineFunction{Float64}-in-MOI.Zeros
 * Unsupported constraint: MOI.ScalarAffineFunction{Float64}-in-MOI.Interval{Float64}
 |  bridged by:
 |   MOIB.Constraint.IntervalToHyperRectangleBridge{Float64, MOI.VectorAffineFunction{Float64}, MOI.ScalarAffineFunction{Float64}}
 |  may introduce:
 |   * Unsupported constraint: MOI.VectorAffineFunction{Float64}-in-MOI.HyperRectangle{Float64}
 |   |  bridged by:
 |   |   MOIB.Constraint.SplitHyperRectangleBridge{Float64, MOI.VectorAffineFunction{Float64}, MOI.VectorAffineFunction{Float64}}
 |   |  may introduce:
 |   |   * Supported constraint: MOI.VectorAffineFunction{Float64}-in-MOI.Nonnegatives
 * Unsupported constraint: MOI.ScalarQuadraticFunction{Float64}-in-MOI.LessThan{Float64}
 |  bridged by:
 |   MOIB.Constraint.QuadtoSOCBridge{Float64}
 |  may introduce:
 |   * Supported constraint: MOI.VectorAffineFunction{Float64}-in-MOI.RotatedSecondOrderCone
 * Unsupported variable: MOI.Nonpositives
 |  bridged by:
 |    MOIB.Variable.NonposToNonnegBridge{Float64}
 |  may introduce:
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
 |  may introduce:
 |   * Supported variable: MOI.EqualTo{Float64}
"""
    return
end

function test_print_active_bridges_objective_supported()
    model = MOI.Bridges.full_bridge_optimizer(Model{Float64}(), Float64)
    F = MOI.ScalarAffineFunction{Float64}
    @test sprint(MOI.Bridges.print_active_bridges, model, F) === """
           * Supported objective: MOI.ScalarAffineFunction{Float64}
          """
    return
end

function test_print_active_bridges_objective_bridged()
    model = MOI.Bridges.full_bridge_optimizer(Model{Float64}(), Float64)
    @test sprint(MOI.Bridges.print_active_bridges, model, MOI.VariableIndex) ===
          """
           * Unsupported objective: MOI.VariableIndex
           |  bridged by:
           |   MOIB.Objective.FunctionConversionBridge{Float64, MOI.ScalarAffineFunction{Float64}, MOI.VariableIndex}
           |  may introduce:
           |   * Supported objective: MOI.ScalarAffineFunction{Float64}
          """
    return
end

function test_print_active_bridges_objective_unsupported()
    model = MOI.Bridges.full_bridge_optimizer(Model{Float64}(), Float64)
    F = MOI.ScalarAffineFunction{Int}
    @test_throws(
        MOI.UnsupportedAttribute{MOI.ObjectiveFunction{F}},
        MOI.Bridges.print_active_bridges(model, F),
    )
    return
end

function test_print_active_bridges_constraint_supported()
    model = MOI.Bridges.full_bridge_optimizer(Model{Float64}(), Float64)
    F = MOI.VectorAffineFunction{Float64}
    S = MOI.Nonnegatives
    @test sprint(MOI.Bridges.print_active_bridges, model, F, S) === """
           * Supported constraint: MOI.VectorAffineFunction{Float64}-in-MOI.Nonnegatives
          """
    return
end

function test_print_active_bridges_constraint_bridged()
    model = MOI.Bridges.full_bridge_optimizer(Model{Float64}(), Float64)
    F = MOI.VectorAffineFunction{Float64}
    S = MOI.Nonpositives
    content = """
     * Unsupported constraint: MOI.VectorAffineFunction{Float64}-in-MOI.Nonpositives
     |  bridged by:
     |   MOIB.Constraint.NonposToNonnegBridge{Float64, MOI.VectorAffineFunction{Float64}, MOI.VectorAffineFunction{Float64}}
     |  may introduce:
     |   * Supported constraint: MOI.VectorAffineFunction{Float64}-in-MOI.Nonnegatives
    """
    @test sprint(MOI.Bridges.print_active_bridges, model, F, S) === content
    return
end

function test_print_active_bridges_constraint_unsupported()
    model = MOI.Bridges.full_bridge_optimizer(Model{Float64}(), Float64)
    F = MOI.ScalarAffineFunction{Int}
    S = MOI.ZeroOne
    @test_throws(
        MOI.UnsupportedConstraint{F,S},
        MOI.Bridges.print_active_bridges(model, F, S),
    )
    return
end

function test_print_active_bridges_variable_supported()
    model = MOI.Bridges.full_bridge_optimizer(Model{Float64}(), Float64)
    S = MOI.Nonnegatives
    @test sprint(MOI.Bridges.print_active_bridges, model, S) === """
           * Supported variable: MOI.Nonnegatives
          """
    return
end

function test_print_active_bridges_variable_bridged()
    model = MOI.Bridges.full_bridge_optimizer(Model{Float64}(), Float64)
    S = MOI.Reals
    content = """
     * Unsupported variable: MOI.Reals
     |  bridged by:
     |    MOIB.Variable.FreeBridge{Float64}
     |  may introduce:
     |   * Supported variable: MOI.Nonnegatives
    """
    @test sprint(MOI.Bridges.print_active_bridges, model, S) === content
    return
end

function test_print_active_bridges_variable_unsupported()
    model = MOI.Bridges.full_bridge_optimizer(Model{Float64}(), Float64)
    @test_throws(
        MOI.UnsupportedConstraint{MOI.VariableIndex,MOI.ZeroOne},
        MOI.Bridges.print_active_bridges(model, MOI.ZeroOne),
    )
    @test_throws(
        MOI.UnsupportedConstraint{MOI.VectorOfVariables,MOI.ExponentialCone},
        MOI.Bridges.print_active_bridges(model, MOI.ExponentialCone),
    )
    return
end

MOI.Utilities.@model(
    ConstraintModel,
    (MOI.ZeroOne, MOI.Integer),
    (MOI.GreaterThan, MOI.LessThan, MOI.EqualTo),
    (),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (),
    ()
)

function test_print_active_bridges_variable_bridged_with_constraint()
    model =
        MOI.Bridges.full_bridge_optimizer(ConstraintModel{Float64}(), Float64)
    content = """
     * Unsupported variable: MOI.AllDifferent
     |  adding as constraint:
     |   * Supported variable: MOI.Reals
     |   * Unsupported constraint: MOI.VectorOfVariables-in-MOI.AllDifferent
     |   |  bridged by:
     |   |   MOIB.Constraint.AllDifferentToCountDistinctBridge{Float64, MOI.VectorOfVariables}
     |   |  may introduce:
     |   |   * Unsupported constraint: MOI.VectorOfVariables-in-MOI.CountDistinct
     |   |   |  bridged by:
     |   |   |   MOIB.Constraint.CountDistinctToMILPBridge{Float64, MOI.VectorOfVariables}
     |   |   |  may introduce:
     |   |   |   * Supported constraint: MOI.ScalarAffineFunction{Float64}-in-MOI.EqualTo{Float64}
     |   |   |   * Supported constraint: MOI.ScalarAffineFunction{Float64}-in-MOI.LessThan{Float64}
     |   |   |   * Supported variable: MOI.ZeroOne
     |   |   * Supported variable: MOI.EqualTo{Float64}
    """
    S = MOI.AllDifferent
    @test sprint(MOI.Bridges.print_active_bridges, model, S) === content
    return
end

function test_print_graph_stdout()
    model = MOI.Utilities.Model{Float64}()
    bridged = MOI.Bridges.full_bridge_optimizer(model, Float64)
    dir = mktempdir()
    filename = joinpath(dir, "tmp.out")
    open(filename, "w") do io
        redirect_stdout(io) do
            MOI.Bridges.print_graph(bridged)
            return
        end
        return
    end
    @test read(filename, String) ==
          "Bridge graph with 0 variable nodes, 0 constraint nodes and 0 objective nodes.\n"
    return
end

end

TestBridgesDebug.runtests()
