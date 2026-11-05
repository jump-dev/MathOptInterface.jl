# Copyright (c) 2026: jump-dev contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestBridgeContext

using Test
import MathOptInterface as MOI

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith(string(name), "test_")
            @testset "$name" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

struct InternalValidity <: MOI.AbstractConstraintAttribute end
struct ReplaceBounds <: MOI.AbstractConstraintAttribute end
struct ThrowFromBridge <: MOI.AbstractConstraintAttribute end

# This bridge changes its native children in final_touch and in a setter.
# Counting its list queries detects accidental scans from is_valid.
mutable struct TrackingBridge <: MOI.Bridges.Constraint.AbstractBridge
    variables::Vector{MOI.VariableIndex}
    constraints::Vector{
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.GreaterThan{Float64}},
    }
    list_queries::Int
end

function MOI.supports_constraint(
    ::Type{TrackingBridge},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.Nonnegatives},
)
    return true
end

function MOI.Bridges.Constraint.concrete_bridge_type(
    ::Type{TrackingBridge},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.Nonnegatives},
)
    return TrackingBridge
end

function MOI.Bridges.added_constraint_types(::Type{TrackingBridge})
    return Tuple{Type,Type}[(MOI.VariableIndex, MOI.GreaterThan{Float64})]
end

function MOI.Bridges.added_constrained_variable_types(::Type{TrackingBridge})
    return Tuple{Type}[]
end

function MOI.Bridges.Constraint.bridge_constraint(
    ::Type{TrackingBridge},
    model::MOI.ModelLike,
    f::MOI.VectorOfVariables,
    ::MOI.Nonnegatives,
)
    if isempty(f.variables)
        error("bridge construction failed")
    end
    constraints = MOI.add_constraints(
        model,
        f.variables,
        fill(MOI.GreaterThan(0.0), length(f.variables)),
    )
    return TrackingBridge(copy(f.variables), constraints, 0)
end

function MOI.get(
    bridge::TrackingBridge,
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.GreaterThan{Float64}},
)
    bridge.list_queries += 1
    return copy(bridge.constraints)
end

function MOI.get(
    model::MOI.ModelLike,
    ::InternalValidity,
    bridge::TrackingBridge,
)
    return all(ci -> MOI.is_valid(model, ci), bridge.constraints)
end

function MOI.get(::MOI.ModelLike, ::ThrowFromBridge, ::TrackingBridge)
    return error("bridge query failed")
end

function MOI.set(
    model::MOI.ModelLike,
    ::ReplaceBounds,
    bridge::TrackingBridge,
    value::Float64,
)
    MOI.delete(model, bridge.constraints)
    bridge.constraints = MOI.add_constraints(
        model,
        bridge.variables,
        fill(MOI.GreaterThan(value), length(bridge.variables)),
    )
    return
end

function MOI.set(
    model::MOI.ModelLike,
    ::ThrowFromBridge,
    bridge::TrackingBridge,
    x::MOI.VariableIndex,
)
    push!(
        bridge.constraints,
        MOI.add_constraint(model, x, MOI.GreaterThan(3.0)),
    )
    return error("bridge update failed")
end

function MOI.delete(model::MOI.ModelLike, bridge::TrackingBridge)
    MOI.delete(model, bridge.constraints)
    return
end

MOI.Bridges.needs_final_touch(::TrackingBridge) = true

function MOI.Bridges.final_touch(bridge::TrackingBridge, model::MOI.ModelLike)
    MOI.set(model, ReplaceBounds(), bridge, 1.0)
    return
end

function test_updates_and_recursive_access()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Constraint.SingleBridgeOptimizer{TrackingBridge}(inner)
    @test_throws ErrorException("bridge construction failed") MOI.add_constraint(
        model,
        MOI.VectorOfVariables(MOI.VariableIndex[]),
        MOI.Nonnegatives(0),
    )
    @test model.bridge_context.depth == 0
    @test model.bridge_context.constraints === nothing
    x = MOI.add_variables(model, 2)
    parent =
        MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.Nonnegatives(2))
    bridge = MOI.Bridges.bridge(model, parent)
    @test all(ci -> MOI.is_valid(inner, ci), bridge.constraints)
    @test all(ci -> !MOI.is_valid(model, ci), bridge.constraints)
    @test MOI.get(model, InternalValidity(), parent)
    @test MOI.Bridges.call_in_context(
        model,
        parent,
        _ -> all(ci -> MOI.is_valid(model, ci), bridge.constraints),
    )
    queries = bridge.list_queries
    @test all(_ -> !MOI.is_valid(model, bridge.constraints[1]), 1:1000)
    @test bridge.list_queries == queries
    @test_throws MOI.InvalidIndex(bridge.constraints[1]) MOI.delete(
        model,
        bridge.constraints[1],
    )
    @test_throws MOI.InvalidIndex(bridge.constraints[1]) MOI.delete(
        model,
        bridge.constraints,
    )
    @test MOI.get(model, InternalValidity(), parent)
    MOI.set(model, ReplaceBounds(), parent, 2.0)
    @test all(ci -> !MOI.is_valid(model, ci), bridge.constraints)
    @test MOI.get(model, InternalValidity(), parent)
    MOI.Bridges.final_touch(model)
    @test MOI.get(model, InternalValidity(), parent)
    @test all(ci -> !MOI.is_valid(model, ci), bridge.constraints)
    @test_throws ErrorException("bridge query failed") MOI.get(
        model,
        ThrowFromBridge(),
        parent,
    )
    @test model.bridge_context.depth == 0
    @test all(ci -> !MOI.is_valid(model, ci), bridge.constraints)
    z = MOI.add_variable(model)
    @test_throws ErrorException("bridge update failed") MOI.set(
        model,
        ThrowFromBridge(),
        parent,
        z,
    )
    @test model.bridge_context.depth == 0
    @test MOI.get(model, InternalValidity(), parent)
    @test !MOI.is_valid(model, last(bridge.constraints))
    MOI.delete(model, parent)
    @test model.bridge_context.constraints === nothing
    return
end

function test_sparse_registry()
    model = MOI.Bridges.LazyBridgeOptimizer(MOI.Utilities.Model{Float64}())
    x = MOI.add_variables(model, 1000)
    bounds =
        MOI.add_constraints(model, x, fill(MOI.GreaterThan(0.0), length(x)))
    @test model.bridge_context.constraints === nothing
    @test all(ci -> MOI.is_valid(model, ci), bounds)
    @test model.bridge_context.constraints === nothing
    MOI.Bridges.add_bridge(model, MOI.Bridges.Constraint.ZeroOneBridge{Float64})
    # A single-bridge wrapper forces ZeroOne even though the inner model
    # supports it, so this also checks ownership in stacked wrappers.
    outer = MOI.Bridges.Constraint.ZeroOne{Float64}(model)
    y = MOI.add_variable(outer)
    binary = MOI.add_constraint(outer, y, MOI.ZeroOne())
    registry = outer.bridge_context.constraints
    @test sum(length, values(registry)) == 2
    @test model.bridge_context.constraints === nothing
    @test all(ci -> MOI.is_valid(outer, ci), bounds)
    @test sum(length, values(registry)) == 2
    integer = MOI.ConstraintIndex{MOI.VariableIndex,MOI.Integer}(y.value)
    @test !MOI.is_valid(outer, integer)
    MOI.delete(outer, binary)
    @test outer.bridge_context.constraints === nothing
    MOI.add_constraint(outer, y, MOI.ZeroOne())
    MOI.empty!(outer)
    @test outer.bridge_context.constraints === nothing
    @test model.bridge_context.constraints === nothing
    return
end

end

TestBridgeContext.runtests()
