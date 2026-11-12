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
    bridge::TrackingBridge,
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.GreaterThan{Float64}},
)
    attr =
        MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.GreaterThan{Float64}}()
    return length(MOI.get(bridge, attr))
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

MOI.Utilities.@model(
    BoundsModel,
    (),
    (MOI.GreaterThan,),
    (),
    (),
    (),
    (),
    (),
    ()
)

function test_constraint_lists_without_bridge_queries()
    for lazy in (false, true)
        inner = BoundsModel{Float64}()
        model = if lazy
            b = MOI.Bridges.LazyBridgeOptimizer(inner)
            MOI.Bridges.add_bridge(b, TrackingBridge)
            b
        else
            MOI.Bridges.Constraint.SingleBridgeOptimizer{TrackingBridge}(inner)
        end
        x = MOI.add_variables(model, 3)
        F, S = MOI.VariableIndex, MOI.GreaterThan{Float64}
        attr = MOI.ListOfConstraintIndices{F,S}()
        count_attr = MOI.NumberOfConstraints{F,S}()
        user = MOI.add_constraint(model, x[3], MOI.GreaterThan(0.0))
        @test MOI.get(model, attr) == [user]
        @test MOI.get(model, count_attr) == 1
        @test model.bridge_context.constraints === nothing
        parent = MOI.add_constraint(
            model,
            MOI.VectorOfVariables(x[1:2]),
            MOI.Nonnegatives(2),
        )
        bridge = MOI.Bridges.bridge(model, parent)
        queries = bridge.list_queries
        @test MOI.get(model, attr) == [user]
        @test MOI.get(model, count_attr) == 1
        @test MOI.Bridges.call_in_context(
            model,
            parent,
            _ -> MOI.get(model, attr),
        ) == [user]
        empty_attr = MOI.ListOfConstraintIndices{F,MOI.EqualTo{Float64}}()
        @test isempty(MOI.get(model, empty_attr))
        parent_attr =
            MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Nonnegatives}()
        @test MOI.get(model, parent_attr) == [parent]
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonnegatives}(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.EqualTo{Float64}}(),
        ) == 0
        @test Set(MOI.get(model, MOI.ListOfConstraintTypesPresent())) ==
              Set([(F, S), (MOI.VectorOfVariables, MOI.Nonnegatives)])
        @test bridge.list_queries == queries
        # Refreshing children must keep the list filter in sync.
        MOI.set(model, ReplaceBounds(), parent, 2.0)
        queries = bridge.list_queries
        @test MOI.get(model, attr) == [user]
        @test MOI.get(model, count_attr) == 1
        @test bridge.list_queries == queries
        MOI.delete(model, parent)
        @test MOI.get(model, attr) == [user]
        @test MOI.get(model, count_attr) == 1
        @test model.bridge_context.constraints === nothing
    end
    return
end

struct UntrackedOptimizer{M} <: MOI.Bridges.AbstractBridgeOptimizer
    model::M
    bridges::NTuple{3,Vector{TrackingBridge}}
end

MOI.Bridges.Variable.bridges(b::UntrackedOptimizer) = b.bridges[1]
MOI.Bridges.Constraint.bridges(b::UntrackedOptimizer) = b.bridges[2]
MOI.Bridges.Objective.bridges(b::UntrackedOptimizer) = b.bridges[3]
MOI.Bridges.is_bridged(::UntrackedOptimizer, ::Type, ::Type) = false
MOI.Bridges.is_bridged(::UntrackedOptimizer, ::Type) = false
MOI.Bridges.supports_constraint_bridges(::UntrackedOptimizer) = false

function test_constraint_list_fallback()
    inner = BoundsModel{Float64}()
    x = MOI.add_variables(inner, 4)
    bounds = MOI.add_constraints(inner, x, fill(MOI.GreaterThan(0.0), 4))
    bridges = ntuple(i -> [TrackingBridge([x[i]], [bounds[i]], 0)], 3)
    model = UntrackedOptimizer(inner, bridges)
    list = copy(bounds)
    attr =
        MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.GreaterThan{Float64}}()
    MOI.Bridges._remove_bridge_constraints!(model, list, attr)
    @test list == bounds[4:4]
    @test MOI.get(
        model,
        MOI.NumberOfConstraints{MOI.VariableIndex,MOI.GreaterThan{Float64}}(),
    ) == 1
    @test all(b -> only(b).list_queries == 2, bridges)
    return
end

function test_context_fallback()
    model = UntrackedOptimizer(
        BoundsModel{Float64}(),
        (TrackingBridge[], TrackingBridge[], TrackingBridge[]),
    )
    calls = Ref(0)
    result = MOI.Bridges._in_bridge_context(model) do
        calls[] += 1
        return :result
    end
    @test result === :result
    @test calls[] == 1
    @test_throws ErrorException("callback failed") MOI.Bridges._in_bridge_context(
        model,
    ) do
        error("callback failed")
    end
    return
end

MOI.Utilities.@model(
    VariableObjectiveModel,
    (),
    (MOI.GreaterThan, MOI.LessThan),
    (MOI.Nonnegatives,),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (MOI.VectorOfVariables,),
    ()
)

function MOI.supports(
    ::VariableObjectiveModel,
    ::MOI.ObjectiveFunction{F},
) where {F<:MOI.AbstractFunction}
    return F == MOI.VariableIndex
end

function MOI.supports_constraint(
    ::VariableObjectiveModel{T},
    ::Type{MOI.VariableIndex},
    ::Type{MOI.GreaterThan{T}},
) where {T}
    return false
end

function test_modify_bridged_objective_constant_error()
    inner = VariableObjectiveModel{Float64}()
    model = MOI.Bridges.LazyBridgeOptimizer(inner)
    MOI.Bridges.add_bridge(model, MOI.Bridges.Variable.VectorizeBridge{Float64})
    MOI.Bridges.add_bridge(model, MOI.Bridges.Objective.SlackBridge{Float64})
    x, _ = MOI.add_constrained_variable(model, MOI.GreaterThan(0.0))
    f = 2.0 * x + 3.0
    attr = MOI.ObjectiveFunction{typeof(f)}()
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, attr, f)
    bridge = MOI.Bridges.bridge(model, attr)
    child_attr = MOI.ListOfConstraintIndices{typeof(f),MOI.LessThan{Float64}}()
    child = only(MOI.get(bridge, child_attr))
    @test !MOI.is_valid(model, child)
    @test MOI.get(model, attr) ≈ f
    children = deepcopy(model.bridge_context.constraints)
    @test_throws MOI.Bridges.ModifyBridgeNotAllowed MOI.modify(
        model,
        attr,
        MOI.ScalarConstantChange(5.0),
    )
    @test model.bridge_context.constraints == children
    @test !MOI.is_valid(model, child)
    @test model.bridge_context.depth == 0
    return
end

# Some solvers support native deletion without implementing is_valid.
struct DeleteOnlyModel{M} <: MOI.AbstractOptimizer
    model::M
end

MOI.add_variable(m::DeleteOnlyModel) = MOI.add_variable(m.model)

function MOI.supports_constraint(
    m::DeleteOnlyModel,
    ::Type{F},
    ::Type{S},
) where {F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    if S <: MOI.LessThan
        return MOI.supports_constraint(m.model, F, S)
    end
    return false
end

function MOI.add_constraint(
    m::DeleteOnlyModel,
    f::MOI.AbstractFunction,
    s::MOI.AbstractSet,
)
    return MOI.add_constraint(m.model, f, s)
end

function MOI.delete(m::DeleteOnlyModel, ci::MOI.ConstraintIndex)
    return MOI.delete(m.model, ci)
end

function test_native_delete_without_validity()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.LazyBridgeOptimizer(DeleteOnlyModel(inner))
    MOI.Bridges.add_bridge(
        model,
        MOI.Bridges.Constraint.GreaterToLessBridge{Float64},
    )
    x = MOI.add_variable(model)
    native = MOI.add_constraint(model, 1.0 * x, MOI.LessThan(1.0))
    @test !MOI.is_valid(model, native)
    MOI.delete(model, native)
    @test !MOI.is_valid(inner, native)
    parent = MOI.add_constraint(model, 2.0 * x, MOI.GreaterThan(0.0))
    bridge = MOI.Bridges.bridge(model, parent)
    attr = MOI.ListOfConstraintIndices{
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    }()
    child = only(MOI.get(bridge, attr))
    @test MOI.is_valid(inner, child)
    @test_throws MOI.InvalidIndex(child) MOI.delete(model, child)
    MOI.delete(model, parent)
    @test !MOI.is_valid(inner, child)
    @test model.bridge_context.constraints === nothing
    return
end

function MOI.supports(
    model::MOI.Utilities.UniversalFallback{<:VariableObjectiveModel},
    attr::MOI.ObjectiveFunction{F},
) where {F<:MOI.AbstractFunction}
    return MOI.supports(model.model, attr)
end

function MOI.supports_constraint(
    ::MOI.Utilities.UniversalFallback{<:VariableObjectiveModel},
    ::Type{MOI.ScalarAffineFunction{Float64}},
    ::Type{MOI.LessThan{Float64}},
)
    return false
end

function test_objective_start_in_bridge_context()
    inner = MOI.Utilities.UniversalFallback(VariableObjectiveModel{Float64}())
    model = MOI.Bridges.LazyBridgeOptimizer(inner)
    MOI.Bridges.add_bridge(model, MOI.Bridges.Objective.SlackBridge{Float64})
    MOI.Bridges.add_bridge(
        model,
        MOI.Bridges.Constraint.LessToGreaterBridge{Float64},
    )
    x = MOI.add_variable(model)
    f = 2.0 * x + 3.0
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    attr = MOI.ObjectiveFunction{typeof(f)}()
    MOI.set(model, attr, f)
    bridge = MOI.Bridges.bridge(model, attr)
    child_attr = MOI.ListOfConstraintIndices{typeof(f),MOI.LessThan{Float64}}()
    child = only(MOI.get(bridge, child_attr))
    @test !MOI.is_valid(model, child)
    @test MOI.Bridges.call_in_context(
        model,
        attr,
        b -> (b === bridge, MOI.is_valid(model, child)),
    ) == (true, true)
    @test_throws ErrorException("objective callback failed") MOI.Bridges.call_in_context(
        model,
        attr,
        _ -> error("objective callback failed"),
    )
    @test model.bridge_context.depth == 0
    @test MOI.Bridges.is_bridged(model, child)
    @test_throws MOI.InvalidIndex(child) MOI.set(
        model,
        MOI.ConstraintDualStart(),
        child,
        -1.0,
    )
    child_bridge = MOI.Bridges.bridge(model, child)
    native_attr =
        MOI.ListOfConstraintIndices{typeof(f),MOI.GreaterThan{Float64}}()
    native_child = only(MOI.get(child_bridge, native_attr))
    MOI.set(model, MOI.VariablePrimalStart(), x, 4.0)
    MOI.set(model, MOI.Bridges.Objective.SlackBridgePrimalDualStart(), nothing)
    @test MOI.get(inner, MOI.VariablePrimalStart(), bridge.slack) == 11.0
    @test MOI.get(inner, MOI.ConstraintPrimalStart(), native_child) == 0.0
    @test MOI.get(inner, MOI.ConstraintDualStart(), native_child) == 1.0
    @test !MOI.is_valid(model, child)
    @test model.bridge_context.depth == 0
    return
end

end

TestBridgeContext.runtests()
