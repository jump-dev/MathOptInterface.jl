# Copyright (c) 2026: jump-dev contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Only bridge-created constraints are recorded. Native models do not allocate
# a registry, and adding a bridge never enumerates existing user constraints.
mutable struct BridgeContext
    # Recursive bridge calls retain access to the underlying constraints.
    depth::Int
    # Reference counts also handle indices reported by multiple bridges.
    constraints::Union{Nothing,Dict{Tuple{Type,Type},Dict{Int64,Int}}}
end

BridgeContext() = BridgeContext(0, nothing)

bridge_context(::AbstractBridgeOptimizer) = nothing

function _bridge_constraint_indices(bridge::AbstractBridge)
    types = copy(added_constraint_types(typeof(bridge)))
    for (S,) in added_constrained_variable_types(typeof(bridge))
        push!(types, (MOI.Utilities.variable_function_type(S), S))
    end
    indices = MOI.ConstraintIndex[]
    for (F, S) in unique(types)
        append!(indices, MOI.get(bridge, MOI.ListOfConstraintIndices{F,S}()))
    end
    return indices
end

function _register_bridge_constraints!(context::BridgeContext, indices, delta)
    for ci in indices
        if context.constraints === nothing
            context.constraints = Dict{Tuple{Type,Type},Dict{Int64,Int}}()
        end
        _register_bridge_constraint!(something(context.constraints), ci, delta)
    end
    if context.constraints !== nothing && isempty(context.constraints)
        context.constraints = nothing
    end
    return
end

function _register_bridge_constraint!(
    constraints,
    ci::MOI.ConstraintIndex{F,S},
    delta,
) where {F,S}
    indices = get!(Dict{Int64,Int}, constraints, (F, S))
    count = get(indices, ci.value, 0) + delta
    if count == 0
        delete!(indices, ci.value)
        if isempty(indices)
            delete!(constraints, (F, S))
        end
    else
        @assert count > 0
        indices[ci.value] = count
    end
    return
end

function _in_bridge_context(f::F, b::AbstractBridgeOptimizer) where {F}
    context = bridge_context(b)
    if context === nothing
        return f()
    end
    previous = context.depth
    context.depth = previous + 1
    try
        return f()
    finally
        context.depth = previous
    end
end

function _create_bridge(f::F, b::AbstractBridgeOptimizer) where {F}
    bridge = _in_bridge_context(f, b)
    context = bridge_context(b)
    if context !== nothing
        _register_bridge_constraints!(
            context,
            _bridge_constraint_indices(bridge),
            1,
        )
    end
    return bridge
end

# Reads enter the context without querying child lists. Mutations refresh
# only this bridge's children, including when the bridge operation throws.
function _with_bridge(
    f::F,
    b::AbstractBridgeOptimizer,
    bridge::AbstractBridge;
    update::Bool = false,
    deleted::Bool = false,
) where {F}
    context = bridge_context(b)
    before =
        update && context !== nothing ? _bridge_constraint_indices(bridge) :
        nothing
    completed = false
    try
        result = _in_bridge_context(f, b)
        completed = true
        return result
    finally
        if before !== nothing
            _register_bridge_constraints!(context, before, -1)
            if !deleted || !completed
                _register_bridge_constraints!(
                    context,
                    _bridge_constraint_indices(bridge),
                    1,
                )
            end
        end
    end
end

function _is_hidden(b::AbstractBridgeOptimizer, ci::MOI.ConstraintIndex)
    context = bridge_context(b)
    if context === nothing ||
       context.depth > 0 ||
       context.constraints === nothing
        return false
    end
    index = ci
    if recursive_model(b) !== b
        # A single-bridge wrapper's public bridge keys and its inner native
        # indices are different namespaces, even when their values coincide.
        if is_bridged(b, ci)
            return false
        end
        index = _inner_index_or_nothing(b, ci)
    end
    if index === nothing
        return false
    end
    return _is_hidden(something(context.constraints), index)
end

function _is_hidden(constraints, ci::MOI.ConstraintIndex{F,S}) where {F,S}
    indices = get(constraints, (F, S), nothing)
    return indices !== nothing && haskey(indices, ci.value)
end
