# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

include("graph.jl")

"""
    LazyBridgeOptimizer(model::MOI.ModelLike)

The `LazyBridgeOptimizer` is a bridge optimizer that supports multiple bridges,
and only bridges things which are not supported by the internal model.

Internally, the `LazyBridgeOptimizer` solves a shortest hyper-path problem to
determine which bridges to use.

In general, you should use [`full_bridge_optimizer`](@ref) instead of this
constructor because [`full_bridge_optimizer`](@ref) automatically adds a large
number of supported bridges.

See also: [`add_bridge`](@ref), [`remove_bridge`](@ref), [`has_bridge`](@ref)
and [`full_bridge_optimizer`](@ref).

## Example

```jldoctest
julia> model = MOI.Bridges.LazyBridgeOptimizer(MOI.Utilities.Model{Float64}());

julia> MOI.Bridges.add_bridge(model, MOI.Bridges.Variable.FreeBridge{Float64})

julia> MOI.Bridges.has_bridge(model, MOI.Bridges.Variable.FreeBridge{Float64})
true
```
"""
mutable struct LazyBridgeOptimizer{OT<:MOI.ModelLike} <: AbstractBridgeOptimizer
    # Internal model
    model::OT
    # Bridged variables
    variable_map::Variable.Map
    var_to_name::Dict{MOI.VariableIndex,String}
    name_to_var::Union{Dict{String,MOI.VariableIndex},Nothing}
    # Bridged constraints
    constraint_map::Constraint.Map
    con_to_name::Dict{MOI.ConstraintIndex,String}
    name_to_con::Union{Dict{String,MOI.ConstraintIndex},Nothing}
    # Bridged objective
    objective_map::Objective.Map
    # Bellman-Ford graph
    graph::Graph
    # List of types of available bridges
    variable_bridge_types::Vector{Any}
    variable_node::OrderedDict{Tuple{Type},VariableNode}
    variable_types::Vector{Tuple{Type}}
    # List of types of available bridges
    constraint_bridge_types::Vector{Any}
    constraint_node::OrderedDict{Tuple{Type,Type},ConstraintNode}
    constraint_types::Vector{Tuple{Type,Type}}
    # List of types of available bridges
    objective_bridge_types::Vector{Any}
    objective_node::OrderedDict{Tuple{Type},ObjectiveNode}
    objective_types::Vector{Tuple{Type}}
    # Cache for (F, S) -> BridgeType. Avoids having to look up
    # `concrete_bridge_type` at runtime, which is slow.
    cached_bridge_type::Dict{Any,Type}

    function LazyBridgeOptimizer(model::MOI.ModelLike)
        return new{typeof(model)}(
            model,
            Variable.Map(),
            Dict{MOI.VariableIndex,String}(),
            nothing,
            Constraint.Map(),
            Dict{MOI.ConstraintIndex,String}(),
            nothing,
            Objective.Map(),
            Graph(),
            Any[],
            OrderedDict{Tuple{Type},VariableNode}(),
            Tuple{Type}[],
            Any[],
            OrderedDict{Tuple{Type,Type},ConstraintNode}(),
            Tuple{Type,Type}[],
            Any[],
            OrderedDict{Tuple{Type},ObjectiveNode}(),
            Tuple{Type}[],
            Dict{Any,Type}(),
        )
    end
end

Variable.bridges(bridge::LazyBridgeOptimizer) = bridge.variable_map

Constraint.bridges(bridge::LazyBridgeOptimizer) = bridge.constraint_map

Objective.bridges(b::LazyBridgeOptimizer) = b.objective_map

function _print_bridges_from_map(io, map)
    bridges = values(map)
    if isempty(bridges)
        println(io, " none")
        return
    end
    println(io)
    offset = get(io, :offset, "")
    bridge_strings = sort(string.(unique(typeof.(bridges))))
    for (i, bridge) in enumerate(bridge_strings)
        tree = ifelse(i == length(bridge_strings), '└', '├')
        MOI.Utilities.print_with_acronym(io, "$(offset)│ $tree $bridge\n")
    end
    return
end

function Base.show(io::IO, model::LazyBridgeOptimizer)
    offset = get(io, :offset, "")
    MOI.Utilities.print_with_acronym(io, summary(model))
    println(io)
    print(io, offset, "├ Variable bridges:")
    _print_bridges_from_map(io, model.variable_map)
    print(io, offset, "├ Constraint bridges:")
    _print_bridges_from_map(io, model.constraint_map)
    print(io, offset, "├ Objective bridges:")
    _print_bridges_from_map(io, model.objective_map)
    print(io, offset, "└ model: ")
    show(IOContext(io, :offset => offset * "  "), model.model)
    return
end

"""
    _reset_bridge_graph(b::LazyBridgeOptimizer)

Reset the hyper-graph maintained by the [LazyBridgeOptimizer](@ref), but do not
remove the bridges that have been added.

We need to reset the graph whenever we add or delete bridges from `b`, because
it might change the optimal hyper-paths. Recomputing from scratch is a lot of
work, but it will only happen if we interleave calls to [`add_bridge`](@ref),
[`remove_bridge`](@ref), and model builders like [`MOI.add_constraint`](@ref).
"""
function _reset_bridge_graph(b::LazyBridgeOptimizer)
    empty!(b.variable_node)
    empty!(b.variable_types)
    empty!(b.constraint_node)
    empty!(b.constraint_types)
    empty!(b.objective_node)
    empty!(b.objective_types)
    empty!(b.graph)
    empty!(b.cached_bridge_type)
    return
end

"""
    _variable_nodes(b::LazyBridgeOptimizer, ::Type{BT}) where {BT}

Return the list of `VariableNode` that would be added if `BT` is used in `b`.
"""
function _variable_nodes(
    @nospecialize(b::LazyBridgeOptimizer),
    @nospecialize(BT),
)
    return map(added_constrained_variable_types(BT)) do (S,)
        return node(b, S)::VariableNode
    end
end

"""
    _constraint_nodes(b::LazyBridgeOptimizer, ::Type{BT}) where {BT}

Return the list of `ConstraintNode` that would be added if `BT` is used in `b`.
"""
function _constraint_nodes(
    @nospecialize(b::LazyBridgeOptimizer),
    @nospecialize(BT),
)
    return ConstraintNode[
        node(b, F, S) for (F, S) in added_constraint_types(BT)
    ]
end

"""
    _edge(b::LazyBridgeOptimizer, index::Int, BT::Type{<:AbstractBridge})

Return the `Edge` or `ObjectiveEdge` in the hyper-graph associated with the
bridge `BT`, where `index` is the index of `BT` in the list of bridges.
"""
function _edge(
    @nospecialize(b::LazyBridgeOptimizer),
    @nospecialize(index::Int),
    @nospecialize(BT::Type{<:AbstractBridge}),
)
    return Edge(
        index,
        _variable_nodes(b, BT),
        _constraint_nodes(b, BT),
        bridging_cost(BT),
    )
end

# Method for objective bridges because they produce ObjectiveEdge.
function _edge(
    b::LazyBridgeOptimizer,
    index::Int,
    BT::Type{<:Objective.AbstractBridge},
)
    return ObjectiveEdge(
        index,
        _variable_nodes(b, BT)::Vector{VariableNode},
        _constraint_nodes(b, BT)::Vector{ConstraintNode},
        node(b, set_objective_function_type(BT))::ObjectiveNode,
        bridging_cost(BT)::Float64,
    )
end

_functionized_type(::Nothing) = nothing

function _functionized_type(
    ::Type{<:Constraint.ScalarFunctionizeBridge{T}},
) where {T}
    return MOI.ScalarAffineFunction{T}
end

function _functionized_type(
    ::Type{<:Constraint.VectorFunctionizeBridge{T}},
) where {T}
    return MOI.VectorAffineFunction{T}
end

function _functionized_type(b::LazyBridgeOptimizer, ::Type{MOI.VariableIndex})
    return _functionized_type(
        _first_functionize_bridge(
            b.constraint_bridge_types,
            Constraint.ScalarFunctionizeBridge,
        ),
    )
end

function _functionized_type(
    b::LazyBridgeOptimizer,
    ::Type{MOI.VectorOfVariables},
)
    return _functionized_type(
        _first_functionize_bridge(
            b.constraint_bridge_types,
            Constraint.VectorFunctionizeBridge,
        ),
    )
end

"""
    node(b::LazyBridgeOptimizer, S::Type{<:MOI.AbstractSet})

Return the `VariableNode` associated with set `S` in `b`.
"""
function node(
    @nospecialize(b::LazyBridgeOptimizer),
    @nospecialize(S::Type{<:MOI.AbstractSet}),
)
    # If we support the set, the node is 0.
    if (
        S <: MOI.AbstractScalarSet &&
        MOI.supports_add_constrained_variable(b.model, S)
    ) || (
        S <: MOI.AbstractVectorSet &&
        MOI.supports_add_constrained_variables(b.model, S)
    )
        return VariableNode(0)
    end
    # If (S,) is stored in .variable_node, we've already added the node
    # previously.
    variable_node = get(b.variable_node, (S,), nothing)
    if variable_node !== nothing
        return variable_node
    end
    # This is a new (S,). We need to add it to the graph.
    variable_node = add_node(b.graph, VariableNode)
    b.variable_node[(S,)] = variable_node
    push!(b.variable_types, (S,))
    F = MOI.Utilities.variable_function_type(S)
    if is_bridged(b, MOI.Reals)
        # The solver doesn't support adding free variables.
        FF = _functionized_type(b, F)
        if FF !== nothing
            # The cost is `3` because we assume the distance of the variable
            # node `MOI.Reals` is `1` (that is, it is bridged by
            # `Variable.FreeBridge`. We also use the functionize bridge which
            # has cost 1. And we add `+1` as we treat constrained variables as
            # constraints.
            set_variable_constraint_node(
                b.graph,
                variable_node,
                node(b, FF, S),
                3,
            )
        end
    else
        # We add `+1` as we treat constrained variables as constraints.
        set_variable_constraint_node(b.graph, variable_node, node(b, F, S), 1)
    end
    for (i, BT) in enumerate(b.variable_bridge_types)
        if Variable.supports_constrained_variable(BT, S)
            edge = _edge(b, i, Variable.concrete_bridge_type(BT, S))::Edge
            add_edge(b.graph, variable_node, edge)
        end
    end
    return variable_node
end

"""
    node(b::LazyBridgeOptimizer, ::Type{F}, ::Type{S})

Return the `ConstraintNode` associated with constraint `F`-in-`S` in `b`.
"""
function node(
    @nospecialize(b::LazyBridgeOptimizer),
    @nospecialize(F::Type{<:MOI.AbstractFunction}),
    @nospecialize(S::Type{<:MOI.AbstractSet}),
)
    # If we support the constraint type, the node is 0.
    if MOI.supports_constraint(b.model, F, S)
        return ConstraintNode(0)
    end
    # If (F, S) is stored in .constraint_node, we've already added the node
    # previously.
    constraint_node = get(b.constraint_node, (F, S), nothing)
    if constraint_node !== nothing
        return constraint_node
    end
    # This is a new (F, S). We need to add it to the graph.
    constraint_node = add_node(b.graph, ConstraintNode)
    b.constraint_node[(F, S)] = constraint_node
    push!(b.constraint_types, (F, S))
    for (i, BT) in enumerate(b.constraint_bridge_types)
        if MOI.supports_constraint(BT, F, S)
            edge = _edge(b, i, Constraint.concrete_bridge_type(BT, F, S))::Edge
            add_edge(b.graph, constraint_node, edge)
        end
    end
    return constraint_node
end

"""
    node(b::LazyBridgeOptimizer, ::Type{F})

Return the `ObjectiveNode` associated with constraint `F` in `b`.
"""
function node(b::LazyBridgeOptimizer, ::Type{F}) where {F<:MOI.AbstractFunction}
    # If we support the objective function, the node is 0.
    if MOI.supports(b.model, MOI.ObjectiveFunction{F}())
        return ObjectiveNode(0)
    end
    # If (f,) is stored in .objective_node, we've already added the node
    # previously.
    objective_node = get(b.objective_node, (F,), nothing)
    if objective_node !== nothing
        return objective_node
    end
    # This is a new (F,). We need to add it to the graph.
    objective_node = add_node(b.graph, ObjectiveNode)
    b.objective_node[(F,)] = objective_node
    push!(b.objective_types, (F,))
    for (i, BT) in enumerate(b.objective_bridge_types)
        if Objective.supports_objective_function(BT, F)
            bridge_type = Objective.concrete_bridge_type(
                BT,
                F,
            )::Type{<:Objective.AbstractBridge}
            edge = _edge(b, i, bridge_type)::ObjectiveEdge
            add_edge(b.graph, objective_node, edge)
        end
    end
    return objective_node
end

"""
    _bridge_types(b::LazyBridgeOptimizer, BT::Type{<:AbstractBridge}),

A fallback helper to return the correct list of bridges given the type of `BT`.
"""
function _bridge_types(
    b::LazyBridgeOptimizer,
    ::Type{<:Variable.AbstractBridge},
)
    return b.variable_bridge_types
end

function _bridge_types(
    @nospecialize(b::LazyBridgeOptimizer),
    @nospecialize(BT::Type{<:Constraint.AbstractBridge}),
)
    return b.constraint_bridge_types
end

function _bridge_types(
    b::LazyBridgeOptimizer,
    ::Type{<:Objective.AbstractBridge},
)
    return b.objective_bridge_types
end

"""
    add_bridge(b::LazyBridgeOptimizer, BT::Type{<:AbstractBridge})

Enable the use of the bridges of type `BT` by `b`.
"""
function add_bridge(
    @nospecialize(b::LazyBridgeOptimizer),
    @nospecialize(BT::Type{<:AbstractBridge}),
)
    if !has_bridge(b, BT)
        push!(_bridge_types(b, BT), BT)
        _reset_bridge_graph(b)
    end
    return
end

"""
    remove_bridge(b::LazyBridgeOptimizer, BT::Type{<:AbstractBridge})

Disable the use of the bridges of type `BT` by `b`.
"""
function remove_bridge(b::LazyBridgeOptimizer, BT::Type{<:AbstractBridge})
    bridge_types = _bridge_types(b, BT)
    i = findfirst(isequal(BT), bridge_types)
    if i === nothing
        error(
            "Cannot remove bridge `$BT` as it was never added or was already",
            " removed.",
        )
    end
    deleteat!(bridge_types, i)
    _reset_bridge_graph(b)
    return
end

"""
    has_bridge(b::LazyBridgeOptimizer, BT::Type{<:AbstractBridge})

Return a `Bool` indicating whether the bridges of type `BT` are used by `b`.
"""
function has_bridge(
    @nospecialize(b::LazyBridgeOptimizer),
    @nospecialize(BT::Type{<:AbstractBridge}),
)::Bool
    return findfirst(isequal(BT), _bridge_types(b, BT)) !== nothing
end

# It only bridges when the constraint is not supporting, hence the name "Lazy"
function is_bridged(b::LazyBridgeOptimizer, S::Type{<:MOI.AbstractScalarSet})
    return !MOI.supports_add_constrained_variable(b.model, S)
end

function is_bridged(b::LazyBridgeOptimizer, S::Type{<:MOI.AbstractVectorSet})
    return !MOI.supports_add_constrained_variables(b.model, S)
end

function is_bridged(
    b::LazyBridgeOptimizer,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    return !MOI.supports_constraint(b.model, F, S)
end

function is_bridged(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction})
    return !MOI.supports(b.model, MOI.ObjectiveFunction{F}())
end

function bridge_index(b::LazyBridgeOptimizer, S::Type{<:MOI.AbstractSet})
    return bridge_index(b.graph, node(b, S))
end

function bridge_index(
    b::LazyBridgeOptimizer,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    return bridge_index(b.graph, node(b, F, S))
end

function bridge_index(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction})
    return bridge_index(b.graph, node(b, F))
end

supports_constraint_bridges(::LazyBridgeOptimizer) = true

function supports_bridging_constrained_variable(
    b::LazyBridgeOptimizer,
    S::Type{<:MOI.AbstractSet},
)
    variable_node = node(b, S)
    if !iszero(bridge_index(b.graph, variable_node))
        return true
    end
    constraint_node = b.graph.variable_constraint_node[variable_node.index]
    return constraint_node.index != INVALID_NODE_INDEX &&
           !iszero(bridge_index(b.graph, constraint_node))
end

function supports_bridging_constraint(
    b::LazyBridgeOptimizer,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    return !iszero(bridge_index(b, F, S))
end

function supports_bridging_objective_function(
    b::LazyBridgeOptimizer,
    F::Type{<:MOI.AbstractFunction},
)
    return !iszero(bridge_index(b, F))
end

function is_variable_bridged(b::LazyBridgeOptimizer, S::Type{<:MOI.AbstractSet})
    return is_bridged(b, S) && is_variable_edge_best(b.graph, node(b, S))
end

function bridge_type(b::LazyBridgeOptimizer, S::Type{<:MOI.AbstractSet})
    bt = get(b.cached_bridge_type, (S,), nothing)
    if bt !== nothing
        return bt::Type
    end
    index = bridge_index(b, S)
    if iszero(index)
        F = MOI.Utilities.variable_function_type(S)
        throw(MOI.UnsupportedConstraint{F,S}())
    end
    new_bt = Variable.concrete_bridge_type(b.variable_bridge_types[index], S)
    b.cached_bridge_type[(S,)] = new_bt
    return new_bt::Type
end

function bridge_type(
    b::LazyBridgeOptimizer,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    bt = get(b.cached_bridge_type, (F, S), nothing)
    if bt !== nothing
        return bt::Type
    end
    index = bridge_index(b, F, S)
    if iszero(index)
        throw(MOI.UnsupportedConstraint{F,S}())
    end
    new_bt =
        Constraint.concrete_bridge_type(b.constraint_bridge_types[index], F, S)
    b.cached_bridge_type[(F, S)] = new_bt
    return new_bt::Type
end

function bridge_type(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction})
    bt = get(b.cached_bridge_type, (F,), nothing)
    if bt !== nothing
        return bt::Type
    end
    index = bridge_index(b, F)
    if iszero(index)
        throw(MOI.UnsupportedAttribute(MOI.ObjectiveFunction{F}()))
    end
    new_bt = Objective.concrete_bridge_type(b.objective_bridge_types[index], F)
    b.cached_bridge_type[(F,)] = new_bt
    return new_bt::Type
end

function _func_name(::Type{Constraint.ScalarFunctionizeBridge})
    return "VariableIndex", "constraint"
end

function _func_name(::Type{Constraint.VectorFunctionizeBridge})
    return "VectorOfVariables", "constraint"
end

_func_name(::Type{Objective.FunctionizeBridge}) = "VariableIndex", "objective"

function _first_functionize_bridge(bridge_types, target_type)
    index = findfirst(bridge_type -> bridge_type <: target_type, bridge_types)
    if index === nothing
        return nothing
    end
    return bridge_types[index]
end

function _functionize_bridge(bridge_types, target_type)
    bridge_type = _first_functionize_bridge(bridge_types, target_type)
    if bridge_type === nothing
        func, name = _func_name(target_type)
        error(
            "Need to apply a `$target_type` to a `$func` $name because the " *
            "variable is bridged but no such $name bridge type was added. " *
            "Add one with `add_bridge`.",
        )
    end
    return bridge_type
end

function constraint_vector_functionize_bridge(b::LazyBridgeOptimizer)
    return _functionize_bridge(
        b.constraint_bridge_types,
        Constraint.VectorFunctionizeBridge,
    )
end

function constraint_scalar_functionize_bridge(b::LazyBridgeOptimizer)
    return _functionize_bridge(
        b.constraint_bridge_types,
        Constraint.ScalarFunctionizeBridge,
    )
end

function objective_functionize_bridge(b::LazyBridgeOptimizer)
    return _functionize_bridge(
        b.objective_bridge_types,
        Objective.FunctionizeBridge,
    )
end

function bridging_cost(b::LazyBridgeOptimizer, args...)
    return bridging_cost(b.graph, node(b, args...))
end

recursive_model(b::LazyBridgeOptimizer) = b
