using OrderedCollections

include("graph.jl")

"""
    LazyBridgeOptimizer{OT<:MOI.ModelLike} <: AbstractBridgeOptimizer

The `LazyBridgeOptimizer` combines several bridges, which are added using the
[`add_bridge`](@ref) function.

Whenever a constraint is added, it only attempts to bridge it if it is not
supported by the internal model (hence its name `Lazy`).

When bridging a constraint, it selects the minimal number of bridges needed.

For example, if a constraint `F`-in-`S` can be bridged into a constraint
`F1`-in-`S1` (supported by the internal model) using bridge 1 or bridged into a
constraint `F2`-in-`S2` (unsupported by the internal model) using bridge 2 which
can then be bridged into a constraint `F3`-in-`S3` (supported by the internal
model) using bridge 3, it will choose bridge 1 as it allows to bridge `F`-in-`S
using only one bridge instead of two if it uses bridge 2 and 3.
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
    variable_node::OrderedDict{Tuple{DataType},VariableNode}
    variable_types::Vector{Tuple{DataType}}
    # List of types of available bridges
    constraint_bridge_types::Vector{Any}
    constraint_node::OrderedDict{Tuple{DataType,DataType},ConstraintNode}
    constraint_types::Vector{Tuple{DataType,DataType}}
    # List of types of available bridges
    objective_bridge_types::Vector{Any}
    objective_node::OrderedDict{Tuple{DataType},ObjectiveNode}
    objective_types::Vector{Tuple{DataType}}
    # Cache for (F, S) -> BridgeType. Avoids having to look up
    # `concrete_bridge_type` at runtime, which is slow.
    cached_bridge_type::Dict{Any,DataType}
end

function LazyBridgeOptimizer(model::MOI.ModelLike)
    return LazyBridgeOptimizer{typeof(model)}(
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
        OrderedDict{Tuple{DataType},VariableNode}(),
        Tuple{DataType}[],
        Any[],
        OrderedDict{Tuple{DataType,DataType},ConstraintNode}(),
        Tuple{DataType,DataType}[],
        Any[],
        OrderedDict{Tuple{DataType},ObjectiveNode}(),
        Tuple{DataType}[],
        Dict{Any,DataType}(),
    )
end

function Variable.bridges(bridge::LazyBridgeOptimizer)
    return bridge.variable_map
end

function Constraint.bridges(bridge::LazyBridgeOptimizer)
    return bridge.constraint_map
end

function Objective.bridges(b::LazyBridgeOptimizer)
    return b.objective_map
end

# After `add_bridge(b, BT)`, some constrained variables `(S,)` in
# `keys(b.variable_best)` or constraints `(F, S)` in `keys(b.constraint_best)`
# or `(F,)` in `keys(b.objective_best)` may be bridged with less bridges than
# `b.variable_dist[(S,)]`, `b.constraint_dist[(F, S)]` or
# `b.objective_dist[(F,)]` using `BT`.
#
# We could either recompute the distance from every node or clear the dictionary
# so that the distance is computed lazily at the next `supports_constraint`
# call. We prefer clearing the dictionaries so as this is called for each bridge
# added and recomputing the distance for each bridge would be a waste if several
# bridges are added consecutively.
function _reset_dist(b::LazyBridgeOptimizer)
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

function edge(b::LazyBridgeOptimizer, bridge_index, BT::Type{<:AbstractBridge})
    return Edge(
        bridge_index,
        VariableNode[
            node(b, C[1]) for C in added_constrained_variable_types(BT)
        ],
        ConstraintNode[node(b, C[1], C[2]) for C in added_constraint_types(BT)],
    )
end

function edge(
    b::LazyBridgeOptimizer,
    bridge_index,
    BT::Type{<:Objective.AbstractBridge},
)
    return ObjectiveEdge(
        bridge_index,
        VariableNode[
            node(b, C[1]) for C in added_constrained_variable_types(BT)
        ],
        ConstraintNode[node(b, C[1], C[2]) for C in added_constraint_types(BT)],
        node(b, set_objective_function_type(BT)),
    )
end

functionized_type(::Nothing) = nothing

function functionized_type(
    ::Type{<:Constraint.ScalarFunctionizeBridge{T}},
) where {T}
    return MOI.ScalarAffineFunction{T}
end

function functionized_type(
    ::Type{<:Constraint.VectorFunctionizeBridge{T}},
) where {T}
    return MOI.VectorAffineFunction{T}
end

function functionized_type(b::LazyBridgeOptimizer, ::Type{MOI.SingleVariable})
    return functionized_type(
        _first_functionize_bridge(
            b.constraint_bridge_types,
            Constraint.ScalarFunctionizeBridge,
        ),
    )
end

function functionized_type(
    b::LazyBridgeOptimizer,
    ::Type{MOI.VectorOfVariables},
)
    return functionized_type(
        _first_functionize_bridge(
            b.constraint_bridge_types,
            Constraint.VectorFunctionizeBridge,
        ),
    )
end

function node(b::LazyBridgeOptimizer, S::Type{<:MOI.AbstractSet})
    F = MOIU.variable_function_type(S)
    if (
        S <: MOI.AbstractScalarSet &&
        MOI.supports_add_constrained_variable(b.model, S)
    ) || (
        S <: MOI.AbstractVectorSet &&
        MOI.supports_add_constrained_variables(b.model, S)
    )
        return VariableNode(0)
    end
    variable_node = get(b.variable_node, (S,), nothing)
    if variable_node !== nothing
        return variable_node
    end
    variable_node = add_variable_node(b.graph)
    b.variable_node[(S,)] = variable_node
    push!(b.variable_types, (S,))
    if is_bridged(b, MOI.Reals)
        FF = functionized_type(b, F)
        if FF !== nothing
            # We assume the distance of the variable node `MOI.Reals` is `1`,
            # i.e. it is bridged by `Variable.FreeBridge` and then
            # the distance of `MOI.Nonnegatives` is zero.
            # We also use the functionize bridge which has cost 1.
            # And we add `+1` as we treat constrained variables as constraints.
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
            add_edge(
                b.graph,
                variable_node,
                edge(b, i, Variable.concrete_bridge_type(BT, S))::Edge,
            )
        end
    end
    return variable_node
end

function node(
    b::LazyBridgeOptimizer,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    if MOI.supports_constraint(b.model, F, S)
        return ConstraintNode(0)
    end
    constraint_node = get(b.constraint_node, (F, S), nothing)
    if constraint_node !== nothing
        return constraint_node
    end
    constraint_node = add_constraint_node(b.graph)
    b.constraint_node[(F, S)] = constraint_node
    push!(b.constraint_types, (F, S))
    for (i, BT) in enumerate(b.constraint_bridge_types)
        if MOI.supports_constraint(BT, F, S)
            add_edge(
                b.graph,
                constraint_node,
                edge(b, i, Constraint.concrete_bridge_type(BT, F, S))::Edge,
            )
        end
    end
    return constraint_node
end

function node(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractScalarFunction})
    if MOI.supports(b.model, MOI.ObjectiveFunction{F}())
        return ObjectiveNode(0)
    end
    objective_node = get(b.objective_node, (F,), nothing)
    if objective_node !== nothing
        return objective_node
    end
    objective_node = add_objective_node(b.graph)
    b.objective_node[(F,)] = objective_node
    push!(b.objective_types, (F,))
    # Add the added nodes to the graph.
    for (i, BT) in enumerate(b.objective_bridge_types)
        if Objective.supports_objective_function(BT, F)
            add_edge(
                b.graph,
                objective_node,
                edge(
                    b,
                    i,
                    Objective.concrete_bridge_type(BT, F),
                )::ObjectiveEdge,
            )
        end
    end
    return objective_node
end

function _bridge_types(
    b::LazyBridgeOptimizer,
    ::Type{<:Variable.AbstractBridge},
)
    return b.variable_bridge_types
end

function _bridge_types(
    b::LazyBridgeOptimizer,
    ::Type{<:Constraint.AbstractBridge},
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
function add_bridge(b::LazyBridgeOptimizer, BT::Type{<:AbstractBridge})
    if !has_bridge(b, BT)
        _add_bridge(b, BT)
        _reset_dist(b)
    end
    return
end

function _add_bridge(b::LazyBridgeOptimizer, BT::Type{<:AbstractBridge})
    push!(_bridge_types(b, BT), BT)
    return
end

"""
    remove_bridge(b::LazyBridgeOptimizer, BT::Type{<:AbstractBridge})

Disable the use of the bridges of type `BT` by `b`.
"""
function remove_bridge(b::LazyBridgeOptimizer, BT::Type{<:AbstractBridge})
    if !_remove_bridge(b, BT)
        error(
            "Cannot remove bridge `$BT` as it was never added or was already",
            " removed.",
        )
    end
    return _reset_dist(b)
end
function _remove_bridge(b::LazyBridgeOptimizer, BT::Type{<:AbstractBridge})
    bridge_types = _bridge_types(b, BT)
    i = findfirst(isequal(BT), bridge_types)
    if i === nothing
        return false
    end
    deleteat!(bridge_types, i)
    return true
end

"""
    has_bridge(b::LazyBridgeOptimizer, BT::Type{<:AbstractBridge})

Return a `Bool` indicating whether the bridges of type `BT` are used by `b`.
"""
function has_bridge(b::LazyBridgeOptimizer, BT::Type{<:AbstractBridge})
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
function is_bridged(
    b::LazyBridgeOptimizer,
    F::Type{<:MOI.AbstractScalarFunction},
)
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
    n = node(b, F, S)
    index = bridge_index(b.graph, n)
    return index
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
    constraint_node = b.graph.variable_constraint_node[variable_node.index]
    return !iszero(bridge_index(b.graph, variable_node)) || (
        constraint_node.index != INVALID_NODE_INDEX &&
        !iszero(bridge_index(b.graph, constraint_node))
    )
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
    F::Type{<:MOI.AbstractScalarFunction},
)
    return !iszero(bridge_index(b, F))
end
function is_variable_bridged(b::LazyBridgeOptimizer, S::Type{<:MOI.AbstractSet})
    return is_bridged(b, S) && is_variable_edge_best(b.graph, node(b, S))
end

function bridge_type(b::LazyBridgeOptimizer, S::Type{<:MOI.AbstractSet})
    bt = get(b.cached_bridge_type, (S,), nothing)
    if bt !== nothing
        return bt::DataType
    end
    index = bridge_index(b, S)
    if iszero(index)
        throw(MOI.UnsupportedConstraint{MOIU.variable_function_type(S),S}())
    end
    new_bt = Variable.concrete_bridge_type(b.variable_bridge_types[index], S)
    b.cached_bridge_type[(S,)] = new_bt
    return new_bt::DataType
end

function bridge_type(
    b::LazyBridgeOptimizer,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    bt = get(b.cached_bridge_type, (F, S), nothing)
    if bt !== nothing
        return bt::DataType
    end
    index = bridge_index(b, F, S)
    if iszero(index)
        throw(MOI.UnsupportedConstraint{F,S}())
    end
    new_bt =
        Constraint.concrete_bridge_type(b.constraint_bridge_types[index], F, S)
    b.cached_bridge_type[(F, S)] = new_bt
    return new_bt::DataType
end

function bridge_type(
    b::LazyBridgeOptimizer,
    F::Type{<:MOI.AbstractScalarFunction},
)
    bt = get(b.cached_bridge_type, (F,), nothing)
    if bt !== nothing
        return bt::DataType
    end
    index = bridge_index(b, F)
    if iszero(index)
        throw(MOI.UnsupportedAttribute(MOI.ObjectiveFunction{F}()))
    end
    new_bt = Objective.concrete_bridge_type(b.objective_bridge_types[index], F)
    b.cached_bridge_type[(F,)] = new_bt
    return new_bt::DataType
end

function _func_name(::Type{Constraint.ScalarFunctionizeBridge})
    return "SingleVariable", "constraint"
end
function _func_name(::Type{Constraint.VectorFunctionizeBridge})
    return "VectorOfVariables", "constraint"
end
_func_name(::Type{Objective.FunctionizeBridge}) = "SingleVariable", "objective"
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
            "Need to apply a `$target_type` to a",
            " `$func` $name because the variable is",
            " bridged but no such $name bridge type was added. Add one",
            " with `add_bridge`.",
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

function MOI.compute_conflict!(model::LazyBridgeOptimizer)
    return MOI.compute_conflict!(model.model)
end
