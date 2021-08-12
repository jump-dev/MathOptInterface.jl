const INFINITY = -1
const INVALID_NODE_INDEX = -1

abstract type AbstractNode end

struct VariableNode <: AbstractNode
    index::Int
end

struct ConstraintNode <: AbstractNode
    index::Int
end

struct ObjectiveNode <: AbstractNode
    index::Int
end

abstract type AbstractEdge end

struct Edge <: AbstractEdge
    bridge_index::Int
    added_variables::Vector{VariableNode}
    added_constraints::Vector{ConstraintNode}
end

struct ObjectiveEdge <: AbstractEdge
    bridge_index::Int
    added_variables::Vector{VariableNode}
    added_constraints::Vector{ConstraintNode}
    added_objective::ObjectiveNode
end

mutable struct Graph
    variable_edges::Vector{Vector{Edge}}
    variable_constraint_node::Vector{ConstraintNode}
    variable_constraint_cost::Vector{Int}
    # variable node index -> Number of bridges that need to be used
    variable_dist::Vector{Int}
    # variable node index -> Index of bridge to be used
    variable_best::Vector{Int}
    variable_last_correct::Int
    constraint_edges::Vector{Vector{Edge}}
    # constraint node index -> Number of bridges that need to be used
    constraint_dist::Vector{Int}
    # constraint node index -> Index of bridge to be used
    constraint_best::Vector{Int}
    constraint_last_correct::Int
    objective_edges::Vector{Vector{ObjectiveEdge}}
    # objective node index -> Number of bridges that need to be used
    objective_dist::Vector{Int}
    # objective node index -> Index of bridge to be used
    objective_best::Vector{Int}
    objective_last_correct::Int
end

function Graph()
    return Graph(
        Vector{Edge}[],
        ConstraintNode[],
        Int[],
        Int[],
        Int[],
        0,
        Vector{Edge}[],
        Int[],
        Int[],
        0,
        Vector{ObjectiveEdge}[],
        Int[],
        Int[],
        0,
    )
end

function Base.show(io::IO, graph::Graph)
    print(io, "Bridge graph with ")
    print(io, length(graph.variable_best), " variable nodes, ")
    print(io, length(graph.constraint_best), " constraint nodes and ")
    return print(io, length(graph.objective_best), " objective nodes.")
end

function variable_nodes(graph::Graph)
    return LazyMap{VariableNode}(eachindex(graph.variable_best)) do i
        return VariableNode(i)
    end
end

function constraint_nodes(graph::Graph)
    return LazyMap{ConstraintNode}(eachindex(graph.constraint_best)) do i
        return ConstraintNode(i)
    end
end

function objective_nodes(graph::Graph)
    return LazyMap{ObjectiveNode}(eachindex(graph.objective_best)) do i
        return ObjectiveNode(i)
    end
end

# After `add_bridge(b, BT)`, some constrained variables `(S,)` in
# `keys(b.variable_best)` or constraints `(F, S)` in `keys(b.constraint_best)`
# or `(F,)` in `keys(b.objective_best)` may be bridged
# with less bridges than `b.variable_dist[(S,)]`,
# `b.constraint_dist[(F, S)]` or `b.objective_dist[(F,)]` using `BT`.
# We could either recompute the distance from every node or clear the
# dictionary so that the distance is computed lazily at the next `supports_constraint`
# call. We prefer clearing the dictionaries so as this is called for each
# bridge added and recomputing the distance for each bridge would be a wase
# if several bridges are added consecutively.
function Base.empty!(graph::Graph)
    empty!(graph.variable_edges)
    empty!(graph.variable_constraint_node)
    empty!(graph.variable_constraint_cost)
    empty!(graph.variable_dist)
    empty!(graph.variable_best)
    graph.variable_last_correct = 0
    empty!(graph.constraint_edges)
    empty!(graph.constraint_dist)
    empty!(graph.constraint_best)
    graph.constraint_last_correct = 0
    empty!(graph.objective_edges)
    empty!(graph.objective_dist)
    empty!(graph.objective_best)
    graph.objective_last_correct = 0
    return
end

function add_edge(graph::Graph, node::VariableNode, edge::Edge)
    push!(graph.variable_edges[node.index], edge)
    return
end

function add_edge(graph::Graph, node::ConstraintNode, edge::Edge)
    push!(graph.constraint_edges[node.index], edge)
    return
end

function add_edge(graph::Graph, node::ObjectiveNode, edge::ObjectiveEdge)
    push!(graph.objective_edges[node.index], edge)
    return
end

function add_variable_node(graph::Graph)
    push!(graph.variable_edges, Edge[])
    # Use an invalid index so that the code errors instead return something
    # incorrect in case `set_variable_constraint_node` is not called.
    push!(graph.variable_constraint_node, ConstraintNode(INVALID_NODE_INDEX))
    push!(graph.variable_constraint_cost, 0)
    push!(graph.variable_dist, INFINITY)
    push!(graph.variable_best, 0)
    return VariableNode(length(graph.variable_best))
end

function set_variable_constraint_node(
    graph::Graph,
    variable_node::VariableNode,
    constraint_node::ConstraintNode,
    cost::Int,
)
    graph.variable_constraint_node[variable_node.index] = constraint_node
    graph.variable_constraint_cost[variable_node.index] = cost
    return
end

function add_constraint_node(graph::Graph)
    push!(graph.constraint_edges, Edge[])
    push!(graph.constraint_dist, INFINITY)
    push!(graph.constraint_best, 0)
    return ConstraintNode(length(graph.constraint_best))
end

function add_objective_node(graph::Graph)
    push!(graph.objective_edges, ObjectiveEdge[])
    push!(graph.objective_dist, INFINITY)
    push!(graph.objective_best, 0)
    return ObjectiveNode(length(graph.objective_best))
end

function bridging_cost(graph::Graph, node::AbstractNode)
    bellman_ford!(graph)
    dist = _dist(graph, node)
    return dist == INFINITY ? Inf : float(dist)
end

function bridge_index(graph::Graph, node::VariableNode)
    bellman_ford!(graph)
    return graph.variable_best[node.index]
end

function bridge_index(graph::Graph, node::ConstraintNode)
    bellman_ford!(graph)
    return graph.constraint_best[node.index]
end

function bridge_index(graph::Graph, node::ObjectiveNode)
    bellman_ford!(graph)
    return graph.objective_best[node.index]
end

"""
    is_variable_edge_best(graph::Graph, node::VariableNode)

Return a `Bool` indicating whether the value of `_dist(graph, node)` can be
achieved with a variable bridge.
"""
function is_variable_edge_best(graph::Graph, node::VariableNode)
    bellman_ford!(graph)
    return graph.variable_dist[node.index] == _dist(graph, node)
end

# Update `b.variable_dist`, `b.constraint_dist` `b.variable_best` and
# `b.constraint_best` for constrained variable types in `variables` and
# constraint types in `constraints`.
function updated_dist(graph::Graph, current::Int, edges::Vector{<:AbstractEdge})
    bridge_index = 0
    for edge in edges
        if supports_added_no_update(graph, edge)
            # `added_dist != INFINITY` because `supports_added_no_update`
            # returned `true`.
            dist = 1 + added_dist(graph, edge)
            if current == INFINITY || dist < current
                current = dist
                bridge_index = edge.bridge_index
            end
        end
    end
    return current, bridge_index
end

function bellman_ford!(graph::Graph)
    # Bellman-Ford algorithm
    changed = true # Has b.constraint_dist changed in the last iteration ?
    while changed
        changed = false
        for i in (graph.variable_last_correct+1):length(graph.variable_best)
            dist, best = updated_dist(
                graph,
                graph.variable_dist[i],
                graph.variable_edges[i],
            )
            if !iszero(best)
                graph.variable_dist[i] = dist
                graph.variable_best[i] = best
                changed = true
            end
        end
        for i in (graph.constraint_last_correct+1):length(graph.constraint_best)
            dist, best = updated_dist(
                graph,
                graph.constraint_dist[i],
                graph.constraint_edges[i],
            )
            if !iszero(best)
                graph.constraint_dist[i] = dist
                graph.constraint_best[i] = best
                changed = true
            end
        end
        for i in (graph.objective_last_correct+1):length(graph.objective_best)
            dist, best = updated_dist(
                graph,
                graph.objective_dist[i],
                graph.objective_edges[i],
            )
            if !iszero(best)
                graph.objective_dist[i] = dist
                graph.objective_best[i] = best
                changed = true
            end
        end
    end
    graph.variable_last_correct = length(graph.variable_best)
    graph.constraint_last_correct = length(graph.constraint_best)
    graph.objective_last_correct = length(graph.objective_best)
    return
end

function _dist(graph::Graph, node::VariableNode)
    if iszero(node.index)
        return 0
    end
    constraint_node = graph.variable_constraint_node[node.index]
    # If free variables are bridged but the functionize bridge was not added,
    # constraint_node is `ConstraintNode(INVALID_NODE_INDEX)`.
    dc =
        constraint_node.index == INVALID_NODE_INDEX ? INFINITY :
        _dist(graph, constraint_node)
    dv = graph.variable_dist[node.index]
    if dc == INFINITY
        return dv
    else
        dc += graph.variable_constraint_cost[node.index]
        if dv == INFINITY
            return dc
        else
            return min(dv, dc)
        end
    end
end

function _dist(graph::Graph, node::ConstraintNode)
    return iszero(node.index) ? 0 : graph.constraint_dist[node.index]
end

function _dist(graph::Graph, node::ObjectiveNode)
    return iszero(node.index) ? 0 : graph.objective_dist[node.index]
end

function _sum_dist(graph::Graph, nodes::Vector{<:AbstractNode})
    return mapreduce(node -> _dist(graph, node), +, nodes, init = 0)
end

function added_dist(graph::Graph, edge::Edge)
    return _sum_dist(graph, edge.added_variables) +
           _sum_dist(graph, edge.added_constraints)
end

function added_dist(graph::Graph, edge::ObjectiveEdge)
    return _sum_dist(graph, edge.added_variables) +
           _sum_dist(graph, edge.added_constraints) +
           _dist(graph, edge.added_objective)
end

function supports_no_update(graph::Graph, node::AbstractNode)
    return iszero(node.index) || _dist(graph, node) != INFINITY
end

function _supports_all_no_update(graph::Graph, nodes::Vector{<:AbstractNode})
    return all(node -> supports_no_update(graph, node), nodes)
end

function supports_added_no_update(graph::Graph, edge::Edge)
    return _supports_all_no_update(graph, edge.added_variables) &&
           _supports_all_no_update(graph, edge.added_constraints)
end

function supports_added_no_update(graph::Graph, edge::ObjectiveEdge)
    return _supports_all_no_update(graph, edge.added_variables) &&
           _supports_all_no_update(graph, edge.added_constraints) &&
           supports_no_update(graph, edge.added_objective)
end
