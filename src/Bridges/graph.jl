const INFINITY = -1

abstract type AbstractNode end
struct ConstraintNode <: AbstractNode
    index::Int
end
struct VariableNode <: AbstractNode
    index::Int
    constraint_node::ConstraintNode
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
        Vector{Edge}[], Int[], Int[], 0,
        Vector{Edge}[], Int[], Int[], 0,
        Vector{ObjectiveEdge}[], Int[], Int[], 0,
    )
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
end

function add_edge(graph::Graph, node::VariableNode, edge::Edge)
    push!(graph.variable_edges[node.index], edge)
end
function add_edge(graph::Graph, node::ConstraintNode, edge::Edge)
    push!(graph.constraint_edges[node.index], edge)
end
function add_edge(graph::Graph, node::ObjectiveNode, edge::ObjectiveEdge)
    push!(graph.objective_edges[node.index], edge)
end
function add_variable_node(graph::Graph, constraint_node::ConstraintNode)
    push!(graph.variable_edges, Edge[])
    push!(graph.variable_dist, INFINITY)
    push!(graph.variable_best, 0)
    return VariableNode(length(graph.variable_best), constraint_node)
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

# Update `b.variable_dist`, `b.constraint_dist` `b.variable_best` and
# `b.constraint_best` for constrained variable types in `variables` and
# constraint types in `constraints`.
function updated_dist(graph::Graph, current, edges::Vector{<:AbstractEdge})
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
        for i in (graph.variable_last_correct + 1):length(graph.variable_best)
            dist, best = updated_dist(graph, graph.variable_dist[i],
                                      graph.variable_edges[i])
            if !iszero(best)
                graph.variable_dist[i] = dist
                graph.variable_best[i] = best
                changed = true
            end
        end
        for i in (graph.constraint_last_correct + 1):length(graph.constraint_best)
            dist, best = updated_dist(graph, graph.constraint_dist[i],
                                      graph.constraint_edges[i])
            if !iszero(best)
                graph.constraint_dist[i] = dist
                graph.constraint_best[i] = best
                changed = true
            end
        end
        for i in (graph.objective_last_correct + 1):length(graph.objective_best)
            dist, best = updated_dist(graph, graph.objective_dist[i],
                                      graph.objective_edges[i])
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
    dc = _dist(graph, node.constraint_node)
    if iszero(dc)
        return dc
    elseif iszero(node.index)
        return 0
    else
        dv = graph.variable_dist[node.index]
        if dc == INFINITY
            return dv
        elseif dv == INFINITY
            return dc
        else
            return min(dv, dc + 1)
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
