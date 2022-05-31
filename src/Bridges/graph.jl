# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

const INFINITY = typemax(Int)
const INVALID_NODE_INDEX = -1

abstract type AbstractNode end

"""
    VariableNode(index::Int)

A node in [`Graph`](@ref) representing a variable constrained on creation.
"""
struct VariableNode <: AbstractNode
    index::Int
end

"""
    ConstraintNode(index::Int)

A node in [`Graph`](@ref) representing a constraint.
"""
struct ConstraintNode <: AbstractNode
    index::Int
end

"""
    ObjectiveNode(index::Int)

A node in [`Graph`](@ref) representing an objective function.
"""
struct ObjectiveNode <: AbstractNode
    index::Int
end

abstract type AbstractEdge end

"""
    Edge(
        bridge_index::Int,
        added_variables::Vector{VariableNode},
        added_constraints::Vector{ConstraintNode},
    )

Return a new datastructure representing an edge in [`Graph`](@ref) that starts
at a [`VariableNode`](@ref) or a [`ConstraintNode`](@ref).
"""
struct Edge <: AbstractEdge
    bridge_index::Int
    added_variables::Vector{VariableNode}
    added_constraints::Vector{ConstraintNode}
end

"""
    ObjectiveEdge(
        bridge_index::Int,
        added_variables::Vector{VariableNode},
        added_constraints::Vector{ConstraintNode},
    )

Return a new datastructure representing an edge in [`Graph`](@ref) that starts
at an [`ObjectiveNode`](@ref).
"""
struct ObjectiveEdge <: AbstractEdge
    bridge_index::Int
    added_variables::Vector{VariableNode}
    added_constraints::Vector{ConstraintNode}
    added_objective::ObjectiveNode
end

"""
    Graph()

A type-stable datastructure for computing the shortest hyperpath problem.

## Nodes

There are three types of nodes in the graph:

  * [`VariableNode`](@ref)
  * [`ConstraintNode`](@ref)
  * [`ObjectiveNode`](@ref)

Add nodes to the graph using [`add_node`](@ref).

## Edges

There are two types of edges in the graph:

  * [`Edge`](@ref)
  * [`ObjectiveEdge`](@ref)

Add edges to the graph using [`add_edge`](@ref).

For the ability to add a variable constrained  on creation as a free variable
followed by a constraint, use [`set_variable_constraint_node`](@ref).

## Optimal hyper-edges

Use [`bridge_index`](@ref) to compute the minimum-cost bridge leaving a node.

Note that [`bridge_index`](@ref) lazy runs a Bellman-Ford algorithm to compute
the set of minimum cost edges. Thus, the first call to [`bridge_index`](@ref)
after adding new nodes or edges will take longer than subsequent calls.
"""
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

    function Graph()
        return new(
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
end

function Base.show(io::IO, graph::Graph)
    print(io, "Bridge graph with ")
    print(io, length(graph.variable_best), " variable nodes, ")
    print(io, length(graph.constraint_best), " constraint nodes and ")
    print(io, length(graph.objective_best), " objective nodes.")
    return
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

"""
    add_edge(graph::Graph, node::VariableNode, edge::Edge)::Nothing
    add_edge(graph::Graph, node::ConstraintNode, edge::Edge)::Nothing
    add_edge(graph::Graph, node::ObjectiveNode, edge::ObjectiveEdge)::Nothing

Add `edge` to `graph`, where `edge` starts at `node` and connects to the nodes
defined in `edge`.
"""
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

"""
    add_node(graph::Graph, ::Type{VariableNode})::VariableNode
    add_node(graph::Graph, ::Type{ConstraintNode})::ConstraintNode
    add_node(graph::Graph, ::Type{ObjectiveNode})::ObjectiveNode

Add a new node to `graph`.
"""
function add_node(graph::Graph, ::Type{VariableNode})
    push!(graph.variable_edges, Edge[])
    # Use an invalid index so that the code errors instead return something
    # incorrect in case `set_variable_constraint_node` is not called.
    push!(graph.variable_constraint_node, ConstraintNode(INVALID_NODE_INDEX))
    push!(graph.variable_constraint_cost, 0)
    push!(graph.variable_dist, INFINITY)
    push!(graph.variable_best, 0)
    return VariableNode(length(graph.variable_best))
end

function add_node(graph::Graph, ::Type{ConstraintNode})
    push!(graph.constraint_edges, Edge[])
    push!(graph.constraint_dist, INFINITY)
    push!(graph.constraint_best, 0)
    return ConstraintNode(length(graph.constraint_best))
end

function add_node(graph::Graph, ::Type{ObjectiveNode})
    push!(graph.objective_edges, ObjectiveEdge[])
    push!(graph.objective_dist, INFINITY)
    push!(graph.objective_best, 0)
    return ObjectiveNode(length(graph.objective_best))
end

"""
    set_variable_constraint_node(
        graph::Graph,
        variable_node::VariableNode,
        constraint_node::ConstraintNode,
        cost::Int,
    )

As an alternative to `variable_node`, add a virtual edge to `graph` that
represents adding a free variable, followed by a constraint of type
`constraint_node`, with bridging cost `cost`.

## Why is this needed?

Variables can either be added as a variable constrained on creation, or as a
free variable which then has a constraint added to it.
"""
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

function bridging_cost(graph::Graph, node::AbstractNode)
    _compute_bellman_ford(graph)
    dist = _dist(graph, node)
    return dist == INFINITY ? Inf : float(dist)
end

"""
    bridge_index(graph::Graph, node::VariableNode)::Int
    bridge_index(graph::Graph, node::ConstraintNode)::Int
    bridge_index(graph::Graph, node::ObjectiveNode)::Int

Return the optimal index of the bridge to chose from `node`.
"""
function bridge_index(graph::Graph, node::VariableNode)
    _compute_bellman_ford(graph)
    return graph.variable_best[node.index]
end

function bridge_index(graph::Graph, node::ConstraintNode)
    _compute_bellman_ford(graph)
    return graph.constraint_best[node.index]
end

function bridge_index(graph::Graph, node::ObjectiveNode)
    _compute_bellman_ford(graph)
    return graph.objective_best[node.index]
end

"""
    is_variable_edge_best(graph::Graph, node::VariableNode)::Bool

Return a `Bool` indicating whether `node` should be added as a variable
constrained on creation, or as a free variable followed by a constraint.
"""
function is_variable_edge_best(graph::Graph, node::VariableNode)
    _compute_bellman_ford(graph)
    return graph.variable_dist[node.index] == _dist(graph, node)
end

function _updated_dist(
    graph::Graph,
    current::Int,
    edges::Vector{<:AbstractEdge},
)
    bridge_index = 0
    for edge in edges
        cost = _dist(graph, edge)
        if cost == INFINITY
            continue
        end
        dist = 1 + cost
        if dist < current
            current = dist
            bridge_index = edge.bridge_index
        end
    end
    return current, bridge_index
end

function _compute_bellman_ford(graph::Graph)
    # Has a distance changed in the last iteration?
    changed = true
    while changed
        changed = false
        for i in (graph.variable_last_correct+1):length(graph.variable_best)
            dist, best = _updated_dist(
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
            dist, best = _updated_dist(
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
            dist, best = _updated_dist(
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
    # This is the cost of adding a constrained variable
    dist_as_variable = graph.variable_dist[node.index]
    # This is the cost of adding the constraint, if we were to add it.
    dist_as_constraint = INFINITY
    # If free variables are bridged but the functionize bridge was not
    # added, constraint_node is `ConstraintNode(INVALID_NODE_INDEX)`.
    constraint_node = graph.variable_constraint_node[node.index]
    if constraint_node.index != INVALID_NODE_INDEX
        dist_as_constraint = _dist(graph, constraint_node)
        if dist_as_constraint != INFINITY
            dist_as_constraint += graph.variable_constraint_cost[node.index]
        end
    end
    if dist_as_constraint == INFINITY
        return dist_as_variable
    elseif dist_as_variable == INFINITY
        return dist_as_constraint
    end
    return min(dist_as_variable, dist_as_constraint)
end

function _dist(graph::Graph, node::ConstraintNode)
    return iszero(node.index) ? 0 : graph.constraint_dist[node.index]
end

function _dist(graph::Graph, node::ObjectiveNode)
    return iszero(node.index) ? 0 : graph.objective_dist[node.index]
end

function _dist(graph::Graph, nodes::Vector{<:AbstractNode})
    dist = 0
    for node in nodes
        d = _dist(graph, node)
        if d == INFINITY
            return INFINITY
        end
        dist += d
    end
    return dist
end

function _dist(graph::Graph, edge::Edge)
    dist_var = _dist(graph, edge.added_variables)
    if dist_var == INFINITY
        return INFINITY
    end
    dist_con = _dist(graph, edge.added_constraints)
    if dist_con == INFINITY
        return INFINITY
    end
    return dist_var + dist_con
end

function _dist(graph::Graph, edge::ObjectiveEdge)
    dist_obj = _dist(graph, edge.added_objective)
    if dist_obj == INFINITY
        return INFINITY
    end
    dist_var = _dist(graph, edge.added_variables)
    if dist_var == INFINITY
        return INFINITY
    end
    dist_con = _dist(graph, edge.added_constraints)
    if dist_con == INFINITY
        return INFINITY
    end
    return dist_obj + dist_var + dist_con
end
