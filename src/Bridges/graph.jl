const INFINITY = -1
const INVALID_NODE_INDEX = -1

@enum(_NodeType, _VariableNode, _ConstraintNode, _ObjectiveNode)

struct Node
    index::Int
    type::_NodeType
end

struct Edge
    bridge_index::Int
    added_variables::Vector{Node}
    added_constraints::Vector{Node}
    added_objective::Union{Nothing,Node}
end

function Edge(bridge_index, added_variables, added_constraints)
    return Edge(bridge_index, added_variables, added_constraints, nothing)
end

mutable struct Graph
    variable_edges::Vector{Vector{Edge}}
    variable_constraint_node::Vector{Node}
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
    objective_edges::Vector{Vector{Edge}}
    # objective node index -> Number of bridges that need to be used
    objective_dist::Vector{Int}
    # objective node index -> Index of bridge to be used
    objective_best::Vector{Int}
    objective_last_correct::Int
end

function Graph()
    return Graph(
        Vector{Edge}[],
        Node[],
        Int[],
        Int[],
        Int[],
        0,
        Vector{Edge}[],
        Int[],
        Int[],
        0,
        Vector{Edge}[],
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
    return MOIU.LazyMap{Node}(
        i -> Variable(i, _VariableNode),
        eachindex(graph.variable_best),
    )
end

function constraint_nodes(graph::Graph)
    return MOIU.LazyMap{Node}(
        i -> Node(i, _ConstraintNode),
        eachindex(graph.constraint_best),
    )
end

function objective_nodes(graph::Graph)
    return MOIU.LazyMap{Node}(
        i -> Node(i, _ObjectiveNode),
        eachindex(graph.objective_best),
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

function add_edge(graph::Graph, node::Node, edge::Edge)
    if node.type == _VariableNode
        push!(graph.variable_edges[node.index], edge)
    elseif node.type == _ConstraintNode
        push!(graph.constraint_edges[node.index], edge)
    else
        push!(graph.objective_edges[node.index], edge)
    end
    return
end

function add_variable_node(graph::Graph)
    push!(graph.variable_edges, Edge[])
    # Use an invalid index so that the code errors instead return something
    # incorrect in case `set_variable_constraint_node` is not called.
    push!(
        graph.variable_constraint_node,
        Node(INVALID_NODE_INDEX, _ConstraintNode),
    )
    push!(graph.variable_constraint_cost, 0)
    push!(graph.variable_dist, INFINITY)
    push!(graph.variable_best, 0)
    return Node(length(graph.variable_best), _VariableNode)
end

function set_variable_constraint_node(
    graph::Graph,
    variable_node::Node,
    constraint_node::Node,
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
    return Node(length(graph.constraint_best), _ConstraintNode)
end

function add_objective_node(graph::Graph)
    push!(graph.objective_edges, Edge[])
    push!(graph.objective_dist, INFINITY)
    push!(graph.objective_best, 0)
    return Node(length(graph.objective_best), _ObjectiveNode)
end

function bridging_cost(graph::Graph, node::Node)
    bellman_ford!(graph)
    dist = _dist(graph, node)
    return dist == INFINITY ? Inf : float(dist)
end

function bridge_index(graph::Graph, node::Node)
    bellman_ford!(graph)
    if node.type == _VariableNode
        return graph.variable_best[node.index]
    elseif node.type == _ConstraintNode
        return graph.constraint_best[node.index]
    else
        return graph.objective_best[node.index]
    end
end

"""
    is_variable_edge_best(graph::Graph, node::Node)

Return a `Bool` indicating whether the value of `_dist(graph, node)` can be
achieved with a variable bridge.
"""
function is_variable_edge_best(graph::Graph, node::Node)
    bellman_ford!(graph)
    return graph.variable_dist[node.index] == _dist(graph, node)
end

# Update `b.variable_dist`, `b.constraint_dist` `b.variable_best` and
# `b.constraint_best` for constrained variable types in `variables` and
# constraint types in `constraints`.
function updated_dist(graph::Graph, current::Int, edges::Vector{Edge})
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

function _dist(graph::Graph, node::Node)
    if iszero(node.index)
        return 0
    elseif node.type == _ConstraintNode
        return graph.constraint_dist[node.index]
    elseif node.type == _ObjectiveNode
        return graph.objective_dist[node.index]
    end
    constraint_node = graph.variable_constraint_node[node.index]
    # If free variables are bridged but the functionize bridge was not added,
    # constraint_node is `Node(INVALID_NODE_INDEX, _ConstraintNode)`.
    dc =
        constraint_node.index == INVALID_NODE_INDEX ? INFINITY :
        _dist(graph, constraint_node)
    dv = graph.variable_dist[node.index]
    if dc == INFINITY
        return dv
    end
    dc += graph.variable_constraint_cost[node.index]
    return dv == INFINITY ? dc : min(dv, dc)
end

_dist(::Graph, ::Nothing) = 0

function added_dist(graph::Graph, edge::Edge)
    d = _dist(graph, edge.added_objective)
    for n in edge.added_constraints
        d += _dist(graph, n)
    end
    for n in edge.added_constraints
        d += _dist(graph, n)
    end
    return d
end

function supports_no_update(graph::Graph, node::Node)
    return iszero(node.index) || _dist(graph, node) != INFINITY
end

supports_no_update(::Graph, ::Nothing) = true

function _supports_all_no_update(graph::Graph, nodes::Vector{Node})
    return all(node -> supports_no_update(graph, node), nodes)
end

function supports_added_no_update(graph::Graph, edge::Edge)
    return _supports_all_no_update(graph, edge.added_variables) &&
           _supports_all_no_update(graph, edge.added_constraints) &&
           supports_no_update(graph, edge.added_objective)
end
