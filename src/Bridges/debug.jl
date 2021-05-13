function print_node(io::IO, b::LazyBridgeOptimizer, node::VariableNode)
    S, = b.variable_types[node.index]
    MOIU.print_with_acronym(
        io,
        "[$(node.index)] constrained variables in `$S` are",
    )
    return
end

function print_node(io::IO, b::LazyBridgeOptimizer, node::ConstraintNode)
    F, S = b.constraint_types[node.index]
    MOIU.print_with_acronym(io, "($(node.index)) `$F`-in-`$S` constraints are")
    return
end

function print_node(io::IO, b::LazyBridgeOptimizer, node::ObjectiveNode)
    F, = b.objective_types[node.index]
    MOIU.print_with_acronym(
        io,
        "|$(node.index)| objective function of type `$F` is",
    )
    return
end

function print_node_info(
    io::IO,
    b::LazyBridgeOptimizer,
    node::AbstractNode;
    debug_unsupported = false,
)
    print(io, " ")
    print_node(io, b, node)
    d = _dist(b.graph, node)
    if d == INFINITY
        print(io, " not supported")
        if debug_unsupported
            print(io, " because")
            print_unsupported(io, b, node)
        else
            println(io)
        end
    else
        index = bridge_index(b.graph, node)
        if iszero(index) ||
           (node isa VariableNode && !is_variable_edge_best(b.graph, node))
            @assert node isa VariableNode
            println(
                io,
                " supported (distance $d) by adding free variables and then constrain them, see ($(b.graph.variable_constraint_node[node.index].index)).",
            )
        else
            print(io, " bridged (distance $d) by ")
            MOIU.print_with_acronym(io, string(_bridge_type(b, node, index)))
            println(io, ".")
        end
    end
    return
end

function print_graph(b::LazyBridgeOptimizer; kwargs...)
    print_graph(Base.stdout, b; kwargs...)
    return
end

function print_graph(io::IO, b::LazyBridgeOptimizer; kwargs...)
    println(io, b.graph)
    print_nodes(
        io,
        b,
        variable_nodes(b.graph),
        constraint_nodes(b.graph),
        objective_nodes(b.graph);
        kwargs...,
    )
    return
end

function print_nodes(
    io::IO,
    b::LazyBridgeOptimizer,
    variable_nodes,
    constraint_nodes,
    objective_nodes;
    kwargs...,
)
    for node in variable_nodes
        print_node_info(io, b, node; kwargs...)
    end
    for node in constraint_nodes
        print_node_info(io, b, node; kwargs...)
    end
    for node in objective_nodes
        print_node_info(io, b, node; kwargs...)
    end
    return
end

function print_if_unsupported(
    io::IO,
    b::LazyBridgeOptimizer,
    node::AbstractNode,
)
    if _dist(b.graph, node) != INFINITY
        return
    end
    print(io, "   ")
    print_node(io, b, node)
    print(io, " not supported\n")
    return
end

function print_unsupported(io::IO, b::LazyBridgeOptimizer, edge::Edge)
    for node in edge.added_variables
        print_if_unsupported(io, b, node)
    end
    for node in edge.added_constraints
        print_if_unsupported(io, b, node)
    end
    return
end

function print_unsupported(io::IO, b::LazyBridgeOptimizer, edge::ObjectiveEdge)
    for node in edge.added_variables
        print_if_unsupported(io, b, node)
    end
    for node in edge.added_constraints
        print_if_unsupported(io, b, node)
    end
    print_if_unsupported(io, b, edge.added_objective)
    return
end

function print_unsupported(
    io::IO,
    b::LazyBridgeOptimizer,
    edges::Vector,
    index_to_bridge::Function,
)
    no_bridge = true
    for edge in edges
        if no_bridge
            println(io, ":")
            no_bridge = false
        end
        MOIU.print_with_acronym(
            io,
            "   Cannot use `$(index_to_bridge(edge.bridge_index))` because:\n",
        )
        print_unsupported(io, b, edge)
    end
    if no_bridge
        println(io, " no added bridge supports bridging it.")
    end
    return
end

function _bridge_type(
    b::LazyBridgeOptimizer,
    node::VariableNode,
    bridge_index::Int,
)
    return Variable.concrete_bridge_type(
        b.variable_bridge_types[bridge_index],
        b.variable_types[node.index]...,
    )
end

function _bridge_type(
    b::LazyBridgeOptimizer,
    node::ConstraintNode,
    bridge_index::Int,
)
    return Constraint.concrete_bridge_type(
        b.constraint_bridge_types[bridge_index],
        b.constraint_types[node.index]...,
    )
end

function _bridge_type(
    b::LazyBridgeOptimizer,
    node::ObjectiveNode,
    bridge_index::Int,
)
    return Objective.concrete_bridge_type(
        b.objective_bridge_types[bridge_index],
        b.objective_types[node.index]...,
    )
end

function print_unsupported(io::IO, b::LazyBridgeOptimizer, node::VariableNode)
    print_unsupported(
        io,
        b,
        b.graph.variable_edges[node.index],
        bridge_index -> _bridge_type(b, node, bridge_index),
    )
    print(io, "   Cannot add free variables and then constrain them because")
    constraint_node = b.graph.variable_constraint_node[node.index]
    if constraint_node.index == -1
        println(
            io,
            " free variables are bridged but no functionize bridge was added.",
        )
    else
        println(io, ":")
        print_if_unsupported(io, b, constraint_node)
    end
    return
end

function print_unsupported(io::IO, b::LazyBridgeOptimizer, node::ConstraintNode)
    print_unsupported(
        io,
        b,
        b.graph.constraint_edges[node.index],
        bridge_index -> _bridge_type(b, node, bridge_index),
    )
    return
end

function print_unsupported(io::IO, b::LazyBridgeOptimizer, node::ObjectiveNode)
    print_unsupported(
        io,
        b,
        b.graph.objective_edges[node.index],
        bridge_index -> _bridge_type(b, node, bridge_index),
    )
    return
end

function add_unsupported(
    graph::Graph,
    edges::Vector{Edge},
    variables,
    constraints,
    objectives,
)
    for edge in edges
        for node in edge.added_variables
            add_unsupported(graph, node, variables, constraints, objectives)
        end
        for node in edge.added_constraints
            add_unsupported(graph, node, variables, constraints, objectives)
        end
    end
    return
end

function add_unsupported(
    graph::Graph,
    edges::Vector{ObjectiveEdge},
    variables,
    constraints,
    objectives,
)
    for edge in edges
        for node in edge.added_variables
            add_unsupported(graph, node, variables, constraints, objectives)
        end
        for node in edge.added_constraints
            add_unsupported(graph, node, variables, constraints, objectives)
        end
        add_unsupported(
            graph,
            edge.added_objective,
            variables,
            constraints,
            objectives,
        )
    end
    return
end

function add_unsupported(
    graph::Graph,
    node::VariableNode,
    variables,
    constraints,
    objectives,
)
    if _dist(graph, node) != INFINITY || node in variables
        return
    end
    push!(variables, node)
    add_unsupported(
        graph,
        graph.variable_edges[node.index],
        variables,
        constraints,
        objectives,
    )
    constraint_node = graph.variable_constraint_node[node.index]
    if constraint_node.index != -1
        add_unsupported(
            graph,
            graph.variable_constraint_node[node.index],
            variables,
            constraints,
            objectives,
        )
    end
    return
end

function add_unsupported(
    graph::Graph,
    node::ConstraintNode,
    variables,
    constraints,
    objectives,
)
    if _dist(graph, node) != INFINITY || node in constraints
        return
    end
    push!(constraints, node)
    add_unsupported(
        graph,
        graph.constraint_edges[node.index],
        variables,
        constraints,
        objectives,
    )
    return
end

function add_unsupported(
    graph::Graph,
    node::ObjectiveNode,
    variables,
    constraints,
    objectives,
)
    if _dist(graph, node) != INFINITY || node in objectives
        return
    end
    push!(objectives, node)
    add_unsupported(
        graph,
        graph.objective_edges[node.index],
        variables,
        constraints,
        objectives,
    )
    return
end

function _sort(nodes::Set)
    vector = collect(nodes)
    sort!(vector, by = node -> node.index)
    return vector
end

function debug_unsupported(io::IO, b::LazyBridgeOptimizer, node::AbstractNode)
    variables = Set{VariableNode}()
    constraints = Set{ConstraintNode}()
    objectives = Set{ObjectiveNode}()
    add_unsupported(b.graph, node, variables, constraints, objectives)
    print_nodes(
        io,
        b,
        _sort(variables),
        _sort(constraints),
        _sort(objectives),
        debug_unsupported = true,
    )
    return
end

const UNSUPPORTED_MESSAGE =
    " are not supported and cannot be bridged into supported" *
    " constrained variables and constraints. See details below:"

function debug(
    b::LazyBridgeOptimizer,
    S::Type{<:MOI.AbstractSet};
    io::IO = Base.stdout,
)
    if (
        S <: MOI.AbstractScalarSet &&
        MOI.supports_add_constrained_variable(b, S)
    ) || (
        S <: MOI.AbstractVectorSet &&
        MOI.supports_add_constrained_variables(b, S)
    )
        MOIU.print_with_acronym(
            io,
            "Constrained variables in `$S` are supported.\n",
        )
    else
        MOIU.print_with_acronym(io, "Constrained variables in `$S`")
        println(io, UNSUPPORTED_MESSAGE)
        debug_unsupported(io, b, node(b, S))
    end
    return
end

"""
    debug_supports_add_constrained_variable(
        b::LazyBridgeOptimizer,
        S::Type{<:MOI.AbstractSet};
        io::IO = Base.stdout
    )

Prints to `io` explanations for the value of
[`MOI.supports_add_constrained_variable`](@ref) with the same arguments.
"""
function debug_supports_add_constrained_variable(
    b::LazyBridgeOptimizer,
    S::Type{<:MOI.AbstractSet};
    kwargs...,
)
    debug(b, S; kwargs...)
    return
end

function debug(
    b::LazyBridgeOptimizer,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet};
    io::IO = Base.stdout,
)
    if MOI.supports_constraint(b, F, S)
        MOIU.print_with_acronym(io, "`$F`-in-`$S` constraints are supported.\n")
    else
        MOIU.print_with_acronym(io, "`$F`-in-`$S` constraints")
        println(io, UNSUPPORTED_MESSAGE)
        debug_unsupported(io, b, node(b, F, S))
    end
    return
end

"""
    debug_supports_constraint(
        b::LazyBridgeOptimizer,
        F::Type{<:MOI.AbstractFunction},
        S::Type{<:MOI.AbstractSet};
        io::IO = Base.stdout,
    )

Prints to `io` explanations for the value of [`MOI.supports_constraint`](@ref)
with the same arguments.
"""
function debug_supports_constraint(
    b::LazyBridgeOptimizer,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet};
    kwargs...,
)
    return debug(b, F, S; kwargs...)
end

function debug(
    b::LazyBridgeOptimizer,
    F::Type{<:MOI.AbstractScalarFunction};
    io::IO = Base.stdout,
)
    MOIU.print_with_acronym(io, "Objective function of type `$F` is")
    if supports_bridging_objective_function(b, F)
        MOIU.print_with_acronym(io, " supported.\n")
    else
        MOIU.print_with_acronym(
            io,
            " not supported and cannot be bridged" *
            " into a supported objective function by adding only supported" *
            " constrained variables and constraints. See details below:\n",
        )
        debug_unsupported(io, b, node(b, F))
    end
    return
end

"""
    debug_supports(
        b::LazyBridgeOptimizer,
        ::MOI.ObjectiveFunction{F};
        io::IO = Base.stdout,
    ) where F

Prints to `io` explanations for the value of [`MOI.supports`](@ref) with the
same arguments.
"""
function debug_supports(
    b::LazyBridgeOptimizer,
    ::MOI.ObjectiveFunction{F};
    kwargs...,
) where {F}
    debug(b, F; kwargs...)
    return
end
