function print_node(io::IO, b::LazyBridgeOptimizer, node::VariableNode)
    S, = b.variable_types[node.index]
    MOIU.print_with_acronym(io, "[$(node.index)] constrained variables in `$S` are")
end
function print_node(io::IO, b::LazyBridgeOptimizer, node::ConstraintNode)
    F, S = b.constraint_types[node.index]
    MOIU.print_with_acronym(io, "($(node.index)) `$F`-in-`$S` constraints are")
end
function print_node(io::IO, b::LazyBridgeOptimizer, node::ObjectiveNode)
    F, = b.objective_types[node.index]
    MOIU.print_with_acronym(io, "|$(node.index)| objective function of type `$F` is")
end
function print_unsupported(io::IO, b::LazyBridgeOptimizer, node::AbstractNode)
    if _dist(b.graph, node) != INFINITY
        return
    end
    print(io, "  ")
    print_node(io, b, node)
    print(io, " not supported\n")
end
function print_unsupported(io::IO, b::LazyBridgeOptimizer, edge::Edge)
    for node in edge.added_variables
        print_unsupported(io, b, node)
    end
    for node in edge.added_constraints
        print_unsupported(io, b, node)
    end
end
function print_unsupported(io::IO, b::LazyBridgeOptimizer, edge::ObjectiveEdge)
    for node in edge.added_variables
        print_unsupported(io, b, node)
    end
    for node in edge.added_constraints
        print_unsupported(io, b, node)
    end
    print_unsupported(io, b, edge.added_objective)
end
function print_unsupported(io::IO, b::LazyBridgeOptimizer, edges::Vector, index_to_bridge::Function)
    no_bridge = true
    for edge in edges
        if no_bridge
            println(io, ":")
            no_bridge = false
        end
        MOIU.print_with_acronym(io, "  Cannot use `$(index_to_bridge(edge.bridge_index))` because:\n")
        print_unsupported(io, b, edge)
    end
    if no_bridge
        println(io, " no added bridge supports bridging it.")
    end
end
function print_unsupported(io::IO, b::LazyBridgeOptimizer, variables, constraints, objectives)
    for node in variables
        print_node(io, b, node)
        print(io, " not supported because")
        print_unsupported(io, b, b.graph.variable_edges[node.index],
                          bridge_index -> Variable.concrete_bridge_type(b.variable_bridge_types[bridge_index], b.variable_types[node.index]...))
        println(io, "  Cannot add free variables and then constrain them because:")
        print_unsupported(io, b, node.constraint_node)
    end
    for node in constraints
        print_node(io, b, node)
        print(io, " not supported because")
        print_unsupported(io, b, b.graph.constraint_edges[node.index],
                          bridge_index -> Constraint.concrete_bridge_type(b.constraint_bridge_types[bridge_index], b.constraint_types[node.index]...))
    end
    for node in objectives
        print_node(io, b, node)
        print(io, " not supported because")
        print_unsupported(io, b, b.graph.objective_edges[node.index],
                          bridge_index -> Objective.concrete_bridge_type(b.objective_bridge_types[bridge_index], b.objective_types[node.index]...))
    end
end

function add_unsupported(graph::Graph, edges::Vector{Edge},
                         variables, constraints, objectives)
    for edge in edges
        for node in edge.added_variables
            add_unsupported(graph, node, variables, constraints, objectives)
        end
        for node in edge.added_constraints
            add_unsupported(graph, node, variables, constraints, objectives)
        end
    end
end
function add_unsupported(graph::Graph, edges::Vector{ObjectiveEdge},
                         variables, constraints, objectives)
    for edge in edges
        for node in edge.added_variables
            add_unsupported(graph, node, variables, constraints, objectives)
        end
        for node in edge.added_constraints
            add_unsupported(graph, node, variables, constraints, objectives)
        end
        add_unsupported(graph, edge.added_objective, variables, constraints, objectives)
    end
end
function add_unsupported(graph::Graph, node::VariableNode,
                         variables, constraints, objectives)
    if _dist(graph, node) != INFINITY
        return
    end
    push!(variables, node)
    add_unsupported(graph, graph.variable_edges[node.index], variables, constraints, objectives)
    add_unsupported(graph, node.constraint_node, variables, constraints, objectives)
end
function add_unsupported(graph::Graph, node::ConstraintNode,
                         variables, constraints, objectives)
    if _dist(graph, node) != INFINITY
        return
    end
    push!(constraints, node)
    add_unsupported(graph, graph.constraint_edges[node.index], variables, constraints, objectives)
end
function add_unsupported(graph::Graph, node::ObjectiveNode,
                         variables, constraints, objectives)
    if _dist(graph, node) != INFINITY
        return
    end
    push!(objectives, node)
    add_unsupported(graph, graph.objective_edges[node.index], variables, constraints, objectives)
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
    print_unsupported(io, b, _sort(variables), _sort(constraints), _sort(objectives))
end
function debug(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction},
               S::Type{<:MOI.AbstractSet}; io::IO = Base.stdout)
    if MOI.supports_constraint(b, F, S)
        if F == MOIU.variable_function_type(S)
            # This may be thanks to a variable bridge so `F`-in-`S` constraints
            # are maybe not supported but constrained variables in `S` are
            # definitely supported.
            MOIU.print_with_acronym(io, "Constrained variables in `$S` are supported.\n")
        else
            MOIU.print_with_acronym(io, "`$F`-in-`$S` constraints are supported.\n")
        end
    else
        message = " are not supported and cannot be bridged into supported" *
            " constrained variables and constraints. See details below:"
        if F == MOIU.variable_function_type(S)
            MOIU.print_with_acronym(io, "Constrained variables in `$S`")
            println(io, message)
            debug_unsupported(io, b, node(b, S))
        else
            MOIU.print_with_acronym(io, "`$F`-in-`$S` constraints")
            println(io, message)
            debug_unsupported(io, b, node(b, F, S))
        end
    end
end

"""
    debug_supports_constraint(
        b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction},
        S::Type{<:MOI.AbstractSet}; io::IO = Base.stdout)

Prints to `io` explanations for the value of [`MOI.supports_constraint`](@ref)
with the same arguments.
"""
function debug_supports_constraint(
    b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet}; kws...)
    debug(b, F, S; kws...)
end

function debug(b::LazyBridgeOptimizer,
               F::Type{<:MOI.AbstractScalarFunction}; io::IO = Base.stdout)
    MOIU.print_with_acronym(io, "Objective function of type `$F` is")
    if supports_bridging_objective_function(b, F)
        MOIU.print_with_acronym(io, " supported.\n")
    else
        MOIU.print_with_acronym(io, " not supported and cannot be bridged" *
            " into a supported objective function by adding only supported" *
            " constrained variables and constraints. See details below:\n")
        debug_unsupported(io, b, node(b, F))
    end
end

"""
    debug_supports(b::LazyBridgeOptimizer, ::MOI.ObjectiveFunction{F}; io::IO = Base.stdout) where F

Prints to `io` explanations for the value of [`MOI.supports`](@ref) with the
same arguments.
"""
function debug_supports(
    b::LazyBridgeOptimizer, ::MOI.ObjectiveFunction{F}; kws...) where F
    debug(b, F; kws...)
end
