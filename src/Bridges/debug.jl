# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

function print_node(io::IO, b::LazyBridgeOptimizer, node::VariableNode)
    S, = b.variable_types[node.index]
    MOI.Utilities.print_with_acronym(
        io,
        "[$(node.index)] constrained variables in `$S` are",
    )
    return
end

function print_node(io::IO, b::LazyBridgeOptimizer, node::ConstraintNode)
    F, S = b.constraint_types[node.index]
    MOI.Utilities.print_with_acronym(
        io,
        "($(node.index)) `$F`-in-`$S` constraints are",
    )
    return
end

function print_node(io::IO, b::LazyBridgeOptimizer, node::ObjectiveNode)
    F, = b.objective_types[node.index]
    MOI.Utilities.print_with_acronym(
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
            MOI.Utilities.print_with_acronym(
                io,
                string(_bridge_type(b, node, index)),
            )
            println(io, ".")
        end
    end
    return
end

function print_graph(b::LazyBridgeOptimizer; kwargs...)
    print_graph(Base.stdout, b; kwargs...)
    return
end

"""
    print_graph([io::IO = stdout,] b::LazyBridgeOptimizer)

Print the hyper-graph containing all variable, constraint, and objective types
that could be obtained by bridging the variables, constraints, and objectives
that are present in the model by all the bridges added to `b`.

Each node in the hyper-graph corresponds to a variable, constraint, or objective
type.

 * Variable nodes are indicated by `[ ]`
 * Constraint nodes are indicated by `( )`
 * Objective nodes are indicated by `| |`

The number inside each pair of brackets is an index of the node in the
hyper-graph.

Note that this hyper-graph is the full list of possible transformations. When
the bridged model is created, we select the shortest hyper-path(s) from this
graph, so many nodes may be un-used.

To see which nodes are used, call [`print_active_bridges`](@ref).

For more information, see Legat, B., Dowson, O., Garcia, J., and Lubin, M.
(2020).  "MathOptInterface: a data structure for mathematical optimization
problems." URL: [https://arxiv.org/abs/2002.03447](https://arxiv.org/abs/2002.03447)
"""
function print_graph(io::IO, b::LazyBridgeOptimizer; kwargs...)
    println(io, b.graph)
    print_nodes(
        io,
        b,
        VariableNode.(eachindex(b.graph.variable_best)),
        ConstraintNode.(eachindex(b.graph.constraint_best)),
        ObjectiveNode.(eachindex(b.graph.objective_best));
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
        MOI.Utilities.print_with_acronym(
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
    if b.variable_types[node.index] == (MOI.Reals,)
        return
    end
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
        MOI.Utilities.print_with_acronym(
            io,
            "Constrained variables in `$S` are supported.\n",
        )
    else
        MOI.Utilities.print_with_acronym(io, "Constrained variables in `$S`")
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
        MOI.Utilities.print_with_acronym(
            io,
            "`$F`-in-`$S` constraints are supported.\n",
        )
    else
        MOI.Utilities.print_with_acronym(io, "`$F`-in-`$S` constraints")
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
    F::Type{<:MOI.AbstractFunction};
    io::IO = Base.stdout,
)
    MOI.Utilities.print_with_acronym(io, "Objective function of type `$F` is")
    if supports_bridging_objective_function(b, F)
        MOI.Utilities.print_with_acronym(io, " supported.\n")
    else
        MOI.Utilities.print_with_acronym(
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

"""
    print_active_bridges([io::IO=stdout,] b::MOI.Bridges.LazyBridgeOptimizer)

Print the set of bridges that are active in the model `b`.
"""
function print_active_bridges(io::IO, b::MOI.Bridges.LazyBridgeOptimizer)
    F = MOI.get(b, MOI.ObjectiveFunctionType())
    print_active_bridges(io, b, F)
    types = MOI.get(b, MOI.ListOfConstraintTypesPresent())
    # We add a sort here to make the order reproducible, and to group similar
    # constraints together.
    for (F, S) in sort(types; by = string)
        print_active_bridges(io, b, F, S)
    end
    return
end

function print_active_bridges(b::MOI.Bridges.LazyBridgeOptimizer, args...)
    return print_active_bridges(stdout, b, args...)
end

function _print_supported(io, arg...)
    s = sprint(MOI.Utilities.print_with_acronym, arg...)
    return Printf.printstyled(io, s; bold = false, color = :green)
end

function _print_unsupported(io, arg...)
    s = sprint(MOI.Utilities.print_with_acronym, arg...)
    return Printf.printstyled(io, s; bold = false, color = :red)
end

function _print_bridge(io, b, ::Type{BT}, offset) where {BT}
    new_offset = offset * " |  "
    for (F, S) in MOI.Bridges.added_constraint_types(BT)
        print_active_bridges(io, b, F, S, new_offset)
    end
    for (S,) in MOI.Bridges.added_constrained_variable_types(BT)
        print_active_bridges(io, b, S, new_offset)
    end
    return
end

"""
    print_active_bridges(
        [io::IO=stdout,]
        b::MOI.Bridges.LazyBridgeOptimizer,
        F::Type{<:MOI.AbstractFunction}
    )

Print the set of bridges required for an objective function of type `F`.
"""
function print_active_bridges(
    io::IO,
    b::MOI.Bridges.LazyBridgeOptimizer,
    ::Type{F},
    offset::String = "",
) where {F<:MOI.AbstractFunction}
    if !MOI.supports(b, MOI.ObjectiveFunction{F}())
        throw(
            MOI.UnsupportedAttribute(
                MOI.ObjectiveFunction{F}(),
                "The objective function cannot be bridged using the set of " *
                "available bridges.",
            ),
        )
    end
    if !MOI.Bridges.is_bridged(b, F)
        print(io, offset, " * ")
        _print_supported(io, "Supported objective: $F\n")
        return
    end
    print(io, offset, " * ")
    _print_unsupported(io, "Unsupported objective: $F\n")
    index = MOI.Bridges.bridge_index(b.graph, b.objective_node[(F,)])
    B = b.objective_bridge_types[index]
    BT = MOI.Bridges.Objective.concrete_bridge_type(B, F)
    println(io, offset, " |  bridged by:")
    print(io, offset, " |   ")
    MOI.Utilities.print_with_acronym(io, "$BT\n")
    println(io, offset, " |  may introduce:")
    G = MOI.Bridges.set_objective_function_type(BT)
    print_active_bridges(io, b, G, offset * " |  ")
    _print_bridge(io, b, BT, offset)
    return
end

"""
    print_active_bridges(
        [io::IO=stdout,]
        b::MOI.Bridges.LazyBridgeOptimizer,
        F::Type{<:MOI.AbstractFunction},
        S::Type{<:MOI.AbstractSet},
    )

Print the set of bridges required for a constraint of type `F`-in-`S`.
"""
function print_active_bridges(
    io::IO,
    b::MOI.Bridges.LazyBridgeOptimizer,
    ::Type{F},
    ::Type{S},
    offset::String = "",
) where {F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    if !MOI.supports_constraint(b, F, S)
        throw(
            MOI.UnsupportedConstraint{F,S}(
                "The constraint cannot be bridged using the set of available " *
                "bridges.",
            ),
        )
    end
    if !MOI.Bridges.is_bridged(b, F, S)
        print(io, offset, " * ")
        _print_supported(io, "Supported constraint: $F-in-$S\n")
        return
    end
    is_constraint_bridged = true
    c_node = b.constraint_node[(F, S)]
    if F == MOI.VariableIndex || F == MOI.VectorOfVariables
        # Call `MOI.supports_` here to build the necessary nodes in the bridging
        # graph.
        if F == MOI.VariableIndex
            MOI.supports_add_constrained_variable(b, S)
        else
            MOI.supports_add_constrained_variables(b, S)
        end
        v_node = b.variable_node[(S,)]
        if bridging_cost(b.graph, v_node) <= bridging_cost(b.graph, c_node)
            is_constraint_bridged = false
        end
    end
    if is_constraint_bridged
        index = MOI.Bridges.bridge_index(b.graph, c_node)
        B = b.constraint_bridge_types[index]
        BT = MOI.Bridges.Constraint.concrete_bridge_type(B, F, S)
        print(io, offset, " * ")
        _print_unsupported(io, "Unsupported constraint: $F-in-$S\n")
        println(io, offset, " |  bridged by:")
        print(io, offset, " |   ")
        MOI.Utilities.print_with_acronym(io, "$BT\n")
        println(io, offset, " |  may introduce:")
        _print_bridge(io, b, BT, offset)
    else
        print_active_bridges(io, b, S, offset)
    end
    return
end

"""
    print_active_bridges(
        [io::IO=stdout,]
        b::MOI.Bridges.LazyBridgeOptimizer,
        S::Type{<:MOI.AbstractSet}
    )

Print the set of bridges required for a variable constrained to set `S`.
"""
function print_active_bridges(
    io::IO,
    b::MOI.Bridges.LazyBridgeOptimizer,
    ::Type{S},
    offset::String = "",
) where {S<:MOI.AbstractSet}
    if S <: MOI.AbstractScalarSet
        if !MOI.supports_add_constrained_variable(b, S)
            throw(
                MOI.UnsupportedConstraint{MOI.VariableIndex,S}(
                    "The variable cannot be bridged using the set of " *
                    "available bridges.",
                ),
            )
        end
    else
        @assert S <: MOI.AbstractVectorSet
        if !MOI.supports_add_constrained_variables(b, S)
            throw(
                MOI.UnsupportedConstraint{MOI.VectorOfVariables,S}(
                    "The variables cannot be bridged using the set of " *
                    "available bridges.",
                ),
            )
        end
    end
    if !MOI.Bridges.is_bridged(b, S)
        print(io, offset, " * ")
        _print_supported(io, "Supported variable: $S\n")
        return
    end
    index = MOI.Bridges.bridge_index(b.graph, b.variable_node[(S,)])
    B = b.variable_bridge_types[index]
    BT = MOI.Bridges.Variable.concrete_bridge_type(B, S)
    print(io, offset, " * ")
    _print_unsupported(io, "Unsupported variable: $S\n")
    println(io, offset, " |  bridged by:")
    print(io, offset, " |    ")
    MOI.Utilities.print_with_acronym(io, "$BT\n")
    println(io, offset, " |  may introduce:")
    _print_bridge(io, b, BT, offset)
    return
end
