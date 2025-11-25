# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    _replace_moi_variables(
        nodes::Vector{Nonlinear.Node},
        moi_index_to_consecutive_index::Dict{MOI.VariableIndex,Int},
    )

Return a new `Vector{Nonlinear.Node}` where all occurences of
`NODE_MOI_VARIABLE` are replaced by `NODE_VARIABLE` that is 1-indexed and
ordered.
"""
function _replace_moi_variables(
    nodes::Vector{Nonlinear.Node},
    moi_index_to_consecutive_index::Dict{MOI.VariableIndex,Int},
)
    new_nodes = Vector{Nonlinear.Node}(undef, length(nodes))
    for (i, node) in enumerate(nodes)
        if node.type == Nonlinear.NODE_MOI_VARIABLE
            new_nodes[i] = Nonlinear.Node(
                Nonlinear.NODE_VARIABLE,
                moi_index_to_consecutive_index[MOI.VariableIndex(node.index)],
                node.parent,
            )
        else
            new_nodes[i] = node
        end
    end
    return new_nodes
end

@enum(Linearity, CONSTANT, LINEAR, PIECEWISE_LINEAR, NONLINEAR)

"""
    _classify_linearity(
        nodes::Vector{Nonlinear.Node},
        adj::SparseArrays.SparseMatrixCSC,
        subexpression_linearity::Vector{Linearity},
    )

Classify the nodes in a tree as constant, linear, or nonlinear with respect to
the input.
"""
function _classify_linearity(
    nodes::Vector{Nonlinear.Node},
    adj::SparseArrays.SparseMatrixCSC,
    subexpression_linearity::Vector{Linearity},
)
    linearity = Array{Linearity}(undef, length(nodes))
    children_arr = SparseArrays.rowvals(adj)
    for k in length(nodes):-1:1
        node = nodes[k]
        if node.type == Nonlinear.NODE_VARIABLE
            linearity[k] = LINEAR
            continue
        elseif node.type == Nonlinear.NODE_VALUE
            linearity[k] = CONSTANT
            continue
        elseif node.type == Nonlinear.NODE_PARAMETER
            linearity[k] = CONSTANT
            continue
        elseif node.type == Nonlinear.NODE_SUBEXPRESSION
            linearity[k] = subexpression_linearity[node.index]
            continue
        end
        children_idx = SparseArrays.nzrange(adj, k)
        num_constant_children, any_nonlinear = 0, false
        for r in children_idx
            if linearity[children_arr[r]] == NONLINEAR
                any_nonlinear = true
                break
            elseif linearity[children_arr[r]] == CONSTANT
                num_constant_children += 1
            end
        end
        if any_nonlinear
            # If any children are nonlinear, then we're nonlinear...
            linearity[k] = NONLINEAR
            # ...except in the case of ifelse. If the operands are linear then
            # we're piecewise linear.
            op = get(
                Nonlinear.DEFAULT_MULTIVARIATE_OPERATORS,
                node.index,
                nothing,
            )
            if (
                node.type == Nonlinear.NODE_CALL_MULTIVARIATE &&
                op == :ifelse &&
                linearity[children_arr[children_idx[2]]] == LINEAR &&
                linearity[children_arr[children_idx[3]]] == LINEAR
            )
                linearity[k] = PIECEWISE_LINEAR
            end
            continue
        elseif num_constant_children == length(children_idx)
            # If all children are constant, then we're constant.
            linearity[k] = CONSTANT
            continue
        end
        # By this point, some children are constant and some are linear, so if
        # the operator is nonlinear, then we're nonlinear.
        if node.type == Nonlinear.NODE_CALL_UNIVARIATE
            op =
                get(Nonlinear.DEFAULT_UNIVARIATE_OPERATORS, node.index, nothing)
            if op == :+ || op == :-
                linearity[k] = LINEAR
            else
                linearity[k] = NONLINEAR
            end
        elseif node.type == Nonlinear.NODE_CALL_MULTIVARIATE
            op = get(
                Nonlinear.DEFAULT_MULTIVARIATE_OPERATORS,
                node.index,
                nothing,
            )
            if op == :+
                linearity[k] = LINEAR
            elseif op == :-
                linearity[k] = LINEAR
            elseif op == :*
                # Multiplication is linear if there is one non-constant term.
                one_op = num_constant_children == length(children_idx) - 1
                linearity[k] = one_op ? LINEAR : NONLINEAR
            elseif op == :^
                linearity[k] = NONLINEAR
            elseif op == :/
                if linearity[children_arr[children_idx[2]]] == CONSTANT
                    # If the denominator is constant, we're linear.
                    linearity[k] = LINEAR
                else
                    linearity[k] = NONLINEAR
                end
            elseif op == :ifelse
                linearity[k] = NONLINEAR
            else  # User-defined functions
                linearity[k] = NONLINEAR
            end
        elseif node.type == Nonlinear.NODE_LOGIC
            linearity[k] = NONLINEAR
        else
            @assert node.type == Nonlinear.NODE_COMPARISON
            linearity[k] = NONLINEAR
        end
    end
    return linearity
end

"""
    _compute_gradient_sparsity!(
        indices::Coloring.IndexedSet,
        nodes::Vector{Nonlinear.Node},
    )

Compute the sparsity pattern of the gradient of an expression (that is, a list of
which variable indices are present).
"""
function _compute_gradient_sparsity!(
    indices::Coloring.IndexedSet,
    nodes::Vector{Nonlinear.Node},
)
    for node in nodes
        if node.type == Nonlinear.NODE_VARIABLE
            push!(indices, node.index)
        elseif node.type == Nonlinear.NODE_MOI_VARIABLE
            error(
                "Internal error: Invalid to compute sparsity if Nonlinear.NODE_MOI_VARIABLE " *
                "nodes are present.",
            )
        end
    end
    return
end

"""
    _get_nonlinear_child_interactions(
        node::Nonlinear.Node,
        num_children::Int,
    )

Get the list of nonlinear child interaction pairs for a node.
Returns empty list of tuples `(i, j)` where `i` and `j` are child indices (1-indexed)
that have nonlinear interactions.

For example, for `*` with 2 children, the result is `[(1, 2)]` because children 1
and 2 interact nonlinearly, but children 1 and 1, or 2 and 2, do not.

For functions like `+` or `-`, the result is `[]` since there are no nonlinear
interactions between children.
"""
function _get_nonlinear_child_interactions(
    node::Nonlinear.Node,
    num_children::Int,
)::Vector{Tuple{Int,Int}}
    if node.type == Nonlinear.NODE_CALL_UNIVARIATE
        @assert num_children == 1
        op = get(Nonlinear.DEFAULT_UNIVARIATE_OPERATORS, node.index, nothing)
        # Univariate operators :+ and :- don't create interactions
        if op in (:+, :-)
            return Tuple{Int,Int}[]
        else
            return [(1, 1)]
        end
    elseif node.type == Nonlinear.NODE_CALL_MULTIVARIATE
        op = get(Nonlinear.DEFAULT_MULTIVARIATE_OPERATORS, node.index, nothing)
        if op in (:+, :-, :ifelse, :min, :max)
            # No nonlinear interactions between children
            return Tuple{Int,Int}[]
        elseif op == :*
            # All pairs of distinct children interact nonlinearly
            result = Tuple{Int,Int}[]
            for i in 1:num_children
                for j in 1:(i-1)
                    push!(result, (j, i))
                end
            end
            return result
        elseif op == :/
            @assert num_children == 2
            # The numerator doesn't have a nonlinear interaction with itself.
            return [(1, 2), (2, 2)]
        else
            # Conservative: assume all pairs interact
            result = Tuple{Int,Int}[]
            for i in 1:num_children
                for j in 1:i
                    push!(result, (j, i))
                end
            end
            return result
        end
    else
        # Logic and comparison nodes don't generate hessian terms.
        # Subexpression nodes are special cased.
        return Tuple{Int,Int}[]
    end
end

"""
    _compute_hessian_sparsity(
        nodes::Vector{Nonlinear.Node},
        adj,
        input_linearity::Vector{Linearity},
        subexpression_edgelist::Vector{Set{Tuple{Int,Int}}},
        subexpression_variables::Vector{Vector{Int}},
    )

Compute the sparsity pattern the Hessian of an expression.

 * `input_linearity` is the linearity with respect to the input, computed by
   `_classify_linearity`
 * `subexpression_edgelist` is the edge_list of each subexpression
 * `subexpression_variables` is the list of all variables which appear in a
   subexpression (including recursively).

Returns a `Set{Tuple{Int,Int}}` containing the nonzero entries of the Hessian.
"""
function _compute_hessian_sparsity(
    nodes::Vector{Nonlinear.Node},
    adj,
    input_linearity::Vector{Linearity},
    subexpression_edgelist::Vector{Set{Tuple{Int,Int}}},
    subexpression_variables::Vector{Vector{Int}},
)
    edge_list = Set{Tuple{Int,Int}}()
    children_arr = SparseArrays.rowvals(adj)
    # Stack entry: (node_index, child_group_index)
    stack = Tuple{Int,Int}[]
    # Map from child_group_index to variable indices
    child_group_variables = Dict{Int,Set{Int}}()
    for (k, node) in enumerate(nodes)
        @assert node.type != Nonlinear.NODE_MOI_VARIABLE
        if input_linearity[k] == CONSTANT
            continue  # No hessian contribution from constant nodes
        end
        # Check if this node has nonlinear child interactions
        children_idx = SparseArrays.nzrange(adj, k)
        num_children = length(children_idx)
        interactions = _get_nonlinear_child_interactions(node, num_children)
        if !isempty(interactions)
            # This node has nonlinear child interactions, so collect variables
            # from its children
            empty!(child_group_variables)
            # DFS from all children, tracking child index
            for (child_position, cidx) in enumerate(children_idx)
                child_node_idx = children_arr[cidx]
                push!(stack, (child_node_idx, child_position))
            end
            while length(stack) > 0
                r, child_group_idx = pop!(stack)
                # Don't traverse into logical conditions or comparisons
                if nodes[r].type == Nonlinear.NODE_LOGIC ||
                   nodes[r].type == Nonlinear.NODE_COMPARISON
                    continue
                end
                r_children_idx = SparseArrays.nzrange(adj, r)
                for cidx in r_children_idx
                    push!(stack, (children_arr[cidx], child_group_idx))
                end
                if nodes[r].type == Nonlinear.NODE_VARIABLE
                    if !haskey(child_group_variables, child_group_idx)
                        child_group_variables[child_group_idx] = Set{Int}()
                    end
                    push!(
                        child_group_variables[child_group_idx],
                        nodes[r].index,
                    )
                elseif nodes[r].type == Nonlinear.NODE_SUBEXPRESSION
                    sub_vars = subexpression_variables[nodes[r].index]
                    if !haskey(child_group_variables, child_group_idx)
                        child_group_variables[child_group_idx] = Set{Int}()
                    end
                    union!(child_group_variables[child_group_idx], sub_vars)
                end
            end
            _add_hessian_edges!(edge_list, interactions, child_group_variables)
        elseif node.type == Nonlinear.NODE_SUBEXPRESSION
            for ij in subexpression_edgelist[node.index]
                push!(edge_list, ij)
            end
        end
    end
    return edge_list
end

"""
    _add_hessian_edges!(
        edge_list::Set{Tuple{Int,Int}},
        interactions::Vector{Tuple{Int,Int}},
        child_variables::Dict{Int,Set{Int}},
    )

Add hessian edges based on the operator's nonlinear interaction pattern.
"""
function _add_hessian_edges!(
    edge_list::Set{Tuple{Int,Int}},
    interactions::Vector{Tuple{Int,Int}},
    child_variables::Dict{Int,Set{Int}},
)
    for (child_i, child_j) in interactions
        if child_i == child_j
            # Within-child interactions: add all pairs from a single child
            if haskey(child_variables, child_i)
                vars = child_variables[child_i]
                for vi in vars
                    for vj in vars
                        i, j = minmax(vi, vj)
                        push!(edge_list, (j, i))
                    end
                end
            end
        else
            # Between-child interactions: add pairs from different children
            if haskey(child_variables, child_i) &&
               haskey(child_variables, child_j)
                vars_i = child_variables[child_i]
                vars_j = child_variables[child_j]
                for vi in vars_i
                    for vj in vars_j
                        i, j = minmax(vi, vj)
                        push!(edge_list, (j, i))
                    end
                end
            end
        end
    end
    return
end

"""
    _list_subexpressions(nodes::Vector{Nonlinear.Node})

Returns the list of subexpressions which a given tape depends on directly
"""
function _list_subexpressions(nodes::Vector{Nonlinear.Node})
    indices = Set{Int}(
        n.index for n in nodes if n.type == Nonlinear.NODE_SUBEXPRESSION
    )
    return sort(collect(indices))
end

"""
    _topological_sort(
        starts::Vector{Int},
        subexpressions::Vector{Vector{Nonlinear.Node}},
        subexpression_dependency_graph::Vector{Vector{Int}} =
            Vector{Vector{Int}}(undef, length(subexpressions)),
    )

Return a topologically sorted list of the integer subexpression indices that
need to be computed to evaluate `subexpressions[s]` for all `s in starts`.

`starts` should be ordered, and not contain duplicates.

`subexpression_dependency_graph[i]` is a lazily computed list of "out" edges
from node `i`, in terms of the integer-valued subexpression index (that is,
`node.index`). This list should be unique and ordered.

If calling `_topological_sort` a single time, you may omit the
`subexpression_dependency_graph` argument.

However, if calling `_topological_sort` multiple times on the _same_ vector of
subexpressions, you should create `subexpression_dependency_graph` once (either
as the uninitialized vector, or by explicitly computing the full
`subexpression_dependency_graph`), and pass it in.

## Notes

* It is important to not use recursion here, because expressions may have
  arbitrary levels of nesting.
* This function assumes `subexpressions` is acyclic.
"""
function _topological_sort(
    starts,
    subexpressions::Vector{Vector{Nonlinear.Node}},
    subexpression_dependency_graph::Vector{Vector{Int}} = Vector{Vector{Int}}(
        undef,
        length(subexpressions),
    ),
)
    ordered = Int[]
    in_order = fill(false, length(subexpressions))
    stack = Tuple{Int,Bool}[]
    for s in starts
        if in_order[s]
            continue  # s is already in `ordered`.
        end
        push!(stack, (s, true))
        while !isempty(stack)
            node, needs_checking = pop!(stack)
            if !needs_checking
                # We must be returning to this node for a second time, and we
                # have already checked all of the children. Therefore, we can
                # add it to the set of ordered nodes.
                push!(ordered, node)
                in_order[node] = true
                continue
            elseif in_order[node]
                continue  # This node has already been added to `ordered`.
            end
            # Re-add the node to the stack, but set the `false` flag this time
            # so next time we visit it, it will go on the `ordered` list
            # instead.
            push!(stack, (node, false))
            if !isassigned(subexpression_dependency_graph, node)
                subexpression_dependency_graph[node] =
                    _list_subexpressions(subexpressions[node])
            end
            for child in subexpression_dependency_graph[node]
                if !in_order[child]
                    push!(stack, (child, true))
                end
            end
        end
    end
    return ordered
end

"""
    _order_subexpressions(
        main_expressions::Vector{Vector{Nonlinear.Node}},
        subexpressions::Vector{Vector{Nonlinear.Node}};
    )

Topologically sort the subexpression needed to evaluate `main_expressions`.

Returns two things:

 * A `Vector{Int}` containing the ordered list of subexpression-indices that
   need to be evaluated to compute all `main_expressions`
 * A `Vector{Vector{Int}}`, containing a list of ordered lists of
   subexpression-indices that need to be evaluated to compute
   `main_expressions[i]`.

**Warning:** This doesn't handle cyclic expressions. But this should be fine
because we can't compute them in JuMP anyway.
"""
function _order_subexpressions(
    main_expressions::Vector{Vector{Nonlinear.Node}},
    subexpressions::Vector{Vector{Nonlinear.Node}},
)
    # The graph of node dependencies. Constructed lazily.
    subexpression_dependency_graph =
        Vector{Vector{Int}}(undef, length(subexpressions))
    # Node dependencies of the main expressions.
    starts = Set{Int}()
    individual_sorts = Vector{Int}[]
    for expression in main_expressions
        s = _list_subexpressions(expression)
        union!(starts, s)
        push!(
            individual_sorts,
            _topological_sort(
                s,
                subexpressions,
                subexpression_dependency_graph,
            ),
        )
    end
    full_sort = _topological_sort(
        starts,
        subexpressions,
        subexpression_dependency_graph,
    )
    return full_sort, individual_sorts
end
