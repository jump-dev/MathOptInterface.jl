# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    parse_expression(data::Model, input)::Expression

Parse `input` into a [`Expression`](@ref).
"""
function parse_expression(data::Model, input)
    expr = Expression()
    parse_expression(data, expr, input, -1)
    return expr
end

"""
    parse_expression(
        data::Model,
        expr::Expression,
        input::Any,
        parent_index::Int,
    )::Expression

Parse `input` into a [`Expression`](@ref), and add it to `expr` as a
child of `expr.nodes[parent_index]`. Existing subexpressions and parameters are
stored in `data`.

You can extend parsing support to new types of objects by overloading this
method with a different type on `input::Any`.
"""
function parse_expression(::Model, ::Expression, x::Any, ::Int)
    return error(
        "Unexpected object $x of type $(typeof(x)) in nonlinear expression.",
    )
end

"""
    _extract_subexpression!(expr::Expression, root::Int)

Prepare the extraction of the subtree rooted at `expr.nodes[root]` into a
separate expression. Return `(I, V)` where `I` is the range of `expr.nodes`
and `V` is the range of `expr.values` (or `nothing` if there are no values)
corresponding to the subtree.

Because the nodes are stored in depth-first order, the subtree is the
contiguous range of nodes `root:(first_out-1)` where `first_out` is the first
node after `root` whose parent is before `root`. Its values are contiguous for
the same reason.

This function only renumbers the nodes, it does not delete anything:

 * the nodes in `I` are rewritten so that their `parent` (and the `index` of
   their `NODE_VALUE`) are relative to the start of `I` (and `V`), so that
   `Expression(expr.nodes[I], expr.values[V])` is a valid expression with
   `root` as its root;
 * the nodes after `I` are rewritten as if the nodes `I[2:end]` and the values
   `V` were already deleted, `root` being kept to be replaced by a
   `NODE_SUBEXPRESSION`.
"""
function _extract_subexpression!(expr::Expression, root::Int)
    n = length(expr.nodes)
    first_out = first_value = last_value = nothing
    # Renumber the nodes of the subtree so that they are relative to `root`.
    for i in root:n
        node = expr.nodes[i]
        if i != root && node.parent < root
            # We have left the subtree rooted at `root`.
            first_out = i
            break
        end
        index = node.index
        if node.type == NODE_VALUE
            if isnothing(first_value)
                first_value = node.index
                last_value = first_value
            else
                last_value = node.index
            end
            index -= first_value - 1
        end
        expr.nodes[i] =
            Node(node.type, index, i == root ? -1 : node.parent - root + 1)
    end
    if isnothing(first_out)
        I = root:n
    else
        I = root:(first_out-1)
    end
    if isnothing(first_value)
        V = nothing
    else
        V = first_value:last_value
    end
    # Shift the nodes after the subtree as if `I[2:end]` and `V` were deleted.
    if !isnothing(first_out)
        for i in (last(I)+1):n
            node = expr.nodes[i]
            index = node.index
            if node.type == NODE_VALUE && !isnothing(V)
                @assert index >= last(V)
                index -= length(V)
            end
            parent = node.parent
            # A parent cannot be in `root+1:last(I)` since this node is not in
            # the subtree. If `parent == root`, it is now the parent of the
            # `NODE_SUBEXPRESSION` so it does not change.
            if parent > root
                @assert parent > last(I)
                parent -= length(I) - 1
            end
            expr.nodes[i] = Node(node.type, index, parent)
        end
    end
    return I, V
end

"""
    _extract_subexpression!(data::Model, expr::Expression, root::Int)

Move the subtree rooted at `expr.nodes[root]` to a new expression in
`data.expressions` and replace it in `expr` by a single `NODE_SUBEXPRESSION`
node pointing to it. Return `(index, I)` where `index` is the
[`ExpressionIndex`](@ref) of the new subexpression and `I` is the range of
nodes that the subtree used to occupy in `expr`. As `root` is kept, the
nodes after `I` are shifted by `length(I) - 1`, the caller is responsible for
updating any node index into `expr` that it stores.

This is called when the same `MOI.ScalarNonlinearFunction` is parsed a second
time: its first occurrence was parsed inline into `expr` and is now replaced by
a subexpression that both occurrences can share.

This costs `O(length(expr.nodes) + length(expr.values))` as we need to shift
the nodes after the subtree and delete the nodes and values of the subtree.
"""
function _extract_subexpression!(data::Model, expr::Expression, root::Int)
    parent = expr.nodes[root].parent
    I, V = _extract_subexpression!(expr, root)
    subexpr =
        Expression(expr.nodes[I], isnothing(V) ? Float64[] : expr.values[V])
    push!(data.expressions, subexpr)
    index = ExpressionIndex(length(data.expressions))
    # We are replacing the subtree by a subexpression so we keep the root node,
    # now a `NODE_SUBEXPRESSION`, and remove all the other nodes and values of
    # the subtree from the tape.
    expr.nodes[root] = Node(NODE_SUBEXPRESSION, index.value, parent)
    if length(I) > 1
        deleteat!(expr.nodes, I[2:end])
        if !isnothing(V)
            deleteat!(expr.values, V)
        end
    else
        @assert isnothing(V)
    end
    return index, I
end

function parse_expression(
    data::Model,
    expr::Expression,
    x::MOI.ScalarNonlinearFunction,
    parent_index::Int,
)
    stack = Tuple{Int,Any}[(parent_index, x)]
    expr_index = 0
    while !isempty(stack)
        parent_node, arg = pop!(stack)
        if arg isa MOI.ScalarNonlinearFunction
            if haskey(data.cache, arg)
                # `arg` was already parsed, we reuse it as a subexpression.
                _expr_index, _node = data.cache[arg]
                if _node == 0
                    # It was already extracted into a subexpression.
                    subexpr = ExpressionIndex(_expr_index)
                else
                    # It was parsed inline at node `_node` of `_expr`. This is
                    # its second occurrence so we extract it into a
                    # subexpression. This shifts the nodes of `_expr` after
                    # `I` so we need to update every node index into `_expr`
                    # that we store: those in `stack` (if `_expr` is the one
                    # we are parsing into) and those in `data.cache`.
                    _expr = data.cache_expressions[_expr_index]
                    subexpr, I = _extract_subexpression!(data, _expr, _node)
                    # The nodes of the subtree were moved to the new
                    # subexpression which may itself contain functions that
                    # are cached so we register it in `cache_expressions`.
                    push!(
                        data.cache_expressions,
                        data.expressions[subexpr.value],
                    )
                    subexpr_index = length(data.cache_expressions)
                    if expr === _expr
                        if parent_node > first(I)
                            @assert parent_node > last(I)
                            parent_node -= length(I) - 1
                        end
                        for i in eachindex(stack)
                            _parent_node = stack[i][1]
                            if _parent_node > first(I)
                                @assert _parent_node > last(I)
                                stack[i] =
                                    (_parent_node - length(I) + 1, stack[i][2])
                            end
                        end
                    end
                    # Cached functions in `I[2:end]` now point into the new
                    # subexpression and those after `I` are shifted.
                    # Note that this loops over all the cached functions, so
                    # extracting `k` subexpressions costs
                    # `O(k * length(data.cache))`.
                    for (key, (__expr_index, __node)) in data.cache
                        if __node != 0
                            __expr = data.cache_expressions[__expr_index]
                            if _expr === __expr && __node > first(I)
                                if __node <= last(I)
                                    data.cache[key] =
                                        (subexpr_index, __node - first(I) + 1)
                                else
                                    data.cache[key] =
                                        (__expr_index, __node - length(I) + 1)
                                end
                            end
                        end
                    end
                    data.cache[arg] = (subexpr.value, 0)
                end
                parse_expression(data, expr, subexpr, parent_node)
            else
                _parse_without_recursion_inner(
                    stack,
                    data,
                    expr,
                    arg,
                    parent_node,
                )
                # Cache the node of `arg` in `expr` so that we can extract it
                # if we encounter it again.
                if expr_index == 0
                    push!(data.cache_expressions, expr)
                    expr_index = length(data.cache_expressions)
                end
                data.cache[arg] = (expr_index, length(expr.nodes))
            end
        else
            # We can use recursion here, because ScalarNonlinearFunction only
            # occur in other ScalarNonlinearFunction.
            parse_expression(data, expr, arg, parent_node)
        end
    end
    return
end

function _get_node_type(data, x)
    id = get(data.operators.univariate_operator_to_id, x.head, nothing)
    if length(x.args) == 1 && id !== nothing
        return id, MOI.Nonlinear.NODE_CALL_UNIVARIATE
    end
    id = get(data.operators.multivariate_operator_to_id, x.head, nothing)
    if id !== nothing
        return id, MOI.Nonlinear.NODE_CALL_MULTIVARIATE
    end
    id = get(data.operators.comparison_operator_to_id, x.head, nothing)
    if id !== nothing
        return id, MOI.Nonlinear.NODE_COMPARISON
    end
    id = get(data.operators.logic_operator_to_id, x.head, nothing)
    if id !== nothing
        return id, MOI.Nonlinear.NODE_LOGIC
    end
    return throw(MOI.UnsupportedNonlinearOperator(x.head))
end

function _parse_without_recursion_inner(stack, data, expr, x, parent)
    id, node_type = _get_node_type(data, x)
    push!(expr.nodes, Node(node_type, id, parent))
    parent = length(expr.nodes)
    # Args need to be pushed onto the stack in reverse because the stack is a
    # first-in last-out datastructure.
    for arg in Iterators.Reverse(x.args)
        push!(stack, (parent, arg))
    end
    return
end

function parse_expression(
    data::Model,
    expr::Expression,
    x::Expr,
    parent_index::Int,
)
    stack = Tuple{Int,Any}[]
    push!(stack, (parent_index, x))
    while !isempty(stack)
        parent, item = pop!(stack)
        if item isa Expr
            _parse_expression(stack, data, expr, item, parent)
        else
            parse_expression(data, expr, item, parent)
        end
    end
    return
end

function _parse_expression(stack, data, expr, x, parent_index)
    if Meta.isexpr(x, :call)
        if length(x.args) == 2 && !Meta.isexpr(x.args[2], :...)
            _parse_univariate_expression(stack, data, expr, x, parent_index)
        else
            # The call is either n-ary, or it is a splat, in which case we
            # cannot tell just yet whether the expression is unary or nary.
            # Punt to multivariate and try to recover later.
            _parse_multivariate_expression(stack, data, expr, x, parent_index)
        end
    elseif Meta.isexpr(x, :comparison)
        _parse_comparison_expression(stack, data, expr, x, parent_index)
    elseif Meta.isexpr(x, :...)
        _parse_splat_expression(stack, data, expr, x, parent_index)
    elseif Meta.isexpr(x, :&&) || Meta.isexpr(x, :||)
        _parse_logic_expression(stack, data, expr, x, parent_index)
    else
        error("Unsupported expression: $x")
    end
end

function _parse_splat_expression(stack, data, expr, x, parent_index)
    @assert Meta.isexpr(x, :...) && length(x.args) == 1
    if parent_index == -1
        error(
            "Unsupported use of the splatting operator. This is only " *
            "supported in the arguments of a function call.",
        )
    elseif x.args[1] isa Expr
        error(
            "Unsupported use of the splatting operator. JuMP supports " *
            "splatting only symbols. For example, `x...` is ok, but " *
            "`(x + 1)...`, `[x; y]...` and `g(f(y)...)` are not.",
        )
    end
    for arg in Iterators.Reverse(x.args[1])
        push!(stack, (parent_index, arg))
    end
    return
end

function _parse_univariate_expression(
    stack::Vector{Tuple{Int,Any}},
    data::Model,
    expr::Expression,
    x::Expr,
    parent_index::Int,
)
    @assert Meta.isexpr(x, :call, 2)
    id = get(data.operators.univariate_operator_to_id, x.args[1], nothing)
    if id === nothing
        # It may also be a multivariate operator like * with one argument.
        if haskey(data.operators.multivariate_operator_to_id, x.args[1])
            _parse_multivariate_expression(stack, data, expr, x, parent_index)
            return
        end
        throw(MOI.UnsupportedNonlinearOperator(x.args[1]))
    end
    push!(expr.nodes, Node(NODE_CALL_UNIVARIATE, id, parent_index))
    push!(stack, (length(expr.nodes), x.args[2]))
    return
end

function _parse_multivariate_expression(
    stack::Vector{Tuple{Int,Any}},
    data::Model,
    expr::Expression,
    x::Expr,
    parent_index::Int,
)
    @assert Meta.isexpr(x, :call)
    id = get(data.operators.multivariate_operator_to_id, x.args[1], nothing)
    if id === nothing
        if haskey(data.operators.univariate_operator_to_id, x.args[1])
            # It may also be a unary variate operator with splatting.
            _parse_univariate_expression(stack, data, expr, x, parent_index)
        elseif x.args[1] in data.operators.comparison_operators
            # Or it may be a binary (in)equality operator.
            _parse_inequality_expression(stack, data, expr, x, parent_index)
        else
            throw(MOI.UnsupportedNonlinearOperator(x.args[1]))
        end
        return
    end
    push!(expr.nodes, Node(NODE_CALL_MULTIVARIATE, id, parent_index))
    for i in length(x.args):-1:2
        push!(stack, (length(expr.nodes), x.args[i]))
    end
    return
end

# This function parses single inequalities like `a <= b`. It's not to be
# confused with `_parse_comparison_expression`, which handles things like
# `a <= b <= c`.
function _parse_inequality_expression(
    stack::Vector{Tuple{Int,Any}},
    data::Model,
    expr::Expression,
    x::Expr,
    parent_index::Int,
)
    operator_id = data.operators.comparison_operator_to_id[x.args[1]]
    push!(expr.nodes, Node(NODE_COMPARISON, operator_id, parent_index))
    for i in length(x.args):-1:2
        push!(stack, (length(expr.nodes), x.args[i]))
    end
    return
end

# This function parses double inequalities like `a <= b <= c`. It's not to be
# confused with `_parse_inequality_expression`, which handles things like
# `a <= b`.
function _parse_comparison_expression(
    stack::Vector{Tuple{Int,Any}},
    data::Model,
    expr::Expression,
    x::Expr,
    parent_index::Int,
)
    for k in 2:2:(length(x.args)-1)
        @assert x.args[k] == x.args[2] # don't handle a <= b >= c
    end
    operator_id = data.operators.comparison_operator_to_id[x.args[2]]
    push!(expr.nodes, Node(NODE_COMPARISON, operator_id, parent_index))
    for i in length(x.args):-2:1
        push!(stack, (length(expr.nodes), x.args[i]))
    end
    return
end

function _parse_logic_expression(
    stack::Vector{Tuple{Int,Any}},
    data::Model,
    expr::Expression,
    x::Expr,
    parent_index::Int,
)
    id = data.operators.logic_operator_to_id[x.head]
    push!(expr.nodes, Node(NODE_LOGIC, id, parent_index))
    parent_var = length(expr.nodes)
    push!(stack, (parent_var, x.args[2]))
    push!(stack, (parent_var, x.args[1]))
    return
end

function parse_expression(
    ::Model,
    expr::Expression,
    x::MOI.VariableIndex,
    parent_index::Int,
)
    push!(expr.nodes, Node(NODE_MOI_VARIABLE, x.value, parent_index))
    return
end

function parse_expression(
    data::Model,
    expr::Expression,
    f::MOI.ScalarAffineFunction,
    parent_index::Int,
)
    if isempty(f.terms)
        parse_expression(data, expr, f.constant, parent_index)
        return
    elseif iszero(f.constant) && length(f.terms) == 1
        # Expression of for `a * x`
        parse_expression(data, expr, only(f.terms), parent_index)
        return
    end
    id_plus = data.operators.multivariate_operator_to_id[:+]
    push!(expr.nodes, Node(NODE_CALL_MULTIVARIATE, id_plus, parent_index))
    new_parent = length(expr.nodes)
    for term in f.terms
        parse_expression(data, expr, term, new_parent)
    end
    if !iszero(f.constant)
        parse_expression(data, expr, f.constant, new_parent)
    end
    return
end

function parse_expression(
    data::Model,
    expr::Expression,
    f::MOI.ScalarQuadraticFunction,
    parent_index::Int,
)
    if isempty(f.quadratic_terms) && isempty(f.affine_terms)
        parse_expression(data, expr, f.constant, parent_index)
        return
    elseif iszero(f.constant)
        if length(f.quadratic_terms) == 1 && isempty(f.affine_terms)
            parse_expression(data, expr, only(f.quadratic_terms), parent_index)
            return
        elseif isempty(f.quadratic_terms) && length(f.affine_terms) == 1
            parse_expression(data, expr, only(f.affine_terms), parent_index)
            return
        end
    end
    id_plus = data.operators.multivariate_operator_to_id[:+]
    push!(expr.nodes, Node(NODE_CALL_MULTIVARIATE, id_plus, parent_index))
    new_parent = length(expr.nodes)
    for term in f.quadratic_terms
        parse_expression(data, expr, term, new_parent)
    end
    for term in f.affine_terms
        parse_expression(data, expr, term, new_parent)
    end
    if !iszero(f.constant)
        parse_expression(data, expr, f.constant, new_parent)
    end
    return
end

function parse_expression(
    data::Model,
    expr::Expression,
    x::MOI.ScalarAffineTerm,
    parent_index::Int,
)
    if isone(x.coefficient)
        parse_expression(data, expr, x.variable, parent_index)
    else
        id_mul = data.operators.multivariate_operator_to_id[:*]
        push!(expr.nodes, Node(NODE_CALL_MULTIVARIATE, id_mul, parent_index))
        mul_parent = length(expr.nodes)
        parse_expression(data, expr, x.coefficient, mul_parent)
        parse_expression(data, expr, x.variable, mul_parent)
    end
    return
end

function parse_expression(
    data::Model,
    expr::Expression,
    x::MOI.ScalarQuadraticTerm,
    parent_index::Int,
)
    # There are four cases:
    #   (1): 1 * x * x  -->  ^(x, 2)
    #   (2): a * x * x  -->  *(a, ^(x, 2))
    #   (3): a * x * y  -->  *(a, x, y)
    #   (4): 1 * x * y  -->  *(x, y)
    if x.variable_1 == x.variable_2
        # Case (1): 1 * x * x  -->  ^(x, 2)
        # Case (2): a * x * x  -->  *(a, ^(x, 2))
        coef = x.coefficient / 2
        parent = parent_index
        if !isone(coef)
            id = data.operators.multivariate_operator_to_id[:*]
            push!(expr.nodes, Node(NODE_CALL_MULTIVARIATE, id, parent_index))
            parent = length(expr.nodes)
            parse_expression(data, expr, coef, parent)
            parent
        end
        id = data.operators.multivariate_operator_to_id[:^]
        push!(expr.nodes, Node(NODE_CALL_MULTIVARIATE, id, parent))
        pow_parent = length(expr.nodes)
        parse_expression(data, expr, x.variable_1, pow_parent)
        parse_expression(data, expr, 2, pow_parent)
    else
        # Case (3): a * x * y  -->  *(a, x, y)
        # Case (4): 1 * x * y  -->  *(x, y)
        id_mul = data.operators.multivariate_operator_to_id[:*]
        push!(expr.nodes, Node(NODE_CALL_MULTIVARIATE, id_mul, parent_index))
        mul_parent = length(expr.nodes)
        if !isone(x.coefficient)
            parse_expression(data, expr, x.coefficient, mul_parent)
        end
        parse_expression(data, expr, x.variable_1, mul_parent)
        parse_expression(data, expr, x.variable_2, mul_parent)
    end
    return
end

function parse_expression(::Model, expr::Expression, x::Real, parent_index::Int)
    push!(expr.values, convert(Float64, x)::Float64)
    push!(expr.nodes, Node(NODE_VALUE, length(expr.values), parent_index))
    return
end

function parse_expression(
    ::Model,
    expr::Expression,
    x::ParameterIndex,
    parent_index::Int,
)
    push!(expr.nodes, Node(NODE_PARAMETER, x.value, parent_index))
    return
end

function parse_expression(
    ::Model,
    expr::Expression,
    x::ExpressionIndex,
    parent_index::Int,
)
    push!(expr.nodes, Node(NODE_SUBEXPRESSION, x.value, parent_index))
    return
end

function parse_expression(
    ::Model,
    expr::Expression,
    x::AbstractArray,
    parent_index::Int,
)
    return error(
        "Unexpected array $x in nonlinear expression. Nonlinear expressions " *
        "may contain only scalar expressions.",
    )
end

"""
    convert_to_expr(data::Model, expr::Expression)

Convert the [`Expression`](@ref) `expr` into a Julia `Expr`.

 * subexpressions are represented by a [`ExpressionIndex`](@ref) object.
 * parameters are represented by a [`ParameterIndex`](@ref) object.
 * variables are represented by an [`MOI.VariableIndex`](@ref) object.
"""
function convert_to_expr(model::Model, expr::Expression)
    tree = Any[]
    for node in expr.nodes
        node_expr = if node.type == NODE_CALL_UNIVARIATE
            Expr(:call, model.operators.univariate_operators[node.index])
        elseif node.type == NODE_CALL_MULTIVARIATE
            Expr(:call, model.operators.multivariate_operators[node.index])
        elseif node.type == NODE_COMPARISON
            Expr(:call, model.operators.comparison_operators[node.index])
        elseif node.type == NODE_LOGIC
            Expr(model.operators.logic_operators[node.index])
        elseif node.type == NODE_MOI_VARIABLE
            MOI.VariableIndex(node.index)
        elseif node.type == NODE_PARAMETER
            ParameterIndex(node.index)
        elseif node.type == NODE_SUBEXPRESSION
            ExpressionIndex(node.index)
        else
            @assert node.type == NODE_VALUE
            expr.values[node.index]
        end
        if 1 <= node.parent <= length(tree)
            push!(tree[node.parent].args, node_expr)
        end
        push!(tree, node_expr)
    end
    return _replace_comparison(tree[1])
end

# TODO(odow): NODE_COMPARISON is a bit of a pain to deal with...
_replace_comparison(expr) = expr

function _replace_comparison(expr::Expr)
    if Meta.isexpr(expr, :call, 4) && expr.args[1] in (:<=, :>=, :(==), :<, :>)
        return Expr(
            :comparison,
            expr.args[2],
            expr.args[1],
            expr.args[3],
            expr.args[1],
            expr.args[4],
        )
    end
    for i in 1:length(expr.args)
        expr.args[i] = _replace_comparison(expr.args[i])
    end
    return expr
end
