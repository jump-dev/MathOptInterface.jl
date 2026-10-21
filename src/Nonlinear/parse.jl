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
    MOI.Utilities.final_touch(model::Model, index_map = nothing)

Extract the subexpressions that were detected by [`parse_expression`](@ref)
since the last call to `final_touch`.

When a `MOI.ScalarNonlinearFunction` is parsed a second time, its first
occurrence, which was parsed inline into some expression, has to be moved into
a new subexpression so that both occurrences can share it. Doing this
immediately would shift the nodes of that expression, so every node index
stored in the cache (and in the stack of the current parse) would need to be
updated for each extraction, which is quadratic. Instead, `parse_expression`
reserves an [`ExpressionIndex`](@ref) for the subexpression, with an empty
placeholder [`Expression`](@ref), and records the node to extract in
`model.cache_pending`. This function then rewrites each affected expression
in a single pass, and remaps the cache in a single pass.

The subexpressions are invalid until this function is called. It is called by
the [`Evaluator`](@ref) constructors and by [`evaluate`](@ref), and it is a
no-op if there is nothing to extract.
"""
function MOI.Utilities.final_touch(model::Model, ::Any = nothing)
    pending = model.cache_pending
    if isempty(pending)
        return
    end
    # Group the nodes to extract by expression, sorted by node index.
    sort!(pending)
    maps = Dict{Int,Tuple{Vector{Int},Vector{Int}}}()
    first = 1
    while first <= length(pending)
        tape = pending[first][1]
        last = first
        while last < length(pending) && pending[last+1][1] == tape
            last += 1
        end
        maps[tape] = _extract_subexpressions!(model, tape, pending, first, last)
        first = last + 1
    end
    empty!(pending)
    # Remap the cached nodes of the expressions that were rewritten.
    for (key, (tape, node)) in model.cache
        if node != 0 && haskey(maps, tape)
            new_tape, new_node = maps[tape]
            model.cache[key] = (new_tape[node], new_node[node])
        end
    end
    return
end

"""
    _extract_subexpressions!(model::Model, tape::Int, pending, first, last)

Extract the nodes `pending[i][2]` of `model.cache_expressions[tape]` into the
subexpressions `model.expressions[pending[i][3]]` for `i in first:last`. The
nodes `pending[first:last]` must be sorted.

The nodes are stored in depth-first order so a node is visited after its
parent. Each node is therefore appended to the same expression as its parent,
unless it is to be extracted, in which case its parent gets a
`NODE_SUBEXPRESSION` node instead and the node becomes the root of its
subexpression. Nested extractions are handled in the same way.

The nodes that stay in `model.cache_expressions[tape]` can only move to a
smaller index so we rewrite it in place.

Return `(new_tape, new_node)` such that node `i` of the original expression is
now node `new_node[i]` of `model.cache_expressions[new_tape[i]]`.
"""
function _extract_subexpressions!(
    model::Model,
    tape::Int,
    pending::Vector{Tuple{Int,Int,Int}},
    first::Int,
    last::Int,
)
    expr = model.cache_expressions[tape]
    n = length(expr.nodes)
    new_tape = Vector{Int}(undef, n)
    new_node = Vector{Int}(undef, n)
    # Number of nodes and values kept in `expr` so far.
    num_nodes = num_values = 0
    for i in 1:n
        node = expr.nodes[i]
        if node.parent == -1
            parent_tape, parent = tape, -1
        else
            parent_tape, parent = new_tape[node.parent], new_node[node.parent]
        end
        if first <= last && pending[first][2] == i
            subexpr = pending[first][3]
            first += 1
            # Replace the node by a `NODE_SUBEXPRESSION` in its parent's
            # expression and continue with the node as root of `subexpr`.
            subexpr_node = Node(NODE_SUBEXPRESSION, subexpr, parent)
            if parent_tape == tape
                num_nodes += 1
                expr.nodes[num_nodes] = subexpr_node
            else
                push!(model.cache_expressions[parent_tape].nodes, subexpr_node)
            end
            # `subexpr` may contain cached nodes so it needs to be in
            # `cache_expressions`.
            push!(model.cache_expressions, model.expressions[subexpr])
            parent_tape, parent = length(model.cache_expressions), -1
        end
        new_tape[i] = parent_tape
        index = node.index
        if parent_tape == tape
            if node.type == NODE_VALUE
                num_values += 1
                expr.values[num_values] = expr.values[index]
                index = num_values
            end
            num_nodes += 1
            expr.nodes[num_nodes] = Node(node.type, index, parent)
            new_node[i] = num_nodes
        else
            target = model.cache_expressions[parent_tape]
            if node.type == NODE_VALUE
                push!(target.values, expr.values[index])
                index = length(target.values)
            end
            push!(target.nodes, Node(node.type, index, parent))
            new_node[i] = length(target.nodes)
        end
    end
    resize!(expr.nodes, num_nodes)
    resize!(expr.values, num_values)
    return new_tape, new_node
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
                    # It was parsed inline at node `_node` of
                    # `data.cache_expressions[_expr_index]`. This is its second
                    # occurrence so it needs to be extracted into a
                    # subexpression. We reserve its index and defer the
                    # extraction to `MOI.Utilities.final_touch` so that no
                    # node is moved in the meantime.
                    push!(data.expressions, Expression())
                    subexpr = ExpressionIndex(length(data.expressions))
                    push!(
                        data.cache_pending,
                        (_expr_index, _node, subexpr.value),
                    )
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
