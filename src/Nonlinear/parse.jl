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

function parse_expression(
    data::Model,
    expr::Expression,
    x::MOI.ScalarNonlinearFunction,
    parent::Int,
)
    stack = Tuple{Int,Any}[(parent, x)]
    while !isempty(stack)
        parent_node, arg = pop!(stack)
        if arg isa MOI.ScalarNonlinearFunction
            _parse_without_recursion_inner(stack, data, expr, arg, parent_node)
        else
            # We can use recursion here, because ScalarNonlinearFunction only
            # occur in other ScalarNonlinearFunction.
            parse_expression(data, expr, arg, parent_node)
        end
    end
    return
end

function _parse_without_recursion_inner(stack, data, expr, x, parent)
    id = get(data.operators.univariate_operator_to_id, x.head, nothing)
    node_type = if length(x.args) == 1 && id !== nothing
        NODE_CALL_UNIVARIATE
    else
        id = get(data.operators.multivariate_operator_to_id, x.head, nothing)
        @assert id !== nothing
        NODE_CALL_MULTIVARIATE
    end
    push!(expr.nodes, Node(node_type, id, parent))
    parent = length(expr.nodes)
    # Args need to be pushed onto the stack in reverse because the stack is a
    # first-in last-out datastructure.
    for arg in reverse(x.args)
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
    if isexpr(x, :call)
        if length(x.args) == 2 && !isexpr(x.args[2], :...)
            _parse_univariate_expression(stack, data, expr, x, parent_index)
        else
            _parse_multivariate_expression(stack, data, expr, x, parent_index)
        end
    elseif isexpr(x, :comparison)
        _parse_comparison_expression(stack, data, expr, x, parent_index)
    elseif isexpr(x, :...)
        _parse_splat_expression(stack, data, expr, x, parent_index)
    elseif isexpr(x, :&&) || isexpr(x, :||)
        _parse_logic_expression(stack, data, expr, x, parent_index)
    else
        error("Unsupported expression: $x")
    end
end

function _parse_splat_expression(stack, data, expr, x, parent_index)
    @assert isexpr(x, :...) && length(x.args) == 1
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
    @assert isexpr(x, :call, 2)
    id = get(data.operators.univariate_operator_to_id, x.args[1], nothing)
    if id === nothing
        # It may also be a multivariate operator like * with one argument.
        if haskey(data.operators.multivariate_operator_to_id, x.args[1])
            _parse_multivariate_expression(stack, data, expr, x, parent_index)
            return
        end
        error("Unable to parse: $x")
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
    @assert isexpr(x, :call)
    id = get(data.operators.multivariate_operator_to_id, x.args[1], nothing)
    if id === nothing
        @assert x.args[1] in data.operators.comparison_operators
        _parse_inequality_expression(stack, data, expr, x, parent_index)
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
    for k in 2:2:length(x.args)-1
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
    x::MOI.ScalarAffineFunction,
    parent::Int,
)
    f = convert(MOI.ScalarNonlinearFunction, x)
    parse_expression(data, expr, f, parent)
    return
end

function parse_expression(
    data::Model,
    expr::Expression,
    x::MOI.ScalarQuadraticFunction,
    parent::Int,
)
    f = convert(MOI.ScalarNonlinearFunction, x)
    parse_expression(data, expr, f, parent)
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
 * variables are represennted by an [`MOI.VariableIndex`](@ref) object.
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
    if isexpr(expr, :call, 4) && expr.args[1] in (:<=, :>=, :(==), :<, :>)
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
