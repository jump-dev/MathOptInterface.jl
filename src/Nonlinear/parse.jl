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

function _extract_subexpression!(expr::Expression, root::Int)
    n = length(expr.nodes)
    # The whole subexpression is continuous in the tape
    first_out = first_value = last_value = nothing
    for i in root:n
        node = expr.nodes[i]
        if i != root && node.parent < root
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
    if !isnothing(first_out)
        for i in (last(I)+1):n
            node = expr.nodes[i]
            index = node.index
            if node.type == NODE_VALUE && !isnothing(V)
                @assert index >= last(V)
                index -= length(V)
            end
            parent = node.parent
            if parent > root
                @assert parent > last(I)
                parent -= length(I) - 1
            end
            expr.nodes[i] = Node(node.type, index, parent)
        end
    end
    return I, V
end

function _extract_subexpression!(data::Model, expr::Expression, root::Int)
    parent = expr.nodes[root].parent
    I, V = _extract_subexpression!(expr, root)
    subexpr =
        Expression(expr.nodes[I], isnothing(V) ? Float64[] : expr.values[V])
    push!(data.expressions, subexpr)
    index = ExpressionIndex(length(data.expressions))
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
    while !isempty(stack)
        parent_node, arg = pop!(stack)
        if arg isa MOI.ScalarNonlinearFunction
            if haskey(data.cache, arg)
                subexpr = data.cache[arg]
                if subexpr isa Tuple{Expression,Int}
                    _expr, _node = subexpr
                    subexpr, I = _extract_subexpression!(data, _expr, _node)
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
                    for (key, val) in data.cache
                        if val isa Tuple{Expression,Int}
                            __expr, __node = val
                            if _expr === __expr && __node > first(I)
                                if __node <= last(I)
                                    data.cache[key] = (
                                        data.expressions[subexpr.value],
                                        __node - first(I) + 1,
                                    )
                                else
                                    data.cache[key] =
                                        (__expr, __node - length(I) + 1)
                                end
                            end
                        end
                    end
                    data.cache[arg] = subexpr
                end
                parse_expression(
                    data,
                    expr,
                    subexpr::ExpressionIndex,
                    parent_node,
                )
            else
                _parse_without_recursion_inner(
                    stack,
                    data,
                    expr,
                    arg,
                    parent_node,
                )
                data.cache[arg] = (expr, length(expr.nodes))
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
    id_mul = data.operators.multivariate_operator_to_id[:*]
    push!(expr.nodes, Node(NODE_CALL_MULTIVARIATE, id_mul, parent_index))
    mul_parent = length(expr.nodes)
    coef = x.coefficient
    if x.variable_1 == x.variable_2
        coef /= 2
    end
    if !isone(coef)
        parse_expression(data, expr, coef, mul_parent)
    end
    parse_expression(data, expr, x.variable_1, mul_parent)
    parse_expression(data, expr, x.variable_2, mul_parent)
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
