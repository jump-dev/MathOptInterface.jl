# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    _reverse_mode(d::NLPEvaluator, x)

Run reverse-mode automatic differentiation on `d` given the primal solution `x`.

This function updates many of the data-structures inside `d` in-place.

At a high level, reverse-mode AD has two phases:

In Phase I, we evaluate the problem in `d` at the primal solution `x`, and
stores the primal solution of each expression in the tree and the first-order
partial derivative information for each node with respect to its arguments.

Because the nodes in our data structure are topologically sorted, we can make a
single pass through the tree by iterating backwards through the vector of stored
nodes.

In Phase II, we propagate the partial derivative information back down the tree
to find the derivative of each function with respect to the input.

Because the nodes in our data structure are topologically sorted, we can make a
single pass through the tree by iterating forwards through the vector of stored
nodes.
"""
function _reverse_mode(d::NLPEvaluator, x)
    if d.last_x == x
        # Fail fast if the primal solution has not changed since last call.
        return
    end
    # Phase I
    for k in d.subexpression_order
        d.subexpression_forward_values[k] =
            _forward_eval(d.subexpressions[k], d, x)
    end
    if d.objective !== nothing
        _forward_eval(d.objective::_FunctionStorage, d, x)
    end
    for con in d.constraints
        _forward_eval(con, d, x)
    end
    # Phase II
    for k in d.subexpression_order
        _reverse_eval(d.subexpressions[k])
    end
    if d.objective !== nothing
        _reverse_eval(d.objective::_FunctionStorage)
    end
    for con in d.constraints
        _reverse_eval(con)
    end
    # If a JuMP model uses the legacy nonlinear interface, then JuMP constructs
    # a NLPEvaluator at the start of a call to `JuMP.optimize!` and it passes in
    # the list of variables in the JuMP model to `.ordered_variables`.
    #
    # During `MOI.initialize`, `.last_x` gets filled with `NaN` to match the
    # length of `ordered_variables`, that is, the number of variables in the
    # JuMP model.
    #
    # However, if the model includes a bridge that adds new decision variables
    # then the total number of variables in the optimizer (in `x`) will be
    # larger than the cache in `last_x`.
    #
    # It is safe to resize `last_x` because only the variables in
    # `ordered_variables` can appear in the NLPBlock.
    #
    # I don't think we need any other fixes because callers to things like
    # `eval_objective` can pass in a longer input `x` vector without fear
    # because the excess elements won't be used.
    if length(d.last_x) < length(x)
        resize!(d.last_x, length(x))
    end
    copyto!(d.last_x, x)
    return
end

"""
    _forward_eval(
        f::Union{_FunctionStorage,_SubexpressionStorage},
        d::NLPEvaluator,
        x::AbstractVector{T},
    ) where {T}

Forward-mode evaluation of an expression tree given in `f`.

 * This function assumes that the values of all sub-expressions have already
   been computed and are stored in `d.subexpression_forward_values`.
 * `f.partials_storage[k]` is the partial derivative of `nodes[k].parent` with
   respect to the value of node `k`. It's efficient to compute this at the same
   time as the value of the parent because we use it in reverse mode and in dual
   forward mode. Note that `partials_storage`` makes a subtle assumption that we
   have a tree instead of a general DAG. If we have a DAG, then need to
   associate storage with each edge of the DAG.
"""
function _forward_eval(
    # !!! warning
    #     This Union depends upon _FunctionStorage and _SubexpressionStorage
    #     having similarly named fields.
    f::Union{_FunctionStorage,_SubexpressionStorage},
    d::NLPEvaluator,
    x::AbstractVector{T},
)::T where {T}
    @assert length(f.forward_storage) >= length(f.nodes)
    @assert length(f.partials_storage) >= length(f.nodes)
    operators = d.data.operators
    # f.nodes is already in order such that parents always appear before
    # children, so a backwards pass through f.nodes is a forward pass through
    # the tree.
    children_arr = SparseArrays.rowvals(f.adj)
    for k in length(f.nodes):-1:1
        node = f.nodes[k]
        f.partials_storage[k] = zero(T)
        if node.type == Nonlinear.NODE_VARIABLE
            f.forward_storage[k] = x[node.index]
            # This should never happen, because we will have replaced these by now.
            # elseif node.type == Nonlinear.NODE_MOI_VARIABLE
            #     f.forward_storage[k] = x[node.index]
        elseif node.type == Nonlinear.NODE_VALUE
            f.forward_storage[k] = f.const_values[node.index]
        elseif node.type == Nonlinear.NODE_SUBEXPRESSION
            f.forward_storage[k] = d.subexpression_forward_values[node.index]
        elseif node.type == Nonlinear.NODE_PARAMETER
            f.forward_storage[k] = d.data.parameters[node.index]
        elseif node.type == Nonlinear.NODE_CALL_MULTIVARIATE
            children_indices = SparseArrays.nzrange(f.adj, k)
            N = length(children_indices)
            # TODO(odow);
            # With appropriate benchmarking, the special-cased if-statements can
            # be removed in favor of the generic user-defined function case.
            if node.index == 1 # :+
                tmp_sum = zero(T)
                for c_idx in children_indices
                    @inbounds ix = children_arr[c_idx]
                    @inbounds f.partials_storage[ix] = one(T)
                    @inbounds tmp_sum += f.forward_storage[ix]
                end
                f.forward_storage[k] = tmp_sum
            elseif node.index == 2 # :-
                @assert N == 2
                child1 = first(children_indices)
                @inbounds ix1 = children_arr[child1]
                @inbounds ix2 = children_arr[child1+1]
                @inbounds tmp_sub = f.forward_storage[ix1]
                @inbounds tmp_sub -= f.forward_storage[ix2]
                @inbounds f.partials_storage[ix1] = one(T)
                @inbounds f.partials_storage[ix2] = -one(T)
                f.forward_storage[k] = tmp_sub
            elseif node.index == 3 # :*
                tmp_prod = one(T)
                for c_idx in children_indices
                    @inbounds tmp_prod *= f.forward_storage[children_arr[c_idx]]
                end
                if tmp_prod == zero(T) || N <= 2
                    # This is inefficient if there are a lot of children.
                    # 2 is chosen as a limit because (x*y)/y does not always
                    # equal x for floating-point numbers. This can produce
                    # unexpected error in partials. There's still an error when
                    # multiplying three or more terms, but users are less likely
                    # to complain about it.
                    for c_idx in children_indices
                        prod_others = one(T)
                        for c_idx2 in children_indices
                            (c_idx == c_idx2) && continue
                            ix = children_arr[c_idx2]
                            prod_others *= f.forward_storage[ix]
                        end
                        f.partials_storage[children_arr[c_idx]] = prod_others
                    end
                else
                    # Compute all-minus-one partial derivatives by dividing from
                    # the total product.
                    for c_idx in children_indices
                        ix = children_arr[c_idx]
                        f.partials_storage[ix] =
                            tmp_prod / f.forward_storage[ix]
                    end
                end
                @inbounds f.forward_storage[k] = tmp_prod
            elseif node.index == 4 # :^
                @assert N == 2
                idx1 = first(children_indices)
                idx2 = last(children_indices)
                @inbounds ix1 = children_arr[idx1]
                @inbounds ix2 = children_arr[idx2]
                @inbounds base = f.forward_storage[ix1]
                @inbounds exponent = f.forward_storage[ix2]
                if exponent == 2
                    @inbounds f.forward_storage[k] = base * base
                    @inbounds f.partials_storage[ix1] = 2 * base
                elseif exponent == 1
                    @inbounds f.forward_storage[k] = base
                    @inbounds f.partials_storage[ix1] = 1.0
                else
                    f.forward_storage[k] = pow(base, exponent)
                    f.partials_storage[ix1] = exponent * pow(base, exponent - 1)
                end
                f.partials_storage[ix2] = f.forward_storage[k] * log(base)
            elseif node.index == 5 # :/
                @assert N == 2
                idx1 = first(children_indices)
                idx2 = last(children_indices)
                @inbounds ix1 = children_arr[idx1]
                @inbounds ix2 = children_arr[idx2]
                @inbounds numerator = f.forward_storage[ix1]
                @inbounds denominator = f.forward_storage[ix2]
                recip_denominator = 1 / denominator
                @inbounds f.partials_storage[ix1] = recip_denominator
                f.partials_storage[ix2] =
                    -numerator * recip_denominator * recip_denominator
                f.forward_storage[k] = numerator * recip_denominator
            elseif node.index == 6 # ifelse
                @assert N == 3
                idx1 = first(children_indices)
                @inbounds condition = f.forward_storage[children_arr[idx1]]
                @inbounds lhs = f.forward_storage[children_arr[idx1+1]]
                @inbounds rhs = f.forward_storage[children_arr[idx1+2]]
                @inbounds f.partials_storage[children_arr[idx1+1]] =
                    condition == 1
                @inbounds f.partials_storage[children_arr[idx1+2]] =
                    !(condition == 1)
                f.forward_storage[k] = ifelse(condition == 1, lhs, rhs)
            else
                f_input = _UnsafeVectorView(d.jac_storage, N)
                ∇f = _UnsafeVectorView(d.user_output_buffer, N)
                for (r, i) in enumerate(children_indices)
                    f_input[r] = f.forward_storage[children_arr[i]]
                    ∇f[r] = 0.0
                end
                f.forward_storage[k] = Nonlinear.eval_multivariate_function(
                    operators,
                    operators.multivariate_operators[node.index],
                    f_input,
                )
                Nonlinear.eval_multivariate_gradient(
                    operators,
                    operators.multivariate_operators[node.index],
                    ∇f,
                    f_input,
                )
                for (r, i) in enumerate(children_indices)
                    f.partials_storage[children_arr[i]] = ∇f[r]
                end
            end
        elseif node.type == Nonlinear.NODE_CALL_UNIVARIATE
            child_idx = children_arr[f.adj.colptr[k]]
            f.forward_storage[k] = Nonlinear.eval_univariate_function(
                operators,
                operators.univariate_operators[node.index],
                f.forward_storage[child_idx],
            )
            f.partials_storage[child_idx] = Nonlinear.eval_univariate_gradient(
                operators,
                operators.univariate_operators[node.index],
                f.forward_storage[child_idx],
            )
        elseif node.type == Nonlinear.NODE_COMPARISON
            children_idx = SparseArrays.nzrange(f.adj, k)
            result = true
            f.partials_storage[children_arr[children_idx[1]]] = zero(T)
            for r in 2:length(children_idx)
                lhs = children_arr[children_idx[r-1]]
                rhs = children_arr[children_idx[r]]
                result &= Nonlinear.eval_comparison_function(
                    operators,
                    operators.comparison_operators[node.index],
                    f.forward_storage[lhs],
                    f.forward_storage[rhs],
                )
                f.partials_storage[rhs] = zero(T)
            end
            f.forward_storage[k] = result
        else
            @assert node.type == Nonlinear.NODE_LOGIC
            children_idx = SparseArrays.nzrange(f.adj, k)
            lhs = children_arr[children_idx[1]]
            rhs = children_arr[children_idx[2]]
            f.forward_storage[k] = Nonlinear.eval_logic_function(
                operators,
                operators.logic_operators[node.index],
                f.forward_storage[lhs] == 1,
                f.forward_storage[rhs] == 1,
            )
            f.partials_storage[lhs] = zero(T)
            f.partials_storage[rhs] = zero(T)
        end
    end
    return f.forward_storage[1]
end

"""
    _reverse_eval(f::Union{_FunctionStorage,_SubexpressionStorage})

Reverse-mode evaluation of an expression tree given in `f`.

 * This function assumes `f.partials_storage` is already updated.
 * This function assumes that `f.reverse_storage` has been initialized with 0.0.
"""
function _reverse_eval(
    # !!! warning
    #     This Union depends upon _FunctionStorage and _SubexpressionStorage
    #     having similarly named fields.
    f::Union{_FunctionStorage,_SubexpressionStorage},
)
    @assert length(f.reverse_storage) >= length(f.nodes)
    @assert length(f.partials_storage) >= length(f.nodes)
    # f.nodes is already in order such that parents always appear before
    # children so a forward pass through nodes is a backwards pass through the
    # tree.
    f.reverse_storage[1] = one(Float64)
    for k in 2:length(f.nodes)
        node = f.nodes[k]
        if node.type == Nonlinear.NODE_VALUE ||
           node.type == Nonlinear.NODE_LOGIC ||
           node.type == Nonlinear.NODE_COMPARISON ||
           node.type == Nonlinear.NODE_PARAMETER
            continue
        end
        rev_parent = f.reverse_storage[node.parent]
        partial = f.partials_storage[k]
        f.reverse_storage[k] = ifelse(
            rev_parent == 0.0 && !isfinite(partial),
            rev_parent,
            rev_parent * partial,
        )
    end
    return
end

"""
    _extract_reverse_pass(
        g::AbstractVector{T},
        d::NLPEvaluator,
        f::_FunctionStorage,
    ) where {T}

Fill the gradient vector `g` with the values from the reverse pass. Assumes you
have already called `_reverse_eval_all(d, x)`.
"""
function _extract_reverse_pass(
    g::AbstractVector{T},
    d::NLPEvaluator,
    f::_FunctionStorage,
) where {T}
    for i in f.dependent_subexpressions
        d.subexpression_reverse_values[i] = 0.0
    end
    _extract_reverse_pass_inner(g, f, d.subexpression_reverse_values, 1.0)
    for i in length(f.dependent_subexpressions):-1:1
        k = f.dependent_subexpressions[i]
        _extract_reverse_pass_inner(
            g,
            d.subexpressions[k],
            d.subexpression_reverse_values,
            d.subexpression_reverse_values[k],
        )
    end
    return
end

function _extract_reverse_pass_inner(
    output::AbstractVector{T},
    # !!! warning
    #     This Union depends upon _FunctionStorage and _SubexpressionStorage
    #     having similarly named fields.
    f::Union{_FunctionStorage,_SubexpressionStorage},
    subexpressions::AbstractVector{T},
    scale::T,
) where {T}
    @assert length(f.reverse_storage) >= length(f.nodes)
    for (k, node) in enumerate(f.nodes)
        if node.type == Nonlinear.NODE_VARIABLE
            output[node.index] += scale * f.reverse_storage[k]
        elseif node.type == Nonlinear.NODE_SUBEXPRESSION
            subexpressions[node.index] += scale * f.reverse_storage[k]
        end
    end
    return
end
