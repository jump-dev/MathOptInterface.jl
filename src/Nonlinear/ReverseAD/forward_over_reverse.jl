# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

const TAG = :ReverseAD

"""
    const MAX_CHUNK::Int = 10

An upper bound on the chunk sie for forward-over-reverse. Increasing this could
improve performance at the cost of extra memory allocation. It has been 10 for a
long time, and nobody seems to have complained.
"""
const MAX_CHUNK = 10

"""
    _eval_hessian(
        d::NLPEvaluator,
        f::_FunctionStorage,
        H::AbstractVector{Float64},
        λ::Float64,
        offset::Int,
    )::Int

Evaluate the hessian matrix of the function `f` and store the result, scaled by
`λ`, in `H`, beginning at element `offset+1`. This function assumes that
`_reverse_mode(d, x)` has already been called.

Returns the number of non-zeros in the computed Hessian, which will be used to
update the offset for the next call.
"""
function _eval_hessian(
    d::NLPEvaluator,
    ex::_FunctionStorage,
    H::AbstractVector{Float64},
    scale::Float64,
    nzcount::Int,
)::Int
    if ex.linearity == LINEAR
        @assert length(ex.hess_I) == 0
        return 0
    end
    chunk = min(size(ex.seed_matrix, 2), d.max_chunk)
    Coloring.prepare_seed_matrix!(ex.seed_matrix, ex.rinfo)
    # Compute hessian-vector products
    num_products = size(ex.seed_matrix, 2) # number of hessian-vector products
    num_chunks = div(num_products, chunk)
    @assert size(ex.seed_matrix, 1) == length(ex.rinfo.local_indices)
    for offset in 1:chunk:(chunk*num_chunks)
        _eval_hessian_chunk(d, ex, offset, chunk, chunk)
    end
    # leftover chunk
    remaining = num_products - chunk * num_chunks
    if remaining > 0
        offset = chunk * num_chunks + 1
        _eval_hessian_chunk(d, ex, offset, remaining, chunk)
    end
    want, got = nzcount + length(ex.hess_I), length(H)
    if want > got
        error(
            "Vector provided for Hessian storage has too few elements. Got " *
            "$got, want $want.",
        )
    end
    # TODO(odow): consider reverting to a view.
    output_slice = _UnsafeVectorView(nzcount, length(ex.hess_I), pointer(H))
    Coloring.recover_from_matmat!(
        output_slice,
        ex.seed_matrix,
        ex.rinfo,
        d.output_ϵ,
    )
    for i in 1:length(output_slice)
        output_slice[i] *= scale
    end
    return length(ex.hess_I)
end

function _eval_hessian_chunk(
    d::NLPEvaluator,
    ex::_FunctionStorage,
    offset::Int,
    chunk::Int,
    chunk_size::Int,
)
    for r in eachindex(ex.rinfo.local_indices)
        # set up directional derivatives
        @inbounds idx = ex.rinfo.local_indices[r]
        # load up ex.seed_matrix[r,k,k+1,...,k+remaining-1] into input_ϵ
        for s in 1:chunk
            # If `chunk < chunk_size`, leaves junk in the unused components
            d.input_ϵ[(idx-1)*chunk_size+s] = ex.seed_matrix[r, offset+s-1]
        end
    end
    _hessian_slice_inner(d, ex, chunk_size)
    fill!(d.input_ϵ, 0.0)
    # collect directional derivatives
    for r in eachindex(ex.rinfo.local_indices)
        @inbounds idx = ex.rinfo.local_indices[r]
        # load output_ϵ into ex.seed_matrix[r,k,k+1,...,k+remaining-1]
        for s in 1:chunk
            ex.seed_matrix[r, offset+s-1] = d.output_ϵ[(idx-1)*chunk_size+s]
        end
    end
    return
end

# A wrapper function to avoid dynamic dispatch.
function _generate_hessian_slice_inner()
    exprs = map(1:MAX_CHUNK) do id
        T = ForwardDiff.Partials{id,Float64}
        return :(return _hessian_slice_inner(d, ex, $T))
    end
    return MOI.Nonlinear._create_binary_switch(1:MAX_CHUNK, exprs)
end

@eval function _hessian_slice_inner(d, ex, id::Int)
    $(_generate_hessian_slice_inner())
    return error("Invalid chunk size: $id")
end

function _hessian_slice_inner(d, ex, ::Type{T}) where {T}
    fill!(d.output_ϵ, 0.0)
    output_ϵ = _reinterpret_unsafe(T, d.output_ϵ)
    subexpr_forward_values_ϵ =
        _reinterpret_unsafe(T, d.subexpression_forward_values_ϵ)
    for i in ex.dependent_subexpressions
        subexpr = d.subexpressions[i]
        subexpr_forward_values_ϵ[i] = _forward_eval_ϵ(
            d,
            subexpr,
            _reinterpret_unsafe(T, subexpr.partials_storage_ϵ),
        )
    end
    _forward_eval_ϵ(d, ex, _reinterpret_unsafe(T, d.partials_storage_ϵ))
    # do a reverse pass
    subexpr_reverse_values_ϵ =
        _reinterpret_unsafe(T, d.subexpression_reverse_values_ϵ)
    for i in ex.dependent_subexpressions
        subexpr_reverse_values_ϵ[i] = zero(T)
        d.subexpression_reverse_values[i] = 0.0
    end
    _reverse_eval_ϵ(
        output_ϵ,
        ex,
        _reinterpret_unsafe(T, d.storage_ϵ),
        _reinterpret_unsafe(T, d.partials_storage_ϵ),
        d.subexpression_reverse_values,
        subexpr_reverse_values_ϵ,
        1.0,
        zero(T),
    )
    for i in length(ex.dependent_subexpressions):-1:1
        j = ex.dependent_subexpressions[i]
        subexpr = d.subexpressions[j]
        _reverse_eval_ϵ(
            output_ϵ,
            subexpr,
            _reinterpret_unsafe(T, d.storage_ϵ),
            _reinterpret_unsafe(T, subexpr.partials_storage_ϵ),
            d.subexpression_reverse_values,
            subexpr_reverse_values_ϵ,
            d.subexpression_reverse_values[j],
            subexpr_reverse_values_ϵ[j],
        )
    end
    return
end

"""
    _forward_eval_ϵ(
        d::NLPEvaluator,
        ex::Union{_FunctionStorage,_SubexpressionStorage},
        partials_storage_ϵ::AbstractVector{ForwardDiff.Partials{N,T}},
    ) where {N,T}

Evaluate the directional derivatives of the expression tree in `ex`.

This is equivalent to evaluating the expression tree using DualNumbers or
ForwardDiff, but instead we keep the `ex.forward_storage` for the epsilon
components separate so that we don't need to recompute the real components.

This assumes that `_reverse_model(d, x)` has already been called.
"""
function _forward_eval_ϵ(
    d::NLPEvaluator,
    ex::Union{_FunctionStorage,_SubexpressionStorage},
    partials_storage_ϵ::AbstractVector{P},
) where {N,T,P<:ForwardDiff.Partials{N,T}}
    storage_ϵ = _reinterpret_unsafe(P, d.storage_ϵ)
    x_values_ϵ = _reinterpret_unsafe(P, d.input_ϵ)
    subexpression_values_ϵ =
        _reinterpret_unsafe(P, d.subexpression_forward_values_ϵ)
    @assert length(storage_ϵ) >= length(ex.nodes)
    @assert length(partials_storage_ϵ) >= length(ex.nodes)
    zero_ϵ = zero(P)
    # ex.nodes is already in order such that parents always appear before children
    # so a backwards pass through ex.nodes is a forward pass through the tree
    children_arr = SparseArrays.rowvals(ex.adj)
    for k in length(ex.nodes):-1:1
        node = ex.nodes[k]
        partials_storage_ϵ[k] = zero_ϵ
        if node.type == Nonlinear.NODE_VARIABLE
            storage_ϵ[k] = x_values_ϵ[node.index]
        elseif node.type == Nonlinear.NODE_VALUE
            storage_ϵ[k] = zero_ϵ
        elseif node.type == Nonlinear.NODE_SUBEXPRESSION
            storage_ϵ[k] = subexpression_values_ϵ[node.index]
        elseif node.type == Nonlinear.NODE_PARAMETER
            storage_ϵ[k] = zero_ϵ
        else
            @assert node.type != Nonlinear.NODE_MOI_VARIABLE
            ϵtmp = zero_ϵ
            @inbounds children_idx = SparseArrays.nzrange(ex.adj, k)
            for c_idx in children_idx
                @inbounds ix = children_arr[c_idx]
                @inbounds partial = ex.partials_storage[ix]
                @inbounds storage_val = storage_ϵ[ix]
                # TODO: This "if" statement can take 8% of the hessian
                # evaluation time. Find a more efficient way.
                if !isfinite(partial) && storage_val == zero_ϵ
                    continue
                end
                ϵtmp += storage_val * ex.partials_storage[ix]
            end
            storage_ϵ[k] = ϵtmp
            if node.type == Nonlinear.NODE_CALL_MULTIVARIATE
                # TODO(odow): consider how to refactor this into Nonlinear.
                op = node.index
                n_children = length(children_idx)
                if op == 3 # :*
                    # Lazy approach for now.
                    anyzero = false
                    tmp_prod = one(ForwardDiff.Dual{TAG,T,N})
                    for c_idx in children_idx
                        ix = children_arr[c_idx]
                        sval = ex.forward_storage[ix]
                        gnum = ForwardDiff.Dual{TAG}(sval, storage_ϵ[ix])
                        tmp_prod *= gnum
                        anyzero = ifelse(sval * sval == zero(T), true, anyzero)
                    end
                    # By a quirk of floating-point numbers, we can have
                    # anyzero == true && ForwardDiff.value(tmp_prod) != zero(T)
                    if anyzero || n_children <= 2
                        for c_idx in children_idx
                            prod_others = one(ForwardDiff.Dual{TAG,T,N})
                            for c_idx2 in children_idx
                                (c_idx == c_idx2) && continue
                                ix = children_arr[c_idx2]
                                gnum = ForwardDiff.Dual{TAG}(
                                    ex.forward_storage[ix],
                                    storage_ϵ[ix],
                                )
                                prod_others *= gnum
                            end
                            partials_storage_ϵ[children_arr[c_idx]] =
                                ForwardDiff.partials(prod_others)
                        end
                    else
                        for c_idx in children_idx
                            ix = children_arr[c_idx]
                            prod_others =
                                tmp_prod / ForwardDiff.Dual{TAG}(
                                    ex.forward_storage[ix],
                                    storage_ϵ[ix],
                                )
                            partials_storage_ϵ[ix] =
                                ForwardDiff.partials(prod_others)
                        end
                    end
                elseif op == 4 # :^
                    @assert n_children == 2
                    idx1 = first(children_idx)
                    idx2 = last(children_idx)
                    @inbounds ix1 = children_arr[idx1]
                    @inbounds ix2 = children_arr[idx2]
                    @inbounds base = ex.forward_storage[ix1]
                    @inbounds base_ϵ = storage_ϵ[ix1]
                    @inbounds exponent = ex.forward_storage[ix2]
                    @inbounds exponent_ϵ = storage_ϵ[ix2]
                    base_gnum = ForwardDiff.Dual{TAG}(base, base_ϵ)
                    exponent_gnum = ForwardDiff.Dual{TAG}(exponent, exponent_ϵ)
                    if exponent == 2
                        partials_storage_ϵ[ix1] = 2 * base_ϵ
                    elseif exponent == 1
                        partials_storage_ϵ[ix1] = zero_ϵ
                    else
                        partials_storage_ϵ[ix1] = ForwardDiff.partials(
                            exponent_gnum * pow(base_gnum, exponent_gnum - 1),
                        )
                    end
                    result_gnum = ForwardDiff.Dual{TAG}(
                        ex.forward_storage[k],
                        storage_ϵ[k],
                    )
                    # TODO(odow): fix me to use NaNMath.jl instead
                    log_base_gnum = base_gnum < 0 ? NaN : log(base_gnum)
                    partials_storage_ϵ[ix2] =
                        ForwardDiff.partials(result_gnum * log_base_gnum)
                elseif op == 5 # :/
                    @assert n_children == 2
                    idx1 = first(children_idx)
                    idx2 = last(children_idx)
                    @inbounds ix1 = children_arr[idx1]
                    @inbounds ix2 = children_arr[idx2]
                    @inbounds numerator = ex.forward_storage[ix1]
                    @inbounds numerator_ϵ = storage_ϵ[ix1]
                    @inbounds denominator = ex.forward_storage[ix2]
                    @inbounds denominator_ϵ = storage_ϵ[ix2]
                    recip_denominator =
                        1 / ForwardDiff.Dual{TAG}(denominator, denominator_ϵ)
                    partials_storage_ϵ[ix1] =
                        ForwardDiff.partials(recip_denominator)
                    partials_storage_ϵ[ix2] = ForwardDiff.partials(
                        -ForwardDiff.Dual{TAG}(numerator, numerator_ϵ) *
                        recip_denominator *
                        recip_denominator,
                    )
                elseif op > 6
                    f_input = _UnsafeVectorView(d.jac_storage, n_children)
                    for (i, c) in enumerate(children_idx)
                        f_input[i] = ex.forward_storage[children_arr[c]]
                    end
                    H = _UnsafeLowerTriangularMatrixView(
                        d.user_output_buffer,
                        n_children,
                    )
                    has_hessian = Nonlinear.eval_multivariate_hessian(
                        d.data.operators,
                        d.data.operators.multivariate_operators[node.index],
                        H,
                        f_input,
                    )
                    # This might be `false` if we extend this code to all
                    # multivariate functions.
                    @assert has_hessian
                    for col in 1:n_children
                        dual = zero(P)
                        for row in 1:n_children
                            # Make sure we get the lower-triangular component.
                            h = row >= col ? H[row, col] : H[col, row]
                            # Performance optimization: hessians can be quite
                            # sparse
                            if !iszero(h)
                                i = children_arr[children_idx[row]]
                                dual += h * storage_ϵ[i]
                            end
                        end
                        i = children_arr[children_idx[col]]
                        partials_storage_ϵ[i] = dual
                    end
                end
            elseif node.type == Nonlinear.NODE_CALL_UNIVARIATE
                @inbounds child_idx = children_arr[ex.adj.colptr[k]]
                f′′ = Nonlinear.eval_univariate_hessian(
                    d.data.operators,
                    node.index,
                    ex.forward_storage[child_idx],
                )
                partials_storage_ϵ[child_idx] = f′′ * storage_ϵ[child_idx]
            end
        end
    end
    return storage_ϵ[1]
end

# Compute directional derivatives of the reverse pass, goes with _forward_eval_ϵ
# to compute hessian-vector products.
function _reverse_eval_ϵ(
    output_ϵ::AbstractVector{ForwardDiff.Partials{N,T}},
    ex::Union{_FunctionStorage,_SubexpressionStorage},
    reverse_storage_ϵ,
    partials_storage_ϵ,
    subexpression_output,
    subexpression_output_ϵ,
    scale::T,
    scale_ϵ::ForwardDiff.Partials{N,T},
) where {N,T}
    @assert length(reverse_storage_ϵ) >= length(ex.nodes)
    @assert length(partials_storage_ϵ) >= length(ex.nodes)
    if ex.nodes[1].type == Nonlinear.NODE_VARIABLE
        @inbounds output_ϵ[ex.nodes[1].index] += scale_ϵ
        return
    elseif ex.nodes[1].type == Nonlinear.NODE_SUBEXPRESSION
        @inbounds subexpression_output[ex.nodes[1].index] +=
            scale * ex.reverse_storage[1]
        @inbounds subexpression_output_ϵ[ex.nodes[1].index] += scale_ϵ
        return
    end
    reverse_storage_ϵ[1] = scale_ϵ
    for k in 2:length(ex.nodes)
        @inbounds node = ex.nodes[k]
        if node.type == Nonlinear.NODE_VALUE ||
           node.type == Nonlinear.NODE_LOGIC ||
           node.type == Nonlinear.NODE_COMPARISON ||
           node.type == Nonlinear.NODE_PARAMETER
            continue
        end
        parent_value = scale * ex.reverse_storage[node.parent]
        if !isfinite(ex.partials_storage[k]) && iszero(parent_value)
            reverse_storage_ϵ[k] = zero(ForwardDiff.Partials{N,T})
        else
            reverse_storage_ϵ[k] = ForwardDiff._mul_partials(
                partials_storage_ϵ[k],
                reverse_storage_ϵ[node.parent],
                parent_value,
                ex.partials_storage[k],
            )
        end
        if node.type == Nonlinear.NODE_VARIABLE
            @inbounds output_ϵ[node.index] += reverse_storage_ϵ[k]
        elseif node.type == Nonlinear.NODE_SUBEXPRESSION
            @inbounds subexpression_output[node.index] +=
                scale * ex.reverse_storage[k]
            @inbounds subexpression_output_ϵ[node.index] += reverse_storage_ϵ[k]
        end
    end
    return
end
