# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

struct _SubexpressionStorage
    nodes::Vector{Nonlinear.Node}
    adj::SparseArrays.SparseMatrixCSC{Bool,Int}
    const_values::Vector{Float64}
    forward_storage::Vector{Float64}
    partials_storage::Vector{Float64}
    reverse_storage::Vector{Float64}
    partials_storage_ϵ::Vector{Float64}
    linearity::Linearity

    function _SubexpressionStorage(
        nodes::Vector{Nonlinear.Node},
        adj::SparseArrays.SparseMatrixCSC{Bool,Int},
        const_values::Vector{Float64},
        partials_storage_ϵ::Vector{Float64},
        linearity::Linearity,
    )
        N = length(nodes)
        return new(
            nodes,
            adj,
            const_values,
            zeros(N),  # forward_storage,
            zeros(N),  # partials_storage,
            zeros(N),  # reverse_storage,
            partials_storage_ϵ,
            linearity,
        )
    end
end

# We don't need to store the full vector of `linearity` but we return
# it because it is needed in `compute_hessian_sparsity`.
function _subexpression_and_linearity(
    expr::Nonlinear.Expression,
    moi_index_to_consecutive_index,
    partials_storage_ϵ::Vector{Float64},
    d,
)
    nodes =
        _replace_moi_variables(expr.nodes, moi_index_to_consecutive_index)
    adj = Nonlinear.adjacency_matrix(nodes)
    linearity = if d.want_hess
        _classify_linearity(nodes, adj, d.subexpression_linearity)
    else
        [NONLINEAR]
    end
    return _SubexpressionStorage(
        nodes,
        adj,
        expr.values,
        partials_storage_ϵ,
        linearity[1],
    ), linearity
end

struct _FunctionStorage
    expr::_SubexpressionStorage
    grad_sparsity::Vector{Int}
    # Nonzero pattern of Hessian matrix
    hess_I::Vector{Int}
    hess_J::Vector{Int}
    rinfo::Coloring.RecoveryInfo # coloring info for hessians
    seed_matrix::Matrix{Float64}
    # subexpressions which this function depends on, ordered for forward pass.
    dependent_subexpressions::Vector{Int}

    function _FunctionStorage(
        expr::_SubexpressionStorage,
        num_variables,
        coloring_storage::Coloring.IndexedSet,
        want_hess::Bool,
        subexpressions::Vector{_SubexpressionStorage},
        dependent_subexpressions,
        subexpression_edgelist,
        subexpression_variables,
        linearity::Vector{Linearity},
    )
        empty!(coloring_storage)
        _compute_gradient_sparsity!(coloring_storage, expr.nodes)
        for k in dependent_subexpressions
            _compute_gradient_sparsity!(
                coloring_storage,
                subexpressions[k].nodes,
            )
        end
        grad_sparsity = sort!(collect(coloring_storage))
        empty!(coloring_storage)
        if want_hess
            edgelist = _compute_hessian_sparsity(
                expr.nodes,
                expr.adj,
                linearity,
                coloring_storage,
                subexpression_edgelist,
                subexpression_variables,
            )
            hess_I, hess_J, rinfo = Coloring.hessian_color_preprocess(
                edgelist,
                num_variables,
                coloring_storage,
            )
            seed_matrix = Coloring.seed_matrix(rinfo)
            return new(
                expr,
                grad_sparsity,
                hess_I,
                hess_J,
                rinfo,
                seed_matrix,
                dependent_subexpressions,
            )
        else
            return new(
                expr,
                grad_sparsity,
                Int[],
                Int[],
                Coloring.RecoveryInfo(),
                Array{Float64}(undef, 0, 0),
                dependent_subexpressions,
            )
        end
    end
end

"""
    NLPEvaluator(
        model::Nonlinear.Model,
        ordered_variables::Vector{MOI.VariableIndex},
    )

Return an `NLPEvaluator` object that implements the `MOI.AbstractNLPEvaluator`
interface.

!!! warning
    Before using, you must initialize the evaluator using `MOI.initialize`.
"""
mutable struct NLPEvaluator <: MOI.AbstractNLPEvaluator
    data::Nonlinear.Model
    ordered_variables::Vector{MOI.VariableIndex}

    objective::Union{Nothing,_FunctionStorage}
    constraints::Vector{_FunctionStorage}
    subexpressions::Vector{_SubexpressionStorage}
    subexpression_order::Vector{Int}
    # Storage for the subexpressions in reverse-mode automatic differentiation.
    subexpression_forward_values::Vector{Float64}
    subexpression_reverse_values::Vector{Float64}
    subexpression_linearity::Vector{Linearity}

    # A cache of the last x. This is used to guide whether we need to re-run
    # reverse-mode automatic differentiation.
    last_x::Vector{Float64}

    # Temporary storage for computing Jacobians. This is also used as temporary
    # storage for the input of multivariate functions.
    jac_storage::Vector{Float64}
    # Temporary storage for the gradient of multivariate functions
    user_output_buffer::Vector{Float64}

    # storage for computing hessians
    # these Float64 vectors are reinterpreted to hold multiple epsilon components
    # so the length should be multiplied by the maximum number of epsilon components
    disable_2ndorder::Bool # don't offer Hess or HessVec
    want_hess::Bool
    storage_ϵ::Vector{Float64} # (longest expression including subexpressions)
    input_ϵ::Vector{Float64} # (number of variables)
    output_ϵ::Vector{Float64} # (number of variables)
    subexpression_forward_values_ϵ::Vector{Float64} # (number of subexpressions)
    subexpression_reverse_values_ϵ::Vector{Float64} # (number of subexpressions)
    hessian_sparsity::Vector{Tuple{Int64,Int64}}
    max_chunk::Int # chunk size for which we've allocated storage

    function NLPEvaluator(
        data::Nonlinear.Model,
        ordered_variables::Vector{MOI.VariableIndex},
    )
        return new(data, ordered_variables)
    end
end
