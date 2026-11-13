# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    EvaluatorWithQuad(
        model::ModelWithQuad,
        inner::MOI.AbstractNLPEvaluator,
    ) <: MOI.AbstractNLPEvaluator

!!! warning
    This API is experimental. Until this message is removed, breaking changes
    may be introduced in any minor or patch release of MathOptInterface.

The evaluator of a [`ModelWithQuad`](@ref) layer. It implements the
[`MOI.AbstractNLPEvaluator`](@ref) interface: the rows of the QP block come
first, followed by the rows of `inner`, and the Jacobian and Hessian product
callbacks compose the contributions of the two blocks.

Create it with `Evaluator(model::ModelWithQuad, backend)`, which recursively
creates the evaluator of the inner model, or construct it directly from an
existing inner evaluator.

The QP block is evaluated as stored: [`ModelWithQuad`](@ref) owns the
variables of the model, so their indices are the columns `1:n` and no
remapping is needed.
"""
mutable struct EvaluatorWithQuad{T,M,E<:MOI.AbstractNLPEvaluator} <:
               MOI.AbstractNLPEvaluator
    model::ModelWithQuad{T,M}
    inner::E
    # The number of entries of the Jacobian and of the Hessian of the
    # Lagrangian of the QP block, computed during `MOI.initialize`.
    qp_nnzj::Int
    qp_nnzh::Int

    function EvaluatorWithQuad(
        model::ModelWithQuad{T,M},
        inner::E,
    ) where {T,M,E<:MOI.AbstractNLPEvaluator}
        return new{T,M,E}(model, inner, 0, 0)
    end
end

function Evaluator(
    model::ModelWithQuad,
    backend::AbstractAutomaticDifferentiation,
)
    vars = MOI.get(model.variables, MOI.ListOfVariableIndices())
    inner = Evaluator(model.inner, backend, vars)
    return EvaluatorWithQuad(model, inner)
end

function Evaluator(
    model::ModelWithQuad,
    backend::AbstractAutomaticDifferentiation,
    vars::Vector{MOI.VariableIndex},
)
    return EvaluatorWithQuad(model, Evaluator(model.inner, backend, vars))
end

function MOI.features_available(d::EvaluatorWithQuad)
    features = MOI.features_available(d.inner)
    return filter(f -> f in (:Grad, :Jac, :JacVec, :Hess, :HessVec), features)
end

function MOI.initialize(d::EvaluatorWithQuad, features::Vector{Symbol})
    d.qp_nnzj = length(MOI.jacobian_structure(d.model.qp))
    d.qp_nnzh = length(MOI.hessian_lagrangian_structure(d.model.qp))
    MOI.initialize(d.inner, features)
    return
end

function MOI.eval_objective(d::EvaluatorWithQuad{T}, x) where {T}
    sink = d.model.objective_sink
    if sink == _QUAD
        value = MOI.eval_objective(d.model.qp, x)
        return _objective_sign(MOI.get(d.model, MOI.ObjectiveSense())) * value
    elseif sink == _INNER
        return MOI.eval_objective(d.inner, x)
    else
        return zero(T)
    end
end

function MOI.eval_objective_gradient(d::EvaluatorWithQuad{T}, grad, x) where {T}
    sink = d.model.objective_sink
    if sink == _QUAD
        MOI.eval_objective_gradient(d.model.qp, grad, x)
        grad .*= _objective_sign(MOI.get(d.model, MOI.ObjectiveSense()))
    elseif sink == _INNER
        MOI.eval_objective_gradient(d.inner, grad, x)
    else
        grad .= zero(T)
    end
    return
end

function MOI.eval_constraint(d::EvaluatorWithQuad, g, x)
    m = length(d.model.qp)
    MOI.eval_constraint(d.model.qp, view(g, 1:m), x)
    MOI.eval_constraint(d.inner, view(g, (m+1):length(g)), x)
    return
end

function MOI.jacobian_structure(d::EvaluatorWithQuad)
    J = MOI.jacobian_structure(d.model.qp)
    offset = length(d.model.qp)
    for (row, col) in MOI.jacobian_structure(d.inner)
        push!(J, (row + offset, col))
    end
    return J
end

function MOI.eval_constraint_jacobian(d::EvaluatorWithQuad, J, x)
    MOI.eval_constraint_jacobian(d.model.qp, J, x)
    MOI.eval_constraint_jacobian(d.inner, view(J, (d.qp_nnzj+1):length(J)), x)
    return
end

function MOI.hessian_lagrangian_structure(d::EvaluatorWithQuad)
    H = MOI.hessian_lagrangian_structure(d.model.qp)
    append!(H, MOI.hessian_lagrangian_structure(d.inner))
    return H
end

function MOI.eval_hessian_lagrangian(d::EvaluatorWithQuad, H, x, σ, μ)
    m = length(d.model.qp)
    # If the objective is not in the QP block, `d.model.qp.objective` is zero, so
    # passing `σ` is harmless; and vice versa for the inner evaluator.
    qp_σ =
        d.model.objective_sink == _QUAD ?
        _objective_sign(MOI.get(d.model, MOI.ObjectiveSense())) * σ : σ
    MOI.eval_hessian_lagrangian(d.model.qp, H, x, qp_σ, view(μ, 1:m))
    MOI.eval_hessian_lagrangian(
        d.inner,
        view(H, (d.qp_nnzh+1):length(H)),
        x,
        σ,
        view(μ, (m+1):length(μ)),
    )
    return
end

# The rows of the two blocks are disjoint: the inner evaluator stores its
# rows, and the QP block accumulates into its rows, which must be zeroed
# first.
function MOI.eval_constraint_jacobian_product(d::EvaluatorWithQuad, y, x, w)
    m = length(d.model.qp)
    fill!(view(y, 1:m), zero(eltype(y)))
    MOI.eval_constraint_jacobian_product(
        d.inner,
        view(y, (m+1):length(y)),
        x,
        w,
    )
    _add_constraint_jacobian_product(d.model.qp, y, x, w)
    return
end

# Both blocks contribute to the same variable-dimensional output.
# `MOI.eval_constraint_jacobian_transpose_product` is called first as it
# zeroes the output before accumulating, then the QP block accumulates.
function MOI.eval_constraint_jacobian_transpose_product(
    d::EvaluatorWithQuad,
    y,
    x,
    w,
)
    m = length(d.model.qp)
    MOI.eval_constraint_jacobian_transpose_product(
        d.inner,
        y,
        x,
        view(w, (m+1):length(w)),
    )
    _add_constraint_jacobian_transpose_product(d.model.qp, y, x, view(w, 1:m))
    return
end

# `MOI.eval_hessian_lagrangian_product` is called first as it zeroes the
# output before accumulating, then the QP block accumulates.
function MOI.eval_hessian_lagrangian_product(
    d::EvaluatorWithQuad,
    H,
    x,
    v,
    σ,
    μ,
)
    m = length(d.model.qp)
    MOI.eval_hessian_lagrangian_product(
        d.inner,
        H,
        x,
        v,
        σ,
        view(μ, (m+1):length(μ)),
    )
    qp_σ =
        d.model.objective_sink == _QUAD ?
        _objective_sign(MOI.get(d.model, MOI.ObjectiveSense())) * σ : σ
    _add_hessian_lagrangian_product(d.model.qp, H, x, v, qp_σ, view(μ, 1:m))
    return
end

_has_objective(d::Evaluator) = d.model.objective !== nothing

function _has_objective(d::EvaluatorWithQuad)
    if d.model.objective_sink == _QUAD
        return true
    end
    return _has_objective(d.inner)
end

function MOI.NLPBlockData(d::EvaluatorWithQuad)
    bounds = MOI.Utilities.constraint_bounds(d.model)
    pairs = MOI.NLPBoundsPair.(bounds.lower, bounds.upper)
    return MOI.NLPBlockData(pairs, d, _has_objective(d))
end
