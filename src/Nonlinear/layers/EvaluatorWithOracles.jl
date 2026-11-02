# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    EvaluatorWithOracles(
        model::ModelWithOracles{T,M},
        inner::E,
        ordered_variables::Vector{MOI.VariableIndex},
    ) where {T,M,E}

An [`MOI.AbstractNLPEvaluator`](@Ref) that wraps `ModelWithOracles`.
"""
mutable struct EvaluatorWithOracles{T,M,E<:MOI.AbstractNLPEvaluator} <:
               MOI.AbstractNLPEvaluator
    model::ModelWithOracles{T,M}
    inner::E
    ordered_variables::Vector{MOI.VariableIndex}
    columns::Vector{Vector{Int}}
    x_buffer::Vector{Vector{T}}
    jacobian_buffer::Vector{Vector{T}}
    hessian_buffer::Vector{Vector{T}}

    function EvaluatorWithOracles(
        model::ModelWithOracles{T,M},
        inner::E,
        ordered_variables::Vector{MOI.VariableIndex},
    ) where {T,M,E}
        return new{T,M,E}(
            model,
            inner,
            ordered_variables,
            Vector{Int}[],
            Vector{T}[],
            Vector{T}[],
            Vector{T}[],
        )
    end
end

function Evaluator(
    model::ModelWithOracles,
    backend,
    ordered_variables::Vector{MOI.VariableIndex},
)
    return EvaluatorWithOracles(
        model,
        Evaluator(model.inner, backend, ordered_variables),
        ordered_variables,
    )
end

function MOI.features_available(d::EvaluatorWithOracles)
    features = filter(
        f -> f in (:Grad, :Jac, :JacVec, :Hess, :HessVec),
        MOI.features_available(d.inner),
    )
    if any(
        s.eval_hessian_lagrangian === nothing for (_, s) in d.model.constraints
    )
        filter!(f -> !(f in (:Hess, :HessVec)), features)
    end
    return features
end

function MOI.initialize(d::EvaluatorWithOracles{T}, features) where {T}
    map = Dict(x => i for (i, x) in enumerate(d.ordered_variables))
    empty!(d.columns)
    empty!(d.x_buffer)
    empty!(d.jacobian_buffer)
    empty!(d.hessian_buffer)
    for (f, s) in d.model.constraints
        push!(d.columns, [map[x] for x in f.variables])
        push!(d.x_buffer, zeros(T, s.input_dimension))
        push!(d.jacobian_buffer, zeros(T, length(s.jacobian_structure)))
        n = length(s.hessian_lagrangian_structure)
        push!(d.hessian_buffer, zeros(T, n))
    end
    MOI.initialize(d.inner, features)
    return
end

function _gather!(d::EvaluatorWithOracles, k, x)
    buffer = d.x_buffer[k]
    for (j, col) in enumerate(d.columns[k])
        buffer[j] = x[col]
    end
    return buffer
end

MOI.eval_objective(d::EvaluatorWithOracles, x) = MOI.eval_objective(d.inner, x)
function MOI.eval_objective_gradient(d::EvaluatorWithOracles, g, x)
    return MOI.eval_objective_gradient(d.inner, g, x)
end

function MOI.eval_constraint(d::EvaluatorWithOracles, g, x)
    offset = 0
    for (k, (_, s)) in enumerate(d.model.constraints)
        s.eval_f(view(g, offset .+ (1:s.output_dimension)), _gather!(d, k, x))
        offset += s.output_dimension
    end
    MOI.eval_constraint(d.inner, view(g, (offset+1):length(g)), x)
    return
end

function MOI.jacobian_structure(d::EvaluatorWithOracles)
    J, offset = Tuple{Int,Int}[], 0
    for (k, (_, s)) in enumerate(d.model.constraints)
        for (row, col) in s.jacobian_structure
            push!(J, (offset + row, d.columns[k][col]))
        end
        offset += s.output_dimension
    end
    append!(
        J,
        ((row + offset, col) for (row, col) in MOI.jacobian_structure(d.inner)),
    )
    return J
end

function MOI.eval_constraint_jacobian(d::EvaluatorWithOracles, J, x)
    offset = 0
    for (k, (_, s)) in enumerate(d.model.constraints)
        n = length(s.jacobian_structure)
        s.eval_jacobian(view(J, offset .+ (1:n)), _gather!(d, k, x))
        offset += n
    end
    MOI.eval_constraint_jacobian(d.inner, view(J, (offset+1):length(J)), x)
    return
end

function MOI.hessian_lagrangian_structure(d::EvaluatorWithOracles)
    H = Tuple{Int,Int}[]
    for (k, (_, s)) in enumerate(d.model.constraints)
        for (i, j) in s.hessian_lagrangian_structure
            push!(H, (d.columns[k][i], d.columns[k][j]))
        end
    end
    append!(H, MOI.hessian_lagrangian_structure(d.inner))
    return H
end

function MOI.eval_hessian_lagrangian(d::EvaluatorWithOracles, H, x, σ, μ)
    offset = row_offset = 0
    for (k, (_, s)) in enumerate(d.model.constraints)
        n = length(s.hessian_lagrangian_structure)
        rows = row_offset .+ (1:s.output_dimension)
        s.eval_hessian_lagrangian(
            view(H, offset .+ (1:n)),
            _gather!(d, k, x),
            view(μ, rows),
        )
        offset += n
        row_offset += s.output_dimension
    end
    MOI.eval_hessian_lagrangian(
        d.inner,
        view(H, (offset+1):length(H)),
        x,
        σ,
        view(μ, (row_offset+1):length(μ)),
    )
    return
end

# The rows of the oracles come first and are written directly, then the inner
# evaluator stores the remaining rows.
function MOI.eval_constraint_jacobian_product(d::EvaluatorWithOracles, y, x, w)
    offset = 0
    for (k, (_, s)) in enumerate(d.model.constraints)
        J = d.jacobian_buffer[k]
        s.eval_jacobian(J, _gather!(d, k, x))
        rows = view(y, offset .+ (1:s.output_dimension))
        fill!(rows, zero(eltype(y)))
        for ((row, col), v) in zip(s.jacobian_structure, J)
            rows[row] += v * w[d.columns[k][col]]
        end
        offset += s.output_dimension
    end
    MOI.eval_constraint_jacobian_product(
        d.inner,
        view(y, (offset+1):length(y)),
        x,
        w,
    )
    return
end

# `MOI.eval_constraint_jacobian_transpose_product` of the inner evaluator is
# called first as it zeroes the output, then the oracles accumulate.
function MOI.eval_constraint_jacobian_transpose_product(
    d::EvaluatorWithOracles,
    y,
    x,
    w,
)
    m = sum(s.output_dimension for (_, s) in d.model.constraints; init = 0)
    MOI.eval_constraint_jacobian_transpose_product(
        d.inner,
        y,
        x,
        view(w, (m+1):length(w)),
    )
    offset = 0
    for (k, (_, s)) in enumerate(d.model.constraints)
        J = d.jacobian_buffer[k]
        s.eval_jacobian(J, _gather!(d, k, x))
        for ((row, col), v) in zip(s.jacobian_structure, J)
            y[d.columns[k][col]] += v * w[offset+row]
        end
        offset += s.output_dimension
    end
    return
end

# `MOI.eval_hessian_lagrangian_product` of the inner evaluator is called first
# as it zeroes the output, then the oracles accumulate. The Hessian structure
# of an oracle only lists one triangle, so off-diagonal entries are counted
# twice.
function MOI.eval_hessian_lagrangian_product(
    d::EvaluatorWithOracles,
    H,
    x,
    v,
    σ,
    μ,
)
    m = sum(s.output_dimension for (_, s) in d.model.constraints; init = 0)
    MOI.eval_hessian_lagrangian_product(
        d.inner,
        H,
        x,
        v,
        σ,
        view(μ, (m+1):length(μ)),
    )
    offset = 0
    for (k, (_, s)) in enumerate(d.model.constraints)
        h = d.hessian_buffer[k]
        rows = offset .+ (1:s.output_dimension)
        s.eval_hessian_lagrangian(h, _gather!(d, k, x), view(μ, rows))
        columns = d.columns[k]
        for ((i, j), hij) in zip(s.hessian_lagrangian_structure, h)
            H[columns[i]] += hij * v[columns[j]]
            if i != j
                H[columns[j]] += hij * v[columns[i]]
            end
        end
        offset += s.output_dimension
    end
    return
end

_has_objective(d::EvaluatorWithOracles) = _has_objective(d.inner)
