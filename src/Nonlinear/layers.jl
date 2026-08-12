# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# The evaluator layers in this file are adapted from
# `Ipopt.jl/ext/IpoptMathOptInterfaceExt`.

"""
    ModelWithOracles{T,M}(inner::M) where {T,M}

A model layer that stores `MOI.VectorOfVariables`-in-
[`MOI.VectorNonlinearOracle`](@ref) constraints and forwards everything else
to the `inner` model.

`ModelWithOracles(inner)` defaults `T` to `Float64`.

When wrapped in an [`Evaluator`](@ref), the layer's rows come first, followed
by the rows of the inner evaluator.
"""
mutable struct ModelWithOracles{T,M}
    constraints::Vector{
        Tuple{MOI.VectorOfVariables,MOI.VectorNonlinearOracle{T}},
    }
    inner::M

    # This constructor wraps an existing vector of constraints so that
    # solvers with their own storage can assemble a layer without copying.
    function ModelWithOracles{T}(
        constraints::Vector{
            Tuple{MOI.VectorOfVariables,MOI.VectorNonlinearOracle{T}},
        },
        inner::M,
    ) where {T,M}
        return new{T,M}(constraints, inner)
    end
end

function ModelWithOracles{T}(inner) where {T}
    constraints = Tuple{MOI.VectorOfVariables,MOI.VectorNonlinearOracle{T}}[]
    return ModelWithOracles{T}(constraints, inner)
end

ModelWithOracles(inner) = ModelWithOracles{Float64}(inner)

"""
    ModelWithQuad{T,M}(inner::M) where {T,M}

A model layer that stores affine and quadratic objectives and constraints in a
[`QPBlockData`](@ref) and forwards everything else to the `inner` model.

`ModelWithQuad(inner)` defaults `T` to `Float64`.

When wrapped in an [`Evaluator`](@ref), the layer's rows come first, followed
by the rows of the inner evaluator.

Functions added to this layer may contain parameters, following the
convention documented in [`QPBlockData`](@ref): a variable is a parameter if
and only if its index is a key of `qp.parameters`.
"""
mutable struct ModelWithQuad{T,M}
    qp::QPBlockData{T}
    inner::M
    objective_sink::Symbol # :none, :quad or :inner

    # This constructor wraps an existing QPBlockData so that solvers with
    # their own storage can assemble a layer without copying.
    function ModelWithQuad{T}(
        qp::QPBlockData{T},
        inner::M;
        objective_sink::Symbol = :none,
    ) where {T,M}
        return new{T,M}(qp, inner, objective_sink)
    end
end

function ModelWithQuad{T}(inner) where {T}
    return ModelWithQuad{T}(QPBlockData{T}(), inner)
end

ModelWithQuad(inner) = ModelWithQuad{Float64}(inner)

const _LayerModel = Union{ModelWithQuad,ModelWithOracles}

# Forwarded methods common to all layers.

function add_parameter(model::_LayerModel, value::Real)
    return add_parameter(model.inner, value)
end

add_expression(model::_LayerModel, expr) = add_expression(model.inner, expr)

Base.getindex(model::_LayerModel, index::ExpressionIndex) = model.inner[index]

function register_operator(
    model::_LayerModel,
    op::Symbol,
    nargs::Int,
    f::Function...,
)
    return register_operator(model.inner, op, nargs, f...)
end

# ModelWithQuad

function set_objective(
    model::ModelWithQuad{T},
    obj::Union{
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
    },
) where {T}
    MOI.set(model.qp, MOI.ObjectiveFunction{typeof(obj)}(), obj)
    set_objective(model.inner, nothing)
    model.objective_sink = :quad
    return
end

function set_objective(model::ModelWithQuad{T}, obj) where {T}
    F = MOI.ScalarAffineFunction{T}
    MOI.set(model.qp, MOI.ObjectiveFunction{F}(), zero(F))
    set_objective(model.inner, obj)
    model.objective_sink = obj === nothing ? :none : :inner
    return
end

function add_constraint(
    model::ModelWithQuad{T},
    func::Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
    set::Union{
        MOI.LessThan{T},
        MOI.GreaterThan{T},
        MOI.EqualTo{T},
        MOI.Interval{T},
    },
) where {T}
    return MOI.add_constraint(model.qp, func, set)
end

function add_constraint(model::ModelWithQuad, func, set)
    return add_constraint(model.inner, func, set)
end

# ModelWithOracles

set_objective(model::ModelWithOracles, obj) = set_objective(model.inner, obj)

function add_constraint(
    model::ModelWithOracles{T},
    func::MOI.VectorOfVariables,
    set::MOI.VectorNonlinearOracle{T},
) where {T}
    if length(func.variables) != set.input_dimension
        throw(DimensionMismatch())
    end
    push!(model.constraints, (func, set))
    F, S = MOI.VectorOfVariables, MOI.VectorNonlinearOracle{T}
    return MOI.ConstraintIndex{F,S}(length(model.constraints))
end

function add_constraint(model::ModelWithOracles, func, set)
    return add_constraint(model.inner, func, set)
end

"""
    EvaluatorWithQuad(
        model::ModelWithQuad,
        inner::MOI.AbstractNLPEvaluator,
        ordered_variables::Vector{MOI.VariableIndex},
    ) <: MOI.AbstractNLPEvaluator

The evaluator of a [`ModelWithQuad`](@ref) layer. The rows of the
[`QPBlockData`](@ref) come first, followed by the rows of `inner`.

Create it with `Evaluator(model::ModelWithQuad, backend, ordered_variables)`,
which recursively creates the evaluator of the inner model.
"""
mutable struct EvaluatorWithQuad{T,M,E<:MOI.AbstractNLPEvaluator} <:
               MOI.AbstractNLPEvaluator
    model::ModelWithQuad{T,M}
    inner::E
    ordered_variables::Vector{MOI.VariableIndex}
    # A copy of `model.qp` with the variables mapped to their consecutive
    # 1-based index in `ordered_variables`. Rebuilt during `MOI.initialize`.
    qp::QPBlockData{T}

    function EvaluatorWithQuad(
        model::ModelWithQuad{T,M},
        inner::E,
        ordered_variables::Vector{MOI.VariableIndex},
    ) where {T,M,E<:MOI.AbstractNLPEvaluator}
        return new{T,M,E}(model, inner, ordered_variables, QPBlockData{T}())
    end
end

function Evaluator(
    model::ModelWithQuad,
    backend::AbstractAutomaticDifferentiation,
    ordered_variables::Vector{MOI.VariableIndex},
)
    inner = Evaluator(model.inner, backend, ordered_variables)
    return EvaluatorWithQuad(model, inner, ordered_variables)
end

function MOI.features_available(d::EvaluatorWithQuad)
    features = MOI.features_available(d.inner)
    return filter(f -> f in (:Grad, :Jac, :JacVec, :Hess, :HessVec), features)
end

function MOI.initialize(
    d::EvaluatorWithQuad{T},
    features::Vector{Symbol},
) where {T}
    index_map = Dict{MOI.VariableIndex,MOI.VariableIndex}(
        x => MOI.VariableIndex(i) for (i, x) in enumerate(d.ordered_variables)
    )
    # Variables absent from `ordered_variables` are parameters: they keep
    # their index, which the `parameters` dictionary uses as key.
    fmap = v::MOI.VariableIndex -> get(index_map, v, v)
    src = d.model.qp
    qp = QPBlockData{T}()
    qp.objective = MOI.Utilities.map_indices(fmap, src.objective)
    qp.objective_function_type = src.objective_function_type
    for f in src.constraints
        push!(qp.constraints, MOI.Utilities.map_indices(fmap, f))
    end
    append!(qp.g_L, src.g_L)
    append!(qp.g_U, src.g_U)
    append!(qp.mult_g, src.mult_g)
    append!(qp.function_type, src.function_type)
    append!(qp.bound_type, src.bound_type)
    # Alias, do not copy: parameter values updated in the model between
    # solves must be visible to the evaluator without re-initializing.
    qp.parameters = src.parameters
    d.qp = qp
    MOI.initialize(d.inner, features)
    return
end

function MOI.eval_objective(d::EvaluatorWithQuad{T}, x) where {T}
    sink = d.model.objective_sink
    if sink == :quad
        return MOI.eval_objective(d.qp, x)
    elseif sink == :inner
        return MOI.eval_objective(d.inner, x)
    else
        return zero(T)
    end
end

function MOI.eval_objective_gradient(d::EvaluatorWithQuad{T}, grad, x) where {T}
    sink = d.model.objective_sink
    if sink == :quad
        MOI.eval_objective_gradient(d.qp, grad, x)
    elseif sink == :inner
        MOI.eval_objective_gradient(d.inner, grad, x)
    else
        grad .= zero(T)
    end
    return
end

function MOI.eval_constraint(d::EvaluatorWithQuad, g, x)
    m = length(d.qp)
    MOI.eval_constraint(d.qp, view(g, 1:m), x)
    MOI.eval_constraint(d.inner, view(g, (m+1):length(g)), x)
    return
end

function MOI.jacobian_structure(d::EvaluatorWithQuad)
    J = MOI.jacobian_structure(d.qp)
    offset = length(d.qp)
    # An evaluator is only required to implement `jacobian_structure` if it
    # supports `:Jac`. If the inner evaluator does not (it then must not have
    # any rows for the stack to be usable), append nothing.
    if :Jac in MOI.features_available(d.inner)
        for (row, col) in MOI.jacobian_structure(d.inner)
            push!(J, (row + offset, col))
        end
    end
    return J
end

function MOI.eval_constraint_jacobian(d::EvaluatorWithQuad, J, x)
    nnz = MOI.eval_constraint_jacobian(d.qp, J, x)
    MOI.eval_constraint_jacobian(d.inner, view(J, (nnz+1):length(J)), x)
    return
end

function MOI.hessian_lagrangian_structure(d::EvaluatorWithQuad)
    H = MOI.hessian_lagrangian_structure(d.qp)
    if :Hess in MOI.features_available(d.inner)
        append!(H, MOI.hessian_lagrangian_structure(d.inner))
    end
    return H
end

function MOI.eval_hessian_lagrangian(d::EvaluatorWithQuad, H, x, σ, μ)
    m = length(d.qp)
    # If the objective is not in the QP block, `d.qp.objective` is zero, so
    # passing `σ` is harmless; and vice versa for the inner evaluator.
    nnz = MOI.eval_hessian_lagrangian(d.qp, H, x, σ, view(μ, 1:m))
    MOI.eval_hessian_lagrangian(
        d.inner,
        view(H, (nnz+1):length(H)),
        x,
        σ,
        view(μ, (m+1):length(μ)),
    )
    return
end

# The rows of the two blocks are disjoint, so zero everything and let each
# block write its own rows.
function MOI.eval_constraint_jacobian_product(d::EvaluatorWithQuad, y, x, w)
    fill!(y, zero(eltype(y)))
    m = length(d.qp)
    MOI.eval_constraint_jacobian_product(
        d.inner,
        view(y, (m+1):length(y)),
        x,
        w,
    )
    MOI.eval_constraint_jacobian_product(d.qp, y, x, w)
    return
end

# Both blocks accumulate into the same variable-dimensional output. Call the
# inner evaluator FIRST because implementations are allowed to overwrite the
# output, and accumulate the QP block afterwards.
function MOI.eval_constraint_jacobian_transpose_product(
    d::EvaluatorWithQuad,
    y,
    x,
    w,
)
    fill!(y, zero(eltype(y)))
    m = length(d.qp)
    MOI.eval_constraint_jacobian_transpose_product(
        d.inner,
        y,
        x,
        view(w, (m+1):length(w)),
    )
    MOI.eval_constraint_jacobian_transpose_product(d.qp, y, x, view(w, 1:m))
    return
end

function MOI.eval_hessian_lagrangian_product(
    d::EvaluatorWithQuad,
    H,
    x,
    v,
    σ,
    μ,
)
    fill!(H, zero(eltype(H)))
    m = length(d.qp)
    MOI.eval_hessian_lagrangian_product(
        d.inner,
        H,
        x,
        v,
        σ,
        view(μ, (m+1):length(μ)),
    )
    MOI.eval_hessian_lagrangian_product(d.qp, H, x, v, σ, view(μ, 1:m))
    return
end

"""
    EvaluatorWithOracles(
        model::ModelWithOracles,
        inner::MOI.AbstractNLPEvaluator,
        ordered_variables::Vector{MOI.VariableIndex},
    ) <: MOI.AbstractNLPEvaluator

The evaluator of a [`ModelWithOracles`](@ref) layer. The rows of the oracles
come first, in the order they were added, followed by the rows of `inner`.

Create it with `Evaluator(model::ModelWithOracles, backend,
ordered_variables)`, which recursively creates the evaluator of the inner
model.

If an oracle does not implement `eval_hessian_lagrangian`, the `:Hess` feature
is removed from [`MOI.features_available`](@ref).
"""
mutable struct EvaluatorWithOracles{T,M,E<:MOI.AbstractNLPEvaluator} <:
               MOI.AbstractNLPEvaluator
    model::ModelWithOracles{T,M}
    inner::E
    ordered_variables::Vector{MOI.VariableIndex}
    # For each oracle, the consecutive 1-based index of each of its input
    # variables. Rebuilt during `MOI.initialize`.
    columns::Vector{Vector{Int}}
    # For each oracle, a buffer to gather its input variables into. The public
    # `MOI.VectorNonlinearOracle` has no scratch storage of its own.
    x_buffer::Vector{Vector{T}}

    function EvaluatorWithOracles(
        model::ModelWithOracles{T,M},
        inner::E,
        ordered_variables::Vector{MOI.VariableIndex},
    ) where {T,M,E<:MOI.AbstractNLPEvaluator}
        return new{T,M,E}(
            model,
            inner,
            ordered_variables,
            Vector{Int}[],
            Vector{T}[],
        )
    end
end

function Evaluator(
    model::ModelWithOracles,
    backend::AbstractAutomaticDifferentiation,
    ordered_variables::Vector{MOI.VariableIndex},
)
    inner = Evaluator(model.inner, backend, ordered_variables)
    return EvaluatorWithOracles(model, inner, ordered_variables)
end

function _num_rows(d::EvaluatorWithOracles)
    return sum(s.output_dimension for (_, s) in d.model.constraints; init = 0)
end

function MOI.features_available(d::EvaluatorWithOracles)
    features = MOI.features_available(d.inner)
    features =
        filter(f -> f in (:Grad, :Jac, :JacVec, :Hess, :HessVec), features)
    if !isempty(d.model.constraints)
        # The oracles have no product callbacks.
        filter!(f -> !(f in (:JacVec, :HessVec)), features)
    end
    no_hessian = any(d.model.constraints) do (_, s)
        return s.eval_hessian_lagrangian === nothing
    end
    if no_hessian
        filter!(f -> f != :Hess, features)
    end
    return features
end

function MOI.initialize(
    d::EvaluatorWithOracles{T},
    features::Vector{Symbol},
) where {T}
    index_map = Dict{MOI.VariableIndex,Int}(
        x => i for (i, x) in enumerate(d.ordered_variables)
    )
    empty!(d.columns)
    empty!(d.x_buffer)
    for (f, s) in d.model.constraints
        push!(d.columns, [index_map[x] for x in f.variables])
        push!(d.x_buffer, zeros(T, s.input_dimension))
    end
    MOI.initialize(d.inner, features)
    return
end

function _gather!(d::EvaluatorWithOracles, k::Int, x)
    xk = d.x_buffer[k]
    for (j, col) in enumerate(d.columns[k])
        xk[j] = x[col]
    end
    return xk
end

MOI.eval_objective(d::EvaluatorWithOracles, x) = MOI.eval_objective(d.inner, x)

function MOI.eval_objective_gradient(d::EvaluatorWithOracles, grad, x)
    MOI.eval_objective_gradient(d.inner, grad, x)
    return
end

function MOI.eval_constraint(d::EvaluatorWithOracles, g, x)
    offset = 0
    for (k, (_, s)) in enumerate(d.model.constraints)
        xk = _gather!(d, k, x)
        s.eval_f(view(g, offset .+ (1:s.output_dimension)), xk)
        offset += s.output_dimension
    end
    MOI.eval_constraint(d.inner, view(g, (offset+1):length(g)), x)
    return
end

function MOI.jacobian_structure(d::EvaluatorWithOracles)
    J = Tuple{Int,Int}[]
    row_offset = 0
    for (k, (_, s)) in enumerate(d.model.constraints)
        columns = d.columns[k]
        for (i, j) in s.jacobian_structure
            push!(J, (row_offset + i, columns[j]))
        end
        row_offset += s.output_dimension
    end
    if :Jac in MOI.features_available(d.inner)
        for (row, col) in MOI.jacobian_structure(d.inner)
            push!(J, (row + row_offset, col))
        end
    end
    return J
end

function MOI.eval_constraint_jacobian(d::EvaluatorWithOracles, J, x)
    offset = 0
    for (k, (_, s)) in enumerate(d.model.constraints)
        xk = _gather!(d, k, x)
        nnz = length(s.jacobian_structure)
        s.eval_jacobian(view(J, offset .+ (1:nnz)), xk)
        offset += nnz
    end
    MOI.eval_constraint_jacobian(d.inner, view(J, (offset+1):length(J)), x)
    return
end

function MOI.hessian_lagrangian_structure(d::EvaluatorWithOracles)
    H = Tuple{Int,Int}[]
    for (k, (_, s)) in enumerate(d.model.constraints)
        columns = d.columns[k]
        for (i, j) in s.hessian_lagrangian_structure
            push!(H, (columns[i], columns[j]))
        end
    end
    if :Hess in MOI.features_available(d.inner)
        append!(H, MOI.hessian_lagrangian_structure(d.inner))
    end
    return H
end

function MOI.eval_hessian_lagrangian(d::EvaluatorWithOracles, H, x, σ, μ)
    offset, μ_offset = 0, 0
    for (k, (_, s)) in enumerate(d.model.constraints)
        xk = _gather!(d, k, x)
        nnz = length(s.hessian_lagrangian_structure)
        μk = view(μ, μ_offset .+ (1:s.output_dimension))
        s.eval_hessian_lagrangian(view(H, offset .+ (1:nnz)), xk, μk)
        offset += nnz
        μ_offset += s.output_dimension
    end
    MOI.eval_hessian_lagrangian(
        d.inner,
        view(H, (offset+1):length(H)),
        x,
        σ,
        view(μ, (μ_offset+1):length(μ)),
    )
    return
end

# The product callbacks are only supported when the layer has no oracle
# constraints (see `MOI.features_available`), in which case the layer is a
# pass-through.

function _check_no_oracles(d::EvaluatorWithOracles)
    if !isempty(d.model.constraints)
        error(
            "The product callbacks are not supported in the presence of " *
            "VectorNonlinearOracle constraints.",
        )
    end
    return
end

function MOI.eval_constraint_jacobian_product(d::EvaluatorWithOracles, y, x, w)
    _check_no_oracles(d)
    MOI.eval_constraint_jacobian_product(d.inner, y, x, w)
    return
end

function MOI.eval_constraint_jacobian_transpose_product(
    d::EvaluatorWithOracles,
    y,
    x,
    w,
)
    _check_no_oracles(d)
    MOI.eval_constraint_jacobian_transpose_product(d.inner, y, x, w)
    return
end

function MOI.eval_hessian_lagrangian_product(
    d::EvaluatorWithOracles,
    H,
    x,
    v,
    σ,
    μ,
)
    _check_no_oracles(d)
    MOI.eval_hessian_lagrangian_product(d.inner, H, x, v, σ, μ)
    return
end

# Linearity and row queries

function num_constraints(d::EvaluatorWithQuad)
    return length(d.model.qp) + num_constraints(d.inner)
end

function num_constraints(d::EvaluatorWithOracles)
    return _num_rows(d) + num_constraints(d.inner)
end

# Like `num_constraints`, but returns `nothing` instead of erroring when an
# evaluator at the bottom of the stack does not implement the query.
function _try_num_constraints(ev::MOI.AbstractNLPEvaluator)
    if !applicable(num_constraints, ev)
        return nothing
    end
    return num_constraints(ev)
end

function _try_num_constraints(d::EvaluatorWithQuad)
    n = _try_num_constraints(d.inner)
    return n === nothing ? nothing : length(d.model.qp) + n
end

function _try_num_constraints(d::EvaluatorWithOracles)
    n = _try_num_constraints(d.inner)
    return n === nothing ? nothing : _num_rows(d) + n
end

# Returns `nothing` if the inner evaluator implements neither
# `constraint_linearity` nor `num_constraints`, in which case the layer
# cannot describe its rows either.
function _inner_constraint_linearity(inner::MOI.AbstractNLPEvaluator)
    linearity = constraint_linearity(inner)
    if linearity !== nothing
        return linearity
    end
    n = _try_num_constraints(inner)
    if n === nothing
        return nothing
    end
    return fill(NONLINEAR, n)
end

function constraint_linearity(d::EvaluatorWithQuad)
    inner = _inner_constraint_linearity(d.inner)
    if inner === nothing
        return nothing
    end
    linearity = Linearity[
        ft == _kFunctionTypeScalarQuadratic ? QUADRATIC : LINEAR for
        ft in d.model.qp.function_type
    ]
    return vcat(linearity, inner)
end

function constraint_linearity(d::EvaluatorWithOracles)
    inner = _inner_constraint_linearity(d.inner)
    if inner === nothing
        return nothing
    end
    return vcat(fill(NONLINEAR, _num_rows(d)), inner)
end

function objective_linearity(d::EvaluatorWithQuad)
    sink = d.model.objective_sink
    if sink == :quad
        if d.model.qp.objective_function_type == _kFunctionTypeScalarQuadratic
            return QUADRATIC
        end
        return LINEAR
    elseif sink == :inner
        return objective_linearity(d.inner)
    else
        return CONSTANT
    end
end

objective_linearity(d::EvaluatorWithOracles) = objective_linearity(d.inner)

"""
    constraint_bounds(
        evaluator::MOI.AbstractNLPEvaluator,
    )::Vector{MOI.NLPBoundsPair}

Return the lower and upper bounds of each constraint in `evaluator`, aligned
with the rows of [`MOI.eval_constraint`](@ref).
"""
function constraint_bounds(evaluator::Evaluator)
    return MOI.NLPBoundsPair[
        _bound(c.set) for (_, c) in evaluator.model.constraints
    ]
end

function constraint_bounds(d::EvaluatorWithQuad)
    bounds = MOI.NLPBoundsPair[
        MOI.NLPBoundsPair(l, u) for
        (l, u) in zip(d.model.qp.g_L, d.model.qp.g_U)
    ]
    return vcat(bounds, constraint_bounds(d.inner))
end

function constraint_bounds(d::EvaluatorWithOracles)
    bounds = MOI.NLPBoundsPair[]
    for (_, s) in d.model.constraints
        for (l, u) in zip(s.l, s.u)
            push!(bounds, MOI.NLPBoundsPair(l, u))
        end
    end
    return vcat(bounds, constraint_bounds(d.inner))
end

_has_objective(d::Evaluator) = d.model.objective !== nothing

function _has_objective(d::EvaluatorWithQuad)
    if d.model.objective_sink == :quad
        return true
    end
    return _has_objective(d.inner)
end

_has_objective(d::EvaluatorWithOracles) = _has_objective(d.inner)

function MOI.NLPBlockData(d::Union{EvaluatorWithQuad,EvaluatorWithOracles})
    return MOI.NLPBlockData(constraint_bounds(d), d, _has_objective(d))
end
