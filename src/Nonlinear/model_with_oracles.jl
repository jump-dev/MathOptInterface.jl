# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

mutable struct ModelWithOracles{T,M} <: MOI.ModelLike
    constraints::Vector{
        Tuple{MOI.VectorOfVariables,MOI.VectorNonlinearOracle{T}},
    }
    multiplier_start::Vector{Union{Nothing,Vector{T}}}
    inner::M
end

function ModelWithOracles{T}(inner::M) where {T,M}
    constraints = Tuple{MOI.VectorOfVariables,MOI.VectorNonlinearOracle{T}}[]
    return ModelWithOracles{T,M}(
        constraints,
        Union{Nothing,Vector{T}}[],
        inner,
    )
end

ModelWithOracles(inner) = ModelWithOracles{Float64}(inner)

_parameter_values(model::ModelWithOracles) = _parameter_values(model.inner)
_has_nonlinear_data(model::ModelWithOracles) =
    !isempty(model.constraints) || _has_nonlinear_data(model.inner)
_is_nonlinear_input(
    ::ModelWithOracles{T},
    ::MOI.VectorOfVariables,
    ::MOI.VectorNonlinearOracle{T},
) where {T} = true
_is_nonlinear_input(model::ModelWithOracles, f, s) =
    _is_nonlinear_input(model.inner, f, s)
_is_nonlinear_objective(model::ModelWithOracles, f) =
    _is_nonlinear_objective(model.inner, f)

# Backward-compatible forwarding for users of the pre-MOI Nonlinear API.
add_parameter(model::ModelWithOracles, value::Real) =
    add_parameter(model.inner, value)
add_expression(model::ModelWithOracles, expression) =
    add_expression(model.inner, expression)
set_objective(model::ModelWithOracles, objective) =
    set_objective(model.inner, objective)
add_constraint(model::ModelWithOracles, f, s) =
    add_constraint(model.inner, f, s)
Base.getindex(model::ModelWithOracles, index::ExpressionIndex) =
    model.inner[index]
function register_operator(model::ModelWithOracles, op, nargs, functions...)
    return register_operator(model.inner, op, nargs, functions...)
end

MOI.supports_incremental_interface(model::ModelWithOracles) =
    MOI.supports_incremental_interface(model.inner)
MOI.supports(model::ModelWithOracles, attr::MOI.AbstractModelAttribute) =
    MOI.supports(model.inner, attr)
MOI.get(model::ModelWithOracles, attr::MOI.AbstractModelAttribute) =
    MOI.get(model.inner, attr)
MOI.set(model::ModelWithOracles, attr::MOI.AbstractModelAttribute, value) =
    MOI.set(model.inner, attr, value)
MOI.supports(
    model::ModelWithOracles,
    attr::MOI.AbstractConstraintAttribute,
    CI::Type{<:MOI.ConstraintIndex},
) = MOI.supports(model.inner, attr, CI)
function MOI.get(
    model::ModelWithOracles,
    attr::MOI.ListOfSupportedNonlinearOperators,
)
    return MOI.get(model.inner, attr)
end
MOI.add_variable(model::ModelWithOracles) = MOI.add_variable(model.inner)
MOI.add_constrained_variable(
    model::ModelWithOracles,
    set::MOI.AbstractScalarSet,
) =
    MOI.add_constrained_variable(model.inner, set)
MOI.supports_add_constrained_variable(
    model::ModelWithOracles,
    S::Type{<:MOI.AbstractScalarSet},
) =
    MOI.supports_add_constrained_variable(model.inner, S)
MOI.is_valid(model::ModelWithOracles, x::MOI.VariableIndex) =
    MOI.is_valid(model.inner, x)
MOI.get(model::ModelWithOracles, attr::MOI.AbstractVariableAttribute, x) =
    MOI.get(model.inner, attr, x)
MOI.set(model::ModelWithOracles, attr::MOI.AbstractVariableAttribute, x, v) =
    MOI.set(model.inner, attr, x, v)

const _OracleFunction = MOI.VectorOfVariables
const _OracleSet{T} = MOI.VectorNonlinearOracle{T}

MOI.supports_constraint(
    ::ModelWithOracles{T},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.VectorNonlinearOracle{T}},
) where {T} = true
MOI.supports_constraint(
    model::ModelWithOracles,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
) =
    MOI.supports_constraint(model.inner, F, S)

function MOI.add_constraint(
    model::ModelWithOracles{T},
    f::MOI.VectorOfVariables,
    s::MOI.VectorNonlinearOracle{T},
) where {T}
    length(f.variables) == s.input_dimension || throw(DimensionMismatch())
    push!(model.constraints, (f, s))
    push!(model.multiplier_start, nothing)
    return MOI.ConstraintIndex{typeof(f),typeof(s)}(length(model.constraints))
end

MOI.add_constraint(
    model::ModelWithOracles,
    f::MOI.AbstractFunction,
    s::MOI.AbstractSet,
) =
    MOI.add_constraint(model.inner, f, s)

function MOI.is_valid(
    model::ModelWithOracles{T},
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.VectorNonlinearOracle{T}},
) where {T}
    return 1 <= ci.value <= length(model.constraints)
end
MOI.is_valid(model::ModelWithOracles, ci::MOI.ConstraintIndex) =
    MOI.is_valid(model.inner, ci)

function MOI.get(
    model::ModelWithOracles{T},
    ::MOI.ListOfConstraintIndices{F,S},
) where {T,F<:MOI.VectorOfVariables,S<:MOI.VectorNonlinearOracle{T}}
    return MOI.ConstraintIndex{F,S}.(eachindex(model.constraints))
end
function MOI.get(
    model::ModelWithOracles{T},
    ::MOI.NumberOfConstraints{F,S},
) where {T,F<:MOI.VectorOfVariables,S<:MOI.VectorNonlinearOracle{T}}
    return length(model.constraints)
end
function MOI.get(
    model::ModelWithOracles{T},
    ::MOI.ConstraintFunction,
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.VectorNonlinearOracle{T}},
) where {T}
    return model.constraints[ci.value][1]
end
function MOI.get(
    model::ModelWithOracles{T},
    ::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.VectorNonlinearOracle{T}},
) where {T}
    return model.constraints[ci.value][2]
end
function MOI.supports(
    ::ModelWithOracles{T},
    ::MOI.LagrangeMultiplierStart,
    ::Type{MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.VectorNonlinearOracle{T}}},
) where {T}
    return true
end
function MOI.get(
    model::ModelWithOracles{T},
    ::MOI.LagrangeMultiplierStart,
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.VectorNonlinearOracle{T}},
) where {T}
    return model.multiplier_start[ci.value]
end
function MOI.set(
    model::ModelWithOracles{T},
    ::MOI.LagrangeMultiplierStart,
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.VectorNonlinearOracle{T}},
    value::Union{Nothing,Vector{T}},
) where {T}
    model.multiplier_start[ci.value] = value
    return
end

MOI.get(model::ModelWithOracles, attr::MOI.AbstractConstraintAttribute, ci) =
    MOI.get(model.inner, attr, ci)
MOI.get(model::ModelWithOracles, attr::MOI.AbstractConstraintAttribute) =
    MOI.get(model.inner, attr)
MOI.set(model::ModelWithOracles, attr::MOI.AbstractConstraintAttribute, ci, v) =
    MOI.set(model.inner, attr, ci, v)
MOI.delete(model::ModelWithOracles, ci::MOI.ConstraintIndex) =
    MOI.delete(model.inner, ci)

function MOI.get(model::ModelWithOracles, ::MOI.ListOfConstraintTypesPresent)
    types = MOI.get(model.inner, MOI.ListOfConstraintTypesPresent())
    if !isempty(model.constraints)
        pushfirst!(types, (MOI.VectorOfVariables, MOI.VectorNonlinearOracle{Float64}))
    end
    return types
end

function MOI.empty!(model::ModelWithOracles)
    empty!(model.constraints)
    empty!(model.multiplier_start)
    MOI.empty!(model.inner)
    return
end
MOI.is_empty(model::ModelWithOracles) =
    isempty(model.constraints) && MOI.is_empty(model.inner)

_variable_bounds(model::ModelWithOracles) = _variable_bounds(model.inner)

function constraint_rows(
    model::ModelWithOracles{T},
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.VectorNonlinearOracle{T}},
) where {T}
    offset = sum(
        model.constraints[i][2].output_dimension for i in 1:(ci.value-1);
        init = 0,
    )
    return offset .+ (1:model.constraints[ci.value][2].output_dimension)
end

function constraint_rows(model::ModelWithOracles, ci::MOI.ConstraintIndex)
    offset = sum(s.output_dimension for (_, s) in model.constraints; init = 0)
    return offset .+ constraint_rows(model.inner, ci)
end

function constraint_dual_starts(model::ModelWithOracles{T}) where {T}
    starts = Union{Nothing,T}[]
    for (start, (_, set)) in zip(model.multiplier_start, model.constraints)
        if start === nothing
            append!(starts, fill(nothing, set.output_dimension))
        else
            append!(starts, start)
        end
    end
    return vcat(starts, constraint_dual_starts(model.inner))
end

mutable struct EvaluatorWithOracles{T,M,E<:MOI.AbstractNLPEvaluator} <:
               MOI.AbstractNLPEvaluator
    model::ModelWithOracles{T,M}
    inner::E
    ordered_variables::Vector{MOI.VariableIndex}
    columns::Vector{Vector{Int}}
    x_buffer::Vector{Vector{T}}
end

function EvaluatorWithOracles(model::ModelWithOracles{T,M}, inner::E, vars) where {T,M,E}
    return EvaluatorWithOracles{T,M,E}(model, inner, vars, Vector{Int}[], Vector{T}[])
end

function Evaluator(model::ModelWithOracles, backend, vars::Vector{MOI.VariableIndex})
    return EvaluatorWithOracles(model, Evaluator(model.inner, backend, vars), vars)
end

_num_rows(d::EvaluatorWithOracles) =
    sum(s.output_dimension for (_, s) in d.model.constraints; init = 0)

function MOI.features_available(d::EvaluatorWithOracles)
    features = filter(
        f -> f in (:Grad, :Jac, :JacVec, :Hess, :HessVec),
        MOI.features_available(d.inner),
    )
    if !isempty(d.model.constraints)
        filter!(f -> !(f in (:JacVec, :HessVec)), features)
    end
    if any(s.eval_hessian_lagrangian === nothing for (_, s) in d.model.constraints)
        filter!(f -> f != :Hess, features)
    end
    return features
end

function MOI.initialize(d::EvaluatorWithOracles{T}, features) where {T}
    map = Dict(x => i for (i, x) in enumerate(d.ordered_variables))
    empty!(d.columns)
    empty!(d.x_buffer)
    for (f, s) in d.model.constraints
        push!(d.columns, [map[x] for x in f.variables])
        push!(d.x_buffer, zeros(T, s.input_dimension))
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
MOI.eval_objective_gradient(d::EvaluatorWithOracles, g, x) =
    MOI.eval_objective_gradient(d.inner, g, x)

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
    append!(J, ((row + offset, col) for (row, col) in MOI.jacobian_structure(d.inner)))
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

function _constraint_bounds(d::EvaluatorWithOracles)
    bounds = MOI.NLPBoundsPair[]
    for (_, s) in d.model.constraints
        append!(bounds, MOI.NLPBoundsPair.(s.l, s.u))
    end
    return append!(bounds, _constraint_bounds(d.inner))
end

_has_objective(d::EvaluatorWithOracles) = _has_objective(d.inner)
