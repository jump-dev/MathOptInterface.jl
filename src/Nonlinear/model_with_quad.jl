# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Where the objective of a `ModelWithQuad` currently lives.
@enum(_ObjectiveSink, _NONE, _QUAD, _INNER)

const _QPFunction{T} =
    Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}

const _QPSet{T} =
    Union{MOI.LessThan{T},MOI.GreaterThan{T},MOI.EqualTo{T},MOI.Interval{T}}

"""
    ModelWithQuad{T,M}(
        qp::QPBlockData{T},
        inner::M;
        objective_sink::_ObjectiveSink = _NONE,
    ) where {T,M}

A model layer that owns the variables of the model, stores affine and
quadratic objectives and constraints in a [`QPBlockData`](@ref), and forwards
everything else to the `inner` model, typically a [`Model`](@ref).

`ModelWithQuad(inner)` and `ModelWithQuad{T}(inner)` create an empty
[`QPBlockData`](@ref), with `T` defaulting to `Float64`.

Add variables with `MOI.add_variable`: the layer guarantees that the variable
indices are `1:n`, like `MOI.Utilities.MatrixOfConstraints`. Add parameters
with `MOI.add_constrained_variable(model, ::MOI.Parameter)`: parameters get
indices offset by `_PARAMETER_OFFSET`, and their values are stored in
the inner model through [`add_parameter`](@ref). The QP block aliases the
innermost nonlinear model's parameter storage, so a parameter update is
visible to both blocks.

Add constraints with [`add_constraint`](@ref) or `MOI.add_constraint`, and
set the objective with [`set_objective`](@ref): affine and quadratic
functions are routed to the QP block, everything else to the inner model.
`objective_sink` records where the objective currently lives (`_NONE`,
`_QUAD` or `_INNER`).

Create the corresponding evaluator, [`EvaluatorWithQuad`](@ref), with
`Evaluator(model, backend)`, or construct it directly from an inner
`MOI.AbstractNLPEvaluator`. The rows of the QP block come first, followed by
the rows of the inner evaluator.
"""
mutable struct ModelWithQuad{T,M} <: MOI.ModelLike
    variables::MOI.Utilities.VariablesContainer{T}
    # The variables and the parameters, in the order they were added, as
    # `MOI.ListOfVariableIndices` requires.
    list_of_variable_indices::Vector{MOI.VariableIndex}
    qp::QPBlockData{T}
    inner::M
    objective_sink::_ObjectiveSink

    function ModelWithQuad{T}(
        qp::QPBlockData{T},
        inner::M;
        objective_sink::_ObjectiveSink = _NONE,
    ) where {T,M}
        model = new{T,M}(
            MOI.Utilities.VariablesContainer{T}(),
            MOI.VariableIndex[],
            qp,
            inner,
            objective_sink,
        )
        # The QP block reads the parameter values from the storage of the
        # inner model, which must expose them as `parameters::Vector{T}`,
        # like [`Model`](@ref) does.
        model.qp.parameters = _parameter_values(inner)
        return model
    end
end

MOI.supports_incremental_interface(model::ModelWithQuad) =
    MOI.supports_incremental_interface(model.inner)

# Model attributes not owned by this layer, including `ObjectiveSense`, are
# deliberately forwarded through the MOI API.
MOI.supports(model::ModelWithQuad, attr::MOI.AbstractModelAttribute) =
    MOI.supports(model.inner, attr)
MOI.get(model::ModelWithQuad, attr::MOI.AbstractModelAttribute) =
    MOI.get(model.inner, attr)
MOI.set(model::ModelWithQuad, attr::MOI.AbstractModelAttribute, value) =
    MOI.set(model.inner, attr, value)
MOI.supports(
    model::ModelWithQuad,
    attr::MOI.AbstractConstraintAttribute,
    CI::Type{<:MOI.ConstraintIndex},
) = MOI.supports(model.inner, attr, CI)

function ModelWithQuad{T}(inner) where {T}
    return ModelWithQuad{T}(QPBlockData{T}(), inner)
end

ModelWithQuad(inner) = ModelWithQuad{Float64}(inner)

# The variables and the parameters.

function MOI.add_variable(model::ModelWithQuad)
    x = MOI.add_variable(model.variables)
    push!(model.list_of_variable_indices, x)
    return x
end

function MOI.supports_constraint(
    ::ModelWithQuad{T},
    ::Type{MOI.VariableIndex},
    ::Type{<:_QPSet{T}},
) where {T}
    return true
end

function MOI.supports(
    ::ModelWithQuad{T},
    ::MOI.ConstraintDualStart,
    ::Type{<:MOI.ConstraintIndex{F,S}},
) where {T,F<:_QPFunction{T},S<:_QPSet{T}}
    return true
end

function MOI.add_constraint(model::ModelWithQuad, x::MOI.VariableIndex, set::_QPSet)
    return MOI.add_constraint(model.variables, x, set)
end

function MOI.is_valid(
    model::ModelWithQuad,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,<:_QPSet},
)
    return MOI.is_valid(model.variables, ci)
end

function MOI.get(
    model::ModelWithQuad,
    attr::Union{
        MOI.NumberOfConstraints{MOI.VariableIndex,<:_QPSet},
        MOI.ListOfConstraintIndices{MOI.VariableIndex,<:_QPSet},
    },
)
    return MOI.get(model.variables, attr)
end

function MOI.get(
    model::ModelWithQuad,
    attr::Union{MOI.ConstraintFunction,MOI.ConstraintSet},
    ci::MOI.ConstraintIndex{MOI.VariableIndex,<:_QPSet},
)
    return MOI.get(model.variables, attr, ci)
end

function MOI.set(
    model::ModelWithQuad,
    attr::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
    set::S,
) where {S<:_QPSet}
    return MOI.set(model.variables, attr, ci, set)
end

function MOI.delete(
    model::ModelWithQuad,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,<:_QPSet},
)
    return MOI.delete(model.variables, ci)
end

function MOI.add_constrained_variable(
    model::ModelWithQuad{T},
    set::MOI.Parameter{T},
) where {T}
    p = add_parameter(model.inner, set.value)
    x = MOI.VariableIndex(_PARAMETER_OFFSET + p.value)
    push!(model.list_of_variable_indices, x)
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.Parameter{T}}(x.value)
    return x, ci
end

MOI.supports_add_constrained_variable(
    ::ModelWithQuad{T},
    ::Type{MOI.Parameter{T}},
) where {T} = true

function MOI.get(model::ModelWithQuad, ::MOI.NumberOfVariables)
    return length(model.list_of_variable_indices)
end

function MOI.get(model::ModelWithQuad, ::MOI.ListOfVariableIndices)
    return model.list_of_variable_indices
end

function MOI.is_valid(model::ModelWithQuad, x::MOI.VariableIndex)
    if _is_parameter(x)
        return 1 <= x.value - _PARAMETER_OFFSET <= length(model.qp.parameters)
    end
    return MOI.is_valid(model.variables, x)
end

function MOI.is_valid(
    model::ModelWithQuad{T},
    ci::MOI.ConstraintIndex{MOI.VariableIndex,MOI.Parameter{T}},
) where {T}
    return MOI.is_valid(model, MOI.VariableIndex(ci.value))
end

function MOI.get(
    model::ModelWithQuad{T},
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.Parameter{T}},
) where {T}
    return length(model.qp.parameters)
end

function MOI.supports_constraint(
    model::ModelWithQuad,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    return MOI.supports_constraint(model.inner, F, S)
end

function MOI.add_constraint(
    model::ModelWithQuad,
    func::MOI.AbstractFunction,
    set::MOI.AbstractSet,
)
    return MOI.add_constraint(model.inner, func, set)
end

function MOI.is_valid(model::ModelWithQuad, ci::MOI.ConstraintIndex)
    return MOI.is_valid(model.inner, ci)
end

function MOI.get(model::ModelWithQuad, attr::MOI.AbstractConstraintAttribute, ci)
    return MOI.get(model.inner, attr, ci)
end

function MOI.get(model::ModelWithQuad, attr::MOI.AbstractConstraintAttribute)
    return MOI.get(model.inner, attr)
end

function MOI.set(
    model::ModelWithQuad,
    attr::MOI.AbstractConstraintAttribute,
    ci,
    value,
)
    return MOI.set(model.inner, attr, ci, value)
end

MOI.delete(model::ModelWithQuad, ci::MOI.ConstraintIndex) =
    MOI.delete(model.inner, ci)

function MOI.get(
    model::ModelWithQuad{T},
    ::MOI.ListOfConstraintIndices{F,S},
) where {T,F<:MOI.VariableIndex,S<:MOI.Parameter{T}}
    n = length(model.qp.parameters)
    return MOI.ConstraintIndex{F,S}.(_PARAMETER_OFFSET .+ (1:n))
end

function MOI.get(
    model::ModelWithQuad{T},
    ::MOI.ConstraintFunction,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,MOI.Parameter{T}},
) where {T}
    return MOI.VariableIndex(ci.value)
end

function MOI.get(
    model::ModelWithQuad{T},
    ::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,MOI.Parameter{T}},
) where {T}
    return MOI.Parameter(model.qp.parameters[ci.value-_PARAMETER_OFFSET])
end

function MOI.set(
    model::ModelWithQuad{T},
    ::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,MOI.Parameter{T}},
    set::MOI.Parameter{T},
) where {T}
    model.qp.parameters[ci.value-_PARAMETER_OFFSET] = set.value
    return
end

"""
    Base.length(model::ModelWithQuad)

The number of affine and quadratic constraint rows of `model`, which come
before the rows of the inner model in the corresponding evaluator.
"""
Base.length(model::ModelWithQuad) = length(model.qp)

_variable_bounds(model::ModelWithQuad) =
    (model.variables.lower, model.variables.upper)
_has_nonlinear_data(model::ModelWithQuad) = _has_nonlinear_data(model.inner)
_is_nonlinear_input(
    ::ModelWithQuad{T},
    ::_QPFunction{T},
    ::_QPSet{T},
) where {T} = false
_is_nonlinear_input(model::ModelWithQuad, f, s) =
    _is_nonlinear_input(model.inner, f, s)
_is_nonlinear_objective(::ModelWithQuad{T}, ::_QPFunction{T}) where {T} = false
_is_nonlinear_objective(::ModelWithQuad, ::MOI.VariableIndex) = false
_is_nonlinear_objective(model::ModelWithQuad, f) =
    _is_nonlinear_objective(model.inner, f)

function constraint_rows(
    ::ModelWithQuad{T},
    ci::MOI.ConstraintIndex{F,S},
) where {T,F<:_QPFunction{T},S<:_QPSet{T}}
    return [ci.value]
end

function constraint_rows(model::ModelWithQuad, ci::MOI.ConstraintIndex)
    return length(model.qp) .+ constraint_rows(model.inner, ci)
end

function constraint_dual_starts(model::ModelWithQuad)
    return vcat(model.qp.mult_g, constraint_dual_starts(model.inner))
end

# Replace the parameters of `f`, encoded as `MOI.VariableIndex`es offset by
# [`_PARAMETER_OFFSET`](@ref), by the corresponding [`ParameterIndex`](@ref),
# which the inner model understands. An affine or quadratic function that
# contains a parameter is converted to `MOI.ScalarNonlinearFunction`, because
# the inner model parses such functions with their variable indices verbatim.
_replace_parameters(f) = f

function _replace_parameters(f::MOI.VariableIndex)
    if _is_parameter(f)
        return ParameterIndex(f.value - _PARAMETER_OFFSET)
    end
    return f
end

function _replace_parameters(f::MOI.ScalarAffineFunction)
    if any(_is_parameter, f.terms)
        return _replace_parameters(convert(MOI.ScalarNonlinearFunction, f))
    end
    return f
end

function _replace_parameters(f::MOI.ScalarQuadraticFunction)
    if any(_is_parameter, f.affine_terms) ||
       any(_is_parameter, f.quadratic_terms)
        return _replace_parameters(convert(MOI.ScalarNonlinearFunction, f))
    end
    return f
end

function _replace_parameters(f::MOI.ScalarNonlinearFunction)
    for (i, arg) in enumerate(f.args)
        f.args[i] = _replace_parameters(arg)
    end
    return f
end

# Methods forwarded to the inner model.

function add_parameter(model::ModelWithQuad, value::Real)
    return add_parameter(model.inner, value)
end

add_expression(model::ModelWithQuad, expr) = add_expression(model.inner, expr)

Base.getindex(model::ModelWithQuad, index::ExpressionIndex) = model.inner[index]

function register_operator(
    model::ModelWithQuad,
    op::Symbol,
    nargs::Int,
    f::Function...,
)
    return register_operator(model.inner, op, nargs, f...)
end

function MOI.is_valid(model::ModelWithQuad, index::ConstraintIndex)
    return MOI.is_valid(model.inner, index)
end

function MOI.get(
    model::ModelWithQuad,
    attr::MOI.ListOfSupportedNonlinearOperators,
)
    return MOI.get(model.inner, attr)
end

# The objective.

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
    model.objective_sink = _QUAD
    if MOI.get(model, MOI.ObjectiveSense()) == MOI.FEASIBILITY_SENSE
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    end
    return
end

function set_objective(model::ModelWithQuad{T}, obj) where {T}
    F = MOI.ScalarAffineFunction{T}
    MOI.set(model.qp, MOI.ObjectiveFunction{F}(), zero(F))
    if !isempty(model.qp.parameters)
        obj = _replace_parameters(obj)
    end
    if obj === nothing || !(obj isa MOI.AbstractFunction)
        set_objective(model.inner, nothing)
        obj === nothing || set_objective(model.inner, obj)
    else
        MOI.set(model.inner, MOI.ObjectiveFunction{typeof(obj)}(), obj)
    end
    model.objective_sink = obj === nothing ? _NONE : _INNER
    if obj !== nothing &&
       MOI.get(model, MOI.ObjectiveSense()) == MOI.FEASIBILITY_SENSE
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    end
    return
end

function MOI.supports(
    ::ModelWithQuad{T},
    ::MOI.ObjectiveFunction{F},
) where {T,F<:Union{MOI.VariableIndex,MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}}
    return true
end

function MOI.set(model::ModelWithQuad, ::MOI.ObjectiveFunction{F}, f::F) where {F}
    sense = MOI.get(model, MOI.ObjectiveSense())
    set_objective(model, f)
    MOI.set(model, MOI.ObjectiveSense(), sense)
    return
end

function MOI.get(model::ModelWithQuad, attr::MOI.ObjectiveFunctionType)
    if model.objective_sink == _QUAD
        return MOI.get(model.qp, attr)
    elseif model.objective_sink == _INNER
        return MOI.get(model.inner, attr)
    end
    return MOI.get(model.qp, attr)
end

function MOI.get(model::ModelWithQuad, attr::MOI.ObjectiveFunction{F}) where {F}
    if model.objective_sink == _QUAD
        return MOI.get(model.qp, attr)
    end
    return MOI.get(model.inner, attr)
end

# The affine and quadratic constraints. The MOI attribute methods are
# forwarded to the QP block, which implements them.

function MOI.supports_constraint(
    ::ModelWithQuad{T},
    ::Type{<:_QPFunction{T}},
    ::Type{<:_QPSet{T}},
) where {T}
    return true
end

function add_constraint(
    model::ModelWithQuad{T},
    func::_QPFunction{T},
    set::_QPSet{T},
) where {T}
    return MOI.add_constraint(model.qp, func, set)
end

function add_constraint(model::ModelWithQuad, func, set)
    if !isempty(model.qp.parameters)
        func = _replace_parameters(func)
    end
    return add_constraint(model.inner, func, set)
end

function MOI.add_constraint(
    model::ModelWithQuad{T},
    func::_QPFunction{T},
    set::_QPSet{T},
) where {T}
    return MOI.add_constraint(model.qp, func, set)
end

function MOI.get(model::ModelWithQuad, attr::MOI.ListOfConstraintTypesPresent)
    types = MOI.get(model.variables, attr)
    append!(types, MOI.get(model.qp, attr))
    append!(types, MOI.get(model.inner, attr))
    if !isempty(model.qp.parameters)
        push!(types, (MOI.VariableIndex, MOI.Parameter{eltype(model.qp.parameters)}))
    end
    return unique!(types)
end

function MOI.is_valid(
    model::ModelWithQuad{T},
    ci::MOI.ConstraintIndex{F,S},
) where {T,F<:_QPFunction{T},S<:_QPSet{T}}
    return MOI.is_valid(model.qp, ci)
end

function MOI.get(
    model::ModelWithQuad{T},
    attr::Union{MOI.ListOfConstraintIndices{F,S},MOI.NumberOfConstraints{F,S}},
) where {T,F<:_QPFunction{T},S<:_QPSet{T}}
    return MOI.get(model.qp, attr)
end

function MOI.get(
    model::ModelWithQuad{T},
    attr::Union{
        MOI.ConstraintFunction,
        MOI.ConstraintSet,
        MOI.ConstraintDualStart,
    },
    ci::MOI.ConstraintIndex{F,S},
) where {T,F<:_QPFunction{T},S<:_QPSet{T}}
    return MOI.get(model.qp, attr, ci)
end

function MOI.set(
    model::ModelWithQuad{T},
    attr::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{F,S},
    set::S,
) where {T,F<:_QPFunction{T},S<:_QPSet{T}}
    return MOI.set(model.qp, attr, ci, set)
end

function MOI.set(
    model::ModelWithQuad{T},
    attr::MOI.ConstraintDualStart,
    ci::MOI.ConstraintIndex{F,S},
    value,
) where {T,F<:_QPFunction{T},S<:_QPSet{T}}
    return MOI.set(model.qp, attr, ci, value)
end

"""
    EvaluatorWithQuad(
        model::ModelWithQuad,
        inner::MOI.AbstractNLPEvaluator,
    ) <: MOI.AbstractNLPEvaluator

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
    qp_σ = d.model.objective_sink == _QUAD ?
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
    qp_σ = d.model.objective_sink == _QUAD ?
           _objective_sign(MOI.get(d.model, MOI.ObjectiveSense())) * σ : σ
    _add_hessian_lagrangian_product(
        d.model.qp,
        H,
        x,
        v,
        qp_σ,
        view(μ, 1:m),
    )
    return
end

# The lower and upper bounds of each constraint row, in the row order of the
# evaluator. Solvers that use their own inner evaluator type can add a method
# for it so that `MOI.NLPBlockData(::EvaluatorWithQuad)` works.
function _constraint_bounds(evaluator::Evaluator)
    return MOI.NLPBoundsPair[
        _bound(c.set) for (_, c) in evaluator.model.constraints
    ]
end

function _constraint_bounds(d::EvaluatorWithQuad)
    bounds = MOI.NLPBoundsPair[
        MOI.NLPBoundsPair(l, u) for
        (l, u) in zip(d.model.qp.g_L, d.model.qp.g_U)
    ]
    return append!(bounds, _constraint_bounds(d.inner))
end

_has_objective(d::Evaluator) = d.model.objective !== nothing

function _has_objective(d::EvaluatorWithQuad)
    if d.model.objective_sink == _QUAD
        return true
    end
    return _has_objective(d.inner)
end

function MOI.NLPBlockData(d::EvaluatorWithQuad)
    return MOI.NLPBlockData(_constraint_bounds(d), d, _has_objective(d))
end
