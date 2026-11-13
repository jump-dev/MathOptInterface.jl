# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    ModelWithOracles{T}(inner::M) where {T,M}

!!! warning
    This API is experimental. Until this message is removed, breaking changes
    may be introduced in any minor or patch release of MathOptInterface.

A nonlinear model that wraps `inner` and supports
[`MOI.VectorNonlinearOracle`](@ref).
"""
mutable struct ModelWithOracles{T,M} <: MOI.ModelLike
    # The inner model.
    inner::M

    # The function-in-set pairs of the VectorNonlinearOracle constraints.
    constraints::Vector{
        Tuple{MOI.VectorOfVariables,MOI.VectorNonlinearOracle{T}},
    }

    # The multiplier start associated the VectorNonlinearOracle constraints.
    multiplier_start::Vector{Union{Nothing,Vector{T}}}

    function ModelWithOracles{T}(inner::M) where {T,M}
        return new{T,M}(
            inner,
            Tuple{MOI.VectorOfVariables,MOI.VectorNonlinearOracle{T}}[],
            Union{Nothing,Vector{T}}[],
        )
    end
end

ModelWithOracles(inner) = ModelWithOracles{Float64}(inner)

_parameter_values(model::ModelWithOracles) = _parameter_values(model.inner)

# Backward-compatible forwarding for users of the pre-MOI Nonlinear API.
function add_parameter(model::ModelWithOracles, value::Real)
    return add_parameter(model.inner, value)
end

function add_expression(model::ModelWithOracles, expression)
    return add_expression(model.inner, expression)
end

function set_objective(model::ModelWithOracles, objective)
    return set_objective(model.inner, objective)
end

function add_constraint(model::ModelWithOracles, f, s)
    return add_constraint(model.inner, f, s)
end

function Base.getindex(model::ModelWithOracles, index::ExpressionIndex)
    return model.inner[index]
end

function register_operator(model::ModelWithOracles, op, nargs, functions...)
    return register_operator(model.inner, op, nargs, functions...)
end

function MOI.supports_incremental_interface(model::ModelWithOracles)
    return MOI.supports_incremental_interface(model.inner)
end

function MOI.supports(model::ModelWithOracles, attr::MOI.AbstractModelAttribute)
    return MOI.supports(model.inner, attr)
end

function MOI.get(model::ModelWithOracles, attr::MOI.AbstractModelAttribute)
    return MOI.get(model.inner, attr)
end

function MOI.set(
    model::ModelWithOracles,
    attr::MOI.AbstractModelAttribute,
    value,
)
    return MOI.set(model.inner, attr, value)
end

function MOI.supports(
    model::ModelWithOracles,
    attr::MOI.AbstractConstraintAttribute,
    CI::Type{<:MOI.ConstraintIndex},
)
    return MOI.supports(model.inner, attr, CI)
end

function MOI.get(
    model::ModelWithOracles,
    attr::MOI.ListOfSupportedNonlinearOperators,
)
    return MOI.get(model.inner, attr)
end

MOI.add_variable(model::ModelWithOracles) = MOI.add_variable(model.inner)

function MOI.add_constrained_variable(
    model::ModelWithOracles,
    set::MOI.AbstractScalarSet,
)
    return MOI.add_constrained_variable(model.inner, set)
end

function MOI.supports_add_constrained_variable(
    model::ModelWithOracles,
    S::Type{<:MOI.AbstractScalarSet},
)
    return MOI.supports_add_constrained_variable(model.inner, S)
end

function MOI.is_valid(model::ModelWithOracles, x::MOI.VariableIndex)
    return MOI.is_valid(model.inner, x)
end

function MOI.get(
    model::ModelWithOracles,
    attr::MOI.AbstractVariableAttribute,
    x::MOI.VariableIndex,
)
    return MOI.get(model.inner, attr, x)
end

function MOI.set(
    model::ModelWithOracles,
    attr::MOI.AbstractVariableAttribute,
    x::MOI.VariableIndex,
    v,
)
    return MOI.set(model.inner, attr, x, v)
end

function MOI.supports_constraint(
    ::ModelWithOracles{T},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.VectorNonlinearOracle{T}},
) where {T}
    return true
end

function MOI.supports_constraint(
    model::ModelWithOracles,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    return MOI.supports_constraint(model.inner, F, S)
end

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

function MOI.add_constraint(
    model::ModelWithOracles,
    f::MOI.AbstractFunction,
    s::MOI.AbstractSet,
)
    return MOI.add_constraint(model.inner, f, s)
end

function MOI.is_valid(
    model::ModelWithOracles{T},
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.VectorNonlinearOracle{T}},
) where {T}
    return 1 <= ci.value <= length(model.constraints)
end

function MOI.is_valid(model::ModelWithOracles, ci::MOI.ConstraintIndex)
    return MOI.is_valid(model.inner, ci)
end

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
    ::Type{
        MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.VectorNonlinearOracle{T}},
    },
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

function MOI.get(
    model::ModelWithOracles,
    attr::MOI.AbstractConstraintAttribute,
    ci::MOI.ConstraintIndex,
)
    return MOI.get(model.inner, attr, ci)
end

function MOI.set(
    model::ModelWithOracles,
    attr::MOI.AbstractConstraintAttribute,
    ci::MOI.ConstraintIndex,
    v,
)
    return MOI.set(model.inner, attr, ci, v)
end

function MOI.get(model::ModelWithOracles, ::MOI.ListOfConstraintTypesPresent)
    types = MOI.get(model.inner, MOI.ListOfConstraintTypesPresent())
    if !isempty(model.constraints)
        pushfirst!(
            types,
            (MOI.VectorOfVariables, MOI.VectorNonlinearOracle{Float64}),
        )
    end
    return types
end

function MOI.empty!(model::ModelWithOracles)
    empty!(model.constraints)
    empty!(model.multiplier_start)
    MOI.empty!(model.inner)
    return
end

function MOI.is_empty(model::ModelWithOracles)
    return isempty(model.constraints) && MOI.is_empty(model.inner)
end

function MOI.Utilities.variable_bounds(model::ModelWithOracles)
    return MOI.Utilities.variable_bounds(model.inner)
end

function MOI.Utilities.rows(
    model::ModelWithOracles{T},
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.VectorNonlinearOracle{T}},
) where {T}
    offset = sum(
        model.constraints[i][2].output_dimension for i in 1:(ci.value-1);
        init = 0,
    )
    return offset .+ (1:model.constraints[ci.value][2].output_dimension)
end

function MOI.Utilities.rows(model::ModelWithOracles, ci::MOI.ConstraintIndex)
    offset = sum(s.output_dimension for (_, s) in model.constraints; init = 0)
    return offset .+ MOI.Utilities.rows(model.inner, ci)
end

function MOI.Utilities.constraint_bounds(model::ModelWithOracles{T}) where {T}
    lower = T[]
    upper = T[]
    for (_, set) in model.constraints
        append!(lower, set.l)
        append!(upper, set.u)
    end
    inner = MOI.Utilities.constraint_bounds(model.inner)
    append!(lower, inner.lower)
    append!(upper, inner.upper)
    return MOI.Utilities.Hyperrectangle(lower, upper)
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
