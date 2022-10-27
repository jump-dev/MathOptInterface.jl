# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    PenaltyRelaxation(
        penalties = Dict{MOI.ConstraintIndex,Float64}();
        default::Union{Nothing,T} = 1.0,
    )

A problem modifier that, when passed to [`MOI.modify`](@ref), destructively
modifies the model in-place to create a penalized relaxation of the constraints.

!!! warning
    This is a destructive routine that modifies the model in-place. If you don't
    want to modify the original model, use `JuMP.copy_model` to create a copy
    before setting this attribute.

## Reformulation

The penalty relaxation modifies constraints of the form ``f(x) \\in S`` into
``f(x) + y - z \\in S``, where ``y, z \\ge 0``, and then it introduces a penalty
term into the objective of ``a \\times (y + z)`` (if minimizing, else ``-a``),
where `a` is the value in the `penalties` dictionary associated with the
constraint that is being relaxed. If no value exists, the default is `default`.

When `S` is [`MOI.LessThan`](@ref) or [`MOI.GreaterThan`](@ref), we omit `y` or
`z` respectively as a performance optimization.

## Relax a subset of constraints

To relax a subset of constraints, pass a `penalties` dictionary` and set
`default = nothing`.

## Supported constraint types

The penalty relaxation is currently limited to modifying
[`MOI.ScalarAffineFunction`](@ref) and [`MOI.ScalarQuadraticFunction`](@ref)
constraints in the linear sets [`MOI.LessThan`](@ref), [`MOI.GreaterThan`](@ref),
[`MOI.EqualTo`](@ref) and [`MOI.Interval`](@ref).

It does not include variable bound or integrality constraints, because these
cannot be modified in-place.

To modify variable bounds, rewrite them as linear constraints.

## Examples

```jldoctest; setup=:(import MathOptInterface; const MOI = MathOptInterface)
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model);

julia> c = MOI.add_constraint(model, 1.0 * x, MOI.LessThan(2.0));

julia> MOI.modify(model, MOI.Utilities.PenaltyRelaxation(default = 2.0))

julia> print(model)
Minimize ScalarAffineFunction{Float64}:
 0.0 + 2.0 v[2]

Subject to:

ScalarAffineFunction{Float64}-in-LessThan{Float64}
 0.0 + 1.0 v[1] - 1.0 v[2] <= 2.0

VariableIndex-in-GreaterThan{Float64}
 v[2] >= 0.0
```

```jldoctest; setup=:(import MathOptInterface; const MOI = MathOptInterface)
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model);

julia> c = MOI.add_constraint(model, 1.0 * x, MOI.LessThan(2.0));

julia> MOI.modify(model, MOI.Utilities.PenaltyRelaxation(Dict(c => 3.0)))

julia> print(model)
Minimize ScalarAffineFunction{Float64}:
 0.0 + 3.0 v[2]

Subject to:

ScalarAffineFunction{Float64}-in-LessThan{Float64}
 0.0 + 1.0 v[1] - 1.0 v[2] <= 2.0

VariableIndex-in-GreaterThan{Float64}
 v[2] >= 0.0
```
"""
mutable struct PenaltyRelaxation{T}
    default::Union{Nothing,T}
    penalties::Dict{MOI.ConstraintIndex,T}

    function PenaltyRelaxation(
        p::Dict{MOI.ConstraintIndex,T};
        default::T = one(T),
    ) where {T}
        return new{T}(default, p)
    end
end

function PenaltyRelaxation(; kwargs...)
    return PenaltyRelaxation(Dict{MOI.ConstraintIndex,Float64}(); kwargs...)
end

function PenaltyRelaxation(
    d::Dict{<:MOI.ConstraintIndex,T};
    kwargs...,
) where {T}
    return PenaltyRelaxation(convert(Dict{MOI.ConstraintIndex,T}, d); kwargs...)
end

function MOI.modify(model::MOI.ModelLike, relax::PenaltyRelaxation{T}) where {T}
    sense = MOI.get(model, MOI.ObjectiveSense())
    if sense == MOI.FEASIBILITY_SENSE
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
        f = zero(MOI.ScalarAffineFunction{T})
        MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    end
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        _modify_penalty_relaxation(model, relax, F, S)
    end
    return
end

function _modify_penalty_relaxation(
    model::MOI.ModelLike,
    relax::PenaltyRelaxation,
    ::Type{F},
    ::Type{S},
) where {F,S}
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        if relax.default !== nothing || haskey(relax.penalties, ci)
            MOI.modify(model, ci, relax)
        end
    end
    return
end

function MOI.modify(
    ::MOI.ModelLike,
    ci::MOI.ConstraintIndex,
    ::PenaltyRelaxation,
)
    # We use this fallback to avoid ambiguity errors that would occur if we
    # added {F,S} directly to the argument.
    _eltype(::MOI.ConstraintIndex{F,S}) where {F,S} = F, S
    F, S = _eltype(ci)
    @warn(
        "Skipping PenaltyRelaxation of constraints of type $F-in-$S",
        maxlog = 1,
    )
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    ci::MOI.ConstraintIndex{F,<:MOI.AbstractScalarSet},
    relax::PenaltyRelaxation{T},
) where {T,F<:Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}}
    y = MOI.add_variable(model)
    z = MOI.add_variable(model)
    MOI.add_constraint(model, y, MOI.GreaterThan(zero(T)))
    MOI.add_constraint(model, z, MOI.GreaterThan(zero(T)))
    MOI.modify(model, ci, MOI.ScalarCoefficientChange(y, one(T)))
    MOI.modify(model, ci, MOI.ScalarCoefficientChange(z, -one(T)))
    sense = MOI.get(model, MOI.ObjectiveSense())
    scale = sense == MOI.MIN_SENSE ? one(T) : -one(T)
    a = scale * get(relax.penalties, ci, relax.default)
    O = MOI.get(model, MOI.ObjectiveFunctionType())
    obj = MOI.ObjectiveFunction{O}()
    MOI.modify(model, obj, MOI.ScalarCoefficientChange(y, a))
    MOI.modify(model, obj, MOI.ScalarCoefficientChange(z, a))
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    ci::MOI.ConstraintIndex{F,MOI.GreaterThan{T}},
    relax::PenaltyRelaxation{T},
) where {T,F<:Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}}
    # Performance optimization: we don't need the z relaxation variable.
    y = MOI.add_variable(model)
    MOI.add_constraint(model, y, MOI.GreaterThan(zero(T)))
    MOI.modify(model, ci, MOI.ScalarCoefficientChange(y, one(T)))
    sense = MOI.get(model, MOI.ObjectiveSense())
    scale = sense == MOI.MIN_SENSE ? one(T) : -one(T)
    a = scale * get(relax.penalties, ci, relax.default)
    O = MOI.get(model, MOI.ObjectiveFunctionType())
    obj = MOI.ObjectiveFunction{O}()
    MOI.modify(model, obj, MOI.ScalarCoefficientChange(y, a))
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    ci::MOI.ConstraintIndex{F,MOI.LessThan{T}},
    relax::PenaltyRelaxation{T},
) where {T,F<:Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}}
    # Performance optimization: we don't need the y relaxation variable.
    z = MOI.add_variable(model)
    MOI.add_constraint(model, z, MOI.GreaterThan(zero(T)))
    MOI.modify(model, ci, MOI.ScalarCoefficientChange(z, -one(T)))
    sense = MOI.get(model, MOI.ObjectiveSense())
    scale = sense == MOI.MIN_SENSE ? one(T) : -one(T)
    a = scale * get(relax.penalties, ci, relax.default)
    O = MOI.get(model, MOI.ObjectiveFunctionType())
    obj = MOI.ObjectiveFunction{O}()
    MOI.modify(model, obj, MOI.ScalarCoefficientChange(z, a))
    return
end
