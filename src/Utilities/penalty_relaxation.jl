# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    ScalarPenaltyRelaxation(penalty::T) where {T}

A problem modifier that, when passed to [`MOI.modify`](@ref), destructively
modifies the constraint in-place to create a penalized relaxation of the
constraint.

!!! warning
    This is a destructive routine that modifies the constraint in-place. If you
    don't want to modify the original model, use `JuMP.copy_model` to create a
    copy before calling [`MOI.modify`](@ref).

## Reformulation

The penalty relaxation modifies constraints of the form ``f(x) \\in S`` into
``f(x) + y - z \\in S``, where ``y, z \\ge 0``, and then it introduces a penalty
term into the objective of ``a \\times (y + z)`` (if minimizing, else ``-a``),
where ``a`` is `penalty`

When `S` is [`MOI.LessThan`](@ref) or [`MOI.GreaterThan`](@ref), we omit `y` or
`z` respectively as a performance optimization.

## Return value

`MOI.modify(model, ci, ScalarPenaltyRelaxation(penalty))` returns `y + z` as a
[`MOI.ScalarAffineFunction`](@ref). In an optimal solution, query the value of
this function to compute the violation of the constraint.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model);

julia> c = MOI.add_constraint(model, 1.0 * x, MOI.LessThan(2.0));

julia> f = MOI.modify(model, c, MOI.Utilities.ScalarPenaltyRelaxation(2.0));

julia> print(model)
Minimize ScalarAffineFunction{Float64}:
 0.0 + 2.0 v[2]

Subject to:

ScalarAffineFunction{Float64}-in-LessThan{Float64}
 0.0 + 1.0 v[1] - 1.0 v[2] <= 2.0

VariableIndex-in-GreaterThan{Float64}
 v[2] >= 0.0

julia> f isa MOI.ScalarAffineFunction{Float64}
true
```
"""
struct ScalarPenaltyRelaxation{T} # <: MOI.AbstractFunctionModification
    # We don't make this a subtype of AbstractFunctionModification to avoid some
    # ambiguities with generic methods in Utilities and Bridges. The underlying
    # reason is that these reformulations can be written using the high-level
    # MOI API, so we don't need special handling for bridges and utilities.
    penalty::T
end

function _change_sense_to_min_if_necessary(
    ::Type{T},
    model::MOI.ModelLike,
) where {T}
    sense = MOI.get(model, MOI.ObjectiveSense())
    if sense != MOI.FEASIBILITY_SENSE
        return sense
    end
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = zero(MOI.ScalarAffineFunction{T})
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    return MOI.MIN_SENSE
end

function MOI.modify(
    model::MOI.ModelLike,
    ci::MOI.ConstraintIndex{F,<:MOI.AbstractScalarSet},
    relax::ScalarPenaltyRelaxation{T},
) where {T,F<:Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}}
    sense = _change_sense_to_min_if_necessary(T, model)
    y = MOI.add_variable(model)
    z = MOI.add_variable(model)
    MOI.add_constraint(model, y, MOI.GreaterThan(zero(T)))
    MOI.add_constraint(model, z, MOI.GreaterThan(zero(T)))
    MOI.modify(model, ci, MOI.ScalarCoefficientChange(y, one(T)))
    MOI.modify(model, ci, MOI.ScalarCoefficientChange(z, -one(T)))
    scale = sense == MOI.MIN_SENSE ? one(T) : -one(T)
    a = scale * relax.penalty
    O = MOI.get(model, MOI.ObjectiveFunctionType())
    obj = MOI.ObjectiveFunction{O}()
    MOI.modify(model, obj, MOI.ScalarCoefficientChange(y, a))
    MOI.modify(model, obj, MOI.ScalarCoefficientChange(z, a))
    return one(T) * y + one(T) * z
end

function MOI.modify(
    model::MOI.ModelLike,
    ci::MOI.ConstraintIndex{F,MOI.GreaterThan{T}},
    relax::ScalarPenaltyRelaxation{T},
) where {T,F<:Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}}
    sense = _change_sense_to_min_if_necessary(T, model)
    # Performance optimization: we don't need the z relaxation variable.
    y = MOI.add_variable(model)
    MOI.add_constraint(model, y, MOI.GreaterThan(zero(T)))
    MOI.modify(model, ci, MOI.ScalarCoefficientChange(y, one(T)))
    scale = sense == MOI.MIN_SENSE ? one(T) : -one(T)
    a = scale * relax.penalty
    O = MOI.get(model, MOI.ObjectiveFunctionType())
    obj = MOI.ObjectiveFunction{O}()
    MOI.modify(model, obj, MOI.ScalarCoefficientChange(y, a))
    return one(T) * y
end

function MOI.modify(
    model::MOI.ModelLike,
    ci::MOI.ConstraintIndex{F,MOI.LessThan{T}},
    relax::ScalarPenaltyRelaxation{T},
) where {T,F<:Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}}
    sense = _change_sense_to_min_if_necessary(T, model)
    # Performance optimization: we don't need the y relaxation variable.
    z = MOI.add_variable(model)
    MOI.add_constraint(model, z, MOI.GreaterThan(zero(T)))
    MOI.modify(model, ci, MOI.ScalarCoefficientChange(z, -one(T)))
    scale = sense == MOI.MIN_SENSE ? one(T) : -one(T)
    a = scale * relax.penalty
    O = MOI.get(model, MOI.ObjectiveFunctionType())
    obj = MOI.ObjectiveFunction{O}()
    MOI.modify(model, obj, MOI.ScalarCoefficientChange(z, a))
    return one(T) * z
end

"""
    PenaltyRelaxation(
        penalties = Dict{MOI.ConstraintIndex,Float64}();
        default::Union{Nothing,T} = 1.0,
        no_warning_skip_constraint::Bool = false,
    )

A problem modifier that, when passed to [`MOI.modify`](@ref), destructively
modifies the model in-place to create a penalized relaxation of the constraints.

!!! warning
    This is a destructive routine that modifies the model in-place. If you don't
    want to modify the original model, use `JuMP.copy_model` to create a copy
    before calling [`MOI.modify`](@ref).

## Reformulation

See [`Utilities.ScalarPenaltyRelaxation`](@ref) for details of the
reformulation.

For each constraint `ci`, the penalty passed to [`Utilities.ScalarPenaltyRelaxation`](@ref)
is `get(penalties, ci, default)`. If the value is `nothing`, because `ci` does
not exist in `penalties` and `default = nothing`, then the constraint is
skipped.

## Return value

`MOI.modify(model, PenaltyRelaxation())` returns a
`Dict{MOI.ConstraintIndex,MOI.ScalarAffineFunction}` that maps each constraint
index to the corresponding `y + z` as a [`MOI.ScalarAffineFunction`](@ref). In
an optimal solution, query the value of these functions to compute the violation
of each constraint.

## Relax a subset of constraints

To relax a subset of constraints, pass a `penalties` dictionary and set
`default = nothing`.

## Supported constraint types

The penalty relaxation is currently limited to modifying
[`MOI.ScalarAffineFunction`](@ref) and [`MOI.ScalarQuadraticFunction`](@ref)
constraints in the linear sets [`MOI.LessThan`](@ref), [`MOI.GreaterThan`](@ref),
[`MOI.EqualTo`](@ref) and [`MOI.Interval`](@ref).

It does not include variable bound or integrality constraints, because these
cannot be modified in-place.

To modify variable bounds, rewrite them as linear constraints.

If a constraint type can not be modified, a warning is logged and the
constraint is skipped. This can be disabled by setting
`no_warning_skip_constraint = true`.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model);

julia> c = MOI.add_constraint(model, 1.0 * x, MOI.LessThan(2.0));

julia> map = MOI.modify(model, MOI.Utilities.PenaltyRelaxation(default = 2.0));

julia> print(model)
Minimize ScalarAffineFunction{Float64}:
 0.0 + 2.0 v[2]

Subject to:

ScalarAffineFunction{Float64}-in-LessThan{Float64}
 0.0 + 1.0 v[1] - 1.0 v[2] <= 2.0

VariableIndex-in-GreaterThan{Float64}
 v[2] >= 0.0

julia> map[c] isa MOI.ScalarAffineFunction{Float64}
true
```

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model);

julia> c = MOI.add_constraint(model, 1.0 * x, MOI.LessThan(2.0));

julia> map = MOI.modify(model, MOI.Utilities.PenaltyRelaxation(Dict(c => 3.0)));

julia> print(model)
Minimize ScalarAffineFunction{Float64}:
 0.0 + 3.0 v[2]

Subject to:

ScalarAffineFunction{Float64}-in-LessThan{Float64}
 0.0 + 1.0 v[1] - 1.0 v[2] <= 2.0

VariableIndex-in-GreaterThan{Float64}
 v[2] >= 0.0

julia> map[c] isa MOI.ScalarAffineFunction{Float64}
true
```
"""
mutable struct PenaltyRelaxation{T}
    default::Union{Nothing,T}
    penalties::Dict{MOI.ConstraintIndex,T}
    no_warning_skip_constraint::Bool

    function PenaltyRelaxation(
        p::Dict{MOI.ConstraintIndex,T};
        default::Union{Nothing,T} = one(T),
        no_warning_skip_constraint::Bool = false,
    ) where {T}
        return new{T}(default, p, no_warning_skip_constraint)
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
    map = Dict{MOI.ConstraintIndex,MOI.ScalarAffineFunction{T}}()
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        _modify_penalty_relaxation(map, model, relax, F, S)
    end
    return map
end

function _modify_penalty_relaxation(
    map::Dict{MOI.ConstraintIndex,MOI.ScalarAffineFunction{T}},
    model::MOI.ModelLike,
    relax::PenaltyRelaxation,
    ::Type{F},
    ::Type{S},
) where {T,F,S}
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        penalty = get(relax.penalties, ci, relax.default)
        if penalty === nothing
            continue
        end
        try
            map[ci] = MOI.modify(model, ci, ScalarPenaltyRelaxation(penalty))
        catch err
            if err isa MethodError && err.f == MOI.modify
                if !relax.no_warning_skip_constraint
                    @warn(
                        "Skipping PenaltyRelaxation for ConstraintIndex{$F,$S}"
                    )
                end
                return
            end
            rethrow(err)
        end
    end
    return
end
