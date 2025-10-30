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
    if MOI.get(model, MOI.ObjectiveSense()) != MOI.FEASIBILITY_SENSE
        return
    end
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = zero(MOI.ScalarAffineFunction{T})
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    return
end

function _add_penalty_to_objective(
    model::MOI.ModelLike,
    ::Type{F},
    penalty::MOI.ScalarAffineFunction{T},
) where {
    T,
    F<:Union{
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
    },
}
    f = MOI.get(model, MOI.ObjectiveFunction{F}())
    g = if MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
        MOI.Utilities.operate(+, T, f, penalty)
    else
        MOI.Utilities.operate(-, T, f, penalty)
    end
    MOI.set(model, MOI.ObjectiveFunction{typeof(g)}(), g)
    return
end

function _add_penalty_to_objective(
    ::MOI.ModelLike,
    ::Type{F},
    ::MOI.ScalarAffineFunction,
) where {F}
    return error(
        "Cannot perform `ScalarPenaltyRelaxation` with an objective function of type `$F`",
    )
end

function _relax_constraint(
    ::Type{T},
    model::MOI.ModelLike,
    ci::MOI.ConstraintIndex{F,<:MOI.AbstractScalarSet},
) where {T,F<:Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}}
    x = MOI.add_variables(model, 2)
    MOI.add_constraint.(model, x, MOI.GreaterThan(zero(T)))
    MOI.modify(model, ci, MOI.ScalarCoefficientChange(x[1], one(T)))
    MOI.modify(model, ci, MOI.ScalarCoefficientChange(x[2], -one(T)))
    return x
end

function _relax_constraint(
    ::Type{T},
    model::MOI.ModelLike,
    ci::MOI.ConstraintIndex{F,MOI.GreaterThan{T}},
) where {T,F<:Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}}
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.GreaterThan(zero(T)))
    MOI.modify(model, ci, MOI.ScalarCoefficientChange(x, one(T)))
    return [x]
end

function _relax_constraint(
    ::Type{T},
    model::MOI.ModelLike,
    ci::MOI.ConstraintIndex{F,MOI.LessThan{T}},
) where {T,F<:Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}}
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.GreaterThan(zero(T)))
    MOI.modify(model, ci, MOI.ScalarCoefficientChange(x, -one(T)))
    return [x]
end

function _relax_constraint(
    ::Type{T},
    model::MOI.ModelLike,
    ci::MOI.ConstraintIndex{MOI.ScalarNonlinearFunction,S},
) where {T,S<:MOI.AbstractScalarSet}
    x, _ = MOI.add_constrained_variable(model, MOI.GreaterThan(zero(T)))
    y, _ = MOI.add_constrained_variable(model, MOI.GreaterThan(zero(T)))
    f = MOI.get(model, MOI.ConstraintFunction(), ci)
    g = MOI.ScalarNonlinearFunction(
        :+,
        Any[f, x, MOI.ScalarNonlinearFunction(:-, Any[y])],
    )
    MOI.set(model, MOI.ConstraintFunction(), ci, g)
    return [x, y]
end

function _relax_constraint(
    ::Type{T},
    model::MOI.ModelLike,
    ci::MOI.ConstraintIndex{MOI.ScalarNonlinearFunction,MOI.GreaterThan{T}},
) where {T}
    x, _ = MOI.add_constrained_variable(model, MOI.GreaterThan(zero(T)))
    f = MOI.get(model, MOI.ConstraintFunction(), ci)
    g = MOI.ScalarNonlinearFunction(:+, [f, x])
    MOI.set(model, MOI.ConstraintFunction(), ci, g)
    return [x]
end

function _relax_constraint(
    ::Type{T},
    model::MOI.ModelLike,
    ci::MOI.ConstraintIndex{MOI.ScalarNonlinearFunction,MOI.LessThan{T}},
) where {T}
    x, _ = MOI.add_constrained_variable(model, MOI.GreaterThan(zero(T)))
    f = MOI.get(model, MOI.ConstraintFunction(), ci)
    g = MOI.ScalarNonlinearFunction(:-, [f, x])
    MOI.set(model, MOI.ConstraintFunction(), ci, g)
    return [x]
end

function MOI.modify(
    model::MOI.ModelLike,
    ci::MOI.ConstraintIndex{F,<:MOI.AbstractScalarSet},
    relax::ScalarPenaltyRelaxation{T},
) where {
    T,
    F<:Union{
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
    },
}
    x = _relax_constraint(T, model, ci)
    p = MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.(relax.penalty, x),
        zero(T),
    )
    _change_sense_to_min_if_necessary(T, model)
    O = MOI.get(model, MOI.ObjectiveFunctionType())
    _add_penalty_to_objective(model, O, p)
    return MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(one(T), x), zero(T))
end

"""
    PenaltyRelaxation(
        penalties = Dict{MOI.ConstraintIndex,Float64}();
        default::Union{Nothing,T} = 1.0,
        warn::Bool = true,
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

If a constraint cannot be modified, a warning is logged and the
constraint is skipped. The warning can be disabled by setting `warn = false`.

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
    warn::Bool

    function PenaltyRelaxation(
        p::Dict{MOI.ConstraintIndex,T};
        default::Union{Nothing,T} = one(T),
        warn::Bool = true,
    ) where {T}
        return new{T}(default, p, warn)
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
    penalty_expr = zero(MOI.ScalarAffineFunction{T})
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        _modify_penalty_relaxation(penalty_expr, map, model, relax, F, S)
    end
    if !isempty(penalty_expr.terms)
        _change_sense_to_min_if_necessary(T, model)
        O = MOI.get(model, MOI.ObjectiveFunctionType())
        _add_penalty_to_objective(model, O, penalty_expr)
    end
    return map
end

function _modify_penalty_relaxation(
    penalty_expr::MOI.ScalarAffineFunction{T},
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
            x = _relax_constraint(T, model, ci)
            map[ci] = MOI.ScalarAffineFunction(
                MOI.ScalarAffineTerm.(one(T), x),
                zero(T),
            )
            append!(penalty_expr.terms, MOI.ScalarAffineTerm{T}.(penalty, x))
        catch err
            if err isa MethodError && err.f == _relax_constraint
                if relax.warn
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
