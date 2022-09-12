# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    FeasibilityRelaxation(
        penalties = Dict{MOI.ConstraintIndex,Float64}(),
    ) <: MOI.AbstractModelAttribute

A model attribute that, when set, destructively modifies the model in-place to
create a feasibility relaxation.

!!! warning
    This is a destructive routine that modifies the model in-place. If you don't
    want to modify the original model, use `copy_model` to create a copy before
    setting this attribute.

## Reformulation

The feasibility relaxation modifies constraints of the form ``f(x) \\in S`` into
``f(x) + y - z \\in S``, where ``y, z \\ge 0``, and then it introduces a penalty
term into the objective of ``a \\times (y + z)`` (if minimizing, else ``-a``),
where `a` is the value in the `penalties` dictionary associated with the
constraint that is being relaxed. If no value exists, the default is `1.0`.

The feasibility relaxation is limited to modifying constraint types for which
`MOI.supports(model, ::FeasibilityRelaxation, MOI.ConstraintIndex{F,S})` is
`true`. By default, this is only true for [`MOI.ScalarAffineFunction`](@ref) and
[`MOI.MOI.ScalarQuadraticFunction`](@ref) constraints in the linear sets
[`MOI.LessThan`](@ref), [`MOI.GreaterThan`](@ref), [`MOI.EqualTo`](@ref) and
[`MOI.Interval`](@ref). It does not include variable bound or integrality
constraints, because these cannot be modified in-place.

## Example

```jldoctest; setup=:(import MathOptInterface; const MOI = MathOptInterface)
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model);

julia> c = MOI.add_constraint(model, 1.0 * x, MOI.LessThan(2.0));

julia> MOI.set(model, MOI.Utilities.FeasibilityRelaxation(Dict(c => 2.0)))

julia> print(model)
Minimize ScalarAffineFunction{Float64}:
 0.0 + 2.0 v[2]

Subject to:

ScalarAffineFunction{Float64}-in-LessThan{Float64}
 0.0 + 1.0 v[1] - 1.0 v[2] <= 2.0

VariableIndex-in-GreaterThan{Float64}
 v[2] >= 0.0
```
"""
mutable struct FeasibilityRelaxation{T} <: MOI.AbstractModelAttribute
    penalties::Dict{MOI.ConstraintIndex,T}
    scale::T
    function FeasibilityRelaxation(p::Dict{MOI.ConstraintIndex,T}) where {T}
        return new{T}(p, zero(T))
    end
end

function FeasibilityRelaxation()
    return FeasibilityRelaxation(Dict{MOI.ConstraintIndex,Float64}())
end

function FeasibilityRelaxation(d::Dict{<:MOI.ConstraintIndex,T}) where {T}
    return FeasibilityRelaxation(convert(Dict{MOI.ConstraintIndex,T}, d))
end

function MOI.set(
    model::MOI.ModelLike,
    relax::FeasibilityRelaxation{T},
) where {T}
    sense = MOI.get(model, MOI.ObjectiveSense())
    if sense == MOI.FEASIBILITY_SENSE
        relax.scale = one(T)
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
        f = zero(MOI.ScalarAffineFunction{T})
        MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    elseif sense == MOI.MIN_SENSE
        relax.scale = one(T)
    elseif sense == MOI.MAX_SENSE
        relax.scale = -one(T)
    end
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        MOI.set(model, relax, F, S)
    end
    return
end

function MOI.set(
    model::MOI.ModelLike,
    relax::FeasibilityRelaxation,
    ::Type{F},
    ::Type{S},
) where {F,S}
    if MOI.supports(model, relax, MOI.ConstraintIndex{F,S})
        for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            MOI.set(model, relax, ci)
        end
    end
    return
end

function MOI.supports(
    ::MOI.ModelLike,
    ::FeasibilityRelaxation,
    ::Type{MOI.ConstraintIndex{F,<:MOI.AbstractScalarSet}},
) where {T,F<:Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}}
    return true
end

function MOI.supports_fallback(
    ::MOI.ModelLike,
    ::FeasibilityRelaxation,
    ::Type{MOI.ConstraintIndex{F,S}},
) where {F,S}
    return false
end

function MOI.set(
    model::MOI.ModelLike,
    relax::FeasibilityRelaxation,
    ci::MOI.ConstraintIndex{F,<:MOI.AbstractScalarSet},
) where {T,F<:Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}}
    y = MOI.add_variable(model)
    z = MOI.add_variable(model)
    MOI.add_constraint(model, y, MOI.GreaterThan(zero(T)))
    MOI.add_constraint(model, z, MOI.GreaterThan(zero(T)))
    MOI.modify(model, ci, MOI.ScalarCoefficientChange(y, one(T)))
    MOI.modify(model, ci, MOI.ScalarCoefficientChange(z, -one(T)))
    a = relax.scale * get(relax.penalties, ci, one(T))
    O = MOI.get(model, MOI.ObjectiveFunctionType())
    obj = MOI.ObjectiveFunction{O}()
    MOI.modify(model, obj, MOI.ScalarCoefficientChange(y, a))
    MOI.modify(model, obj, MOI.ScalarCoefficientChange(z, a))
    return
end

function MOI.set(
    model::MOI.ModelLike,
    relax::FeasibilityRelaxation,
    ci::MOI.ConstraintIndex{F,MOI.GreaterThan{T}},
) where {T,F<:Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}}
    # Performance optimization: we don't need the z relaxation variable.
    y = MOI.add_variable(model)
    MOI.add_constraint(model, y, MOI.GreaterThan(zero(T)))
    MOI.modify(model, ci, MOI.ScalarCoefficientChange(y, one(T)))
    a = relax.scale * get(relax.penalties, ci, one(T))
    O = MOI.get(model, MOI.ObjectiveFunctionType())
    obj = MOI.ObjectiveFunction{O}()
    MOI.modify(model, obj, MOI.ScalarCoefficientChange(y, a))
    return
end

function MOI.set(
    model::MOI.ModelLike,
    relax::FeasibilityRelaxation,
    ci::MOI.ConstraintIndex{F,MOI.LessThan{T}},
) where {T,F<:Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}}
    # Performance optimization: we don't need the y relaxation variable.
    z = MOI.add_variable(model)
    MOI.add_constraint(model, z, MOI.GreaterThan(zero(T)))
    MOI.modify(model, ci, MOI.ScalarCoefficientChange(z, -one(T)))
    a = relax.scale * get(relax.penalties, ci, one(T))
    O = MOI.get(model, MOI.ObjectiveFunctionType())
    obj = MOI.ObjectiveFunction{O}()
    MOI.modify(model, obj, MOI.ScalarCoefficientChange(z, a))
    return
end
