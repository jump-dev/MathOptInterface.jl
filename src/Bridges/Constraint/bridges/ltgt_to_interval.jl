# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    AbstractToIntervalBridge{T<:AbstractFloat,S,F}

An abstract type that simplifies the creation of other bridges.

!!! warning
    `T` must be a `AbstractFloat` type because otherwise `typemin` and `typemax`
    would either be not implemented (e.g. `BigInt`), or would not give infinite
    value (e.g. `Int`). For this reason, this bridge is only added to
    [`MOI.Bridges.full_bridge_optimizer`](@ref) when `T` is a subtype of
    `AbstractFloat`.
"""
abstract type AbstractToIntervalBridge{
    T<:AbstractFloat,
    S<:MOI.AbstractScalarSet,
    F<:MOI.AbstractScalarFunction,
} <: SetMapBridge{T,MOI.Interval{T},S,F,F} end

MOI.Bridges.map_function(::Type{<:AbstractToIntervalBridge}, f) = f

MOI.Bridges.inverse_map_function(::Type{<:AbstractToIntervalBridge}, f) = f

MOI.Bridges.adjoint_map_function(::Type{<:AbstractToIntervalBridge}, f) = f

function MOI.Bridges.inverse_adjoint_map_function(
    ::Type{<:AbstractToIntervalBridge},
    f,
)
    return f
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::AbstractToIntervalBridge,
    change::MOI.ScalarCoefficientChange,
)
    MOI.modify(model, bridge.constraint, change)
    return
end

"""
    GreaterToIntervalBridge{T,F} <: Bridges.Constraint.AbstractBridge

`GreaterToIntervalBridge` implements the following reformulations:

  * ``f(x) \\ge l`` into ``f(x) \\in [l, \\infty)``

## Source node

`GreaterToIntervalBridge` supports:

  * `F` in [`MOI.GreaterThan{T}`](@ref)

## Target nodes

`GreaterToIntervalBridge` creates:

  * `F` in [`MOI.Interval{T}`](@ref)
"""
struct GreaterToIntervalBridge{T,F<:MOI.AbstractScalarFunction} <:
       AbstractToIntervalBridge{T,MOI.GreaterThan{T},F}
    constraint::MOI.ConstraintIndex{F,MOI.Interval{T}}
end

const GreaterToInterval{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{GreaterToIntervalBridge{T},OT}

function MOI.Bridges.map_set(
    ::Type{<:GreaterToIntervalBridge},
    set::MOI.GreaterThan,
)
    return MOI.Interval(set.lower, typemax(set.lower))
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:GreaterToIntervalBridge},
    set::MOI.Interval,
)
    return MOI.GreaterThan(set.lower)
end

function concrete_bridge_type(
    ::Type{<:GreaterToIntervalBridge{T}},
    F::Type{<:MOI.AbstractScalarFunction},
    ::Type{MOI.GreaterThan{T}},
) where {T}
    return GreaterToIntervalBridge{T,F}
end

"""
    LessToIntervalBridge{T,F} <: Bridges.Constraint.AbstractBridge

`LessToIntervalBridge` implements the following reformulations:

  * ``f(x) \\le u`` into ``f(x) \\in (-\\infty, u]``

## Source node

`LessToIntervalBridge` supports:

  * `F` in [`MOI.LessThan{T}`](@ref)

## Target nodes

`LessToIntervalBridge` creates:

  * `F` in [`MOI.Interval{T}`](@ref)
"""
struct LessToIntervalBridge{T,F<:MOI.AbstractScalarFunction} <:
       AbstractToIntervalBridge{T,MOI.LessThan{T},F}
    constraint::MOI.ConstraintIndex{F,MOI.Interval{T}}
end

const LessToInterval{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{LessToIntervalBridge{T},OT}

function MOI.Bridges.map_set(::Type{<:LessToIntervalBridge}, set::MOI.LessThan)
    return MOI.Interval(typemin(set.upper), set.upper)
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:LessToIntervalBridge},
    set::MOI.Interval,
)
    return MOI.LessThan(set.upper)
end

function concrete_bridge_type(
    ::Type{<:LessToIntervalBridge{T}},
    F::Type{<:MOI.AbstractScalarFunction},
    ::Type{MOI.LessThan{T}},
) where {T}
    return LessToIntervalBridge{T,F}
end
