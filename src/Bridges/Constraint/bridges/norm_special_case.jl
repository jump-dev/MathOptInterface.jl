# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

struct AbstractNormSpecialCaseBridge{T,S,F} <:
       SetMapBridge{T,MOI.NormCone,S,F,F}
    constraint::MOI.ConstraintIndex{F,MOI.NormCone}
end

MOI.Bridges.map_function(::Type{<:AbstractNormSpecialCaseBridge}, f) = f

MOI.Bridges.inverse_map_function(::Type{<:AbstractNormSpecialCaseBridge}, f) = f

MOI.Bridges.adjoint_map_function(::Type{<:AbstractNormSpecialCaseBridge}, f) = f

function MOI.Bridges.inverse_adjoint_map_function(
    ::Type{<:AbstractNormSpecialCaseBridge},
    f,
)
    return f
end

function MOI.Bridges.map_set(
    ::Type{<:AbstractNormSpecialCaseBridge{T,S}},
    set::S,
) where {T,S}
    return MOI.NormCone(set)
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:AbstractNormSpecialCaseBridge},
    set::MOI.NormCone,
)
    d = MOI.dimension(set)
    if set.p == 1
        return MOI.NormOneCone(d)
    elseif set.p == 2
        return MOI.SecondOrderCone(d)
    else
        @assert set.p == Inf
        return MOI.NormInfinityCone(d)
    end
end

"""
    NormOneConeToNormConeBridge{T,F} <: Bridges.Constraint.AbstractBridge

`NormOneConeToNormConeBridge` implements the following reformulations:

  * ``(t, x) in NormOneCone(d)`` into ``(t, x) in NormCone(1, d)``

## Source node

`NormOneConeToNormConeBridge` supports:

  * `F` in [`MOI.NormOneCone`](@ref)

## Target nodes

`NormOneConeToNormConeBridge` creates:

  * `F` in [`MOI.NormCone`](@ref)
"""
const NormOneConeToNormConeBridge{T,F} =
    AbstractNormSpecialCaseBridge{T,MOI.NormOneCone,F}

const NormOneConeToNormCone{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{NormOneConeToNormConeBridge{T},OT}

function concrete_bridge_type(
    ::Type{<:NormOneConeToNormConeBridge{T}},
    ::Type{F},
    ::Type{MOI.NormOneCone},
) where {T,F<:MOI.AbstractVectorFunction}
    return NormOneConeToNormConeBridge{T,F}
end

"""
    SecondOrderConeToNormConeBridge{T,F} <: Bridges.Constraint.AbstractBridge

`SecondOrderConeToNormConeBridge` implements the following reformulations:

  * ``(t, x) in SecondOrderCone(d)`` into ``(t, x) in NormCone(2, d)``

## Source node

`SecondOrderConeToNormConeBridge` supports:

  * `F` in [`MOI.SecondOrderCone`](@ref)

## Target nodes

`SecondOrderConeToNormConeBridge` creates:

  * `F` in [`MOI.NormCone`](@ref)
"""
const SecondOrderConeToNormConeBridge{T,F} =
    AbstractNormSpecialCaseBridge{T,MOI.SecondOrderCone,F}

const SecondOrderConeToNormCone{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SecondOrderConeToNormConeBridge{T},OT}

function concrete_bridge_type(
    ::Type{<:SecondOrderConeToNormConeBridge{T}},
    ::Type{F},
    ::Type{MOI.SecondOrderCone},
) where {T,F<:MOI.AbstractVectorFunction}
    return SecondOrderConeToNormConeBridge{T,F}
end

"""
    NormInfinityConeToNormConeBridge{T,F} <: Bridges.Constraint.AbstractBridge

`NormInfinityConeToNormConeBridge` implements the following reformulations:

  * ``(t, x) in NormInfinityCone(d)`` into ``(t, x) in NormCone(Inf, d)``

## Source node

`NormInfinityConeToNormConeBridge` supports:

  * `F` in [`MOI.NormInfinityCone`](@ref)

## Target nodes

`NormInfinityConeToNormConeBridge` creates:

  * `F` in [`MOI.NormCone`](@ref)
"""
const NormInfinityConeToNormConeBridge{T,F} =
    AbstractNormSpecialCaseBridge{T,MOI.NormInfinityCone,F}

const NormInfinityConeToNormCone{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{NormInfinityConeToNormConeBridge{T},OT}

function concrete_bridge_type(
    ::Type{<:NormInfinityConeToNormConeBridge{T}},
    ::Type{F},
    ::Type{MOI.NormInfinityCone},
) where {T,F<:MOI.AbstractVectorFunction}
    return NormInfinityConeToNormConeBridge{T,F}
end
