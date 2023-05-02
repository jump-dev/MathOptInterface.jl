# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

struct NormSpecialCaseBridge{S,T,F} <: SetMapBridge{T,MOI.NormCone,S,F,F}
    constraint::MOI.ConstraintIndex{F,MOI.NormCone}
end

MOI.Bridges.map_function(::Type{<:NormSpecialCaseBridge}, f) = f

MOI.Bridges.inverse_map_function(::Type{<:NormSpecialCaseBridge}, f) = f

MOI.Bridges.adjoint_map_function(::Type{<:NormSpecialCaseBridge}, f) = f

MOI.Bridges.inverse_adjoint_map_function(::Type{<:NormSpecialCaseBridge}, f) = f

function MOI.Bridges.map_set(
    ::Type{<:NormSpecialCaseBridge{S}},
    set::S,
) where {S}
    return MOI.NormCone(set)
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:NormSpecialCaseBridge{S}},
    set::MOI.NormCone,
) where {S}
    return S(MOI.dimension(set))
end

function concrete_bridge_type(
    ::Type{<:NormSpecialCaseBridge{S,T}},
    F::Type{<:MOI.AbstractVectorFunction},
    ::Type{S},
) where {T,S}
    return NormSpecialCaseBridge{S,T,F}
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
    NormSpecialCaseBridge{MOI.NormOneCone,T,F}

const NormOneConeToNormCone{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{NormOneConeToNormConeBridge{T},OT}

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
    NormSpecialCaseBridge{MOI.SecondOrderCone,T,F}

const SecondOrderConeToNormCone{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SecondOrderConeToNormConeBridge{T},OT}

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
    NormSpecialCaseBridge{MOI.NormInfinityCone,T,F}

const NormInfinityConeToNormCone{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{NormInfinityConeToNormConeBridge{T},OT}
