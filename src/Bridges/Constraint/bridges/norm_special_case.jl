# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    NormSpecialCaseBridge{T,F} <: Bridges.Constraint.AbstractBridge

`NormSpecialCaseBridge` implements the following reformulations:

  * ``(t, x) in NormOneCone(d)`` into ``(t, x) in NormCone(1, d)``
  * ``(t, x) in SecondOrderCone(d)`` into ``(t, x) in NormCone(2, d)``
  * ``(t, x) in NormInfinityCone(d)`` into ``(t, x) in NormCone(Inf, d)``

## Source node

`NormSpecialCaseBridge` supports:

  * `F` in [`MOI.NormOneCone`](@ref)
  * `F` in [`MOI.NormInfinityCone`](@ref)
  * `F` in [`MOI.SecondOrderCone`](@ref)

## Target nodes

`NormSpecialCaseBridge` creates:

  * `F` in [`MOI.NormCone`](@ref)
"""
struct NormSpecialCaseBridge{T,F} <: AbstractBridge
    constraint::MOI.ConstraintIndex{F,MOI.NormCone}
end

const NormSpecialCase{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{NormSpecialCaseBridge{T},OT}

function bridge_constraint(
    ::Type{NormSpecialCaseBridge{T,F}},
    model::MOI.ModelLike,
    f::F,
    s::Union{MOI.NormOneCone,MOI.SecondOrderCone,MOI.NormInfinityCone},
) where {T,F}
    ci = MOI.add_constraint(model, f, MOI.NormCone(s))
    return NormSpecialCaseBridge{T,F}(ci)
end

function MOI.supports_constraint(
    ::Type{NormSpecialCaseBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{<:Union{MOI.NormOneCone,MOI.SecondOrderCone,MOI.NormInfinityCone}},
) where {T}
    return true
end

function concrete_bridge_type(
    ::Type{<:NormSpecialCaseBridge{T}},
    F::Type{<:MOI.AbstractVectorFunction},
    ::Type{<:Union{MOI.NormOneCone,MOI.SecondOrderCone,MOI.NormInfinityCone}},
) where {T}
    return NormSpecialCaseBridge{T,F}
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:NormSpecialCaseBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{NormSpecialCaseBridge{T,F}},
) where {T,F}
    return Tuple{Type,Type}[(F, MOI.NormCone)]
end

MOI.get(::NormSpecialCaseBridge, ::MOI.NumberOfVariables)::Int64 = 0

function MOI.get(::NormSpecialCaseBridge, ::MOI.ListOfVariableIndices)
    return MOI.VariableIndex[]
end

function MOI.get(
    bridge::NormSpecialCaseBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.NormCone},
)::Int64 where {T,F}
    return 1
end

function MOI.get(
    bridge::NormSpecialCaseBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,MOI.NormCone},
) where {T,F}
    return [bridge.constraint]
end

function MOI.delete(model::MOI.ModelLike, bridge::NormSpecialCaseBridge)
    MOI.delete(model, bridge.constraint)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::NormSpecialCaseBridge,
)
    return MOI.get(model, MOI.ConstraintFunction(), bridge.constraint)
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::NormSpecialCaseBridge,
)
    set = MOI.get(model, MOI.ConstraintSet(), bridge.constraint)
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
