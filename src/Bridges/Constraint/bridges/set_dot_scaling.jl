# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    SetDotScalingBridge{T,S,F,G} <: Bridges.Constraint.AbstractBridge

`SetDotScalingBridge` implements the reformulation from constraints
in `S` to constraints in [`MOI.Scaled{S}`](@ref MOI.Scaled).

## Source node

`SetDotScalingBridge` supports:

  * `G` in `S`

## Target node

`SetDotScalingBridge` creates:

  * `F` in [`MOI.Scaled{S}`](@ref MOI.Scaled)
"""
struct SetDotScalingBridge{T,S,F,G} <: SetMapBridge{T,MOI.Scaled{S},S,F,G}
    constraint::MOI.ConstraintIndex{F,MOI.Scaled{S}}
end

const SetDotScaling{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SetDotScalingBridge{T},OT}

function concrete_bridge_type(
    ::Type{<:SetDotScalingBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{S},
) where {T,S<:MOI.AbstractVectorSet}
    F = MOI.Utilities.promote_operation(
        *,
        T,
        LinearAlgebra.Diagonal{T,Vector{T}},
        G,
    )
    return SetDotScalingBridge{T,S,F,G}
end

function MOI.Bridges.map_set(
    ::Type{<:SetDotScalingBridge{T,S}},
    set::S,
) where {T,S}
    return MOI.Scaled(set)
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:SetDotScalingBridge{T,S}},
    set::MOI.Scaled{S},
) where {T,S}
    return set.set
end

_length(f::MOI.AbstractVectorFunction) = MOI.output_dimension(f)
_length(f::AbstractVector) = length(f)

function _scale(::Type{T}, ::Type{S}, func) where {T,S}
    set = MOI.Utilities.set_with_dimension(S, _length(func))
    scale = MOI.Utilities.SetDotScalingVector{T}(set)
    return MOI.Utilities.operate(*, T, LinearAlgebra.Diagonal(scale), func)
end

function _inverse_scale(::Type{T}, ::Type{S}, func) where {T,S}
    set = MOI.Utilities.set_with_dimension(S, _length(func))
    scale = MOI.Utilities.SetDotScalingVector{T}(set)
    inv_scale = MOI.Utilities.lazy_map(T, inv, scale)
    return MOI.Utilities.operate(*, T, LinearAlgebra.Diagonal(inv_scale), func)
end

function MOI.Bridges.map_function(
    ::Type{<:SetDotScalingBridge{T,S}},
    func,
) where {T,S}
    return _scale(T, S, func)
end

function MOI.Bridges.inverse_map_function(
    ::Type{<:SetDotScalingBridge{T,S}},
    func,
) where {T,S}
    return _inverse_scale(T, S, func)
end

# Since the map is a diagonal matrix `D`, it is symmetric so one would initially
# expect `adjoint_map_function` to be the same as `map_function`. However, the
# scalar product for the scaled PSD cone is `<x, y>_2 = x'y` but the scalar
# product for the PSD cone additionally scales the offdiagonal entries by `2`
# hence by `D^2` so `<x, y>_1 = x'D^2y`.
# So `<Dx, y>_2 = <x, D^(-1)y>_1` hence the adjoint of `D` is its inverse!
function MOI.Bridges.adjoint_map_function(
    ::Type{<:SetDotScalingBridge{T,S}},
    func,
) where {T,S}
    return _inverse_scale(T, S, func)
end

function MOI.Bridges.inverse_adjoint_map_function(
    ::Type{<:SetDotScalingBridge{T,S}},
    func,
) where {T,S}
    return _scale(T, S, func)
end

"""
    SetDotInverseScalingBridge{T,S,F,G} <: Bridges.Constraint.AbstractBridge

`SetDotInverseScalingBridge` implements the reformulation from constraints
in the `MOI.Scaled{S}` to constraints in the `S`.

## Source node

`SetDotInverseScalingBridge` supports:

  * `G` in [`MOI.Scaled{S}`](@ref MOI.Scaled)

## Target node

`SetDotInverseScalingBridge` creates:

  * `F` in `S`
"""
struct SetDotInverseScalingBridge{T,S,F,G} <:
       SetMapBridge{T,S,MOI.Scaled{S},F,G}
    constraint::MOI.ConstraintIndex{F,S}
end

const SetDotInverseScaling{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SetDotInverseScalingBridge{T},OT}

function concrete_bridge_type(
    ::Type{<:SetDotInverseScalingBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.Scaled{S}},
) where {T,S<:MOI.AbstractVectorSet}
    F = MOI.Utilities.promote_operation(
        *,
        T,
        LinearAlgebra.Diagonal{T,Vector{T}},
        G,
    )
    return SetDotInverseScalingBridge{T,S,F,G}
end

function MOI.Bridges.map_set(
    ::Type{<:SetDotInverseScalingBridge{T,S}},
    set::MOI.Scaled{S},
) where {T,S}
    return set.set
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:SetDotInverseScalingBridge{T,S}},
    set::S,
) where {T,S}
    return MOI.Scaled(set)
end

function MOI.Bridges.map_function(
    ::Type{<:SetDotInverseScalingBridge{T,S}},
    func,
) where {T,S}
    return _inverse_scale(T, S, func)
end

function MOI.Bridges.inverse_map_function(
    ::Type{<:SetDotInverseScalingBridge{T,S}},
    func,
) where {T,S}
    return _scale(T, S, func)
end

function MOI.Bridges.adjoint_map_function(
    ::Type{<:SetDotInverseScalingBridge{T,S}},
    func,
) where {T,S}
    return _inverse_scale(T, S, func)
end

function MOI.Bridges.inverse_adjoint_map_function(
    ::Type{<:SetDotInverseScalingBridge{T,S}},
    func,
) where {T,S}
    return _scale(T, S, func)
end

# Since the set type is not defined, the default `MOI.supports_constraint`
# for `SetMapBridge` does not work
function MOI.supports_constraint(
    ::Type{<:SetDotScalingBridge},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{<:MOI.AbstractVectorSet},
)
    return true
end

function MOI.supports_constraint(
    ::Type{<:SetDotInverseScalingBridge},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.Scaled{S}},
) where {S<:MOI.AbstractVectorSet}
    return true
end

# TODO remove in MOI v2
const SymmetricMatrixScalingBridge = SetDotScalingBridge
const SymmetricMatrixInverseScalingBridge = SetDotInverseScalingBridge
