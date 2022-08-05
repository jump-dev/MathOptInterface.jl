# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    SOCtoRSOCBridge{T,F,G} <: Bridges.Constraint.AbstractBridge

`SOCtoRSOCBridge` implements the following reformulation:

  * ``||x||_2 \\le t`` into ``(t+x_1)(t-x_1)\\ge ||(x_2\\ldots,x_N)||_2^2``

## Assumptions

  * `SOCtoRSOCBridge` assumes that the dimension of the cone is at least one.

## Source node

`SOCtoRSOCBridge` supports:

  * `G` in [`MOI.SecondOrderCone`](@ref)

## Target node

`SOCtoRSOCBridge` creates:

  * `F` in [`MOI.RotatedSecondOrderCone`](@ref)
"""
struct SOCtoRSOCBridge{T,F,G} <:
       SetMapBridge{T,MOI.RotatedSecondOrderCone,MOI.SecondOrderCone,F,G}
    constraint::MOI.ConstraintIndex{F,MOI.RotatedSecondOrderCone}
end

const SOCR{T,OT<:MOI.ModelLike} = SingleBridgeOptimizer{SOCtoRSOCBridge{T},OT}

function concrete_bridge_type(
    ::Type{<:SOCtoRSOCBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.SecondOrderCone},
) where {T}
    S = MOI.Utilities.promote_operation(/, T, MOI.Utilities.scalar_type(G), T)
    Y = MOI.Utilities.promote_operation(-, T, S, S)
    Z = MOI.Utilities.promote_operation(+, T, S, S)
    F = MOI.Utilities.promote_operation(vcat, T, Z, Y, G)
    return SOCtoRSOCBridge{T,F,G}
end

function MOI.Bridges.map_set(
    ::Type{<:SOCtoRSOCBridge},
    set::MOI.SecondOrderCone,
)
    if MOI.dimension(set) == 1
        error(
            "Unable to reformulate a `SecondOrderCone` into a " *
            "`RotatedSecondOrderCone` because the dimension of `1` is too " *
            "small",
        )
    end
    return MOI.RotatedSecondOrderCone(MOI.dimension(set))
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:SOCtoRSOCBridge},
    set::MOI.RotatedSecondOrderCone,
)
    return MOI.SecondOrderCone(MOI.dimension(set))
end

function MOI.Bridges.map_function(::Type{<:SOCtoRSOCBridge{T}}, func) where {T}
    scalars = MOI.Utilities.eachscalar(func)
    t, u, x = scalars[1], scalars[2], scalars[3:end]
    ts = MOI.Utilities.operate!(/, T, t, sqrt(T(2)))
    us = MOI.Utilities.operate!(/, T, u, sqrt(T(2)))
    return MOI.Utilities.operate(vcat, T, ts + us, ts - us, x)
end

# The map is an involution
function MOI.Bridges.inverse_map_function(BT::Type{<:SOCtoRSOCBridge}, func)
    return MOI.Bridges.map_function(BT, func)
end

# The map is symmetric
function MOI.Bridges.adjoint_map_function(BT::Type{<:SOCtoRSOCBridge}, func)
    return MOI.Bridges.map_function(BT, func)
end

# The map is a symmetric involution
function MOI.Bridges.inverse_adjoint_map_function(
    BT::Type{<:SOCtoRSOCBridge},
    func,
)
    return MOI.Bridges.map_function(BT, func)
end
