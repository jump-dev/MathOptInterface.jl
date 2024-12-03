# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    SOCtoRSOCBridge{T} <: Bridges.Variable.AbstractBridge

`SOCtoRSOCBridge` implements the following reformulation:

 * ``||x||_2 \\le t`` into ``2uv \\ge ||w||_2^2``, with the substitution rules
   ``t = \\frac{u}{\\sqrt 2} + \\frac{v}{\\sqrt 2}``,
   ``x = (\\frac{u}{\\sqrt 2} - \\frac{v}{\\sqrt 2}, w)``.

## Assumptions

 * `SOCtoRSOCBridge` assumes that ``|x| \\ge 1``.

## Source node

`SOCtoRSOCBridge` supports:

 * [`MOI.VectorOfVariables`](@ref) in [`MOI.SecondOrderCone`](@ref)

## Target node

`SOCtoRSOCBridge` creates:

 * [`MOI.VectorOfVariables`](@ref) in [`MOI.RotatedSecondOrderCone`](@ref)
"""
struct SOCtoRSOCBridge{T} <:
       SetMapBridge{T,MOI.RotatedSecondOrderCone,MOI.SecondOrderCone}
    variables::Vector{MOI.VariableIndex}
    constraint::MOI.ConstraintIndex{
        MOI.VectorOfVariables,
        MOI.RotatedSecondOrderCone,
    }
end

const SOCtoRSOC{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SOCtoRSOCBridge{T},OT}

function MOI.Bridges.map_set(
    ::Type{<:SOCtoRSOCBridge},
    set::MOI.RotatedSecondOrderCone,
)
    return MOI.SecondOrderCone(MOI.dimension(set))
end

function MOI.Bridges.inverse_map_set(
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

function MOI.Bridges.map_function(
    ::Type{<:SOCtoRSOCBridge{T}},
    func,
    i::MOI.Bridges.IndexInVector,
) where {T}
    scalars = MOI.Utilities.eachscalar(func)
    if i.value > 2
        return scalars[i.value]
    end
    t, u = scalars[1], scalars[2]
    ts = MOI.Utilities.operate!(/, T, t, sqrt(T(2)))
    us = MOI.Utilities.operate!(/, T, u, sqrt(T(2)))
    if i.value == 1
        return MOI.Utilities.operate!(+, T, ts, us)
    else
        return MOI.Utilities.operate!(-, T, ts, us)
    end
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

function MOI.supports(
    ::MOI.ModelLike,
    ::MOI.VariablePrimalStart,
    ::Type{<:SOCtoRSOCBridge},
)
    # https://github.com/jump-dev/MathOptInterface.jl/issues/2117
    return false
end
