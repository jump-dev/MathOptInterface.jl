# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    RSOCtoSOCBridge{T} <: Bridges.Variable.AbstractBridge

`RSOCtoSOCBridge` implements the following reformulation:

 * ``||x||_2^2 \\le 2tu`` into ``||v||_2 \\le w ``, with the substitution rules
   ``t = \\frac{w}{\\sqrt 2} + \\frac{v_1}{\\sqrt 2}``,
   ``u = \\frac{w}{\\sqrt 2} - \\frac{v_1}{\\sqrt 2}``, and
   ``x = (v_2,\\ldots,v_N)``.

## Source node

`RSOCtoSOCBridge` supports:

 * [`MOI.VectorOfVariables`](@ref) in [`MOI.RotatedSecondOrderCone`](@ref)

## Target node

`RSOCtoSOCBridge` creates:

 * [`MOI.VectorOfVariables`](@ref) in [`MOI.SecondOrderCone`](@ref)
"""
struct RSOCtoSOCBridge{T} <:
       SetMapBridge{T,MOI.SecondOrderCone,MOI.RotatedSecondOrderCone}
    variables::Vector{MOI.VariableIndex}
    constraint::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.SecondOrderCone}
end

const RSOCtoSOC{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{RSOCtoSOCBridge{T},OT}

function MOI.Bridges.map_set(
    ::Type{<:RSOCtoSOCBridge},
    set::MOI.SecondOrderCone,
)
    return MOI.RotatedSecondOrderCone(MOI.dimension(set))
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:RSOCtoSOCBridge},
    set::MOI.RotatedSecondOrderCone,
)
    return MOI.SecondOrderCone(MOI.dimension(set))
end

function MOI.Bridges.map_function(
    ::Type{<:RSOCtoSOCBridge{T}},
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

function MOI.Bridges.map_function(::Type{<:RSOCtoSOCBridge{T}}, func) where {T}
    scalars = MOI.Utilities.eachscalar(func)
    t, u, x = scalars[1], scalars[2], scalars[3:end]
    ts = MOI.Utilities.operate!(/, T, t, sqrt(T(2)))
    us = MOI.Utilities.operate!(/, T, u, sqrt(T(2)))
    return MOI.Utilities.operate(vcat, T, ts + us, ts - us, x)
end

# The map is an involution
function MOI.Bridges.inverse_map_function(BT::Type{<:RSOCtoSOCBridge}, func)
    return MOI.Bridges.map_function(BT, func)
end

# The map is symmetric
function MOI.Bridges.adjoint_map_function(BT::Type{<:RSOCtoSOCBridge}, func)
    return MOI.Bridges.map_function(BT, func)
end

# The map is a symmetric involution
function MOI.Bridges.inverse_adjoint_map_function(
    BT::Type{<:RSOCtoSOCBridge},
    func,
)
    return MOI.Bridges.map_function(BT, func)
end
