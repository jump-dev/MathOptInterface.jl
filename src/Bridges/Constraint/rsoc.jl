"""
    RSOCtoSOCBridge{T, F, G}

The `RotatedSecondOrderCone` is `SecondOrderCone` representable; see [BN01, p. 104].
Indeed, we have ``2tu = (t/√2 + u/√2)^2 - (t/√2 - u/√2)^2`` hence
```math
2tu \\ge \\lVert x \\rVert_2^2
```
is equivalent to
```math
(t/√2 + u/√2)^2 \\ge \\lVert x \\rVert_2^2 + (t/√2 - u/√2)^2.
```
We can therefore use the transformation ``(t, u, x) \\mapsto (t/√2+u/√2, t/√2-u/√2, x)``.
Note that the linear transformation is a symmetric involution (i.e. it is its own transpose and its own inverse).
That means in particular that the norm of constraint primal and dual values are preserved by the tranformation.

[BN01] Ben-Tal, Aharon, and Nemirovski, Arkadi. *Lectures on modern convex optimization: analysis, algorithms, and engineering applications*. Society for Industrial and Applied Mathematics, 2001.
"""
struct RSOCtoSOCBridge{T,F,G} <:
       SetMapBridge{T,MOI.SecondOrderCone,MOI.RotatedSecondOrderCone,F,G}
    constraint::CI{F,MOI.SecondOrderCone}
end

function rotate_function_type(G::Type{<:MOI.AbstractVectorFunction}, T::Type)
    S = MOIU.promote_operation(/, T, MOIU.scalar_type(G), T)
    Y = MOIU.promote_operation(-, T, S, S)
    Z = MOIU.promote_operation(+, T, S, S)
    return MOIU.promote_operation(vcat, T, Z, Y, G)
end

function concrete_bridge_type(
    ::Type{<:RSOCtoSOCBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.RotatedSecondOrderCone},
) where {T}
    return RSOCtoSOCBridge{T,rotate_function_type(G, T),G}
end

function map_set(::Type{<:RSOCtoSOCBridge}, set::MOI.RotatedSecondOrderCone)
    return MOI.SecondOrderCone(MOI.dimension(set))
end

function inverse_map_set(::Type{<:RSOCtoSOCBridge}, set::MOI.SecondOrderCone)
    return MOI.RotatedSecondOrderCone(MOI.dimension(set))
end

"""
    SOCtoRSOCBridge{T, F, G}

We simply do the inverse transformation of [`RSOCtoSOCBridge`](@ref). In fact, as the
transformation is an involution, we do the same transformation.
"""
struct SOCtoRSOCBridge{T,F,G} <:
       SetMapBridge{T,MOI.RotatedSecondOrderCone,MOI.SecondOrderCone,F,G}
    constraint::CI{F,MOI.RotatedSecondOrderCone}
end

function concrete_bridge_type(
    ::Type{<:SOCtoRSOCBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.SecondOrderCone},
) where {T}
    return SOCtoRSOCBridge{T,rotate_function_type(G, T),G}
end

function map_set(::Type{<:SOCtoRSOCBridge}, set::MOI.SecondOrderCone)
    return MOI.RotatedSecondOrderCone(MOI.dimension(set))
end

function inverse_map_set(
    ::Type{<:SOCtoRSOCBridge},
    set::MOI.RotatedSecondOrderCone,
)
    return MOI.SecondOrderCone(MOI.dimension(set))
end

function map_function(
    ::Type{<:Union{RSOCtoSOCBridge{T},SOCtoRSOCBridge{T}}},
    func,
) where {T}
    scalars = MOIU.eachscalar(func)
    t = scalars[1]
    u = scalars[2]
    x = scalars[3:end]
    s2 = √T(2)
    ts = MOIU.operate!(/, T, t, s2)
    us = MOIU.operate!(/, T, u, s2)
    # Cannot use `operate!` here since `ts` and `us` are needed for the next
    # line
    y = ts - us
    z = MOIU.operate!(+, T, ts, us)
    return MOIU.operate(vcat, T, z, y, x)
end
# The map is an involution
function inverse_map_function(
    BT::Type{<:Union{RSOCtoSOCBridge,SOCtoRSOCBridge}},
    func,
)
    return map_function(BT, func)
end
# The map is symmetric
function adjoint_map_function(
    BT::Type{<:Union{RSOCtoSOCBridge,SOCtoRSOCBridge}},
    func,
)
    return map_function(BT, func)
end
# The map is a symmetric involution
function inverse_adjoint_map_function(
    BT::Type{<:Union{RSOCtoSOCBridge,SOCtoRSOCBridge}},
    func,
)
    return map_function(BT, func)
end
