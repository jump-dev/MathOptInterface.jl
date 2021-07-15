const _SOCtoRSOCMap{T} = Union{Constraint.SOCtoRSOCBridge{T},Variable.RSOCtoSOCBridge{T}}

function map_set(::Type{<:_SOCtoRSOCMap}, set::MOI.SecondOrderCone)
    return MOI.RotatedSecondOrderCone(MOI.dimension(set))
end

function inverse_map_set(
    ::Type{<:_SOCtoRSOCMap},
    set::MOI.RotatedSecondOrderCone,
)
    return MOI.SecondOrderCone(MOI.dimension(set))
end

const _RSOCtoSOCMap{T} = Union{Constraint.RSOCtoSOCBridge{T},Variable.SOCtoRSOCBridge{T}}

function map_set(
    ::Type{<:_RSOCtoSOCMap},
    set::MOI.RotatedSecondOrderCone,
)
    return MOI.SecondOrderCone(MOI.dimension(set))
end

function inverse_map_set(
    ::Type{<:_RSOCtoSOCMap},
    set::MOI.SecondOrderCone,
)
    return MOI.RotatedSecondOrderCone(MOI.dimension(set))
end

const _RSOCMap{T} = Union{_RSOCtoSOCMap{T},_SOCtoRSOCMap{T}}

function map_function(::Type{<:_RSOCMap{T}}, func, i::IndexInVector) where {T}
    scalars = MOIU.eachscalar(func)
    if i.value == 1 || i.value == 2
        t = scalars[1]
        u = scalars[2]
        s2 = √T(2)
        ts = MOIU.operate!(/, T, t, s2)
        us = MOIU.operate!(/, T, u, s2)
        # Cannot use `operate!` here since `ts` and `us` are needed for the next
        # line
        if i.value == 1
            return t / s2 + u / s2
        else
            return t / s2 - u / s2
        end
    else
        return scalars[i.value]
    end
end

function map_function(::Type{<:_RSOCMap{T}}, func) where {T}
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
function inverse_map_function(BT::Type{<:_RSOCMap}, func)
    return map_function(BT, func)
end
# The map is symmetric
function adjoint_map_function(BT::Type{<:_RSOCMap}, func)
    return map_function(BT, func)
end
# The map is a symmetric involution
function inverse_adjoint_map_function(BT::Type{<:_RSOCMap}, func)
    return map_function(BT, func)
end
