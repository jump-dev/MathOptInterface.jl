struct SetConversionBridge{T,S2,S1,F} <:
       MOI.Bridges.Constraint.SetMapBridge{T,S2,S1,F,F}
    constraint::MOI.ConstraintIndex{F,S2}
end

function MOI.supports_constraint(
    ::Type{SetConversionBridge{T,S2}},
    ::Type{F},
    ::Type{S1},
) where {T,F<:MOI.AbstractFunction,S1<:MOI.AbstractSet,S2}
    return isfinite(MOI.Bridges.Constraint.conversion_cost(S2, S1))
end

function MOI.Bridges.Constraint.concrete_bridge_type(
    ::Type{SetConversionBridge{T,S2}},
    ::Type{F},
    ::Type{S1},
) where {T,F<:MOI.AbstractFunction,S1<:MOI.AbstractSet,S2}
    return SetConversionBridge{T,S2,S1,F}
end

function MOI.Bridges.Constraint.conversion_cost(
    ::Type{<:MOI.AbstractSet},
    ::Type{<:MOI.AbstractSet},
)
    return Inf
end

function MOI.Bridges.bridging_cost(
    ::Type{<:SetConversionBridge{T,S2,S1}},
) where {T,S2,S1}
    return MOI.Bridges.Constraint.conversion_cost(S2, S1)
end

function MOI.Bridges.map_set(
    ::Type{<:SetConversionBridge{T,S2,S1}},
    set::S1,
) where {T,S2,S1}
    return convert(S2, set)
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:SetConversionBridge{T,S2,S1}},
    set::S2,
) where {T,S2,S1}
    return convert(S1, set)
end

function MOI.Bridges.map_function(::Type{<:SetConversionBridge}, func)
    return func
end

function MOI.Bridges.inverse_map_function(::Type{<:SetConversionBridge}, func)
    return func
end

function MOI.Bridges.adjoint_map_function(::Type{<:SetConversionBridge}, func)
    return func
end

function MOI.Bridges.inverse_adjoint_map_function(
    ::Type{<:SetConversionBridge},
    func,
)
    return func
end
