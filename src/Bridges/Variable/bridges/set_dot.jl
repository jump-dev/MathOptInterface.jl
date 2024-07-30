struct DotProductsBridge{T,S,A,V} <: SetMapBridge{T,S,MOI.SetWithDotProducts{S,A,V}}
    variables::Vector{MOI.VariableIndex}
    constraint::MOI.ConstraintIndex{MOI.VectorOfVariables,S}
    set::MOI.SetWithDotProducts{S,A,V}
end

function supports_constrained_variable(
    ::Type{<:DotProductsBridge},
    ::Type{<:MOI.SetWithDotProducts},
)
    return true
end

function concrete_bridge_type(
    ::Type{<:DotProductsBridge{T}},
    ::Type{MOI.SetWithDotProducts{S,A,V}},
) where {T,S,A,V}
    return DotProductsBridge{T,S,A,V}
end

function bridge_constrained_variable(
    BT::Type{DotProductsBridge{T,S,A,V}},
    model::MOI.ModelLike,
    set::MOI.SetWithDotProducts{S,A,V},
) where {T,S,A,V}
    variables, constraint =
        _add_constrained_var(model, MOI.Bridges.inverse_map_set(BT, set))
    return BT(variables, constraint, set)
end

function MOI.Bridges.map_set(bridge::DotProductsBridge{T,S}, set::S) where {T,S}
    return bridge.set
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:DotProductsBridge},
    set::MOI.SetWithDotProducts,
)
    return set.set
end

function MOI.Bridges.map_function(
    bridge::DotProductsBridge{T},
    func,
    i::MOI.Bridges.IndexInVector,
) where {T}
    scalars = MOI.Utilities.eachscalar(func)
    return MOI.Utilities.set_dot(
        bridge.set.vectors[i.value],
        scalars,
        bridge.set.set,
    )
end

# This returns `true` by default for `SetMapBridge`
# but is is not supported for this bridge because `inverse_map_function`
# is not implemented
function MOI.supports(::MOI.ModelLike, ::MOI.VariablePrimalStart, ::Type{<:DotProductsBridge})
    return false
end

function unbridged_map(
    ::DotProductsBridge,
    ::Vector{MOI.VariableIndex},
)
    return nothing
end
