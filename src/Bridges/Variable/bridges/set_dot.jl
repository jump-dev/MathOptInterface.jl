struct DotProductsBridge{T,S,V} <: SetMapBridge{T,S,MOI.SetWithDotProducts{S,V}}
    variables::Vector{MOI.VariableIndex}
    constraint::MOI.ConstraintIndex{MOI.VectorOfVariables,S}
    set::MOI.SetWithDotProducts{S,V}
end

function supports_constrained_variable(
    ::Type{<:DotProductsBridge},
    ::Type{<:MOI.SetWithDotProducts},
)
    return true
end

function concrete_bridge_type(
    ::Type{<:DotProductsBridge{T}},
    ::Type{MOI.SetWithDotProducts{S,V}},
) where {T,S,V}
    return DotProductsBridge{T,S,V}
end

function bridge_constrained_variable(
    BT::Type{DotProductsBridge{T,S,V}},
    model::MOI.ModelLike,
    set::MOI.SetWithDotProducts{S,V},
) where {T,S,V}
    variables, constraint =
        _add_constrained_var(model, MOI.Bridges.inverse_map_set(BT, set))
    return BT(variables, constraint, set)
end

function MOI.Bridges.map_set(bridge::DotProductsBridge{T,S}, set::S) where {T,S}
    return MOI.SetWithDotProducts(set, bridge.vectors)
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
    if i.value in eachindex(bridge.set.vectors)
        return MOI.Utilities.set_dot(
            bridge.set.vectors[i.value],
            scalars,
            bridge.set.set,
        )
    else
        return convert(
            MOI.ScalarAffineFunction{T},
            scalars[i.value-length(bridge.vectors)],
        )
    end
end

function MOI.Bridges.inverse_map_function(
    bridge::DotProductsBridge{T},
    func,
) where {T}
    m = length(bridge.set.vectors)
    return MOI.Utilities.operate(
        vcat,
        T,
        MOI.Utilities.eachscalar(func)[(m+1):end],
    )
end
