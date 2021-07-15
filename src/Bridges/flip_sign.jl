const _NonnegToNonposMap{T} = Union{
    Variable.NonposToNonnegBridge{T},
    Constraint.NonnegToNonposBridge{T},
}

function map_set(::Type{<:_NonnegToNonposMap}, set::MOI.Nonnegatives)
    return MOI.Nonpositives(set.dimension)
end

function inverse_map_set(
    ::Type{<:_NonnegToNonposMap},
    set::MOI.Nonpositives,
)
    return MOI.Nonnegatives(set.dimension)
end

const _FlipSignMap{T} = Union{
    Variable.FlipSignBridge{T},
    Constraint.FlipSignBridge{T},
}

function map_function(::Type{<:_FlipSignMap{T}}, func) where {T}
    return MOIU.operate(-, T, func)
end

# The map is an involution
function inverse_map_function(BT::Type{<:_FlipSignMap}, func)
    return map_function(BT, func)
end

# The map is symmetric
function adjoint_map_function(BT::Type{<:_FlipSignMap}, func)
    return map_function(BT, func)
end

# The map is a symmetric involution
function inverse_adjoint_map_function(BT::Type{<:_FlipSignMap}, func)
    return map_function(BT, func)
end
