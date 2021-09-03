"""
    ZeroOneBridge{T}

The `ZeroOneBridge` splits a `MOI.VariableIndex`-in-`MOI.ZeroOne` constraint
into a `MOI.VariableIndex`-in-`MOI.Integer` constraint
and a `MOI.VariableIndex`-in-`MOI.Interval(0, 1)` constraint.
"""
struct ZeroOneBridge{T} <: AbstractBridge
    interval_index::MOI.ConstraintIndex{MOI.VariableIndex,MOI.Interval{T}}
    integer_index::MOI.ConstraintIndex{MOI.VariableIndex,MOI.Integer}
end

function bridge_constraint(
    ::Type{ZeroOneBridge{T}},
    model::MOI.ModelLike,
    f::MOI.VariableIndex,
    ::MOI.ZeroOne,
) where {T<:Real}
    interval_index =
        MOI.add_constraint(model, f, MOI.Interval{T}(zero(T), one(T)))
    integer_index = MOI.add_constraint(model, f, MOI.Integer())
    return ZeroOneBridge{T}(interval_index, integer_index)
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:ZeroOneBridge{T}},
) where {T}
    return Tuple{Type,Type}[
        (MOI.VariableIndex, MOI.Interval{T}),
        (MOI.VariableIndex, MOI.Integer),
    ]
end

function MOI.Bridges.added_constrained_variable_types(::Type{<:ZeroOneBridge})
    return Tuple{Type}[]
end

function concrete_bridge_type(
    ::Type{<:ZeroOneBridge{T}},
    ::Type{MOI.VariableIndex},
    ::Type{MOI.ZeroOne},
) where {T}
    return ZeroOneBridge{T}
end

function MOI.supports_constraint(
    ::Type{<:ZeroOneBridge},
    ::Type{MOI.VariableIndex},
    ::Type{MOI.ZeroOne},
)
    return true
end

MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet, ::ZeroOneBridge) = MOI.ZeroOne()

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::ZeroOneBridge,
)
    return MOI.get(model, attr, bridge.interval_index)
end

function MOI.delete(model::MOI.ModelLike, bridge::ZeroOneBridge)
    MOI.delete(model, bridge.interval_index)
    MOI.delete(model, bridge.integer_index)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal},
    bridge::ZeroOneBridge,
)
    return MOI.get(model, attr, bridge.interval_index)
end

function MOI.get(
    ::ZeroOneBridge{T},
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.Interval{T}},
)::Int64 where {T}
    return 1
end

function MOI.get(
    ::ZeroOneBridge,
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.Integer},
)::Int64
    return 1
end

function MOI.get(
    bridge::ZeroOneBridge,
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.Interval{T}},
) where {T}
    return [bridge.interval_index]
end

function MOI.get(
    bridge::ZeroOneBridge,
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.Integer},
)
    return [bridge.integer_index]
end
