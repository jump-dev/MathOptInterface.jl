"""
    ZeroOneBridge{T}

The `ZeroOneBridge` splits a `MOI.SingleVariable`-in-`MOI.ZeroOne` constraint
into a `MOI.SingleVariable`-in-`MOI.Integer` constraint
and a `MOI.SingleVariable`-in-`MOI.Interval(0, 1)` constraint.
"""
struct ZeroOneBridge{T} <: AbstractBridge
    interval_index::MOI.ConstraintIndex{MOI.SingleVariable, MOI.Interval{T}}
    integer_index::MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integer}
end

function bridge_constraint(::Type{ZeroOneBridge{T}}, model::MOI.ModelLike,
                           f::MOI.SingleVariable, ::MOI.ZeroOne) where {T <: Real}
    interval_index = MOI.add_constraint(model, f, MOI.Interval{T}(zero(T), one(T)))
    integer_index = MOI.add_constraint(model, f, MOI.Integer())
    return ZeroOneBridge{T}(f.variable, interval_index, integer_index)
end

function MOIB.added_constraint_types(::Type{<:ZeroOneBridge{T}}) where {T}
    return [
            (MOI.SingleVariable, MOI.Interval{T}),
            (MOI.SingleVariable, MOI.Integer),
            ]
end

function concrete_bridge_type(::Type{<:ZeroOneBridge{T}},
                              ::Type{MOI.SingleVariable},
                              ::Type{MOI.ZeroOne}) where {T}
    return ZeroOneBridge{T}
end

function MOI.supports_constraint(::Type{<:ZeroOneBridge},
                                 ::Type{MOI.SingleVariable},
                                 ::Type{MOI.ZeroOne})
    return true
end


# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintSet, bridge::ZeroOneBridge)
    return MOI.ZeroOne()
end

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintFunction, bridge::ZeroOneBridge)
    return MOI.SingleVariable(bridge.interval_index)
end

function MOI.delete(model::MOI.ModelLike, bridge::ZeroOneBridge)
    MOI.delete(model, bridge.interval_index)
    MOI.delete(model, bridge.integer_index)
end

function MOI.get(model::MOI.ModelLike,
                 attr::Union{MOI.ConstraintPrimal, MOI.ConstraintPrimalStart},
                 bridge::ZeroOneBridge)
    MOI.get(model, MOI.ConstraintPrimal(attr.N), bridge.interval_index)
end

function MOI.supports(
    ::MOI.ModelLike,
    ::MOI.ConstraintPrimalStart,
    ::Type{<:ZeroOneBridge})
    return true
end

function MOI.set(model::MOI.ModelLike,
                 attr::MOI.ConstraintPrimalStart,
                 bridge::ZeroOneBridge{T}, value) where {T}
    MOI.set(model, attr, bridge.integer_index, value)
end

# Attributes, Bridge acting as a model
function MOI.get(
    bridge::ZeroOneBridge{T},
    ::MOI.NumberOfConstraints{MOI.SingleVariable, MOI.Interval{T}}) where {T}
    return 1
end

function MOI.get(
    bridge::ZeroOneBridge,
    ::MOI.NumberOfConstraints{MOI.SingleVariable, MOI.Integer})
    return 1
end

function MOI.get(
    bridge::ZeroOneBridge,
    ::MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.Interval{T}}) where {T}
    return [bridge.interval_index]
end

function MOI.get(
    bridge::ZeroOneBridge,
    ::MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.Integer})
    return [bridge.integer_index]
end

