"""
    ZeroOneBridge{T}

The `ZeroOneBridge` splits a `F`-in-`MOI.ZeroOne` constraint into a `F`-in-`MOI.Integer` constraint,
a `F`-in-`MOI.LessThan(1)` constraint, and a `F`-in-`MOI.GreaterThan(0)` constraint.
"""
struct ZeroOneBridge{T} <: AbstractBridge
    variable_index::MOI.VariableIndex
    zero_index::MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{T}}
    one_index::MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{T}}
    integer_index::MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integer}
end

function bridge_constraint(::Type{ZeroOneBridge{T}}, model::MOI.ModelLike, f::MOI.SingleVariable, ::MOI.ZeroOne) where {T <: Real}
    zero_index = MOI.add_constraint(model, f, MOI.GreaterThan{T}(zero(T)))
    one_index = MOI.add_constraint(model, f, MOI.LessThan{T}(one(T)))
    integer_index = MOI.add_constraint(model, f, MOI.Integer())
    return ZeroOneBridge{T}(f.variable, zero_index, one_index, integer_index)
end

function MOIB.added_constraint_types(::Type{<:ZeroOneBridge{T}}) where {T}
    return [
            (MOI.SingleVariable, MOI.LessThan{T}),
            (MOI.SingleVariable, MOI.GreaterThan{T}),
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
    return MOI.SingleVariable(bridge.variable_index)
end

function MOI.delete(model::MOI.ModelLike, bridge::ZeroOneBridge)
    MOI.delete(model, bridge.zero_index)
    MOI.delete(model, bridge.one_index)
    MOI.delete(model, bridge.integer_index)
end

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal, bridge::ZeroOneBridge)
    MOI.get(model, MOI.VariablePrimal(attr.N), bridge.variable_index)
end

function MOI.supports(
    ::MOI.ModelLike,
    ::MOI.ConstraintPrimalStart,
    ::Type{<:ZeroOneBridge})
    return true
end

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimalStart, bridge::ZeroOneBridge)
    return MOI.get(model, MOI.VariablePrimalStart(), bridge.variable_index)
end

function MOI.set(model::MOI.ModelLike, attr::MOI.ConstraintPrimalStart,
                 bridge::ZeroOneBridge{T}, value) where {T}
    MOI.set(model, MOI.VariablePrimalStart(), bridge.variable_index, value)
    MOI.set(model, MOI.ConstraintPrimalStart(), bridge.zero_index, value)
    MOI.set(model, MOI.ConstraintPrimalStart(), bridge.one_index, value)
    return
end

# Attributes, Bridge acting as a model
function MOI.get(bridge::ZeroOneBridge{T},
    ::MOI.NumberOfConstraints{MOI.SingleVariable, MOI.GreaterThan{T}}) where {T}
    return 1
end

function MOI.get(bridge::ZeroOneBridge{T},
    ::MOI.NumberOfConstraints{MOI.SingleVariable, MOI.LessThan{T}}) where {T}
    return 1
end

function MOI.get(bridge::ZeroOneBridge{T},
    ::MOI.NumberOfConstraints{MOI.SingleVariable, MOI.Integer}) where {T}
    return 1
end

function MOI.get(bridge::ZeroOneBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.GreaterThan{T}}) where {T}
    return [bridge.zero_index]
end

function MOI.get(bridge::ZeroOneBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.LessThan{T}}) where {T}
    return [bridge.one_index]
end

function MOI.get(bridge::ZeroOneBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.Integer}) where {T}
    return [bridge.integer_index]
end

