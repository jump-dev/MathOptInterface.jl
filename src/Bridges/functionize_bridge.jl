# scalar version

"""
    SingleVariableBridge{T, S}

The `SingleVariableBridge` converts a constraint `SingleVariable`-in-`S`
into the constraint `ScalarAffineFunction{T}`-in-`S`.
"""
struct SingleVariableBridge{T, S} <: AbstractBridge
    constraint::CI{MOI.ScalarAffineFunction{T}, S}
end
function SingleVariableBridge{T, S}(model, f::MOI.SingleVariable, s::S) where {T, S}
    constraint = MOI.add_constraint(model, MOI.ScalarAffineFunction{T}(f), s)
    return SingleVariableBridge{T, S}(constraint)
end

# start allowing everything (scalar)
MOI.supports_constraint(::Type{SingleVariableBridge{T}},
                        ::Type{<:MOI.SingleVariable},
                        ::Type{<:MOI.AbstractScalarSet}) where {T} = true
function added_constraint_types(::Type{SingleVariableBridge{T, S}}) where {T, S}
    return [(MOI.ScalarAffineFunction{T}, S)]
end
function concrete_bridge_type(::Type{<:SingleVariableBridge{T}},
                              ::Type{MOI.SingleVariable},
                              S::Type{<:MOI.AbstractScalarSet}) where T
    return SingleVariableBridge{T, S}
end

# Attributes, Bridge acting as an model
MOI.get(b::SingleVariableBridge{T, S}, ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, S}) where {T, S} = 1
MOI.get(b::SingleVariableBridge{T, S}, ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, S}) where {T, S} = [b.constraint]

# Indices
function MOI.delete(model::MOI.ModelLike, c::SingleVariableBridge)
    MOI.delete(model, c.constraint)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal, c::SingleVariableBridge)
    return MOI.get(model, attr, c.constraint)
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintDual, c::SingleVariableBridge)
    return MOI.get(model, a, c.constraint)
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintBasisStatus,
                 bridge::SingleVariableBridge)
    return MOI.get(model, attr, bridge.constraint)
end


# Constraints
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintFunction,
                 c::SingleVariableBridge{T}, f::MOI.SingleVariable) where {T}
    MOI.set(model, MOI.ConstraintFunction(), c.constraint, MOI.ScalarAffineFunction{T}(f))
end
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintSet,
                 c::SingleVariableBridge{T, S}, change::S) where {T, S}
    MOI.set(model, MOI.ConstraintSet(), c.constraint, change)
end

# vector version

"""
    VectorOfVariablesBridge{T, S}

The `VectorOfVariablesBridge` converts a constraint `VectorOfVariables`-in-`S`
into the constraint `VectorAffineFunction{T}`-in-`S`.
"""
struct VectorOfVariablesBridge{T, S} <: AbstractBridge
    constraint::CI{MOI.VectorAffineFunction{T}, S}
end
function VectorOfVariablesBridge{T, S}(model, f::MOI.VectorOfVariables, s::S) where {T, S}
    constraint = MOI.add_constraint(model, MOI.VectorAffineFunction{T}(f), s)
    return VectorOfVariablesBridge{T, S}(constraint)
end

MOI.supports_constraint(::Type{VectorOfVariablesBridge{T}},
                        ::Type{MOI.VectorOfVariables},
                        ::Type{<:MOI.AbstractVectorSet}) where {T} = true
function added_constraint_types(::Type{VectorOfVariablesBridge{T, S}}) where {T, S}
    return [(MOI.VectorAffineFunction{T}, S)]
end
function concrete_bridge_type(::Type{<:VectorOfVariablesBridge{T}},
                              ::Type{<:MOI.AbstractVectorFunction},
                              S::Type{<:MOI.AbstractVectorSet}) where T
    return VectorOfVariablesBridge{T, S}
end

# Attributes, Bridge acting as an model
MOI.get(b::VectorOfVariablesBridge{T, S},
        ::MOI.NumberOfConstraints{MOI.VectorAffineFunction{T}, S}) where {T, S} = 1
MOI.get(b::VectorOfVariablesBridge{T, S},
        ::MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{T}, S}) where {T, S} = [b.constraint]

# Indices
function MOI.delete(model::MOI.ModelLike, bridge::VectorOfVariablesBridge)
    MOI.delete(model, bridge.constraint)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal,
                 bridge::VectorOfVariablesBridge)
    return MOI.get(model, attr, bridge.constraint)
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintDual,
                 bridge::VectorOfVariablesBridge)
    return MOI.get(model, a, bridge.constraint)
end

# Constraints
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintFunction,
                 bridge::VectorOfVariablesBridge{T},
                 func::MOI.VectorOfVariables) where {T}
    MOI.set(model, MOI.ConstraintFunction(), bridge.constraint,
            MOI.VectorAffineFunction{T}(func))
end
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintSet,
                 bridge::VectorOfVariablesBridge{T, S}, change::S) where {T, S}
    MOI.set(model, MOI.ConstraintSet(), bridge.constraint, change)
end
