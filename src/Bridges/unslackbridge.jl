# scalar version

"""
    SingleVariableBridge{T, F, S}

The `SingleVariableBridge` converts a constraint `SingleVariable`-in-`S`
into the constraint `ScalarAffineFunction{T}`-in-`S`.
"""
struct SingleVariableBridge{T, F, S} <: AbstractBridge
    constraint::CI{F, S}
end
function SingleVariableBridge{T, F, S}(model, f::MOI.SingleVariable, s::S) where {T, F, S}
    constraint = MOI.add_constraint(model, MOI.ScalarAffineFunction{T}(f), s)
    return SingleVariableBridge{T, F, S}(constraint)
end

# start allowing everything (scalar)
MOI.supports_constraint(::Type{SingleVariableBridge{T}},
                        ::Type{<:MOI.SingleVariable},
                        ::Type{<:MOI.AbstractScalarSet}) where {T} = true
function added_constraint_types(::Type{SingleVariableBridge{T, F, S}}) where {T, F, S}
    return [(MOI.ScalarAffineFunction{T}, S)]
end
function concrete_bridge_type(::Type{<:SingleVariableBridge{T}},
                              ::Type{MOI.SingleVariable},
                              S::Type{<:MOI.AbstractScalarSet}) where T
    return SingleVariableBridge{T, MOI.ScalarAffineFunction{T}, S}
end

# Attributes, Bridge acting as an model
MOI.get(b::SingleVariableBridge{T, F, S}, ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, S}) where {T, F, S} = 1
MOI.get(b::SingleVariableBridge{T, F, S}, ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, S}) where {T, F, S} = [b.constraint]

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

# Constraints
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintFunction,
                 c::SingleVariableBridge{T}, f::MOI.SingleVariable) where {T}
    MOI.set(model, MOI.ConstraintFunction(), c.constraint, MOI.ScalarAffineFunction{T}(f))
end
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintSet,
                 c::SingleVariableBridge{T, F, S}, change::S) where {T, F, S}
    MOI.set(model, MOI.ConstraintSet(), c.constraint, change)
end

# vector version

"""
    VectorOfVariablesBridge{T, F, S}

The `VectorOfVariablesBridge` converts a constraint `VectorOfVariables`-in-`S`
into the constraint `VectorAffineFunction{T}`-in-`S`.
"""
struct VectorOfVariablesBridge{T, F, S} <: AbstractBridge
    constraint::CI{F, S}
end
function VectorOfVariablesBridge{T, F, S}(model, f::MOI.VectorOfVariables, s::S) where {T, F, S}
    constraint = MOI.add_constraint(model, MOI.VectorAffineFunction{T}(f), s)
    return VectorOfVariablesBridge{T, F, S}(constraint)
end

MOI.supports_constraint(::Type{VectorOfVariablesBridge{T}},
                        ::Type{MOI.VectorOfVariables},
                        ::Type{<:MOI.AbstractVectorSet}) where {T} = true
function added_constraint_types(::Type{VectorOfVariablesBridge{T, F, S}}) where {T, F<:MOI.AbstractVectorFunction, S}
    return [(MOI.VectorAffineFunction{T}, S)]
end
function concrete_bridge_type(::Type{<:VectorOfVariablesBridge{T}},
                              ::Type{<:MOI.AbstractVectorFunction},
                              S::Type{<:MOI.AbstractVectorSet}) where T
    return VectorOfVariablesBridge{T, MOI.VectorAffineFunction{T}, S}
end

# Attributes, Bridge acting as an model
MOI.get(b::VectorOfVariablesBridge{T, F, S}, 
        ::MOI.NumberOfConstraints{MOI.VectorAffineFunction{T}, S}) where {T, F, S} = 1
MOI.get(b::VectorOfVariablesBridge{T, F, S}, 
        ::MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{T}, S}) where {T, F, S} = [b.constraint]

# Indices
function MOI.delete(model::MOI.ModelLike, c::VectorOfVariablesBridge)
    MOI.delete(model, c.constraint)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal, c::VectorOfVariablesBridge)
    return MOI.get(model, attr, c.constraint)
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintDual, c::VectorOfVariablesBridge)
    return MOI.get(model, a, c.constraint)
end

# Constraints
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintFunction,
                 c::VectorOfVariablesBridge{T, F, S}, func::MOI.VectorOfVariables) where {T, F, S}
    MOI.set(model, MOI.ConstraintFunction(), c.constraint, MOI.VectorAffineFunction{T}(func))
end
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintSet,
                 c::VectorOfVariablesBridge{T,F,S}, change::S)  where {T, F, S}
    MOI.set(model, MOI.ConstraintSet(), c.constraint, change)
end
