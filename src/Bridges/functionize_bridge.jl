# scalar version

"""
    ScalarFunctionizeBridge{T, S}

The `ScalarFunctionizeBridge` converts a constraint `SingleVariable`-in-`S`
into the constraint `ScalarAffineFunction{T}`-in-`S`.
"""
struct ScalarFunctionizeBridge{T, S} <: AbstractBridge
    constraint::CI{MOI.ScalarAffineFunction{T}, S}
end
function ScalarFunctionizeBridge{T, S}(model, f::MOI.SingleVariable, s::S) where {T, S}
    constraint = MOI.add_constraint(model, MOI.ScalarAffineFunction{T}(f), s)
    return ScalarFunctionizeBridge{T, S}(constraint)
end

# start allowing everything (scalar)
MOI.supports_constraint(::Type{ScalarFunctionizeBridge{T}},
                        ::Type{<:MOI.SingleVariable},
                        ::Type{<:MOI.AbstractScalarSet}) where {T} = true
function added_constraint_types(::Type{ScalarFunctionizeBridge{T, S}}) where {T, S}
    return [(MOI.ScalarAffineFunction{T}, S)]
end
function concrete_bridge_type(::Type{<:ScalarFunctionizeBridge{T}},
                              ::Type{MOI.SingleVariable},
                              S::Type{<:MOI.AbstractScalarSet}) where T
    return ScalarFunctionizeBridge{T, S}
end

# Attributes, Bridge acting as an model
MOI.get(b::ScalarFunctionizeBridge{T, S}, ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, S}) where {T, S} = 1
MOI.get(b::ScalarFunctionizeBridge{T, S}, ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, S}) where {T, S} = [b.constraint]

# Indices
function MOI.delete(model::MOI.ModelLike, c::ScalarFunctionizeBridge)
    MOI.delete(model, c.constraint)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal, c::ScalarFunctionizeBridge)
    return MOI.get(model, attr, c.constraint)
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintDual, c::ScalarFunctionizeBridge)
    return MOI.get(model, a, c.constraint)
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintBasisStatus,
                 bridge::ScalarFunctionizeBridge)
    return MOI.get(model, attr, bridge.constraint)
end


# Constraints
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintFunction,
                 c::ScalarFunctionizeBridge{T}, f::MOI.SingleVariable) where {T}
    MOI.set(model, MOI.ConstraintFunction(), c.constraint, MOI.ScalarAffineFunction{T}(f))
end
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintSet,
                 c::ScalarFunctionizeBridge{T, S}, change::S) where {T, S}
    MOI.set(model, MOI.ConstraintSet(), c.constraint, change)
end

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintFunction,
                 b::ScalarFunctionizeBridge)
    f = MOIU.canonical(MOI.get(model, attr, b.constraint))
    return convert(MOI.SingleVariable, f)
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintSet,
                 b::ScalarFunctionizeBridge)
    return MOI.get(model, attr, b.constraint)
end

# vector version

"""
    VectorFunctionizeBridge{T, S}

The `VectorFunctionizeBridge` converts a constraint `VectorOfVariables`-in-`S`
into the constraint `VectorAffineFunction{T}`-in-`S`.
"""
struct VectorFunctionizeBridge{T, S} <: AbstractBridge
    constraint::CI{MOI.VectorAffineFunction{T}, S}
end
function VectorFunctionizeBridge{T, S}(model, f::MOI.VectorOfVariables, s::S) where {T, S}
    constraint = MOI.add_constraint(model, MOI.VectorAffineFunction{T}(f), s)
    return VectorFunctionizeBridge{T, S}(constraint)
end

MOI.supports_constraint(::Type{VectorFunctionizeBridge{T}},
                        ::Type{MOI.VectorOfVariables},
                        ::Type{<:MOI.AbstractVectorSet}) where {T} = true
function added_constraint_types(::Type{VectorFunctionizeBridge{T, S}}) where {T, S}
    return [(MOI.VectorAffineFunction{T}, S)]
end
function concrete_bridge_type(::Type{<:VectorFunctionizeBridge{T}},
                              ::Type{<:MOI.AbstractVectorFunction},
                              S::Type{<:MOI.AbstractVectorSet}) where T
    return VectorFunctionizeBridge{T, S}
end

# Attributes, Bridge acting as an model
MOI.get(b::VectorFunctionizeBridge{T, S},
        ::MOI.NumberOfConstraints{MOI.VectorAffineFunction{T}, S}) where {T, S} = 1
MOI.get(b::VectorFunctionizeBridge{T, S},
        ::MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{T}, S}) where {T, S} = [b.constraint]

# Indices
function MOI.delete(model::MOI.ModelLike, bridge::VectorFunctionizeBridge)
    MOI.delete(model, bridge.constraint)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal,
                 bridge::VectorFunctionizeBridge)
    return MOI.get(model, attr, bridge.constraint)
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintDual,
                 bridge::VectorFunctionizeBridge)
    return MOI.get(model, a, bridge.constraint)
end

# Constraints
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintFunction,
                 bridge::VectorFunctionizeBridge{T},
                 func::MOI.VectorOfVariables) where {T}
    MOI.set(model, MOI.ConstraintFunction(), bridge.constraint,
            MOI.VectorAffineFunction{T}(func))
end
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintSet,
                 bridge::VectorFunctionizeBridge{T, S}, change::S) where {T, S}
    MOI.set(model, MOI.ConstraintSet(), bridge.constraint, change)
end

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintFunction,
                 b::VectorFunctionizeBridge)
    f = MOIU.canonical(MOI.get(model, attr, b.constraint))
    @assert all(iszero, f.constants)
    @assert length(f.terms) == MOI.output_dimension(f)
    @assert all(t -> isone(t.scalar_term.coefficient), f.terms)
    terms = sort(f.terms, by = t -> t.output_index)
    @assert all(i -> terms[i].output_index == i, 1:MOI.output_dimension(f))
    return MOI.VectorOfVariables([t.scalar_term.variable_index for t in terms])
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintSet,
                 b::VectorFunctionizeBridge)
    return MOI.get(model, attr, b.constraint)
end
