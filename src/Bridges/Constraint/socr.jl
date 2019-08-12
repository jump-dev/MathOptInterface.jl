"""
    SOCRBridge{T, F, G}

The `SecondOrderCone` is `RotatedSecondOrderCone`. We simply do the inverse
transformation of [`RSOCBridge`](@ref). In fact, as the transformation is an
involution, we do the same transformation.

"""
struct SOCRBridge{T, F, G} <: AbstractBridge
    rsoc::CI{F, MOI.RotatedSecondOrderCone}
end
function bridge_constraint(::Type{SOCRBridge{T, F, G}}, model,
                           f::MOI.AbstractVectorFunction,
                           s::MOI.SecondOrderCone) where {T, F, G}
    soc = MOI.add_constraint(model, rotate_function(f, T),
                             MOI.RotatedSecondOrderCone(MOI.dimension(s)))
    return SOCRBridge{T, F, G}(soc)
end

function MOI.supports_constraint(::Type{SOCRBridge{T}},
                                ::Type{<:MOI.AbstractVectorFunction},
                                ::Type{MOI.SecondOrderCone}) where T
    return true
end
MOIB.added_constrained_variable_types(::Type{<:SOCRBridge}) = Tuple{DataType}[]
function MOIB.added_constraint_types(::Type{<:SOCRBridge{T, F}}) where {T, F}
    return [(F, MOI.RotatedSecondOrderCone)]
end
function concrete_bridge_type(::Type{<:SOCRBridge{T}},
                              G::Type{<:MOI.AbstractVectorFunction},
                              ::Type{MOI.SecondOrderCone}) where T
    S = MOIU.promote_operation(/, T, MOIU.scalar_type(G), T)
    Y = MOIU.promote_operation(-, T, S, S)
    Z = MOIU.promote_operation(+, T, S, S)
    F = MOIU.promote_operation(vcat, T, Z, Y, G)
    return SOCRBridge{T, F, G}
end

# Attributes, Bridge acting as an model
function MOI.get(b::SOCRBridge{T, F},
                 ::MOI.NumberOfConstraints{F, MOI.RotatedSecondOrderCone}) where {T, F}
    return 1
end
function MOI.get(b::SOCRBridge{T, F},
                 ::MOI.ListOfConstraintIndices{F, MOI.RotatedSecondOrderCone}) where {T, F}
    return [b.rsoc]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::SOCRBridge)
    MOI.delete(model, bridge.rsoc)
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintFunction,
                 bridge::SOCRBridge{T, F, G}) where {T, F, G}
    # As it is an involution, we can just reapply the same transformation
    func = MOI.get(model, attr, bridge.rsoc)
    return MOIU.convert_approx(G, rotate_function(func, T))
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintSet, bridge::SOCRBridge)
    set = MOI.get(model, attr, bridge.rsoc)
    return MOI.SecondOrderCone(MOI.dimension(set))
end
# As the linear transformation is a symmetric involution,
# the constraint primal and dual both need to be processed by reapplying the same transformation
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal,
                 bridge::SOCRBridge)
    return rotate_result(model, attr, bridge.rsoc)
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintDual,
                 bridge::SOCRBridge)
    return rotate_result(model, attr, bridge.rsoc)
end
