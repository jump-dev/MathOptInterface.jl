const VectorLinearSet = Union{MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives}

scalar_set_type(::Type{<:MOI.Zeros}, T::Type) = MOI.EqualTo{T}
scalar_set_type(::Type{<:MOI.Nonpositives}, T::Type) = MOI.LessThan{T}
scalar_set_type(::Type{<:MOI.Nonnegatives}, T::Type) = MOI.GreaterThan{T}

"""
    ScalarizeBridge{T, F, S}

Transforms a constraint `AbstractVectorFunction`-in-`vector_set(S)` where
`S <: LPCone{T}` to `F`-in-`S`.
"""
mutable struct ScalarizeBridge{T, F, S} <: AbstractBridge
    scalar_constraints::Vector{CI{F, S}}
    constants::Vector{T}
end
function bridge_constraint(::Type{ScalarizeBridge{T, F, S}},
                           model::MOI.ModelLike,
                           f::MOI.AbstractVectorFunction,
                           set::VectorLinearSet) where {T, F, S}
    dimension = MOI.output_dimension(f)
    constants = MOI.constant(f, T)
    new_f = MOIU.scalarize(f, true)
    constraints = Vector{CI{F, S}}(undef, dimension)
    for i in 1:dimension
        constraints[i] = MOIU.add_scalar_constraint(model, new_f[i], S(-constants[i]))
    end
    return ScalarizeBridge{T, F, S}(constraints, constants)
end

function MOI.supports_constraint(::Type{ScalarizeBridge{T}},
                                 ::Type{<:MOI.AbstractVectorFunction},
                                 ::Type{<:VectorLinearSet}) where T
    return true
end
function added_constraint_types(::Type{ScalarizeBridge{T, F, S}}) where {T, F, S}
    return [(F, S)]
end
function concrete_bridge_type(::Type{<:ScalarizeBridge{T}},
                              F::Type{<:MOI.AbstractVectorFunction},
                              S::Type{<:VectorLinearSet}) where T
    return ScalarizeBridge{T, MOIU.scalar_type(F), scalar_set_type(S, T)}
end

# Attributes, Bridge acting as an model
function MOI.get(bridge::ScalarizeBridge{T, F, S},
                 ::MOI.NumberOfConstraints{F, S}) where {T, F, S}
    return length(bridge.scalar_constraints)
end
function MOI.get(bridge::ScalarizeBridge{T, F, S},
                 ::MOI.ListOfConstraintIndices{F, S}) where {T, F, S}
    return bridge.scalar_constraints
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::ScalarizeBridge)
    MOI.delete.(model, bridge.scalar_constraints)
    nothing
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintFunction,
                 bridge::ScalarizeBridge{T, F, S}) where {T, F, S}
    func = MOIU.vectorize(MOI.get.(model, attr, bridge.scalar_constraints))
    if F != MOI.SingleVariable
        func = MOIU.operate!(+, T, func, bridge.constants)
    end
    return func
end
function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet,
                 bridge::ScalarizeBridge{T, F, S}) where {T, F, S}
    return vector_set_type(S)(length(bridge.constants))
end

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal,
                 bridge::ScalarizeBridge)
    return MOI.get.(model, attr, bridge.scalar_constraints) .+ bridge.constants
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintDual,
                 bridge::ScalarizeBridge)
    return MOI.get.(model, attr, bridge.scalar_constraints)
end
function MOI.modify(model::MOI.ModelLike, bridge::ScalarizeBridge{T,F,S},
    change::MOI.VectorConstantChange{T}) where {T,F,S}
    bridge.constants = change.new_constant
    MOI.set.(model, MOI.ConstraintSet(), bridge.scalar_constraints,
             S.(-change.new_constant))
end
function MOI.modify(model::MOI.ModelLike, bridge::ScalarizeBridge,
                    change::MOI.MultirowChange{T}) where T
    for (index, value) in change.new_coefficients
        MOI.modify(model, bridge.scalar_constraints[index],
               MOI.ScalarCoefficientChange{T}(change.variable, value))
    end
    nothing
end
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintFunction,
    bridge::ScalarizeBridge{T}, func) where T
    old_constants = bridge.constants
    bridge.constants = MOI.constant(func, T)
    new_func = MOIU.scalarize(func, true)
    MOI.set.(model, MOI.ConstraintFunction(), bridge.scalar_constraints,
             new_func)
    for i in eachindex(bridge.constants)
        if bridge.constants[i] != old_constants[i]
            MOI.set(model, MOI.ConstraintSet(), bridge.scalar_constraints,
                     S(-bridge.constants[i]))
        end
    end
end

# TODO implement transform
