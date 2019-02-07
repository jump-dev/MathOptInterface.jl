const VectorLPSet = Union{MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives}

scalar_set_type(::Type{<:MOI.Zeros}) = MOI.EqualTo
scalar_set_type(::Type{<:MOI.Nonpositives}) = MOI.LessThan
scalar_set_type(::Type{<:MOI.Nonnegatives}) = MOI.GreaterThan

__constant(f::Union{MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction}, T::DataType) = MOIU._constant(f)
__constant(f::Union{MOI.VectorAffineFunction, MOI.VectorQuadraticFunction}, T::DataType) = MOIU._constant(f)
__constant(f::MOI.SingleVariable, T::DataType) = zero(T)
__constant(f::MOI.VectorOfVariables, T::DataType) = zeroS(T, length(f.variables))

"""
    ScalarizeBridge{T}

Transforms a constraint `AbstractVectorFunction`-in-`S` where `S <: LPCone` to
`AbstactVectorFunction`-in-`scalar_set_type(S)`.
"""
mutable struct ScalarizeBridge{T, F, S} <: AbstractBridge
    scalar_constraints::Vector{CI{F, S}}
    constants::Vector{T}
end
function ScalarizeBridge{T, F, S}(model::MOI.ModelLike,
                                  f::MOI.AbstractVectorFunction,
                                  set::VectorLPSet) where {T, F, S}
    dimension = MOI.output_dimension(f)
    constants = __constant(f, T)
    new_f = scalarize(f, true)
    constraints = Vector{CI{F, S}}(undef, dimension)
    for i in 1:dimension
        constraints[i] = MOI.add_scalar_constraint(model, new_f[i], scalar_set_type(set){T}(constants[i]))
    end
    return ScalarizeBridge{T, F, S}(constraints, constants)
end

function MOI.supports_constraint(::Type{ScalarizeBridge{T}},
                                 ::Type{<:MOI.AbstractScalarFunction},
                                 ::Type{<:VectorLPSet}) where T
    return true
end
function added_constraint_types(::Type{ScalarizeBridge{T, F, S}}) where {T, F, S}
    return [(F, S)]
end
function concrete_bridge_type(::Type{<:ScalarizeBridge{T}},
                              F::Type{<:MOI.AbstractVectorFunction},
                              S::Type{<:VectorLPSet}) where T
    return ScalarizeBridge{T, MOIU.scalar_type(F), scalar_set_type(S)}
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

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal,
                 bridge::ScalarizeBridge)
   MOI.get.(model, attr, bridge.scalar_constraints) .+ bridge.constants
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintDual,
                 bridge::ScalarizeBridge)
    MOI.get.(model, attr, bridge.scalar_constraints)
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
    bridge.constants = __constant(func, T)
    new_func = scalarize(func, true)
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