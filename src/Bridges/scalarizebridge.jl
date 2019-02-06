const VectorLPSet = Union{MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives}

vector_set(::T, ::MOI.Zeros) where T = MOI.EqualTo(zero(T))
vector_set(::T, ::MOI.Nonpositives) where T = MOI.LessThan(zero(T))
vector_set(::T, ::MOI.Nonnegatives) where T = MOI.GreaterThan(zero(T))

vector_set_type(::Type{<:MOI.Zeros}) = MOI.EqualTo
vector_set_type(::Type{<:MOI.Nonpositives}) = MOI.LessThan
vector_set_type(::Type{<:MOI.Nonnegatives}) = MOI.GreaterThan

vector_function_type(::Type{<:MOI.VectorOfVariables}) = MOI.SingleVariable
vector_function_type(::Type{<:MOI.VectorAffineFunction}) = MOI.ScalarAffineFunction
vector_function_type(::Type{<:MOI.VectorQuadraticFunction}) = MOI.ScalarQuadraticFunction

"""
    ScalarizeBridge{T}

Transforms a constraint `AbstractVectorFunction`-in-`S` where `S <: LPCone` to
`AbstactVectorFunction`-in-`vector_set_type(S)`.
"""
mutable struct ScalarizeBridge{T, F, S} <: AbstractBridge
    scalar_constraints::Vector{CI{F, S}}
end
function ScalarizeBridge{T, F, S}(model::MOI.ModelLike,
                                  f::MOI.AbstractVectorFunction,
                                  set::VectorLPSet) where {T, F, S}
    dimension = MOI.output_dimension(f)
    new_f = scalarize(f)
    constraints = CI{F,S}[]
    sizehint!(constraints, dimension)
    for i in 1:dim
        push!(constraints, MOI.add_constraint(model, vec[i], vector_set(T, set)))
    end
    return VectorizeBridge{T, F, S}(constraints)
end

function MOI.supports_constraint(::Type{VectorizeBridge{T}},
                                ::Type{<:MOI.AbstractScalarFunction},
                                ::Type{<:VectorLPSet}) where T
    return true
end
function added_constraint_types(::Type{VectorizeBridge{T, F, S}}) where {T, F, S}
    return [(F, S)]
end
function concrete_bridge_type(::Type{<:VectorizeBridge{T}},
                              F::Type{<:MOI.AbstractVectorFunction},
                              S::Type{<:VectorLPSet}) where T
    return VectorizeBridge{T, vector_function_type(F), vector_set_type(S)}
end

# Attributes, Bridge acting as an model
function MOI.get(bridge::VectorizeBridge{T, F, S},
                 ::MOI.NumberOfConstraints{F, S}) where {T, F, S}
    return length(bridge.scalar_constraints)
end
function MOI.get(bridge::VectorizeBridge{T, F, S},
                 ::MOI.ListOfConstraintIndices{F, S}) where {T, F, S}
    return bridge.scalar_constraints
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::VectorizeBridge)
    MOI.delete.(model, bridge.scalar_constraints)
end

# Attributes, Bridge acting as a constraint

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal,
                 bridge::VectorizeBridge)
   MOI.get.(model, attr, bridge.scalar_constraints)
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintDual,
                 bridge::VectorizeBridge)
    MOI.get.(model, attr, bridge.scalar_constraints)
end
function MOI.modify(model::MOI.ModelLike, bridge::VectorizeBridge,
    change::MOI.VectorConstantChange{T}) where T
    MOI.modify.(model, bridge.scalar_constraints,
        MOI.ScalarConstantChange{T}.(change.new_constant))
end
function MOI.modify(model::MOI.ModelLike, bridge::VectorizeBridge,
                    change::MOI.MultirowChange{T})
    for (index, value) in change.new_coefficients
        MOI.modify(model, bridge.scalar_constraints[index],
               MOI.ScalarCoefficientChange{T}(change.variable, value))
    end
    nothing
end
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintFunction,
    bridge::VectorSlackBridge{T}, func) where T
    new_func = scalarize(func)
    MOI.set.(model, MOI.ConstraintFunction(), bridge.scalar_constraints, new_func)
end
