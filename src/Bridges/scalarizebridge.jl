const ConicSet = Union{MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives}
const LPSet{T} = Union{MOI.EqualTo{T}, MOI.LessThan{T}, MOI.GreaterThan{T}}

vector_set(::T, ::MOI.Zeros) where T = MOI.EqualTo(zero(T))
vector_set(::T, ::MOI.Nonpositives) where T = MOI.LessThan(zero(T))
vector_set(::T, ::MOI.Nonnegatives) where T = MOI.GreaterThan(zero(T))

vector_set_type(::Type{<:MOI.Zeros}) = MOI.EqualTo
vector_set_type(::Type{<:MOI.Nonpositives}) = MOI.LessThan
vector_set_type(::Type{<:MOI.Nonnegatives}) = MOI.GreaterThan

"""
    ScalarizeBridge{T}

Transforms a constraint `AbstractVectorFunction`-in-`S` where `S <: LPCone` to
`AbstactVectorFunction`-in-`vector_set_type(S)`.
"""
mutable struct ScalarizeBridge{T, F, S} <: AbstractBridge
    scalar_constraint::Vector{CI{F, S}}
end
function ScalarizeBridge{T, F, S}(model::MOI.ModelLike,
                                  f::MOI.AbstractVectorFunction,
                                  set::MOI.AbstractVectorSet) where {T, F, S}
    dim = MOI.output_dimension(f)
    constants = MOI._constant(f)
    vec = MOI.ScalarAffineFunction.(MOI.ScalarAffineTerm{T}[], constants)
    for term in f.terms
        push!(vec[term.output_dimension], term.scalar_term)
    end
    constraints = [CI{F,S}(0) for i in 1:dim]
    for i in 1:dim
        constraints[i] = MOI.add_constraint(model, vec[i], vector_set(T, set))
    end
    return VectorizeBridge{T, F, S}(constraints)
end

function MOI.supports_constraint(::Type{VectorizeBridge{T}},
                                ::Type{<:MOI.AbstractScalarFunction},
                                ::Type{<:LPCone{T}}) where T
    return true
end
function added_constraint_types(::Type{VectorizeBridge{T, F, S}}) where {T, F, S}
    return [(F, S)]
end
function concrete_bridge_type(::Type{<:VectorizeBridge{T}},
                              F::Type{<:MOI.AbstractScalarFunction},
                              S::Type{<:LPCone{T}}) where T
    G = MOIU.promote_operation(-, T, F, T)
    H = MOIU.promote_operation(vcat, T, G)
    return VectorizeBridge{T, H, vector_set_type(S)}
end

# Attributes, Bridge acting as an model
function MOI.get(::VectorizeBridge{T, F, S},
                 ::MOI.NumberOfConstraints{F, S}) where {T, F, S}
    return 1
end
function MOI.get(bridge::VectorizeBridge{T, F, S},
                 ::MOI.ListOfConstraintIndices{F, S}) where {T, F, S}
    return [bridge.vector_constraint]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::VectorizeBridge)
    MOI.delete(model, bridge.vector_constraint)
end

# Attributes, Bridge acting as a constraint

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal,
                 bridge::VectorizeBridge)
    x = MOI.get(model, attr, bridge.vector_constraint)
    @assert length(x) == 1
    y = x[1]
    status = MOI.get(model, MOI.PrimalStatus(attr.N))
    if status != MOI.INFEASIBILITY_CERTIFICATE &&
       status != MOI.NEARLY_INFEASIBILITY_CERTIFICATE
       # If it is an infeasibility certificate, it is a ray and satisfies the
       # homogenized problem, see https://github.com/JuliaOpt/MathOptInterface.jl/issues/433
       # Otherwise, we need to add the set constant since the ConstraintPrimal
       # is defined as the value of the function and the set_constant was
       # removed from the original function
       y += bridge.set_constant
   end
   return y
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintDual,
                 bridge::VectorizeBridge)
    x = MOI.get(model, attr, bridge.vector_constraint)
    @assert length(x) == 1
    return x[1]
end
function MOI.modify(model::MOI.ModelLike, bridge::VectorizeBridge,
                    change::MOI.ScalarCoefficientChange)
    MOI.modify(model, bridge.vector_constraint,
               MOI.MultirowChange(change.variable,
                                  [(1, change.new_coefficient)]))
end
function MOI.set(model::MOI.ModelLike, ::MOI.ConstraintSet,
                 bridge::VectorizeBridge, new_set::LPCone)
    bridge.set_constant = MOIU.getconstant(new_set)
    MOI.modify(model, bridge.vector_constraint,
               MOI.VectorConstantChange([-bridge.set_constant]))
end
