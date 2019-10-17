import MutableArithmetics
const MA = MutableArithmetics

MA.mutability(::Type{<:TypedLike}) = MA.IsMutable()

# To avoid type piracy, we add at least one `ScalarLike` outside of the `...`.
function MA.promote_operation(op::Function, F::Type{<:ScalarLike{T}}, args::Vararg{Type{<:ScalarLike{T}}, N}) where {T, N}
    promote_operation(op, T, F, args...)
end
function MA.promote_operation(op::Function, F::Type{T}, args::Vararg{Type{<:TypedLike{T}}, N}) where {T, N}
    promote_operation(op, T, F, args...)
end
function MA.promote_operation(op::Function, F::Type{<:TypedLike{T}}, G::Type{T}) where T
    promote_operation(op, T, F, G)
end
function MA.promote_operation(op::Function, F::Type{<:Number}, G::Type{<:Union{MOI.SingleVariable, MOI.VectorOfVariables}})
    promote_operation(op, F, F, G)
end
function MA.promote_operation(op::Function, F::Type{<:Union{MOI.SingleVariable, MOI.VectorOfVariables}}, G::Type{<:Number})
    promote_operation(op, G, F, G)
end

function MA.mutable_operate!(::typeof(-), f::MOI.ScalarQuadraticFunction)
    operate_terms!(-, f.quadratic_terms)
    operate_terms!(-, f.affine_terms)
    f.constant = -f.constant
    return f
end
function MA.mutable_operate!(::typeof(-), f::MOI.ScalarAffineFunction)
    operate_terms!(-, f.terms)
    f.constant = -f.constant
    return f
end
function MA.mutable_operate!(op::Union{typeof(+), typeof(-)},
                             f::MOI.ScalarAffineFunction{T},
                             g::T) where T
    f.constant = op(f.constant, g)
    return f
end
function MA.mutable_operate!(op::Union{typeof(+), typeof(-)},
                             f::MOI.ScalarAffineFunction{T},
                             g::MOI.SingleVariable) where T
    push!(f.terms, MOI.ScalarAffineTerm(op(one(T)), g.variable))
    return f
end
function MA.mutable_operate!(op::Union{typeof(+), typeof(-)},
                             f::MOI.ScalarAffineFunction{T},
                             g::MOI.ScalarAffineFunction{T}) where T
    append!(f.terms, operate_terms(op, g.terms))
    f.constant = op(f.constant, g.constant)
    return f
end
function MA.mutable_operate!(op::Union{typeof(+), typeof(-)},
                             f::MOI.ScalarQuadraticFunction{T},
                             g::T) where T
    f.constant = op(f.constant, g)
    return f
end
function MA.mutable_operate!(op::Union{typeof(+), typeof(-)},
                             f::MOI.ScalarQuadraticFunction{T},
                             g::MOI.SingleVariable) where T
    push!(f.affine_terms, MOI.ScalarAffineTerm(op(one(T)), g.variable))
    return f
end
function MA.mutable_operate!(op::Union{typeof(+), typeof(-)},
                             f::MOI.ScalarQuadraticFunction{T},
                             g::MOI.ScalarAffineFunction{T}) where T
    append!(f.affine_terms, operate_terms(op, g.terms))
    f.constant = op(f.constant, g.constant)
    return f
end
function MA.mutable_operate!(op::Union{typeof(+), typeof(-)},
                             f::MOI.ScalarQuadraticFunction{T},
                             g::MOI.ScalarQuadraticFunction{T}) where T
    append!(f.affine_terms, operate_terms(op, g.affine_terms))
    append!(f.quadratic_terms, operate_terms(op, g.quadratic_terms))
    f.constant = op(f.constant, g.constant)
    return f
end
