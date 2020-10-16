# TODO The mutability of the coefficient type `T` is currently not exploited yet.
#      We first need to make sure that it is copied with `MA.copy_if_mutable` when it
#      is passed from one function to a mutable one.

import MutableArithmetics
const MA = MutableArithmetics

MA.mutability(::Type{<:TypedLike}) = MA.IsMutable()

function MA.mutable_copy(func::MOI.ScalarAffineFunction)
    terms = [MOI.ScalarAffineTerm(MA.copy_if_mutable(t.coefficient),
                                  t.variable_index) for t in func.terms]
    return MOI.ScalarAffineFunction(terms, MA.copy_if_mutable(func.constant))
end
function MA.mutable_copy(func::MOI.ScalarQuadraticFunction)
    affine_terms = [MOI.ScalarAffineTerm(
        MA.copy_if_mutable(t.coefficient),
        t.variable_index) for t in func.affine_terms]
    quadratic_terms = [MOI.ScalarQuadraticTerm(
        MA.copy_if_mutable(t.coefficient),
        t.variable_index_1,
        t.variable_index_2) for t in func.quadratic_terms]
    return MOI.ScalarQuadraticFunction(affine_terms, quadratic_terms,
                                       MA.copy_if_mutable(func.constant))
end

function MA.isequal_canonical(f::F, g::F) where F<:Union{
        MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction,
        MOI.VectorAffineFunction, MOI.VectorQuadraticFunction}

    return MA.isequal_canonical(MOI.constant(f), MOI.constant(g)) &&
        all(MOI.dict_compare.(MOI._dicts(f), MOI._dicts(g), MA.isequal_canonical))
end

function MA.iszero!(f::TypedScalarLike)
    return iszero(MOI.constant(f)) && _is_constant(canonicalize!(f))
end

function MA.scaling(f::TypedScalarLike{T}) where T
    g = canonical(f)
    if !_is_constant(g)
        throw(InexactError(:convert, T, f))
    end
    return MA.scaling(MOI.constant(g))
end

MA.promote_operation(::Union{typeof(zero), typeof(one)}, F::Type{<:TypedScalarLike}) = F

# To avoid type piracy, we add at least one `ScalarLike` outside of the `...`.
const PROMOTE_IMPLEMENTED_OP = Union{typeof(+), typeof(-), typeof(*), typeof(/)}
function MA.promote_operation(op::PROMOTE_IMPLEMENTED_OP,
                              F::Type{<:ScalarLike{T}},
                              G::Type{<:ScalarLike{T}}) where {T, N}
    promote_operation(op, T, F, G)
end
function MA.promote_operation(op::PROMOTE_IMPLEMENTED_OP, F::Type{T},
                              G::Type{<:TypedLike{T}}) where {T, N}
    promote_operation(op, T, F, G)
end
function MA.promote_operation(op::PROMOTE_IMPLEMENTED_OP, F::Type{<:TypedLike{T}},
                              G::Type{T}) where T
    promote_operation(op, T, F, G)
end
function MA.promote_operation(op::PROMOTE_IMPLEMENTED_OP, F::Type{<:Number},
                              G::Type{<:Union{MOI.SingleVariable, MOI.VectorOfVariables}})
    promote_operation(op, F, F, G)
end
function MA.promote_operation(op::PROMOTE_IMPLEMENTED_OP,
                              F::Type{<:Union{MOI.SingleVariable, MOI.VectorOfVariables}},
                              G::Type{<:Number})
    promote_operation(op, G, F, G)
end

function MA.mutable_operate!(op::Union{typeof(zero), typeof(one)}, f::MOI.ScalarAffineFunction)
    empty!(f.terms)
    f.constant = op(f.constant)
    return f
end
function MA.mutable_operate!(op::Union{typeof(zero), typeof(one)}, f::MOI.ScalarQuadraticFunction)
    empty!(f.affine_terms)
    empty!(f.quadratic_terms)
    f.constant = op(f.constant)
    return f
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
function MA.mutable_operate_to!(
    output::MOI.ScalarAffineFunction{T}, op::Union{typeof(+), typeof(-)},
    f::MOI.ScalarAffineFunction{T}, g::MOI.ScalarAffineFunction{T}) where T

    empty!(output.terms)
    append!(output.terms, f.terms)
    append!(output.terms, operate_terms(op, g.terms))
    output.constant = op(f.constant, g.constant)
    return output
end
function MA.mutable_operate_to!(
    output::MOI.ScalarQuadraticFunction{T}, op::Union{typeof(+), typeof(-)},
    f::MOI.ScalarQuadraticFunction{T}, g::MOI.ScalarQuadraticFunction{T}) where T

    empty!(output.affine_terms)
    append!(output.affine_terms, f.affine_terms)
    append!(output.affine_terms, operate_terms(op, g.affine_terms))
    empty!(output.quadratic_terms)
    append!(output.quadratic_terms, f.quadratic_terms)
    append!(output.quadratic_terms, operate_terms(op, g.quadratic_terms))
    output.constant = op(f.constant, g.constant)
    return output
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

_constant(::Type{T}, α::T) where {T} = α
_constant(::Type{T}, ::MOI.SingleVariable) where {T} = zero(T)
_constant(::Type{T}, func::TypedScalarLike{T}) where {T} = MOI.constant(func)

_affine_terms(f::MOI.ScalarAffineFunction) = f.terms
_affine_terms(f::MOI.ScalarQuadraticFunction) = f.affine_terms

function _add_sub_affine_terms(
    op::Union{typeof(+), typeof(-)}, terms::Vector{MOI.ScalarAffineTerm{T}},
    α::T, f::MOI.SingleVariable, β::T) where T
    push!(terms, MOI.ScalarAffineTerm(op(α * β), f.variable))
    return
end
function _add_sub_affine_terms(
    op::Union{typeof(+), typeof(-)}, terms::Vector{MOI.ScalarAffineTerm{T}},
    f::MOI.SingleVariable, β::T) where T
    push!(terms, MOI.ScalarAffineTerm(op(β), f.variable))
    return
end
function _add_sub_affine_terms(
    op::Union{typeof(+), typeof(-)}, terms::Vector{MOI.ScalarAffineTerm{T}},
    α::T, f::MOI.SingleVariable) where T
    push!(terms, MOI.ScalarAffineTerm(op(α), f.variable))
    return
end
function _add_sub_affine_terms(
    op::Union{typeof(+), typeof(-)}, terms::Vector{MOI.ScalarAffineTerm{T}},
    f::MOI.SingleVariable) where T
    push!(terms, MOI.ScalarAffineTerm(op(one(T)), f.variable))
    return
end

function _add_sub_affine_terms(
    op::Union{typeof(+), typeof(-)}, terms::Vector{MOI.ScalarAffineTerm{T}},
    α::T, f::TypedScalarLike{T}, β::T) where T
    for t in _affine_terms(f)
        push!(terms, operate_term(op, operate_term(*, α, t, β)))
    end
end
function _add_sub_affine_terms(
    op::Union{typeof(+), typeof(-)}, terms::Vector{MOI.ScalarAffineTerm{T}},
    f::TypedScalarLike{T}, β::T) where T
    for t in _affine_terms(f)
        push!(terms, operate_term(op, operate_term(*, t, β)))
    end
end
function _add_sub_affine_terms(
    op::Union{typeof(+), typeof(-)}, terms::Vector{MOI.ScalarAffineTerm{T}},
    α::T, f::TypedScalarLike{T}) where T
    for t in _affine_terms(f)
        push!(terms, operate_term(op, operate_term(*, α, t)))
    end
end
function _add_sub_affine_terms(
    op::Union{typeof(+), typeof(-)}, terms::Vector{MOI.ScalarAffineTerm{T}},
    f::TypedScalarLike{T}) where T
    append!(terms, operate_terms(op, _affine_terms(f)))
    return
end
function _add_sub_affine_terms(
    op::Union{typeof(+), typeof(-)}, terms::Vector{MOI.ScalarAffineTerm{T}},
    args::Vararg{T, N}) where {T, N}
    return
end
function _add_sub_affine_terms(
    op::Union{typeof(+), typeof(-)}, terms::Vector{MOI.ScalarAffineTerm{T}},
    α::T, β::T, args::Vararg{ScalarQuadraticLike, N}) where {T, N}
    _add_sub_affine_terms(op, terms, α * β, args...)
end

function MA.mutable_operate!(op::MA.AddSubMul, f::MOI.ScalarAffineFunction{T},
                             args::Vararg{ScalarAffineLike{T}, N}) where {T, N}
    f.constant = op(f.constant, _constant.(T, args)...)
    _add_sub_affine_terms(MA.add_sub_op(op), f.terms, args...)
    return f
end

function _add_quadratic_terms(
    op::Union{typeof(+), typeof(-)}, terms::Vector{MOI.ScalarQuadraticTerm{T}},
    α::ScalarAffineLike{T}, f::MOI.ScalarQuadraticFunction{T},
    β::ScalarAffineLike{T}) where T

    for t in f.quadratic_terms
        push!(terms, operate_term(op, operate_term(*, _constant(T, α), t, _constant(T, β))))
    end
end
function _add_quadratic_terms(
    op::Union{typeof(+), typeof(-)}, terms::Vector{MOI.ScalarQuadraticTerm{T}},
    f::MOI.ScalarQuadraticFunction{T}, β::ScalarAffineLike{T}) where T

    for t in f.quadratic_terms
        push!(terms, operate_term(op, operate_term(*, t, _constant(T, β))))
    end
end
function _add_quadratic_terms(
    op::Union{typeof(+), typeof(-)}, terms::Vector{MOI.ScalarQuadraticTerm{T}},
    α::ScalarAffineLike{T}, f::MOI.ScalarQuadraticFunction{T}) where T
    for t in f.quadratic_terms
        push!(terms, operate_term(op, operate_term(*, _constant(T, α), t)))
    end
end
function _add_quadratic_terms(
    op::Union{typeof(+), typeof(-)}, terms::Vector{MOI.ScalarQuadraticTerm{T}},
    f::MOI.ScalarQuadraticFunction{T}) where T
    append!(terms, operate_terms(op, f.quadratic_terms))
    return
end
function _add_quadratic_terms(
    op::Union{typeof(+), typeof(-)}, terms::Vector{MOI.ScalarQuadraticTerm{T}},
    # Compiler fails in StackOverflowError on Julia v1.1
    #args::Vararg{ScalarAffineLike{T}, N}) where {T, N}
    args::ScalarAffineLike{T}) where T
    return
end
function _merge_constants(::Type{T}, α::ScalarAffineLike{T}, β::ScalarAffineLike{T},
                          args::Vararg{Any, N}) where {T, N}
    return (_constant(T, α) * _constant(T, β), args...)
end
function _add_quadratic_terms(
    op::Union{typeof(+), typeof(-)}, terms::Vector{MOI.ScalarQuadraticTerm{T}},
    args::Vararg{Any, N}) where {T, N}
    _add_quadratic_terms(op, terms, _merge_constants(T, args...)...)
end

_num_function_with_terms(::Type{T}, ::T) where {T} = 0
_num_function_with_terms(::Type{T}, ::ScalarLike{T}) where {T} = 1
function _num_function_with_terms(::Type{T}, f::ScalarQuadraticLike{T},
                                  args::Vararg{ScalarQuadraticLike{T}, N}) where {T, N}
    return _num_function_with_terms(T, f) + _num_function_with_terms(T, args...)
end
function MA.mutable_operate!(op::MA.AddSubMul, f::MOI.ScalarQuadraticFunction{T},
                             args::Vararg{ScalarQuadraticLike{T}, N}) where {T, N}
    if isone(_num_function_with_terms(T, args...))
        f.constant = op(f.constant, _constant.(T, args)...)
        _add_sub_affine_terms(MA.add_sub_op(op), f.affine_terms, args...)
        _add_quadratic_terms(MA.add_sub_op(op), f.quadratic_terms, args...)
        return f
    else
        return MA.mutable_operate!(MA.add_sub_op(op), f, *(args...))
    end
end
# `args` could be `(x', a)` where `a` is a vector of constants and `x` a vector
# of affine functions for instance.
function MA.mutable_operate!(op::MA.AddSubMul, f::TypedScalarLike, args::Vararg{Any, N}) where N
    return MA.mutable_operate!(MA.add_sub_op(op), f, *(args...))
end
