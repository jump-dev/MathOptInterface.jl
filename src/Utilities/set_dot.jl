# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Scalar product. Any vector set defined that does not use the standard scalar
# product between vectors of ``R^n`` should redefine `set_dot` and
# `dot_coefficients`.

"""
    set_dot(x::AbstractVector, y::AbstractVector, set::AbstractVectorSet)

Return the scalar product between a vector `x` of the set `set` and a vector
`y` of the dual of the set `s`.
"""
function set_dot(x::AbstractVector, y::AbstractVector, ::MOI.AbstractVectorSet)
    return LinearAlgebra.dot(x, y)
end

"""
    set_dot(x, y, set::AbstractScalarSet)

Return the scalar product between a number `x` of the set `set` and a number
`y` of the dual of the set `s`.
"""
set_dot(x, y, ::MOI.AbstractScalarSet) = LinearAlgebra.dot(x, y)

function triangle_dot(
    x::AbstractVector{S},
    y::AbstractVector{T},
    dim::Int,
    offset::Int,
) where {S,T}
    U = MA.promote_operation(MA.add_mul, S, T)
    result = zero(U)
    k = offset
    for i in 1:dim
        for j in 1:i
            k += 1
            if i == j
                result = MA.add_mul!!(result, x[k], y[k])
            else
                result = MA.add_mul!!(result, 2, x[k], y[k])
            end
        end
    end
    return result
end

function set_dot(
    x::AbstractVector,
    y::AbstractVector,
    set::MOI.AbstractSymmetricMatrixSetTriangle,
)
    return triangle_dot(x, y, MOI.side_dimension(set), 0)
end

function set_dot(
    x::AbstractVector,
    y::AbstractVector,
    set::MOI.HermitianPositiveSemidefiniteConeTriangle,
)
    sym = MOI.PositiveSemidefiniteConeTriangle(set.side_dimension)
    I = Base.OneTo(MOI.dimension(sym))
    result = set_dot(view(x, I), view(y, I), sym)
    for k in (MOI.dimension(sym)+1):MOI.dimension(set)
        result = MA.add_mul!!(result, 2, x[k], y[k])
    end
    return result
end

function set_dot(
    x::AbstractVector,
    y::AbstractVector,
    set::MOI.RootDetConeTriangle,
)
    return x[1] * y[1] + triangle_dot(x, y, set.side_dimension, 1)
end

function set_dot(
    x::AbstractVector,
    y::AbstractVector,
    set::MOI.LogDetConeTriangle,
)
    return x[1] * y[1] + x[2] * y[2] + triangle_dot(x, y, set.side_dimension, 2)
end

"""
    dot_coefficients(a::AbstractVector, set::AbstractVectorSet)

Return the vector `b` such that for all vector `x` of the set `set`,
`set_dot(b, x, set)` is equal to `dot(a, x)`.
"""
function dot_coefficients(a::AbstractVector, ::MOI.AbstractVectorSet)
    return a
end

function triangle_coefficients!(
    b::AbstractVector{T},
    dim::Int,
    offset::Int,
) where {T}
    k = offset
    for i in 1:dim
        for j in 1:i
            k += 1
            if i != j
                b[k] /= 2
            end
        end
    end
end

function dot_coefficients(
    a::AbstractVector,
    set::MOI.AbstractSymmetricMatrixSetTriangle,
)
    b = copy(a)
    triangle_coefficients!(b, MOI.side_dimension(set), 0)
    return b
end

function dot_coefficients(
    a::AbstractVector,
    set::MOI.HermitianPositiveSemidefiniteConeTriangle,
)
    sym = MOI.PositiveSemidefiniteConeTriangle(set.side_dimension)
    b = dot_coefficients(a, sym)
    for k in (MOI.dimension(sym)+1):MOI.dimension(set)
        b[k] /= 2
    end
    return b
end

function dot_coefficients(a::AbstractVector, set::MOI.RootDetConeTriangle)
    b = copy(a)
    triangle_coefficients!(b, set.side_dimension, 1)
    return b
end

function dot_coefficients(a::AbstractVector, set::MOI.LogDetConeTriangle)
    b = copy(a)
    triangle_coefficients!(b, set.side_dimension, 2)
    return b
end

# For `SetDotScalingVector`, we would like to compute the dot product
# of canonical vectors in O(1) instead of O(n)
# See https://github.com/jump-dev/Dualization.jl/pull/135

"""
    struct ZeroVector{T} <: AbstractVector{T}
        n::Int
    end

Vector of length `n` with only zero elements.
"""
struct ZeroVector{T} <: AbstractVector{T}
    n::Int
end

Base.eltype(::Type{ZeroVector{T}}) where {T} = T

Base.length(v::ZeroVector) = v.n

Base.size(v::ZeroVector) = (v.n,)

function Base.getindex(::ZeroVector{T}, ::Integer) where {T}
    return zero(T)
end

"""
    struct CanonicalVector{T} <: AbstractVector{T}
        index::Int
        n::Int
    end

Vector of length `n` with only zero elements except at index `index` where
the element is one.
"""
struct CanonicalVector{T} <: AbstractVector{T}
    index::Int
    n::Int
end

Base.eltype(::Type{CanonicalVector{T}}) where {T} = T

Base.length(v::CanonicalVector) = v.n

Base.size(v::CanonicalVector) = (v.n,)

function Base.getindex(v::CanonicalVector{T}, i::Integer) where {T}
    return convert(T, i == v.index)
end

function Base.view(v::CanonicalVector{T}, I::AbstractUnitRange) where {T}
    if v.index in I
        return CanonicalVector{T}(v.index - first(I) + 1, length(I))
    else
        return ZeroVector{T}(length(I))
    end
end

# This is much faster than the default implementation that goes
# through all entries even if only one is nonzero.
function LinearAlgebra.dot(
    x::CanonicalVector{T},
    y::CanonicalVector{T},
) where {T}
    return convert(T, x.index == y.index)
end

function triangle_dot(
    x::CanonicalVector{T},
    y::CanonicalVector{T},
    ::Int,
    offset::Int,
) where {T}
    if x.index != y.index || x.index <= offset
        return zero(T)
    elseif is_diagonal_vectorized_index(x.index - offset)
        return one(T)
    else
        return T(2)
    end
end

function _set_dot(i::Integer, s::MOI.AbstractVectorSet, T::Type)
    vec = CanonicalVector{T}(i, MOI.dimension(s))
    return set_dot(vec, vec, s)
end

function _set_dot(::Integer, ::MOI.AbstractScalarSet, T::Type)
    return one(T)
end

"""
    struct SetDotScalingVector{T,S<:MOI.AbstractSet} <: AbstractVector{T}
        set::S
        len::Int
    end

Vector `s` of scaling for the entries of the vectorized form of
a vector `x` in `set` and `y` in `MOI.dual_set(set)` such that
`MOI.Utilities.set_dot(x, y) = LinearAlgebra.dot(s .* x, s .* y)`.

## Example

Combined with `LinearAlgebra`, this vector can be used to scale
a [`MOI.AbstractVectorFunction`](@ref).

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variables(model, 3);

julia> func = MOI.VectorOfVariables(x)
┌                    ┐
│MOI.VariableIndex(1)│
│MOI.VariableIndex(2)│
│MOI.VariableIndex(3)│
└                    ┘

julia> set = MOI.PositiveSemidefiniteConeTriangle(2)
MathOptInterface.PositiveSemidefiniteConeTriangle(2)

julia> MOI.add_constraint(model, func, MOI.Scaled(set))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, MathOptInterface.Scaled{MathOptInterface.PositiveSemidefiniteConeTriangle}}(1)

julia> a = MOI.Utilities.SetDotScalingVector{Float64}(set)
3-element MathOptInterface.Utilities.SetDotScalingVector{Float64, MathOptInterface.PositiveSemidefiniteConeTriangle}:
 1.0
 1.4142135623730951
 1.0

julia> using LinearAlgebra

julia> MOI.Utilities.operate(*, Float64, Diagonal(a), func)
┌                                             ┐
│0.0 + 1.0 MOI.VariableIndex(1)               │
│0.0 + 1.4142135623730951 MOI.VariableIndex(2)│
│0.0 + 1.0 MOI.VariableIndex(3)               │
└                                             ┘

julia> MOI.Utilities.operate(*, Float64, Diagonal(a), ones(3))
3-element Vector{Float64}:
 1.0
 1.4142135623730951
 1.0
```
"""
struct SetDotScalingVector{T,S<:MOI.AbstractSet} <: AbstractVector{T}
    set::S
end

function SetDotScalingVector{T}(s::MOI.AbstractSet) where {T}
    return SetDotScalingVector{T,typeof(s)}(s)
end

function Base.getindex(s::SetDotScalingVector{T}, i::Base.Integer) where {T}
    return sqrt(_set_dot(i, s.set, T))
end

Base.size(x::SetDotScalingVector) = (MOI.dimension(x.set),)

function symmetric_matrix_scaling_vector(::Type{T}, n) where {T}
    d = side_dimension_for_vectorized_dimension(n)
    set = MOI.PositiveSemidefiniteConeTriangle(d)
    return SetDotScalingVector{T}(set)
end

function symmetric_matrix_inverse_scaling_vector(::Type{T}, n) where {T}
    return lazy_map(T, inv, symmetric_matrix_scaling_vector(T, n))
end

"""
    struct SymmetricMatrixScalingVector{T} <: AbstractVector{T}
        no_scaling::T
        scaling::T
        len::Int
    end

Vector of scaling for the entries of the vectorized form of
a symmetric matrix. The values `no_scaling` and `scaling`
are stored in the `struct` to avoid creating a new one for each entry.

!!! warning
    This type is deprecated, use `SetDotScalingVector` instead.
"""
struct SymmetricMatrixScalingVector{T} <: AbstractVector{T}
    scaling::T
    no_scaling::T
    len::Int
end

function SymmetricMatrixScalingVector{T}(scaling::T, len::Int) where {T}
    return SymmetricMatrixScalingVector{T}(scaling, one(T), len)
end

function Base.getindex(s::SymmetricMatrixScalingVector, i::Base.Integer)
    if is_diagonal_vectorized_index(i)
        return s.no_scaling
    else
        return s.scaling
    end
end

Base.size(x::SymmetricMatrixScalingVector) = (x.len,)
