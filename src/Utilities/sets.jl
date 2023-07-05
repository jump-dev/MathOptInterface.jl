# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    shift_constant(set::MOI.AbstractScalarSet, offset)

Returns a new scalar set `new_set` such that `func`-in-`set` is equivalent to
`func + offset`-in-`new_set`.

Only define this function if it makes sense to!

Use [`supports_shift_constant`](@ref) to check if the set supports shifting:
```Julia
if supports_shift_constant(typeof(old_set))
    new_set = shift_constant(old_set, offset)
    f.constant = 0
    add_constraint(model, f, new_set)
else
    add_constraint(model, f, old_set)
end
```

See also [`supports_shift_constant`](@ref).

## Examples

The call `shift_constant(MOI.Interval(-2, 3), 1)` is equal to
`MOI.Interval(-1, 4)`.
"""
function shift_constant end

"""
    supports_shift_constant(::Type{S}) where {S<:MOI.AbstractSet}

Return `true` if [`shift_constant`](@ref) is defined for set `S`.

See also [`shift_constant`](@ref).
"""
supports_shift_constant(::Type{S}) where {S<:MOI.AbstractSet} = false

function shift_constant(set::MOI.LessThan, offset)
    return MOI.LessThan(MOI.constant(set) + offset)
end
supports_shift_constant(::Type{<:MOI.LessThan}) = true

function shift_constant(set::MOI.GreaterThan, offset)
    return MOI.GreaterThan(MOI.constant(set) + offset)
end
supports_shift_constant(::Type{<:MOI.GreaterThan}) = true

function shift_constant(set::MOI.EqualTo, offset)
    return MOI.EqualTo(MOI.constant(set) + offset)
end
supports_shift_constant(::Type{<:MOI.EqualTo}) = true

function shift_constant(set::MOI.Interval, offset)
    return MOI.Interval(set.lower + offset, set.upper + offset)
end
supports_shift_constant(::Type{<:MOI.Interval}) = true

function shift_constant(set::MOI.Parameter, offset)
    return MOI.Parameter(MOI.constant(set) + offset)
end
supports_shift_constant(::Type{<:MOI.Parameter}) = true

"""
    ScalarLinearSet{T}

The union of scalar-valued linear sets with element type `T`.

This is used in the vectorize and scalarize bridges.

See also: [`VectorLinearSet`](@ref).
"""
const ScalarLinearSet{T} =
    Union{MOI.EqualTo{T},MOI.LessThan{T},MOI.GreaterThan{T}}

"""
    VectorLinearSet

The union of vector-valued linear cones.

This is used in the vectorize and scalarize bridges.

See also: [`ScalarLinearSet`](@ref).
"""
const VectorLinearSet = Union{MOI.Zeros,MOI.Nonnegatives,MOI.Nonpositives}

"""
    vector_set_type(::Type{S}) where {S}

A utility function to map scalar sets `S` to their vector equivalents.

This is used in the vectorize and scalarize bridges.

See also: [`scalar_set_type`](@ref).
"""
vector_set_type(::Type{<:MOI.EqualTo}) = MOI.Zeros
vector_set_type(::Type{<:MOI.LessThan}) = MOI.Nonpositives
vector_set_type(::Type{<:MOI.GreaterThan}) = MOI.Nonnegatives

"""
    scalar_set_type(::Type{S}, ::Type{T}) where {S,T}

A utility function to map vector sets `S` to their scalar equivalents with
element type `T`.

This is used in the vectorize and scalarize bridges.

See also: [`vector_set_type`](@ref).
"""
scalar_set_type(::Type{<:MOI.Zeros}, T::Type) = MOI.EqualTo{T}
scalar_set_type(::Type{<:MOI.Nonpositives}, T::Type) = MOI.LessThan{T}
scalar_set_type(::Type{<:MOI.Nonnegatives}, T::Type) = MOI.GreaterThan{T}

"""
    is_diagonal_vectorized_index(index::Base.Integer)

Return whether `index` is the index of a diagonal element in a
[`MOI.AbstractSymmetricMatrixSetTriangle`](@ref) set.
"""
function is_diagonal_vectorized_index(index::Base.Integer)
    # See https://en.wikipedia.org/wiki/Triangular_number#Triangular_roots_and_tests_for_triangular_numbers
    perfect_square = 1 + 8index
    return isqrt(perfect_square)^2 == perfect_square
end

"""
    side_dimension_for_vectorized_dimension(n::Integer)

Return the dimension `d` such that
`MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(d))` is `n`.
"""
function side_dimension_for_vectorized_dimension(n::Base.Integer)
    # We have `d*(d+1)/2 = n` so
    # `d² + d - 2n = 0` hence `d = (-1 ± √(1 + 8d)) / 2`
    # The integer `√(1 + 8d)` is odd and `√(1 + 8d) - 1` is even.
    # We can drop the `- 1` as `div` already discards it.
    return div(isqrt(1 + 8n), 2)
end

"""
    trimap(row::Integer, column::Integer)

Convert between the row and column indices of a matrix, to the linear index of
the corresponding element in the triangular representation.

This is most useful when mapping between `ConeSquare` and `ConeTriangle` sets,
e.g., as part of an [`MOI.AbstractSymmetricMatrixSetTriangle`](@ref) set.
"""
function trimap(row::Integer, column::Integer)
    if row < column
        return trimap(column, row)
    end
    return div((row - 1) * row, 2) + column
end

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
        return 2one(T)
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
"""
struct SetDotScalingVector{T,S<:MOI.AbstractSet} <: AbstractVector{T}
    set::S
    len::Int
end

function Base.getindex(s::SetDotScalingVector{T}, i::Base.Integer) where {T}
    return sqrt(_set_dot(i, s.set, T))
end

Base.size(x::SetDotScalingVector) = (x.len,)

function symmetric_matrix_scaling_vector(::Type{T}, n) where {T}
    d = MOI.side_dimension_for_vectorized_dimension(n)
    set = MOI.ScaledPositiveSemidefiniteConeTriangle(d)
    return SetDotScalingVector(set, n)
end

function symmetric_matrix_inverse_scaling_vector(::Type{T}, n) where {T}
    return LazyMap{T}(inv, symmetric_matrix_scaling_vector(T, n))
end


similar_type(::Type{<:MOI.LessThan}, ::Type{T}) where {T} = MOI.LessThan{T}

function similar_type(::Type{<:MOI.GreaterThan}, ::Type{T}) where {T}
    return MOI.GreaterThan{T}
end

similar_type(::Type{<:MOI.EqualTo}, ::Type{T}) where {T} = MOI.EqualTo{T}

similar_type(::Type{<:MOI.Interval}, ::Type{T}) where {T} = MOI.Interval{T}

function convert_approx(::Type{MOI.LessThan{T}}, set::MOI.LessThan) where {T}
    return MOI.LessThan{T}(set.upper)
end

function convert_approx(
    ::Type{MOI.GreaterThan{T}},
    set::MOI.GreaterThan,
) where {T}
    return MOI.GreaterThan{T}(set.lower)
end

function convert_approx(::Type{MOI.EqualTo{T}}, set::MOI.EqualTo) where {T}
    return MOI.EqualTo{T}(set.value)
end

function convert_approx(::Type{MOI.Interval{T}}, set::MOI.Interval) where {T}
    return MOI.Interval{T}(set.lower, set.upper)
end
