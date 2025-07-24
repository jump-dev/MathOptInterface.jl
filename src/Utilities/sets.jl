# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    shift_constant(set::MOI.AbstractScalarSet, offset)

Returns a new scalar set `new_set` such that `func`-in-`set` is equivalent to
`func + offset`-in-`new_set`.

Use [`supports_shift_constant`](@ref) to check if the set supports shifting:
```Julia
if MOI.Utilities.supports_shift_constant(typeof(set))
    new_set = MOI.Utilities.shift_constant(set, -func.constant)
    func.constant = 0
    MOI.add_constraint(model, func, new_set)
else
    MOI.add_constraint(model, func, set)
end
```

## Note for developers

Only define this function if it makes sense and you have implemented
[`supports_shift_constant`](@ref) to return `true`.

## Example

```jldoctest
julia> set = MOI.Interval(-2.0, 3.0)
MathOptInterface.Interval{Float64}(-2.0, 3.0)

julia> MOI.Utilities.supports_shift_constant(typeof(set))
true

julia> MOI.Utilities.shift_constant(set, 1.0)
MathOptInterface.Interval{Float64}(-1.0, 4.0)
```
"""
function shift_constant end

"""
    supports_shift_constant(::Type{S}) where {S<:MOI.AbstractSet}

Return `true` if [`shift_constant`](@ref) is defined for set `S`.

See also [`shift_constant`](@ref).

## Example

```jldoctest
julia> MOI.Utilities.supports_shift_constant(MOI.Interval{Float64})
true

julia> MOI.Utilities.supports_shift_constant(MOI.ZeroOne)
false
```
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

# We have `d*(d+1)/2 = n` so `2n = d^2 + d` hence
# `d^2 ≤ 2n < d^2 + 2d + 1 = (d + 1)^2`
# this means that `d` is the largest natural number `d`
# such that `d^2 ≤ 2n` hence `d = isqrt(2n)`.
"""
    side_dimension_for_vectorized_dimension(n::Integer)

Return the dimension `d` such that
`MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(d))` is `n`.
"""
side_dimension_for_vectorized_dimension(n::Base.Integer) = isqrt(2n)

"""
    trimap(row::Integer, column::Integer)

Convert between the row and column indices of a matrix, to the linear index of
the corresponding element in the triangular representation.

This is most useful when mapping between `ConeSquare` and `ConeTriangle` sets,
for example, as part of an [`MOI.AbstractSymmetricMatrixSetTriangle`](@ref) set.

!!! note
    Use [`inverse_trimap`](@ref) for the reverse mapping.
"""
function trimap(row::Integer, column::Integer)
    if row < column
        return trimap(column, row)
    end
    return div((row - 1) * row, 2) + column
end

"""
    inverse_trimap(index::Integer)

Convert between the linear index of a
[`MathOptInterface.AbstractSymmetricMatrixSetTriangle`] to the row and column
indices of upper triangular part of the corresponding matrix.

!!! note
    Use [`trimap`](@ref) for the reverse mapping.
"""
function inverse_trimap(index::Integer)
    # Because `isqrt` and `div` are monotone functions
    # because `index` is between `trimap(j - 1, j - 1)` and
    # `trimap(j, j)`,`
    # `side_dimension_for_vectorized_dimension(index)` is between `j - 1`
    # and `j`.
    j = side_dimension_for_vectorized_dimension(index)
    n = trimap(j, j)
    if index <= n
        j -= 1
        n = trimap(j, j)
    end
    i = index - n
    return i, j + 1
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
