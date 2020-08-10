"""
    shift_constant(set::MOI.AbstractScalarSet,
                   offset)

Returns a new scalar set `new_set` such that `func`-in-`set` is equivalent to
`func + offset`-in-`new_set`.

## Examples

The call `shift_constant(MOI.Interval(-2, 3), 1)` is equal to
`MOI.Interval(-1, 4)`.
"""
function shift_constant(set::Union{MOI.LessThan{T},
                                   MOI.GreaterThan{T},
                                   MOI.EqualTo{T}},
                        offset::T) where T
    return typeof(set)(MOI.constant(set) + offset)
end
function shift_constant(set::MOI.Interval, offset)
    return MOI.Interval(set.lower + offset, set.upper + offset)
end

const ScalarLinearSet{T} = Union{MOI.EqualTo{T}, MOI.LessThan{T}, MOI.GreaterThan{T}}
const VectorLinearSet = Union{MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives}

vector_set_type(::Type{<:MOI.EqualTo}) = MOI.Zeros
vector_set_type(::Type{<:MOI.LessThan}) = MOI.Nonpositives
vector_set_type(::Type{<:MOI.GreaterThan}) = MOI.Nonnegatives

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
    return div(isqrt(1 + 8n), 2)
end
