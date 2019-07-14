"""
    supports_dimension_update(S::Type{<:MOI.AbstractVectorSet})

Return a `Bool` indicating whether the elimination of any dimension of
`n`-dimensional sets of type `S` give an `n-1`-dimensional set `S`.
By default, this function returns `false` so it should only be implemented
for sets that supports dimension update.

For instance, `supports_dimension_update(MOI.Nonnegatives}` is `true` because
the elimination of any dimension of the `n`-dimensional nonnegative orthant
gives the `n-1`-dimensional nonnegative orthant. However
`supports_dimension_update(MOI.ExponentialCone}` is `false`.
"""
function supports_dimension_update(::Type{<:MOI.AbstractVectorSet})
    return false
end
function supports_dimension_update(::Type{<:Union{
    MOI.Reals, MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives}})
    return true
end

"""
    update_dimension(s::AbstractVectorSet, new_dim)

Returns a set with the dimension modified to `new_dim`.
"""
function update_dimension(set::Union{
    MOI.Reals, MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives}, new_dim)
    return typeof(set)(new_dim)
end

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
