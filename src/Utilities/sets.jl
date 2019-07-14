const DimensionUpdatableSets = Union{MOI.Reals,
                                     MOI.Zeros,
                                     MOI.Nonnegatives,
                                     MOI.Nonpositives}
"""
    update_dimension(s::AbstractVectorSet, newdim)

Returns a set with the dimension modified to `newdim`.
"""
function update_dimension(::S, newdim) where S<:DimensionUpdatableSets
    S(newdim)
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
