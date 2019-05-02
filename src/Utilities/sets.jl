"""
    getconstant(s::Union{MOI.EqualTo, MOI.GreaterThan, MOI.LessThan})

Returns the constant of the set.
"""
MOI.getconstant(s::MOI.EqualTo) = s.value
MOI.getconstant(s::MOI.GreaterThan) = s.lower
MOI.getconstant(s::MOI.LessThan) = s.upper

const DimensionUpdatableSets = Union{MOI.Reals,
                                     MOI.Zeros,
                                     MOI.Nonnegatives,
                                     MOI.Nonpositives}
"""
    updatedimension(s::AbstractVectorSet, newdim)

Returns a set with the dimension modified to `newdim`.
"""
function updatedimension(::S, newdim) where S<:DimensionUpdatableSets
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
    return typeof(set)(MOI.getconstant(set) + offset)
end
function shift_constant(set::MOI.Interval, offset)
    return MOI.Interval(set.lower + offset, set.upper + offset)
end
