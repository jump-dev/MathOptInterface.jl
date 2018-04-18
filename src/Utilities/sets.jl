"""
    getconstant(s::Union{MOI.EqualTo, MOI.GreaterThan, MOI.LessThan})

Returns the constant of the set.
"""
getconstant(s::MOI.EqualTo) = s.value
getconstant(s::MOI.GreaterThan) = s.lower
getconstant(s::MOI.LessThan) = s.upper

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
