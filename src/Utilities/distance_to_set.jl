# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    distance_to_set(point::T, set::MOI.AbstractScalarSet) where {T}
    distance_to_set(
        point::AbstractVector{T},
        set::MOI.AbstractVectorSet,
    ) where {T}

Compute an upper bound on the Euclidean distance between `point` and the closest
feasible point in `set`. If `point` is in the set `set`, this function _must_
return `zero(T)`.

In most cases, this function should return the smallest upper bound, but it may
return a larger value if the smallest upper bound is expensive to compute.
"""
function distance_to_set(::Any, set::MOI.AbstractSet)
    return error(
        "distance_to_set for set type $(typeof(set)) has not been " *
        "implemented yet.",
    )
end

###
### MOI.AbstractScalarSets
###

function distance_to_set(x::T, set::MOI.LessThan{T}) where {T<:Real}
    return max(x - set.upper, zero(T))
end

function distance_to_set(x::T, set::MOI.GreaterThan{T}) where {T<:Real}
    return max(set.lower - x, zero(T))
end

function distance_to_set(x::T, set::MOI.EqualTo{T}) where {T<:Number}
    return abs(set.value - x)
end

function distance_to_set(x::T, set::MOI.Interval{T}) where {T<:Real}
    return max(x - set.upper, set.lower - x, zero(T))
end

function distance_to_set(x::T, ::MOI.ZeroOne) where {T<:Real}
    return min(abs(x - zero(T)), abs(x - one(T)))
end

function distance_to_set(x::T, ::MOI.Integer) where {T<:Real}
    return abs(x - round(x))
end

function distance_to_set(x::T, set::MOI.Semicontinuous{T}) where {T<:Real}
    return min(max(x - set.upper, set.lower - x, zero(T)), abs(x))
end

function distance_to_set(x::T, set::MOI.Semiinteger{T}) where {T<:Real}
    d = max(ceil(set.lower) - x, x - floor(set.upper), abs(x - round(x)))
    return min(d, abs(x))
end

###
### MOI.AbstractVectorSets
###

function _check_dimension(v::AbstractVector, s)
    if length(v) != MOI.dimension(s)
        throw(DimensionMismatch("Mismatch between value and set"))
    end
    return
end

function distance_to_set(
    x::AbstractVector{T},
    set::MOI.Nonnegatives,
) where {T<:Real}
    _check_dimension(x, set)
    return LinearAlgebra.norm(max(-xi, zero(T)) for xi in x)
end

function distance_to_set(
    x::AbstractVector{T},
    set::MOI.Nonpositives,
) where {T<:Real}
    _check_dimension(x, set)
    return LinearAlgebra.norm(max(xi, zero(T)) for xi in x)
end

function distance_to_set(
    x::AbstractVector{T},
    set::MOI.Zeros,
) where {T<:Number}
    _check_dimension(x, set)
    return LinearAlgebra.norm(x)
end

function distance_to_set(
    x::AbstractVector{T},
    set::MOI.Reals,
) where {T<:Real}
    _check_dimension(x, set)
    return zero(T)
end
