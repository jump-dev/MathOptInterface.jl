# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    abstract type AbstractDistance end

An abstract type used to enabble dispatch of
[`Utilities.distance_to_set`](@ref).
"""
abstract type AbstractDistance end

"""
    ProjectionUpperBoundDistance() <: AbstractDistance

An upper bound on the minimum distance between `point` and the closest
feasible point in `set`.

## Definition of distance

The minimum distance is computed as:
```math
d(x, \\mathcal{K}) = \\min_{y \\in \\mathcal{K}} || x - y ||
```
where ``x`` is `point` and ``\\mathcal{K}`` is `set`. The norm is computed as:
```math
||x|| = \\sqrt{f(x, x, \\mathcal{K})}
```
where ``f`` is [`Utilities.set_dot`](@ref).

In the default case, where the set does not have a specialized method for
[`Utilities.set_dot`](@ref), the norm is equivalent to the Euclidean norm
``||x|| = \\sqrt{\\sum x_i^2}``.

## Why an upper bound?

In most cases, `distance_to_set` should return the smallest upper bound, but it
may return a larger value if the smallest upper bound is expensive to compute.

For example, given an epigraph from of a conic set, ``\\{(t, x) | f(x) \\le t\\}``,
it may be simpler to return ``\\delta`` such that ``f(x) \\le t + \\delta``,
rather than computing the nearest projection onto the set.

If the distance is not the smallest upper bound, the docstring of the
appropriate `distance_to_set` method _must_ describe the way that the distance
is computed.
"""
struct ProjectionUpperBoundDistance <: AbstractDistance end

"""
    distance_to_set(
        [d::AbstractDistance = ProjectionUpperBoundDistance()],]
        point::T,
        set::MOI.AbstractScalarSet,
    ) where {T}

    distance_to_set(
        [d::AbstractDistance = ProjectionUpperBoundDistance(),]
        point::AbstractVector{T},
        set::MOI.AbstractVectorSet,
    ) where {T}

Compute the distance between `point` and `set` using the distance metric `d`. If
`point` is in the set `set`, this function _must_ return `zero(T)`.

If `d` is omitted, the default distance is [`Utilities.ProjectionUpperBoundDistance`](@ref).
"""
function distance_to_set(point, set)
    return distance_to_set(ProjectionUpperBoundDistance(), point, set)
end

function distance_to_set(d::AbstractDistance, ::Any, set::MOI.AbstractSet)
    return error(
        "distance_to_set using the distance metric $d for set type " *
        "$(typeof(set)) has not been implemented yet.",
    )
end

###
### MOI.AbstractScalarSets
###

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::T,
    set::MOI.LessThan{T},
) where {T<:Real}
    return max(x - set.upper, zero(T))
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::T,
    set::MOI.GreaterThan{T},
) where {T<:Real}
    return max(set.lower - x, zero(T))
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::T,
    set::MOI.EqualTo{T},
) where {T<:Number}
    return abs(set.value - x)
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::T,
    set::MOI.Interval{T},
) where {T<:Real}
    return max(x - set.upper, set.lower - x, zero(T))
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::T,
    ::MOI.ZeroOne,
) where {T<:Real}
    return min(abs(x - zero(T)), abs(x - one(T)))
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::T,
    ::MOI.Integer,
) where {T<:Real}
    return abs(x - round(x))
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::T,
    set::MOI.Semicontinuous{T},
) where {T<:Real}
    return min(max(x - set.upper, set.lower - x, zero(T)), abs(x))
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::T,
    set::MOI.Semiinteger{T},
) where {T<:Real}
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
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.Nonnegatives,
) where {T<:Real}
    _check_dimension(x, set)
    return LinearAlgebra.norm(max(-xi, zero(T)) for xi in x)
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.Nonpositives,
) where {T<:Real}
    _check_dimension(x, set)
    return LinearAlgebra.norm(max(xi, zero(T)) for xi in x)
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.Zeros,
) where {T<:Number}
    _check_dimension(x, set)
    return LinearAlgebra.norm(x)
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.Reals,
) where {T<:Real}
    _check_dimension(x, set)
    return zero(T)
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.SecondOrderCone,
) where {T<:Real}
    _check_dimension(x, set)
    # Projections come from:
    # Parikh, N., & Boyd, S. (2014). Proximal algorithms. Foundations and
    # trends in Optimization, page 184, section 6.3.2.
    t, rhs = x[1], LinearAlgebra.norm(@views x[2:end])
    if t >= rhs
        return zero(T) # The point is feasible!
    end
    if rhs <= -t  # Projection to the point (0, [0])
        return LinearAlgebra.norm(x)
    end
    # Projection to the point (t, x) + 0.5 * (|x|_2 - t, (t/|x|_2 - 1) * x)
    return sqrt(2) / 2 * abs(t - rhs)

end
