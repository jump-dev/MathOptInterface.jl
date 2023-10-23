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

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.RotatedSecondOrderCone,
) where {T<:Real}
    _check_dimension(x, set)
    t, u, xs = x[1], x[2], @view(x[3:end])
    element_distance = (
        min(t, zero(T)),  # t >= 0
        min(u, zero(T)),  # u >= 0
        max(LinearAlgebra.dot(xs, xs) - 2 * t * u, zero(T)),
    )
    return LinearAlgebra.norm(element_distance, 2)
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.ExponentialCone,
) where {T<:Real}
    _check_dimension(x, set)
    if x[2] <= 0  # Project to x[2] = 1
        element_distance = (x[2] - one(T), max(exp(x[1]) - x[3], zero(T)))
        return LinearAlgebra.norm(element_distance, 2)
    end
    return max(x[2] * exp(x[1] / x[2]) - x[3], zero(T))
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.DualExponentialCone,
) where {T<:Real}
    _check_dimension(x, set)
    if x[1] >= 0  # Project to x[1] = -1
        element_distance =
            (x[1] - -one(T), max(exp(-x[2]) - exp(1) * x[3], zero(T)))
        return LinearAlgebra.norm(element_distance, 2)
    end
    return max(-x[1] * exp(x[2] / x[1]) - exp(1) * x[3], zero(T))
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.GeometricMeanCone,
) where {T<:Real}
    _check_dimension(x, set)
    t, xs = x[1], @view(x[2:end])
    if any(<(zero(T)), xs)  # Project to x = 0
        return LinearAlgebra.norm((min.(xs, zero(T)), max(t, zero(T))), 2)
    end
    return max(t - prod(xs)^inv(MOI.dimension(set) - 1), zero(T))
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.PowerCone,
) where {T<:Real}
    _check_dimension(x, set)
    α = set.exponent
    if x[1] < 0 || x[2] < 0  # Project to x = 0
        return LinearAlgebra.norm(
            (min(x[1], zero(T)), min(x[2], zero(T)), x[3]),
            2,
        )
    end
    return max(abs(x[3]) - x[1]^α * x[2]^(1 - α), zero(T))
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.DualPowerCone,
) where {T<:Real}
    _check_dimension(x, set)
    α = set.exponent
    if x[1] < 0 || x[2] < 0  # Project to x = 0
        return LinearAlgebra.norm(
            (min(x[1], zero(T)), min(x[2], zero(T)), x[3]),
            2,
        )
    end
    return max(abs(x[3]) - (x[1] / α)^α * (x[2] / (1 - α))^(1 - α), zero(T))
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.NormOneCone,
) where {T<:Real}
    _check_dimension(x, set)
    return max(sum(abs, @view(x[2:end])) - x[1], zero(T))
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.NormInfinityCone,
) where {T<:Real}
    _check_dimension(x, set)
    return max(maximum(abs, @view(x[2:end])) - x[1], zero(T))
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.RelativeEntropyCone,
) where {T<:Real}
    _check_dimension(x, set)
    n = div(MOI.dimension(set) - 1, 2)
    u, v, w = x[1], @view(x[2:(n+1)]), @view(x[(n+2):end])
    _to_one(x) = x <= zero(T) ? one(T) : x
    if any(<=(zero(T)), v) || any(<=(zero(T)), w)  # Project to v = w = 1
        v_p, w_p = _to_one.(v), _to_one.(w)
        element_distance = (
            v_p .- v,
            w_p .- w,
            max(
                sum(w_p[i] * log(w_p[i] / v_p[i]) for i in eachindex(w)) - u,
                zero(T),
            ),
        )
        return LinearAlgebra.norm(element_distance, 2)
    end
    return max(sum(w[i] * log(w[i] / v[i]) for i in eachindex(w)) - u, zero(T))
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.HyperRectangle,
) where {T<:Real}
    _check_dimension(x, set)
    element_distance =
        (max.(set.lower .- x, zero(T)), max.(x .- set.upper, zero(T)))
    return LinearAlgebra.norm(element_distance, 2)
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.NormCone,
) where {T<:Real}
    _check_dimension(x, set)
    return max(LinearAlgebra.norm(@view(x[2:end]), set.p) - x[1], zero(T))
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.SOS1,
) where {T<:Real}
    _check_dimension(x, set)
    _, i = findmax(abs, x)
    return LinearAlgebra.norm2([x[j] for j in eachindex(x) if j != i])
end

function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.SOS2,
) where {T<:Real}
    _check_dimension(x, set)
    p = sortperm(set.weights)
    pairs = collect(zip(p[1:end-1], p[2:end]))
    _, k = findmax([abs(x[i]) + abs(x[j]) for (i, j) in pairs])
    return LinearAlgebra.norm2([x[i] for i in eachindex(x) if !(i in pairs[k])])
end
