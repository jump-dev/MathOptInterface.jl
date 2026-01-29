# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    abstract type AbstractDistance end

An abstract type used to enable dispatch of
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

# This is the minimal L2-norm.
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::T,
    set::MOI.LessThan{T},
) where {T<:Real}
    return max(x - set.upper, zero(T))
end

# This is the minimal L2-norm.
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::T,
    set::MOI.GreaterThan{T},
) where {T<:Real}
    return max(set.lower - x, zero(T))
end

# This is the minimal L2-norm.
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::T,
    set::MOI.EqualTo{T},
) where {T<:Number}
    return abs(set.value - x)
end

# This is the minimal L2-norm.
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::T,
    set::MOI.Parameter{T},
) where {T<:Number}
    return abs(set.value - x)
end

# This is the minimal L2-norm.
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::T,
    set::MOI.Interval{T},
) where {T<:Real}
    return max(x - set.upper, set.lower - x, zero(T))
end

# This is the minimal L2-norm.
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::T,
    ::MOI.ZeroOne,
) where {T<:Real}
    return min(abs(x - zero(T)), abs(x - one(T)))
end

# This is the minimal L2-norm.
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::T,
    ::MOI.Integer,
) where {T<:Real}
    return abs(x - round(x))
end

# This is the minimal L2-norm.
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::T,
    set::MOI.Semicontinuous{T},
) where {T<:Real}
    return min(max(x - set.upper, set.lower - x, zero(T)), abs(x))
end

# This is the minimal L2-norm.
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

function _reshape(
    x::AbstractVector,
    set::Union{
        MOI.PositiveSemidefiniteConeSquare,
        MOI.LogDetConeSquare,
        MOI.RootDetConeSquare,
    },
)
    n = isqrt(length(x))
    return reshape(x, (n, n))
end

function _reshape(
    x::AbstractVector{T},
    set::Union{
        MOI.PositiveSemidefiniteConeTriangle,
        MOI.LogDetConeTriangle,
        MOI.RootDetConeTriangle,
    },
) where {T}
    n = isqrt(2 * length(x))
    # The type annotation is needed for JET.
    X = zeros(T, n, n)::Matrix{T}
    k = 1
    for i in 1:n
        for j in 1:i
            X[j, i] = X[i, j] = x[k]
            k += 1
        end
    end
    return LinearAlgebra.Symmetric(X)
end

# This is the minimal L2-norm.
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.Nonnegatives,
) where {T<:Real}
    _check_dimension(x, set)
    return LinearAlgebra.norm(max(-xi, zero(T)) for xi in x)
end

# This is the minimal L2-norm.
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.Nonpositives,
) where {T<:Real}
    _check_dimension(x, set)
    return LinearAlgebra.norm(max(xi, zero(T)) for xi in x)
end

# This is the minimal L2-norm.
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.Zeros,
) where {T<:Number}
    _check_dimension(x, set)
    return LinearAlgebra.norm(x)
end

# This is the minimal L2-norm.
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.Reals,
) where {T<:Real}
    _check_dimension(x, set)
    return zero(T)
end

# This is the minimal L2-norm.
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.SecondOrderCone,
) where {T<:Real}
    Base.require_one_based_indexing(x)
    _check_dimension(x, set)
    # Projections come from:
    # Parikh, N., & Boyd, S. (2014). Proximal algorithms. Foundations and
    # trends in Optimization, page 184, section 6.3.2.
    t, rhs = x[1], LinearAlgebra.norm(@views x[2:end])
    if t >= rhs
        return zero(T) # The point is feasible
    end
    if rhs <= -t  # Projection to the point (0, [0])
        return LinearAlgebra.norm(x)
    end
    # Projection to the point (t, x) + 0.5 * (|x|_2 - t, (t/|x|_2 - 1) * x)
    return sqrt(2) / 2 * abs(t - rhs)
end

"""
    distance_to_set(::ProjectionUpperBoundDistance, x, ::MOI.RotatedSecondOrderCone)

Let `(t, u, y...) = x`. Return the 2-norm of the vector `d` such that in `x + d`,
`u` is projected to `1` if `u <= 0`, and `t` is increased such that `x + d`
belongs to the set.
"""
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.RotatedSecondOrderCone,
) where {T<:Real}
    Base.require_one_based_indexing(x)
    _check_dimension(x, set)
    u, y = (x[2] <= 0 ? 1 : x[2]), @view(x[3:end])
    element_distance = (
        u - x[2],  # u -> 1
        max(LinearAlgebra.dot(y, y) / (2 * u) - x[1], zero(T)),
    )
    return LinearAlgebra.norm(element_distance, 2)
end

"""
    distance_to_set(::ProjectionUpperBoundDistance, x, ::MOI.ExponentialCone)

Let `(u, v, w) = x`. If `v > 0`, return the epigraph distance `d` such that
`(u, v, w + d)` belongs to the set.

If `v <= 0` return the 2-norm of the vector `d` such that `x + d = (u, 1, z)`
where `z` satisfies the constraints.
"""
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.ExponentialCone,
) where {T<:Real}
    Base.require_one_based_indexing(x)
    _check_dimension(x, set)
    if x[2] <= 0  # Project to x[2] = 1
        element_distance = (x[2] - one(T), max(exp(x[1]) - x[3], zero(T)))
        return LinearAlgebra.norm(element_distance, 2)
    end
    return max(x[2] * exp(x[1] / x[2]) - x[3], zero(T))
end

"""
    distance_to_set(::ProjectionUpperBoundDistance, x, ::MOI.DualExponentialCone)

Let `(u, v, w) = x`. If `u < 0`, return the epigraph distance `d` such that
`(u, v, w + d)` belongs to the set.

If `u >= 0` return the 2-norm of the vector `d` such that `x + d = (u, -1, z)`
where `z` satisfies the constraints.
"""
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.DualExponentialCone,
) where {T<:Real}
    Base.require_one_based_indexing(x)
    _check_dimension(x, set)
    if x[1] >= 0  # Project to x[1] = -1
        element_distance = (x[1] - -one(T), max(exp(-x[2] - 1) - x[3], zero(T)))
        return LinearAlgebra.norm(element_distance, 2)
    end
    return max(-x[1] * exp(x[2] / x[1] - 1) - x[3], zero(T))
end

"""
    distance_to_set(::ProjectionUpperBoundDistance, x, ::MOI.GeometricMeanCone)

Let `(t, y...) = x`. If all `y` are non-negative, return the epigraph distance
`d` such that `(t + d, y...)` belongs to the set.

If any `y` are strictly negative, return the 2-norm of the vector `d` that
projects negative `y` elements to `0` and `t` to `ℝ₋`.
"""
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.GeometricMeanCone,
) where {T<:Real}
    Base.require_one_based_indexing(x)
    _check_dimension(x, set)
    t, xs = x[1], @view(x[2:end])
    if any(<(zero(T)), xs)  # Project to x = 0
        return LinearAlgebra.norm((min.(xs, zero(T)), max(t, zero(T))), 2)
    end
    return max(t - prod(xs)^inv(MOI.dimension(set) - 1), zero(T))
end

"""
    distance_to_set(::ProjectionUpperBoundDistance, x, ::MOI.PowerCone)

Let `(a, b, c) = x`. If `a` and `b` are non-negative, return the epigraph
distance required to increase `c` such that the constraint is satisfied.

If `a` or `b` is strictly negative, return the 2-norm of the vector `d` such
that in the vector `x + d`: `c`, and any negative `a` and `b` are projected to
`0`.
"""
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.PowerCone,
) where {T<:Real}
    Base.require_one_based_indexing(x)
    _check_dimension(x, set)
    α = set.exponent
    if x[1] >= 0 && x[2] >= 0
        return max(abs(x[3]) - x[1]^α * x[2]^(1 - α), zero(T))
    end
    # Project to x = 0
    return LinearAlgebra.norm((min(x[1], zero(T)), min(x[2], zero(T)), x[3]), 2)
end

"""
    distance_to_set(::ProjectionUpperBoundDistance, x, ::MOI.DualPowerCone)

Let `(a, b, c) = x`. If `a` and `b` are non-negative, return the epigraph
distance required to increase `c` such that the constraint is satisfied.

If `a` or `b` is strictly negative, return the 2-norm of the vector `d` such
that in the vector `x + d`: `c`, and any negative `a` and `b` are projected to
`0`.
"""
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.DualPowerCone,
) where {T<:Real}
    Base.require_one_based_indexing(x)
    _check_dimension(x, set)
    α = set.exponent
    if x[1] >= 0 && x[2] >= 0
        return max(abs(x[3]) - (x[1] / α)^α * (x[2] / (1 - α))^(1 - α), zero(T))
    end
    # Project to x = 0
    return LinearAlgebra.norm((min(x[1], zero(T)), min(x[2], zero(T)), x[3]), 2)
end

"""
    distance_to_set(::ProjectionUpperBoundDistance, x, ::MOI.NormOneCone)

Let `(t, y...) = x`. Return the epigraph distance `d` such that `(t + d, y...)`
belongs to the set.
"""
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.NormOneCone,
) where {T<:Real}
    Base.require_one_based_indexing(x)
    _check_dimension(x, set)
    return max(sum(abs, @view(x[2:end])) - x[1], zero(T))
end

"""
    distance_to_set(::ProjectionUpperBoundDistance, x, ::MOI.NormInfinityCone)

Let `(t, y...) = x`. Return the epigraph distance `d` such that `(t + d, y...)`
belongs to the set.
"""
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.NormInfinityCone,
) where {T<:Real}
    Base.require_one_based_indexing(x)
    _check_dimension(x, set)
    return max(maximum(abs, @view(x[2:end])) - x[1], zero(T))
end

"""
    distance_to_set(::ProjectionUpperBoundDistance, x, ::MOI.RelativeEntropyCone)

Let `(u, v..., w...) = x`. If `v` and `w` are strictly positive, return the
epigraph distance required to increase `u` such that the constraint is satisfied.

If any elements in `v` or `w` are non-positive, return the 2-norm of the vector
`d` such that in the vector `x + d`: any non-positive elements in `v` and `w`
are projected to `1`, and `u` is projected such that the epigraph constraint
holds.
"""
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.RelativeEntropyCone,
) where {T<:Real}
    Base.require_one_based_indexing(x)
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

# This is the minimal L2-norm.
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

"""
    distance_to_set(::ProjectionUpperBoundDistance, x, set::MOI.NormCone)

Let `(t, y...) = x`. Return the epigraph distance `d` such that `(t + d, y...)`
belongs to the set.
"""
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.NormCone,
) where {T<:Real}
    Base.require_one_based_indexing(x)
    _check_dimension(x, set)
    return max(LinearAlgebra.norm(@view(x[2:end]), set.p) - x[1], zero(T))
end

# This is the minimal L2-norm.
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.SOS1,
) where {T<:Real}
    _check_dimension(x, set)
    _, i = findmax(abs, x)
    return LinearAlgebra.norm([x[j] for j in eachindex(x) if j != i], 2)
end

# This is the minimal L2-norm.
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.SOS2,
) where {T<:Real}
    _check_dimension(x, set)
    p = sortperm(set.weights)
    pairs = collect(zip(p[1:(end-1)], p[2:end]))
    _, k = findmax([abs(x[i]) + abs(x[j]) for (i, j) in pairs])
    elements = [x[i] for i in eachindex(x) if !(i in pairs[k])]
    return LinearAlgebra.norm(elements, 2)
end

"""
    distance_to_set(
        ::ProjectionUpperBoundDistance,
        x::AbstractVector,
        set::Union{
            MOI.PositiveSemidefiniteConeSquare,
            MOI.PositiveSemidefiniteConeTriangle,
        },
    )

Let ``X`` be `x` reshaped into the appropriate matrix. The returned distance is
``||X - Y||_2^2`` where ``Y`` is the eigen decomposition of ``X`` with negative
eigen values removed.
"""
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::Union{
        MOI.PositiveSemidefiniteConeSquare,
        MOI.PositiveSemidefiniteConeTriangle,
    },
) where {T<:Union{Real,Complex}}
    _check_dimension(x, set)
    # We should return the norm of `A` defined by:
    # ```julia
    # λ, U = LinearAlgebra.eigen(_reshape(x, set))
    # λ_negative = LinearAlgebra.Diagonal(min.(zero(T), λ))
    # A = LinearAlgebra.Symmetric(U * λ_negative * U')
    # LinearAlgebra.norm(A, 2)
    # ```
    # The norm should correspond to `MOI.Utilities.set_dot` so it's the
    # Frobenius norm, which is the Euclidean norm of the vector of eigenvalues.
    eigvals = LinearAlgebra.eigvals(_reshape(x, set))
    return LinearAlgebra.norm(_drop_positives.(eigvals), 2)
end

_drop_positives(x::T) where {T<:Real} = min(zero(T), x)

function _drop_positives(x::T)::T where {T<:Complex}
    return ifelse(isreal(x), _drop_positives(real(x)), x)
end

"""
    distance_to_set(
        ::ProjectionUpperBoundDistance,
        x::AbstractVector{T},
        set::MOI.VectorNonlinearOracle,
    ) where {T<:Real}

!!! warning
    This is not an upper bound on the projection in `x` space; instead, it
    is a projection in `f(x)`-space.
"""
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.VectorNonlinearOracle{T},
) where {T<:Real}
    _check_dimension(x, set)
    y = zeros(T, set.output_dimension)
    set.eval_f(y, x)
    return LinearAlgebra.norm(y .- clamp.(y, set.l, set.u))
end

# This is the minimal L2-norm.
function distance_to_set(
    distance::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.Indicator{MOI.ACTIVATE_ON_ONE},
) where {T<:Real}
    _check_dimension(x, set)
    return min(
        # Distance of x[1] from 0
        abs(x[1]),
        # Distance of x[1] from 1 + distance to set
        sqrt((1 - x[1])^2 + distance_to_set(distance, x[2], set.set)^2),
    )
end

function distance_to_set(
    distance::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.Indicator{MOI.ACTIVATE_ON_ZERO},
) where {T}
    _check_dimension(x, set)
    return min(
        # Distance of x[1] from 1
        abs(one(T) - x[1]),
        # Distance of x[1] from 0 + distance to set
        sqrt(x[1]^2 + distance_to_set(distance, x[2], set.set)^2),
    )
end

"""
    distance_to_set(::ProjectionUpperBoundDistance, x, set::MOI.NormNuclearCone)

Let `(t, y...) = x`. Return the epigraph distance `d` such that `(t + d, y...)`
belongs to the set.
"""
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.NormNuclearCone,
) where {T}
    _check_dimension(x, set)
    X = reshape(x[2:end], set.row_dim, set.column_dim)
    return max(sum(LinearAlgebra.svdvals(X)) - x[1], zero(T))
end

"""
    distance_to_set(::ProjectionUpperBoundDistance, x, set::MOI.NormSpectralCone)

Let `(t, y...) = x`. Return the epigraph distance `d` such that `(t + d, y...)`
belongs to the set.
"""
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.NormSpectralCone,
) where {T}
    _check_dimension(x, set)
    X = reshape(x[2:end], set.row_dim, set.column_dim)
    return max(maximum(LinearAlgebra.svdvals(X)) - x[1], zero(T))
end

"""
    distance_to_set(
        ::ProjectionUpperBoundDistance,
        x::AbstractVector,
        set::Union{MOI.RootDetConeSquare,MOI.RootDetConeTriangle},
    )

Let ``Y`` be `y` in `x = (t, y)`, reshaped into the appropriate matrix. The
returned distance is ``||Y - Z||_2^2`` where ``Z`` is the eigen decomposition of
``Y`` with negative eigen values removed, plus the epigraph distance in `t`
needed to satisfy the root-determinant constraint.
"""
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::Union{MOI.RootDetConeSquare,MOI.RootDetConeTriangle},
) where {T<:Real}
    _check_dimension(x, set)
    eigvals = LinearAlgebra.eigvals(_reshape(x[2:end], set))
    eigvals_neg = min.(zero(T), eigvals)
    eigvals_pos = max.(zero(T), eigvals)
    rootdet = prod(eigvals_pos)^(1 / set.side_dimension)
    push!(eigvals_neg, max(x[1] - rootdet, zero(T)))
    return LinearAlgebra.norm(eigvals_neg, 2)
end

"""
    distance_to_set(
        ::ProjectionUpperBoundDistance,
        x::AbstractVector,
        set::Union{MOI.LogDetConeSquare,MOI.LogDetConeTriangle},
    )

Let ``Y`` be `y` in `x = (t, y)`, reshaped into the appropriate matrix. The
returned distance is ``||Y/u - Z||_2^2`` where ``Z`` is the eigen decomposition
of ``Y`` with negative eigen values removed, plus the epigraph distance in `t`
needed to satisfy the log-determinant constraint.
"""
function distance_to_set(
    ::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::Union{MOI.LogDetConeSquare,MOI.LogDetConeTriangle},
) where {T<:Real}
    _check_dimension(x, set)
    eigvals = LinearAlgebra.eigvals(_reshape(x[3:end] ./ x[2], set))
    eigvals_neg = min.(eps(T), eigvals)
    eigvals_pos = max.(eps(T), eigvals)
    push!(eigvals_neg, max(x[1] - x[2] * sum(log.(eigvals_pos)), zero(T)))
    return LinearAlgebra.norm(eigvals_neg, 2)
end

"""
    distance_to_set(
        ::ProjectionUpperBoundDistance,
        x::AbstractVector,
        set::MOI.Scaled{S},
    )

This is the distance in the un-scaled space.
"""
function distance_to_set(
    dist::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.Scaled{S},
) where {T,S<:MOI.AbstractVectorSet}
    _check_dimension(x, set)
    scale = MOI.Utilities.SetDotScalingVector{T}(set.set)
    return distance_to_set(dist, x ./ scale, set.set)
end

function distance_to_set(
    dist::ProjectionUpperBoundDistance,
    x::AbstractVector{T},
    set::MOI.HermitianPositiveSemidefiniteConeTriangle,
) where {T<:Real}
    _check_dimension(x, set)
    output_set = MOI.PositiveSemidefiniteConeTriangle(set.side_dimension)
    y = zeros(Complex{T}, MOI.dimension(output_set))
    real_offset, imag_offset = 0, length(y)
    for col in 1:set.side_dimension
        for row in 1:col
            real_offset += 1
            y[real_offset] = x[real_offset]
            if row != col
                imag_offset += 1
                y[real_offset] += x[imag_offset] * im
            end
        end
    end
    return distance_to_set(dist, y, output_set)
end
