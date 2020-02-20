"""
    distance_to_set(v, s)

Compute the distance of a value to a set.
For some vector-valued sets, can return a vector of distances.
When `v ∈ s`, the distance is zero (or all individual distances are zero).

Each set `S` implements `distance_to_set(v::T, s::S)` with `T` of appropriate type.
"""
function distance_to_set end

distance_to_set(v::Real, s::LessThan) = max(v - s.upper, zero(v))
distance_to_set(v::Real, s::GreaterThan) = max(s.lower - v, zero(v))
distance_to_set(v::Real, s::EqualTo) = abs(v - s.value)

distance_to_set(v::T, s::Interval) where {T <: Real} = max(s.lower - v, v - s.upper, zero(T))

function _check_dimension(v::AbstractVector, s)
    length(v) != dimension(s) && throw(DimensionMismatch("Mismatch between value and set"))
    return nothing
end

function distance_to_set(v::AbstractVector{<:Real}, s::Reals)
    _check_dimension(v, s)
    return zeros(length(v))
end

function distance_to_set(v::AbstractVector{<:Real}, s::Zeros)
    _check_dimension(v, s)
    return abs.(v)
end

function distance_to_set(v::AbstractVector{<:Real}, s::Nonnegatives)
    _check_dimension(v, s)
    return [ifelse(vi < 0, -vi, zero(vi)) for vi in v]
end
function distance_to_set(v::AbstractVector{<:Real}, s::Nonpositives)
    _check_dimension(v, s)
    return [ifelse(vi > 0, vi, zero(vi)) for vi in v]
end

# Norm cones

function distance_to_set(v::AbstractVector{<:Real}, s::NormInfinityCone)
    _check_dimension(v, s)
    t = first(v)
    xs = v[2:end]
    return [max(t - abs(x), zero(t)) for x in xs]
end

function distance_to_set(v::AbstractVector{<:Real}, s::NormOneCone)
    _check_dimension(v, s)
    t = v[1]
    xs = v[2:end]
    result = t - sum(abs, xs)
    return max(result, zero(result))
end

function distance_to_set(v::AbstractVector{<:Real}, s::SecondOrderCone)
    _check_dimension(v, s)
    t = v[1]
    xs = v[2:end]
    result = LinearAlgebra.norm(xs) - t
    return max(result, zero(result)) # avoids sqrt
end

function distance_to_set(v::AbstractVector{<:Real}, s::RotatedSecondOrderCone)
    _check_dimension(v, s)
    t = v[1]
    u = v[2]
    xs = v[3:end]
    return [
        max(-t, zero(t)),
        max(-u, zero(u)),
        max(dot(xs,xs) - 2 * t * u),
    ]
end

function distance_to_set(v::AbstractVector{<:Real}, s::GeometricMeanCone)
    _check_dimension(v, s)
    t = v[1]
    xs = v[2:end]
    n = dimension(s) - 1
    result = t^n - prod(xs)
    return push!(
        max.(xs, zero(result)), # x >= 0
        max(result, zero(result)),
    )
end

function distance_to_set(v::AbstractVector{<:Real}, s::ExponentialCone)
    _check_dimension(v, s)
    x = v[1]
    y = v[2]
    z = v[3]
    result = y * exp(x/y) - z
    return [max(y, zero(result)), max(result, zero(result))]
end

function distance_to_set(v::AbstractVector{<:Real}, s::DualExponentialCone)
    _check_dimension(v, s)
    u = v[1]
    v = v[2]
    w = v[3]
    result = -u*exp(v/u) - ℯ * w
    return [max(-u, zero(result)), max(result, zero(result))]
end

function distance_to_set(v::AbstractVector{<:Real}, s::PowerCone)
    _check_dimension(v, s)
    x = v[1]
    y = v[2]
    z = v[3]
    e = s.exponent
    result = abs(z) - x^e * y^(1-e)
    return [max(-x, zero(result)), max(-y, zero(result)), max(result, zero(result))]
end

function distance_to_set(v::AbstractVector{<:Real}, s::DualPowerCone)
    _check_dimension(v, s)
    u = v[1]
    v = v[2]
    w = v[3]
    e = s.exponent
    ce = 1-e
    result = abs(w) - (u/e)^e * (v/ce)^ce
    return [max(-u, zero(result)), max(-v, zero(result)), max(result, zero(result))]
end

function distance_to_set(v::AbstractVector{<:Real}, set::RelativeEntropyCone)
    _check_dimension(v, s)
    n = (dimension(set)-1) ÷ 2
    u = v[1]
    v = v[2:(n+1)]
    w = v[(n+2):end]
    s = sum(w[i] * log(w[i]/v[i]) for i in eachindex(w))
    result = s - u
    return push!(
        max.(v[2:end], zero(result)),
        max(result, zero(result)),
    )
end

function distance_to_set(v::AbstractVector{<:Real}, s::NormSpectralCone)
    _check_dimension(v, s)
    t = v[1]
    m = reshape(v[2:end], (s.row_dim, s.column_dim))
    s1 = LinearAlgebra.svd(m).S[1]
    result = s1 - t
    return max(result, zero(result))
end

function distance_to_set(v::AbstractVector{<:Real}, s::NormNuclearCone)
    _check_dimension(v, s)
    t = v[1]
    m = reshape(v[2:end], (s.row_dim, s.column_dim))
    s1 = sum(LinearAlgebra.svd(m).S)
    result = s1 - t
    return max(result, zero(result))
end

## Integer sets

function distance_to_set(v::T, ::ZeroOne) where {T <: Real}
    return min(abs(v - zero(T)), abs(v - one(T)))
end

function distance_to_set(v::Real, ::Integer)
    return min(abs(v - floor(v)), abs(v - ceil(v)))
end

# return the element-wise distance to zero, with the greatest element to 0
function distance_to_set(v::AbstractVector{T}, ::SOS1) where {T <: Real}
    _check_dimension(v, s)
    d = distance_to_set(v, Zeros(length(v)))
    m = maximum(d)
    if m ≈ zero(T)
        return d
    end
    # removing greatest distance
    for i in eachindex(d)
        @inbounds if d[i] == m
            d[i] = zero(T)
            return d
        end
    end
end

# TODO SOS2

# takes in input [z, f(x)]
function distance_to_set(v::AbstractVector{T}, s::IndicatorSet{A}) where {A, T <: Real}
    _check_dimension(v, s)
    z = v[1]
    # inactive constraint
    if A === ACTIVATE_ON_ONE && isapprox(z, 0) || A === ACTIVATE_ON_ZERO && isapprox(z, 1)
        return zeros(T, 2)
    end
    return [distance_to_set(z, ZeroOne()), distance_to_set(v, s.set)]
end

# TODO Complements, requires additional information (bounds for the variables)
