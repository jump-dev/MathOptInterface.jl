
function _correct_distance_to_set(d::Real)
    return ifelse(d < 0, zero(d), d)
end

function _correct_distance_to_set(d::AbstractVector{T}) where {T <: Real}
    return [_correct_distance_to_set(di) for di in d]
end

"""
    distance_to_set(v, s)

Compute the distance of a value to a set.
For some vector-valued sets, can return a vector of distances.
When `v ∈ s`, the distance is zero (or all individual distances are zero).

Each set `S` implements `distance_to_set(v::T, s::S)` with `T` of appropriate type.
"""
function distance_to_set end

distance_to_set(v::Real, s::LessThan) = _correct_distance_to_set(v - s.upper)
distance_to_set(v::Real, s::GreaterThan) = _correct_distance_to_set(s.lower - v)
distance_to_set(v::Real, s::EqualTo) = abs(v - s.value)

distance_to_set(v::T, s::Interval) where {T <: Real} = max(s.lower - v, v - s.upper, zero(T))

function distance_to_set(v::AbstractVector{<:Real}, s::Reals)
    length(v) != dimension(s) && throw(DimensionMismatch("Mismatch between value and set"))
    return zeros(length(v))
end

function distance_to_set(v::AbstractVector{<:Real}, s::Zeros)
    length(v) != dimension(s) && throw(DimensionMismatch("Mismatch between value and set"))
    return abs.(v)
end

function distance_to_set(v::AbstractVector{<:Real}, s::Nonnegatives)
    length(v) != dimension(s) && throw(DimensionMismatch("Mismatch between value and set"))
    return _correct_distance_to_set(-v)
end
function distance_to_set(v::AbstractVector{<:Real}, s::Nonpositives)
    length(v) != dimension(s) && throw(DimensionMismatch("Mismatch between value and set"))
    return _correct_distance_to_set(v)
end

# Norm cones

function distance_to_set(v::AbstractVector{<:Real}, s::NormInfinityCone)
    length(v) != dimension(s) && throw(DimensionMismatch("Mismatch between value and set"))
    t = first(v)
    xs = v[2:end]
    return [max(t - abs(x), zero(t)) for x in xs]
end

function distance_to_set(v::AbstractVector{<:Real}, s::NormOneCone)
    length(v) != dimension(s) && throw(DimensionMismatch("Mismatch between value and set"))
    t = v[1]
    xs = v[2:end]
    return _correct_distance_to_set(t - sum(abs, xs))
end

function distance_to_set(v::AbstractVector{<:Real}, s::SecondOrderCone)
    length(v) != dimension(s) && throw(DimensionMismatch("Mismatch between value and set"))
    t = v[1]
    xs = v[2:end]
    return _correct_distance_to_set([-t, dot(xs, xs) - t^2]) # avoids sqrt
end

function distance_to_set(v::AbstractVector{<:Real}, s::RotatedSecondOrderCone)
    length(v) != dimension(s) && throw(DimensionMismatch("Mismatch between value and set"))
    t = v[1]
    u = v[2]
    xs = v[3:end]
    return _correct_distance_to_set([-t, -u, dot(xs, xs) - 2 * t * u])
end

function distance_to_set(v::AbstractVector{<:Real}, s::GeometricMeanCone)
    length(v) != dimension(s) && throw(DimensionMismatch("Mismatch between value and set"))
    t = v[1]
    xs = v[2:end]
    n = dimension(s) - 1
    return _correct_distance_to_set([-minimum(xs), t^n - prod(xs)])
end

function distance_to_set(v::AbstractVector{<:Real}, s::ExponentialCone)
    length(v) != dimension(s) && throw(DimensionMismatch("Mismatch between value and set"))
    x = v[1]
    y = v[2]
    z = v[3]
    return _correct_distance_to_set([y, y * exp(x/y) - z])
end

function distance_to_set(v::AbstractVector{<:Real}, s::DualExponentialCone)
    length(v) != dimension(s) && throw(DimensionMismatch("Mismatch between value and set"))
    u = v[1]
    v = v[2]
    w = v[3]
    return _correct_distance_to_set([-u, -u*exp(v/u) - ℯ * w])
end

function distance_to_set(v::AbstractVector{<:Real}, s::PowerCone)
    length(v) != dimension(s) && throw(DimensionMismatch("Mismatch between value and set"))
    x = v[1]
    y = v[2]
    z = v[3]
    e = s.exponent
    return _correct_distance_to_set([-x, -y, abs(z) - x^e * y^(1-e)])
end

function distance_to_set(v::AbstractVector{<:Real}, s::DualPowerCone)
    length(v) != dimension(s) && throw(DimensionMismatch("Mismatch between value and set"))
    u = v[1]
    v = v[2]
    w = v[3]
    e = s.exponent
    ce = 1-e
    return _correct_distance_to_set([u, v, abs(w) - (u/e)^e * (v/ce)^ce])
end

function distance_to_set(v::AbstractVector{<:Real}, set::RelativeEntropyCone)
    length(v) != dimension(set) && throw(DimensionMismatch("Mismatch between value and set"))
    all(>=(0), v[2:end]) || return false
    n = (dimension(set)-1) ÷ 2
    u = v[1]
    v = v[2:(n+1)]
    w = v[(n+2):end]
    s = sum(w[i] * log(w[i]/v[i]) for i in eachindex(w))
    return _correct_distance_to_set([-minimum(v[2:end]), s - u])
end

function distance_to_set(v::AbstractVector{<:Real}, s::NormSpectralCone)
    t = v[1]
    m = reshape(v[2:end], (s.row_dim, s.column_dim))
    s1 = LinearAlgebra.svd(m).S[1]
    return _correct_distance_to_set(s1 - t)
end

function distance_to_set(v::AbstractVector{<:Real}, s::NormNuclearCone)
    t = v[1]
    m = reshape(v[2:end], (s.row_dim, s.column_dim))
    s1 = sum(LinearAlgebra.svd(m).S)
    return _correct_distance_to_set(s1 - t)
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
    z = v[1]
    # inactive constraint
    if A === ACTIVATE_ON_ONE && isapprox(z, 0) || A === ACTIVATE_ON_ZERO && isapprox(z, 1)
        return zeros(T, 2)
    end
    return [distance_to_set(z, ZeroOne()), distance_to_set(v, s.set)]
end

# TODO Complements, requires additional information (bounds for the variables)
