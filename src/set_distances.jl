
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

Each set `S` implements `unsigned_distance(v::T, s::S)` with `T` of appropriate type.
"""
function distance_to_set(v, s)
    length(v) != dimension(s) && throw(DimensionMismatch("Mismatch between value and set"))
    d = unsigned_distance(v, s)
    return _correct_distance_to_set(d)
end

unsigned_distance(v::Real, s::LessThan) = v - s.upper
unsigned_distance(v::Real, s::GreaterThan) = s.lower - v
unsigned_distance(v::Real, s::EqualTo) = abs(v - s.value)

unsigned_distance(v::Real, s::Interval) = max(s.lower - v, v - s.upper)

unsigned_distance(v::AbstractVector{<:Real}, ::Reals) = zeros(length(v))

unsigned_distance(v::AbstractVector{<:Real}, ::Zeros) = abs.(v)

unsigned_distance(v::AbstractVector{<:Real}, ::Nonnegatives) = -v
unsigned_distance(v::AbstractVector{<:Real}, ::Nonpositives) = v

# Norm cones

function unsigned_distance(v::AbstractVector{<:Real}, ::NormInfinityCone)
    t = first(v)
    xs = v[2:end]
    return [t - abs(x) for x in xs]
end

function unsigned_distance(v::AbstractVector{<:Real}, ::NormOneCone)
    t = v[1]
    xs = v[2:end]
    return t - sum(abs, xs)
end

function unsigned_distance(v::AbstractVector{<:Real}, ::SecondOrderCone)
    t = v[1]
    xs = v[2:end]
    return [-t, dot(xs, xs) - t^2] # avoids sqrt
end

function unsigned_distance(v::AbstractVector{<:Real}, ::RotatedSecondOrderCone)
    t = v[1]
    u = v[2]
    xs = v[3:end]
    return [-t, -u, dot(xs, xs) - 2 * t * u]
end

function unsigned_distance(v::AbstractVector{<:Real}, s::GeometricMeanCone)
    t = v[1]
    xs = v[2:end]
    n = dimension(s) - 1
    return [-minimum(xs), t^n - prod(xs)]
end

function unsigned_distance(v::AbstractVector{<:Real}, ::ExponentialCone)
    x = v[1]
    y = v[2]
    z = v[3]
    return [y, y * exp(x/y) - z]
end

function unsigned_distance(v::AbstractVector{<:Real}, ::DualExponentialCone)
    u = v[1]
    v = v[2]
    w = v[3]
    return [-u, -u*exp(v/u) - ℯ * w]
end

function unsigned_distance(v::AbstractVector{<:Real}, s::PowerCone)
    x = v[1]
    y = v[2]
    z = v[3]
    e = s.exponent
    return [-x, -y, abs(z) - x^e * y^(1-e)]
end

function unsigned_distance(v::AbstractVector{<:Real}, s::DualPowerCone)
    u = v[1]
    v = v[2]
    w = v[3]
    e = s.exponent
    ce = 1-e
    return [u, v, abs(w) - (u/e)^e * (v/ce)^ce]
end

function unsigned_distance(v::AbstractVector{<:Real}, set::RelativeEntropyCone)
    all(>=(0), v[2:end]) || return false
    n = (dimension(set)-1) ÷ 2
    u = v[1]
    v = v[2:(n+1)]
    w = v[(n+2):end]
    s = sum(w[i] * log(w[i]/v[i]) for i in eachindex(w))
    return [-minimum(v[2:end]), s - u]
end


function unsigned_distance(v::AbstractVector{<:Real}, s::NormSpectralCone)
    t = v[1]
    m = reshape(v[2:end], (s.row_dim, s.column_dim))
    s1 = LinearAlgebra.svd(m).S[1]
    return s1 - t
end

function unsigned_distance(v::AbstractVector{<:Real}, s::NormNuclearCone)
    t = v[1]
    m = reshape(v[2:end], (s.row_dim, s.column_dim))
    s1 = sum(LinearAlgebra.svd(m).S)
    return s1 - t
end

## Integer sets

function unsigned_distance(v::T, ::ZeroOne) where {T <: Real}
    return min(abs(v - zero(T)), abs(v - one(T)))
end

function unsigned_distance(v::Real, ::Integer)
    return min(abs(v - floor(v)), abs(v - ceil(v)))
end

# return the element-wise distance to zero, with the greatest element to 0
function unsigned_distance(v::AbstractVector{T}, ::SOS1) where {T <: Real}
    d = unsigned_distance(v, Zeros(length(v)))
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
function unsigned_distance(v::AbstractVector{T}, s::IndicatorSet{A}) where {A, T <: Real}
    z = v[1]
    # inactive constraint
    if A === ACTIVATE_ON_ONE && isapprox(z, 0) || A === ACTIVATE_ON_ZERO && isapprox(z, 1)
        return zeros(T, 2)
    end
    return [unsigned_distance(z, ZeroOne()), unsigned_distance(v, s.set)]
end

# TODO Complements, requires additional information (bounds for the variables)
