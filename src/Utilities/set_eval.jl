
function set_distance(v::Real, s)
    d = _distance(v, s)
    return d < 0 : zero(d) : d
end

function set_distance(v::AbstractVector{T}, s) where {T <: Real}
    length(v) != MOI.dimension(s) && throw(DimensionMismatch("Mismatch between v and s"))
    d = _distance(v, s)
    return [di < 0 : zero(T) : di for di in d]
end

_distance(v, s::MOI.LessThan) = s.upper - v
_distance(v, s::MOI.GreaterThan) = v - s.lower
_distance(v, s::MOI.EqualTo) = abs(v - s.value)

_distance(v, s::MOI.Interval) = max(v - s.lower, s.upper - v)

_distance(v::AbstractVector{<:Real}, ::MOI.Reals) = false * v

_distance(v, ::MOI.Zeros) = abs.(v)

_distance(v::AbstractVector{<:Real}, ::MOI.Nonnegatives) = -v
_distance(v::AbstractVector{<:Real}, ::MOI.Nonpositives) = v

# Norm cones

function _distance(v::AbstractVector{<:Real}, ::MOI.NormInfinityCone)
    t = first(v)
    xs = v[2:end]
    return [t - abs(x) for x in xs]
end

function _distance(v::AbstractVector{<:Real}, ::MOI.NormOneCone)
    t = v[1]
    xs = v[2:end]
    return t - sum(abs, xs)
end

function _distance(v::AbstractVector{<:Real}, ::MOI.SecondOrderCone)
    t = v[1]
    xs = v[2:end]
    return min(-t, dot(xs, xs) - t^2) # avoids sqrt
end

function _distance(v::AbstractVector{<:Real}, ::MOI.RotatedSecondOrderCone)
    t = v[1]
    u = v[2]
    xs = v[3:end]
    return min(
        -t, -u,
        dot(xs, xs) - 2 * t * u
    )
end

function _distance(v::AbstractVector{<:Real}, s::MOI.GeometricMeanCone)
    t = v[1]
    xs = v[2:end]
    n = MOI.dimension(s) - 1
    return min(
        minimum(xs),
        t^n - prod(xs),
    )
end

function _distance(v::AbstractVector{<:Real}, ::MOI.ExponentialCone)
    x = v[1]
    y = v[2]
    z = v[3]
    return min(
        y,
        y * exp(x/y) - z,
    )
end

function _distance(v::AbstractVector{<:Real}, ::MOI.DualExponentialCone)
    u = v[1]
    v = v[2]
    w = v[3]
    return min(
        -u,
        -u*exp(v/u) - ℯ * w
    )
end

function _distance(v::AbstractVector{<:Real}, s::MOI.PowerCone)
    x = v[1]
    y = v[2]
    z = v[3]
    e = s.exponent
    return min(
        x,
        y,
        abs(z) - x^e * y^(1-e)
    )
end

function _distance(v::AbstractVector{<:Real}, s::MOI.DualPowerCone)
    u = v[1]
    v = v[2]
    w = v[3]
    e = s.exponent
    ce = 1-e
    return min(
        u,
        v,
        abs(w) - (u/e)^e * (v/ce)^ce
    )
end

function _distance(v::AbstractVector{<:Real}, s::MOI.RelativeEntropyCone)
    all(>=(0), v[2:end]) || return false
    n = (MOI.dimension(s)-1) ÷ 2
    u = v[1]
    v = v[2:(n+1)]
    w = v[(n+2):end]
    s = sum(w[i] * log(w[i]/v[i]) for i in eachindex(w))
    return s - u
end


function Base.in(v::AbstractVector{<:Real}, s::MOI.NormSpectralCone)
    _dim_match(v, s) || return false
    t = v[1]
    m = reshape(v[2:end], (s.row_dim, s.column_dim))
    s1 = LinearAlgebra.svd(m).S[1]
    return t ≥ s1
end

function Base.in(v::AbstractVector{<:Real}, s::MOI.NormNuclearCone)
    _dim_match(v, s) || return false
    t = v[1]
    m = reshape(v[2:end], (s.row_dim, s.column_dim))
    s = sum(LinearAlgebra.svd(m).S)
    return t ≥ s
end

function Base.in(v::AbstractVector{<:Real}, s::MOI.SOS1)
    _dim_match(v, s) || return false
    found_nonzero = false
    for x in v
        if !isapprox(x, 0)
            if found_nonzero
                return false
            end
            found_nonzero = true
        end
    end
    return true
end
