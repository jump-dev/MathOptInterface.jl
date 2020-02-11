
Base.in(v, s::MOI.LessThan) = v <= s.upper
Base.in(v, s::MOI.GreaterThan) = v >= s.lower
Base.in(v, s::MOI.EqualTo) = v ≈ s.value

Base.in(v, s::MOI.Interval) = s.lower <= v <= s.upper

_dim_match(v::AbstractVector, s::MOI.AbstractVectorSet) = length(v) == MOI.dimension(s)

Base.in(v::AbstractVector{<:Real}, s::MOI.Reals) = _dim_match(v, s)
Base.in(v, s::MOI.Zeros) = _dim_match(v, s) && all(≈(0), v)

Base.in(v::AbstractVector{<:Real}, s::MOI.Nonnegatives) = _dim_match(v, s) && all(≥(0), v)
Base.in(v::AbstractVector{<:Real}, s::MOI.Nonpositives) = _dim_match(v, s) && all(≤(0), v)

# Norm cones

function Base.in(v::AbstractVector{<:Real}, s::MOI.NormInfinityCone)
    _dim_match(v, s) || return false
    t = first(v)
    xs = v[2:end]
    for x in xs
        if abs(x) > t
            return false
        end
    end
    return true
end

function Base.in(v::AbstractVector{<:Real}, s::MOI.NormOneCone)
    _dim_match(v, s) || return false
    t = v[1]
    xs = v[2:end]
    return t >= sum(abs, xs)
end

function Base.in(v::AbstractVector{<:Real}, s::MOI.SecondOrderCone)
    _dim_match(v, s) || return false
    t = v[1]
    xs = v[2:end]
    return t ≥ 0 && t^2 >= dot(xs, xs) # avoids sqrt
end


function Base.in(v::AbstractVector{<:Real}, s::MOI.RotatedSecondOrderCone)
    _dim_match(v, s) || return false
    t = v[1]
    u = v[2]
    t ≥ 0 && u ≥ 0 || return false
    xs = v[3:end]
    return 2 * t * u >= dot(xs, xs)
end

function Base.in(v::AbstractVector{<:Real}, s::MOI.GeometricMeanCone)
    t = v[1]
    xs = v[2:end]
    n = MOI.dimension(s) - 1
    xs in MOI.Nonnegatives(n) || return false # this also checks dimensions
    return t^n <= prod(xs)
end

function Base.in(v::AbstractVector{<:Real}, s::MOI.ExponentialCone)
    length(v) == 3 || return false
    @inbounds y = v[2]
    y > 0 || return false
    @inbounds x = v[1]
    @inbounds z = v[3]
    return y * exp(x/y) <= z
end

function Base.in(v::AbstractVector{<:Real}, s::MOI.DualExponentialCone)
    length(v) == 3 || return false
    @inbounds u = v[1]
    u < 0 || return false
    @inbounds v = v[2]
    @inbounds w = v[3]
    return -u*exp(v/u) ≤ ℯ * w
end

function Base.in(v::AbstractVector{<:Real}, s::MOI.PowerCone)
    length(v) == 3 || return false
    @inbounds x = v[1]
    x ≥ 0 || return false
    @inbounds y = v[2]
    y ≥ 0 || return false
    @inbounds z = v[3]
    e = s.exponent
    return x^e * y^(1-e) ≥ abs(z)
end

function Base.in(v::AbstractVector{<:Real}, s::MOI.DualPowerCone)
    length(v) == 3 || return false
    @inbounds u = v[1]
    u ≥ 0 || return false
    @inbounds v = v[2]
    v ≥ 0 || return false
    @inbounds w = v[3]
    e = s.exponent
    ce = 1-e
    return (u/e)^e * (v/ce)^ce ≥ abs(w)
end

function Base.in(v::AbstractVector{<:Real}, s::MOI.RelativeEntropyCone)
    _dim_match(v, s) || return false
    all(>=(0), v[2:end]) || return false
    n = (MOI.dimension(s)-1) ÷ 2
    @inbounds u = v[1]
    v = [2:n+1]
    w = [n+2:end]
    @inbounds s = sum(w[i] * log(w[i]/v[i]) for i in eachindex(w))
    return u ≥ s
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
