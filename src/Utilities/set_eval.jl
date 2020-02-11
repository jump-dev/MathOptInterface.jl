
function _correct_distance_to_set(d::Real)
    return ifelse(d < 0, zero(d), d)
end

function _correct_distance_to_set(d::AbstractVector{T}) where {T <: Real}
    return [ifelse(di < 0, zero(T), di) for di in d]
end

function distance_to_set(v, s)
    length(v) != MOI.dimension(s) && throw(DimensionMismatch("Mismatch between v and s"))
    d = _distance(v, s)
    return _correct_distance_to_set(d)
end

_distance(v::Real, s::MOI.LessThan) = v - s.upper
_distance(v::Real, s::MOI.GreaterThan) = s.lower - v
_distance(v::Real, s::MOI.EqualTo) = abs(v - s.value)

_distance(v::Real, s::MOI.Interval) = max(s.lower - v, v - s.upper)

_distance(v::AbstractVector{<:Real}, ::MOI.Reals) = false * v

_distance(v::AbstractVector{<:Real}, ::MOI.Zeros) = abs.(v)

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
    return max(-t, dot(xs, xs) - t^2) # avoids sqrt
end

function _distance(v::AbstractVector{<:Real}, ::MOI.RotatedSecondOrderCone)
    t = v[1]
    u = v[2]
    xs = v[3:end]
    return max(
        -t, -u,
        dot(xs, xs) - 2 * t * u
    )
end

function _distance(v::AbstractVector{<:Real}, s::MOI.GeometricMeanCone)
    t = v[1]
    xs = v[2:end]
    n = MOI.dimension(s) - 1
    return max(
        -minimum(xs),
        t^n - prod(xs),
    )
end

function _distance(v::AbstractVector{<:Real}, ::MOI.ExponentialCone)
    x = v[1]
    y = v[2]
    z = v[3]
    return max(
        y,
        y * exp(x/y) - z,
    )
end

function _distance(v::AbstractVector{<:Real}, ::MOI.DualExponentialCone)
    u = v[1]
    v = v[2]
    w = v[3]
    return max(
        -u,
        -u*exp(v/u) - ℯ * w
    )
end

function _distance(v::AbstractVector{<:Real}, s::MOI.PowerCone)
    x = v[1]
    y = v[2]
    z = v[3]
    e = s.exponent
    return max(
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
    return max(
        u,
        v,
        abs(w) - (u/e)^e * (v/ce)^ce
    )
end

function _distance(v::AbstractVector{<:Real}, set::MOI.RelativeEntropyCone)
    all(>=(0), v[2:end]) || return false
    n = (MOI.dimension(set)-1) ÷ 2
    u = v[1]
    v = v[2:(n+1)]
    w = v[(n+2):end]
    s = sum(w[i] * log(w[i]/v[i]) for i in eachindex(w))
    return max(
        -minimum(v[2:end]),
        s - u,
    )
end


function _distance(v::AbstractVector{<:Real}, s::MOI.NormSpectralCone)
    t = v[1]
    m = reshape(v[2:end], (s.row_dim, s.column_dim))
    s1 = LinearAlgebra.svd(m).S[1]
    return s1 - t
end

function _distance(v::AbstractVector{<:Real}, s::MOI.NormNuclearCone)
    t = v[1]
    m = reshape(v[2:end], (s.row_dim, s.column_dim))
    s1 = sum(LinearAlgebra.svd(m).S)
    return s1 - t
end

# return the second largest absolute value (=0 if SOS1 respected)
function _distance(v::AbstractVector{T}, s::MOI.SOS1) where {T <: Real}
    first_non_zero = second_non_zero = -1
    v1 = zero(T)
    v2 = zero(T)
    for (i, x) in enumerate(v)
        if !isapprox(x, 0)
            if first_non_zero < 0
                first_non_zero = i
                v1 = x
            elseif second_non_zero < 0
                second_non_zero = i
                v2 = x
            else # replace one of the two
                xa = abs(x)
                if xa > v1
                    second_non_zero = first_non_zero
                    v2 = v1
                    first_non_zero = i
                    v1 = xa
                elseif v[second_non_zero] <= x < v[first_non_zero]
                    second_non_zero = i
                    v2 = xa
                end                                    
            end
        end
    end
    return v2
end

function _distance(v::T, ::MOI.ZeroOne) where {T <: Real}
    return min(abs(v - zero(T)), abs(v - one(T)))
end

function _distance(v::Real, ::MOI.Integer)
    return min(abs(v - floor(v)), abs(v - ceil(v)))
end
