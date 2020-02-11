
Base.in(v, s::MOI.LessThan) = v <= s.upper
Base.in(v, s::MOI.GreaterThan) = v >= s.lower
Base.in(v, s::MOI.EqualTo) = v ≈ s.value

Base.in(v, s::MOI.Interval) = s.lower <= v <= s.upper

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

_dim_match(v::AbstractVector, s::MOI.AbstractVectorSet) = length(v) == MOI.dimension(s)
