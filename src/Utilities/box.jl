# Sets setting lower bound:
extract_lower_bound(set::MOI.EqualTo) = set.value
function extract_lower_bound(
    set::Union{MOI.GreaterThan,MOI.Interval,MOI.Semicontinuous,MOI.Semiinteger},
)
    return set.lower
end
# 0xcb = 0x80 | 0x40 | 0x8 | 0x2 | 0x1
const LOWER_BOUND_MASK = 0xcb

# Sets setting upper bound:
extract_upper_bound(set::MOI.EqualTo) = set.value
function extract_upper_bound(
    set::Union{MOI.LessThan,MOI.Interval,MOI.Semicontinuous,MOI.Semiinteger},
)
    return set.upper
end
# 0xcd = 0x80 | 0x40 | 0x8 | 0x4 | 0x1
const UPPER_BOUND_MASK = 0xcd

const SUPPORTED_VARIABLE_SCALAR_SETS{T} = Union{
    MOI.EqualTo{T},
    MOI.GreaterThan{T},
    MOI.LessThan{T},
    MOI.Interval{T},
    MOI.Integer,
    MOI.ZeroOne,
    MOI.Semicontinuous{T},
    MOI.Semiinteger{T},
}


"""
    struct Box{T}
        lower::Vector{T}
        upper::Vector{T}
    end

Stores the constants of scalar constraints with the lower bound of the set in
`lower` and the upper bound in `upper`.
"""
struct Box{T}
    lower::Vector{T}
    upper::Vector{T}
end

Box{T}() where {T} = Box{T}(T[], T[])

Base.:(==)(a::Box, b::Box) = a.lower == b.lower && a.upper == b.upper

function Base.empty!(b::Box)
    empty!(b.lower)
    empty!(b.upper)
    return b
end

function Base.resize!(b::Box, n)
    resize!(b.lower, n)
    resize!(b.upper, n)
    return
end

function add_free(b::Box{T}) where {T}
    push!(b.lower, _no_lower_bound(T))
    push!(b.upper, _no_upper_bound(T))
    return
end

# Use `-Inf` and `Inf` for `AbstractFloat` subtypes.
_no_lower_bound(::Type{T}) where {T} = zero(T)
_no_lower_bound(::Type{T}) where {T<:AbstractFloat} = typemin(T)
_no_upper_bound(::Type{T}) where {T} = zero(T)
_no_upper_bound(::Type{T}) where {T<:AbstractFloat} = typemax(T)

function load_constants(
    b::Box{T},
    offset,
    set::SUPPORTED_VARIABLE_SCALAR_SETS{T},
) where {T}
    flag = single_variable_flag(typeof(set))
    if iszero(flag & LOWER_BOUND_MASK)
        b.lower[offset+1] = _no_lower_bound(T)
    else
        b.lower[offset+1] = extract_lower_bound(set)
    end
    if iszero(flag & UPPER_BOUND_MASK)
        b.upper[offset+1] = _no_upper_bound(T)
    else
        b.upper[offset+1] = extract_upper_bound(set)
    end
    return
end

function merge_bounds(b::Box, index, set)
    flag = single_variable_flag(typeof(set))
    if !iszero(flag & LOWER_BOUND_MASK)
        b.lower[index] = extract_lower_bound(set)
    end
    if !iszero(flag & UPPER_BOUND_MASK)
        b.upper[index] = extract_upper_bound(set)
    end
end

function set_from_constants(
    b::Box,
    ::Type{<:MOI.EqualTo},
    index,
)
    return MOI.EqualTo(b.lower[index])
end
function set_from_constants(
    b::Box,
    S::Type{<:Union{MOI.GreaterThan,MOI.EqualTo}},
    index,
)
    # Lower and upper bounds are equal for `EqualTo`, we can take either of them.
    return S(b.lower[index])
end
function set_from_constants(
    b::Box,
    S::Type{<:MOI.LessThan},
    index,
)
    return S(b.upper[index])
end
function set_from_constants(
    b::Box,
    S::Type{<:Union{MOI.Interval,MOI.Semicontinuous,MOI.Semiinteger}},
    index,
)
    return S(b.lower[index], b.upper[index])
end
function set_from_constants(
    ::Box,
    S::Type{<:Union{MOI.Integer,MOI.ZeroOne}},
    index,
)
    return S()
end
