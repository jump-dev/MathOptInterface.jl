"""
    SUPPORTED_VARIABLE_SCALAR_SETS{T}

The union of scalar sets for `SingleVariable` constraints supported by
`Utilities.Box` (and therefore in `Utilities.Model`).
"""
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

# 0xcb = 0x80 | 0x40 | 0x8 | 0x2 | 0x1
const LOWER_BOUND_MASK = 0xcb
# 0xcd = 0x80 | 0x40 | 0x8 | 0x4 | 0x1
const UPPER_BOUND_MASK = 0xcd

single_variable_flag(::Type{<:MOI.EqualTo}) = 0x1
single_variable_flag(::Type{<:MOI.GreaterThan}) = 0x2
single_variable_flag(::Type{<:MOI.LessThan}) = 0x4
single_variable_flag(::Type{<:MOI.Interval}) = 0x8
single_variable_flag(::Type{MOI.Integer}) = 0x10
single_variable_flag(::Type{MOI.ZeroOne}) = 0x20
single_variable_flag(::Type{<:MOI.Semicontinuous}) = 0x40
single_variable_flag(::Type{<:MOI.Semiinteger}) = 0x80
# If a set is added here, a line should be added in
# `MOI.delete(::AbstractModel, ::MOI.VariableIndex)`

function flag_to_set_type(flag::UInt8, ::Type{T}) where {T}
    if flag == 0x1
        return MOI.EqualTo{T}
    elseif flag == 0x2
        return MOI.GreaterThan{T}
    elseif flag == 0x4
        return MOI.LessThan{T}
    elseif flag == 0x8
        return MOI.Interval{T}
    elseif flag == 0x10
        return MOI.Integer
    elseif flag == 0x20
        return MOI.ZeroOne
    elseif flag == 0x40
        return MOI.Semicontinuous{T}
    else
        @assert flag == 0x80
        return MOI.Semiinteger{T}
    end
end

# Julia doesn't infer `S1` correctly, so we use a function barrier to improve
# inference.
function _throw_if_lower_bound_set(variable, S2, mask, T)
    S1 = flag_to_set_type(mask, T)
    throw(MOI.LowerBoundAlreadySet{S1,S2}(variable))
    return
end

function throw_if_lower_bound_set(variable, S2, mask, T)
    lower_mask = mask & LOWER_BOUND_MASK
    if iszero(lower_mask)
        return  # No lower bound set.
    elseif iszero(single_variable_flag(S2) & LOWER_BOUND_MASK)
        return  # S2 isn't related to the lower bound.
    end
    return _throw_if_lower_bound_set(variable, S2, lower_mask, T)
end

# Julia doesn't infer `S1` correctly, so we use a function barrier to improve
# inference.
function _throw_if_upper_bound_set(variable, S2, mask, T)
    S1 = flag_to_set_type(mask, T)
    throw(MOI.UpperBoundAlreadySet{S1,S2}(variable))
    return
end

function throw_if_upper_bound_set(variable, S2, mask, T)
    upper_mask = mask & UPPER_BOUND_MASK
    if iszero(upper_mask)
        return  # No upper bound set.
    elseif iszero(single_variable_flag(S2) & UPPER_BOUND_MASK)
        return  # S2 isn't related to the upper bound.
    end
    return _throw_if_upper_bound_set(variable, S2, upper_mask, T)
end

function _lower_bound(
    set::Union{MOI.GreaterThan,MOI.Interval,MOI.Semicontinuous,MOI.Semiinteger},
)
    return set.lower
end

_lower_bound(set::MOI.EqualTo) = set.value

function _upper_bound(
    set::Union{MOI.LessThan,MOI.Interval,MOI.Semicontinuous,MOI.Semiinteger},
)
    return set.upper
end

_upper_bound(set::MOI.EqualTo) = set.value

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
        b.lower[offset+1] = _lower_bound(set)
    end
    if iszero(flag & UPPER_BOUND_MASK)
        b.upper[offset+1] = _no_upper_bound(T)
    else
        b.upper[offset+1] = _upper_bound(set)
    end
    return
end

function_constants(::Box{T}, row) where {T} = zero(T)

function set_from_constants(b::Box, ::Type{<:MOI.EqualTo}, index)
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

function set_from_constants(b::Box, S::Type{<:MOI.LessThan}, index)
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

# Function used in MOI.Utilities.AbstractModel.

function _merge_bounds(b::Box, index, set)
    flag = single_variable_flag(typeof(set))
    if !iszero(flag & LOWER_BOUND_MASK)
        b.lower[index] = _lower_bound(set)
    end
    if !iszero(flag & UPPER_BOUND_MASK)
        b.upper[index] = _upper_bound(set)
    end
    return
end

function _add_free(b::Box{T}) where {T}
    push!(b.lower, _no_lower_bound(T))
    push!(b.upper, _no_upper_bound(T))
    return
end
