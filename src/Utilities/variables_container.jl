abstract type AbstractVectorBounds end

function set_from_constants(
    b::AbstractVectorBounds,
    ::Type{<:MOI.EqualTo},
    index,
)
    return MOI.EqualTo(b.lower[index])
end

function set_from_constants(
    b::AbstractVectorBounds,
    S::Type{<:Union{MOI.GreaterThan,MOI.EqualTo}},
    index,
)
    # Lower and upper bounds are equal for `EqualTo`, we can take either of them.
    return S(b.lower[index])
end

function set_from_constants(
    b::AbstractVectorBounds,
    S::Type{<:MOI.LessThan},
    index,
)
    return S(b.upper[index])
end

function set_from_constants(
    b::AbstractVectorBounds,
    S::Type{<:Union{MOI.Interval,MOI.Semicontinuous,MOI.Semiinteger}},
    index,
)
    return S(b.lower[index], b.upper[index])
end

function set_from_constants(
    ::AbstractVectorBounds,
    S::Type{<:Union{MOI.Integer,MOI.ZeroOne}},
    index,
)
    return S()
end

"""
    SUPPORTED_VARIABLE_SCALAR_SETS{T}

The union of scalar sets for `VariableIndex` constraints supported by
`Utilities.Hyperrectangle` and `Utilities.VariablesContainer`.
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

# 0xcb = 0x0080 | 0x0040 | 0x0008 | 0x0002 | 0x0001
const _LOWER_BOUND_MASK = 0x00cb
# 0xcd = 0x0080 | 0x0040 | 0x0008 | 0x0004 | 0x0001
const _UPPER_BOUND_MASK = 0x00cd

const _DELETED_VARIABLE = 0x8000

_single_variable_flag(::Type{<:MOI.EqualTo}) = 0x0001
_single_variable_flag(::Type{<:MOI.GreaterThan}) = 0x0002
_single_variable_flag(::Type{<:MOI.LessThan}) = 0x0004
_single_variable_flag(::Type{<:MOI.Interval}) = 0x0008
_single_variable_flag(::Type{MOI.Integer}) = 0x0010
_single_variable_flag(::Type{MOI.ZeroOne}) = 0x0020
_single_variable_flag(::Type{<:MOI.Semicontinuous}) = 0x0040
_single_variable_flag(::Type{<:MOI.Semiinteger}) = 0x0080

function _flag_to_set_type(flag::UInt16, ::Type{T}) where {T}
    if flag == 0x0001
        return MOI.EqualTo{T}
    elseif flag == 0x0002
        return MOI.GreaterThan{T}
    elseif flag == 0x0004
        return MOI.LessThan{T}
    elseif flag == 0x0008
        return MOI.Interval{T}
    elseif flag == 0x0010
        return MOI.Integer
    elseif flag == 0x0020
        return MOI.ZeroOne
    elseif flag == 0x0040
        return MOI.Semicontinuous{T}
    else
        @assert flag == 0x0080
        return MOI.Semiinteger{T}
    end
end

# Julia doesn't infer `S1` correctly, so we use a function barrier to improve
# inference.
function _throw_if_lower_bound_set_inner(variable, S2, mask, T)
    S1 = _flag_to_set_type(mask, T)
    throw(MOI.LowerBoundAlreadySet{S1,S2}(variable))
    return
end

function _throw_if_lower_bound_set(variable, S2, mask, T)
    lower_mask = mask & _LOWER_BOUND_MASK
    if iszero(lower_mask)
        return  # No lower bound set.
    elseif iszero(_single_variable_flag(S2) & _LOWER_BOUND_MASK)
        return  # S2 isn't related to the lower bound.
    end
    return _throw_if_lower_bound_set_inner(variable, S2, lower_mask, T)
end

# Julia doesn't infer `S1` correctly, so we use a function barrier to improve
# inference.
function _throw_if_upper_bound_set_inner(variable, S2, mask, T)
    S1 = _flag_to_set_type(mask, T)
    throw(MOI.UpperBoundAlreadySet{S1,S2}(variable))
    return
end

function _throw_if_upper_bound_set(variable, S2, mask, T)
    upper_mask = mask & _UPPER_BOUND_MASK
    if iszero(upper_mask)
        return  # No upper bound set.
    elseif iszero(_single_variable_flag(S2) & _UPPER_BOUND_MASK)
        return  # S2 isn't related to the upper bound.
    end
    return _throw_if_upper_bound_set_inner(variable, S2, upper_mask, T)
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

# Use `-Inf` and `Inf` for `AbstractFloat` subtypes.
_no_lower_bound(::Type{T}) where {T} = zero(T)
_no_lower_bound(::Type{T}) where {T<:AbstractFloat} = typemin(T)
_no_upper_bound(::Type{T}) where {T} = zero(T)
_no_upper_bound(::Type{T}) where {T<:AbstractFloat} = typemax(T)

###
### VariablesContainer
###
### For use in MOI.Utilities.Model
###

"""
    struct VariablesContainer{T} <: AbstractVectorBounds
        set_mask::Vector{UInt16}
        lower::Vector{T}
        upper::Vector{T}
    end

A struct for storing variables and VariableIndex-related constraints. Used in
`MOI.Utilities.Model` by default.
"""
mutable struct VariablesContainer{T} <: AbstractVectorBounds
    set_mask::Vector{UInt16}
    lower::Vector{T}
    upper::Vector{T}
end

function VariablesContainer{T}() where {T}
    return VariablesContainer{T}(UInt16[], T[], T[])
end

function Base.:(==)(a::VariablesContainer, b::VariablesContainer)
    return a.set_mask == b.set_mask && a.lower == b.lower && a.upper == b.upper
end

function MOI.empty!(b::VariablesContainer)
    empty!(b.set_mask)
    empty!(b.lower)
    empty!(b.upper)
    return b
end

function MOI.is_empty(b::VariablesContainer)
    if length(b.set_mask) == 0
        return true
    end
    return all(isequal(_DELETED_VARIABLE), b.set_mask)
end

function Base.resize!(b::VariablesContainer, n)
    resize!(b.set_mask, n)
    resize!(b.lower, n)
    resize!(b.upper, n)
    return
end

function MOI.add_variable(b::VariablesContainer{T}) where {T}
    push!(b.set_mask, 0x0000)
    push!(b.lower, _no_lower_bound(T))
    push!(b.upper, _no_upper_bound(T))
    x = MOI.VariableIndex(length(b.set_mask))
    return x
end

function MOI.get(b::VariablesContainer, ::MOI.ListOfVariableIndices)
    return MOI.VariableIndex[
        MOI.VariableIndex(i) for
        i in 1:length(b.set_mask) if b.set_mask[i] != _DELETED_VARIABLE
    ]
end

function MOI.is_valid(b::VariablesContainer, x::MOI.VariableIndex)
    mask = get(b.set_mask, x.value, _DELETED_VARIABLE)
    return mask != _DELETED_VARIABLE
end

function MOI.get(b::VariablesContainer, ::MOI.NumberOfVariables)::Int64
    if length(b.set_mask) == 0
        return 0
    end
    return sum(x != _DELETED_VARIABLE for x in b.set_mask)
end

function MOI.supports_constraint(
    ::VariablesContainer{T},
    ::Type{MOI.VariableIndex},
    ::Type{<:SUPPORTED_VARIABLE_SCALAR_SETS{T}},
) where {T}
    return true
end

function MOI.add_constraint(
    b::VariablesContainer{T},
    f::MOI.VariableIndex,
    set::S,
) where {T,S<:SUPPORTED_VARIABLE_SCALAR_SETS}
    flag = _single_variable_flag(S)
    mask = b.set_mask[f.value]
    _throw_if_lower_bound_set(f, S, mask, T)
    _throw_if_upper_bound_set(f, S, mask, T)
    if !iszero(flag & _LOWER_BOUND_MASK)
        b.lower[f.value] = _lower_bound(set)
    end
    if !iszero(flag & _UPPER_BOUND_MASK)
        b.upper[f.value] = _upper_bound(set)
    end
    b.set_mask[f.value] = mask | flag
    return MOI.ConstraintIndex{MOI.VariableIndex,S}(f.value)
end

function MOI.delete(
    b::VariablesContainer{T},
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
) where {T,S}
    MOI.throw_if_not_valid(b, ci)
    flag = _single_variable_flag(S)
    b.set_mask[ci.value] &= ~flag
    if !iszero(flag & _LOWER_BOUND_MASK)
        b.lower[ci.value] = _no_lower_bound(T)
    end
    if !iszero(flag & _UPPER_BOUND_MASK)
        b.upper[ci.value] = _no_upper_bound(T)
    end
    return
end

function MOI.delete(b::VariablesContainer, x::MOI.VariableIndex)
    MOI.throw_if_not_valid(b, x)
    b.set_mask[x.value] = _DELETED_VARIABLE
    return
end

function MOI.is_valid(
    b::VariablesContainer,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
) where {S}
    if !(1 <= ci.value <= length(b.set_mask))
        return false
    end
    return !iszero(b.set_mask[ci.value] & _single_variable_flag(S))
end

function MOI.get(
    model::VariablesContainer,
    ::MOI.ConstraintFunction,
    ci::CI{MOI.VariableIndex},
)
    MOI.throw_if_not_valid(model, ci)
    return MOI.VariableIndex(ci.value)
end

function MOI.set(
    ::VariablesContainer,
    ::MOI.ConstraintFunction,
    ::MOI.ConstraintIndex{MOI.VariableIndex},
    ::MOI.VariableIndex,
)
    return throw(MOI.SettingVariableIndexNotAllowed())
end

function MOI.get(
    model::VariablesContainer,
    ::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
) where {S}
    MOI.throw_if_not_valid(model, ci)
    return set_from_constants(model, S, ci.value)
end

function MOI.set(
    model::VariablesContainer{T},
    ::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
    set::S,
) where {T,S<:SUPPORTED_VARIABLE_SCALAR_SETS{T}}
    MOI.throw_if_not_valid(model, ci)
    flag = _single_variable_flag(S)
    if !iszero(flag & _LOWER_BOUND_MASK)
        model.lower[ci.value] = _lower_bound(set)
    end
    if !iszero(flag & _UPPER_BOUND_MASK)
        model.upper[ci.value] = _upper_bound(set)
    end
    return
end

function MOI.get(
    b::VariablesContainer,
    ::MOI.NumberOfConstraints{MOI.VariableIndex,S},
)::Int64 where {S}
    flag = _single_variable_flag(S)
    return count(mask -> !iszero(flag & mask), b.set_mask)
end

function _add_constraint_type(
    list,
    b::VariablesContainer,
    S::Type{<:MOI.AbstractScalarSet},
)
    flag = _single_variable_flag(S)::UInt16
    if any(mask -> !iszero(flag & mask), b.set_mask)
        push!(list, (MOI.VariableIndex, S))
    end
    return
end

function MOI.get(
    b::VariablesContainer{T},
    ::MOI.ListOfConstraintTypesPresent,
) where {T}
    list = Tuple{Type,Type}[]
    _add_constraint_type(list, b, MOI.EqualTo{T})
    _add_constraint_type(list, b, MOI.GreaterThan{T})
    _add_constraint_type(list, b, MOI.LessThan{T})
    _add_constraint_type(list, b, MOI.Interval{T})
    _add_constraint_type(list, b, MOI.Semicontinuous{T})
    _add_constraint_type(list, b, MOI.Semiinteger{T})
    _add_constraint_type(list, b, MOI.Integer)
    _add_constraint_type(list, b, MOI.ZeroOne)
    return list
end

function MOI.get(
    b::VariablesContainer,
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,S},
) where {S}
    list = MOI.ConstraintIndex{MOI.VariableIndex,S}[]
    flag = _single_variable_flag(S)
    for (index, mask) in enumerate(b.set_mask)
        if !iszero(mask & flag)
            push!(list, MOI.ConstraintIndex{MOI.VariableIndex,S}(index))
        end
    end
    return list
end

# These `MOI.ModelLike` fallbacks have to be redefined here as
# `VariablesContainer` is not a subtype of `MOI.ModelLike`

function MOI.throw_if_not_valid(b::VariablesContainer, index)
    if !MOI.is_valid(b, index)
        throw(MOI.InvalidIndex(index))
    end
end

function MOI.set(
    ::VariablesContainer,
    ::MOI.ConstraintFunction,
    ci::MOI.ConstraintIndex,
    func,
)
    return throw(MOI.FunctionTypeMismatch{MOI.func_type(ci),typeof(func)}())
end

function MOI.set(
    ::VariablesContainer,
    ::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{MOI.VariableIndex},
    set,
)
    return throw(MOI.SetTypeMismatch{MOI.set_type(ci),typeof(set)}())
end

function MOI.supports_constraint(
    ::VariablesContainer,
    ::Type{<:MOI.AbstractFunction},
    ::Type{<:MOI.AbstractSet},
)
    return false
end

function MOI.add_constraint(
    ::VariablesContainer,
    func::MOI.AbstractFunction,
    set::MOI.AbstractSet,
)
    return throw(MOI.UnsupportedConstraint{typeof(func),typeof(set)}())
end

###
### Hyperrectangle
###

"""
    struct Hyperrectangle{T} <: AbstractVectorBounds
        lower::Vector{T}
        upper::Vector{T}
    end

A struct for the .constants field in MatrixOfConstraints.
"""
struct Hyperrectangle{T} <: AbstractVectorBounds
    lower::Vector{T}
    upper::Vector{T}
end

Hyperrectangle{T}() where {T} = Hyperrectangle{T}(T[], T[])

function Base.:(==)(a::Hyperrectangle, b::Hyperrectangle)
    return a.lower == b.lower && a.upper == b.upper
end

function Base.empty!(b::Hyperrectangle)
    empty!(b.lower)
    empty!(b.upper)
    return b
end

function Base.resize!(b::Hyperrectangle, n)
    resize!(b.lower, n)
    resize!(b.upper, n)
    return
end

function load_constants(
    b::Hyperrectangle{T},
    offset,
    set::SUPPORTED_VARIABLE_SCALAR_SETS{T},
) where {T}
    flag = _single_variable_flag(typeof(set))
    if iszero(flag & _LOWER_BOUND_MASK)
        b.lower[offset+1] = _no_lower_bound(T)
    else
        b.lower[offset+1] = _lower_bound(set)
    end
    if iszero(flag & _UPPER_BOUND_MASK)
        b.upper[offset+1] = _no_upper_bound(T)
    else
        b.upper[offset+1] = _upper_bound(set)
    end
    return
end

function_constants(::Hyperrectangle{T}, row) where {T} = zero(T)
