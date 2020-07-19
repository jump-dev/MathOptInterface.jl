module DoubleDicts

import MathOptInterface

const MOI = MathOptInterface
const CI = MOI.ConstraintIndex
const AD = AbstractDict

DataTypePair = Tuple{DataType, DataType}

abstract type AbstractDoubleDict{K, V, IK, DI<:AD{IK}, DO<:AD{K, DI}} <: AD{CI, V} end

"""
    DoubleDict{V}

Optimized dictionary to map `ConstraintIndex` (`CI`) to values of type `V`.
Works as a `AbstractDict{CI, V}` with minimal differences.
Note that `CI` is not a concrete type, opposed to `CI{MOI.SingleVariable, MOI.Integers}`,
which is a concrete type.

When optimal performance or type stability is required its possible to obtain a
fully type stable dictionary with values of type `V` and keys of type
`CI{MOI.SingleVariable, MOI.Integers}` from the dictionary `dict`, for instance:

    ``inner = dict[MOI.SingleVariable, MOI.Integers]``
"""
struct DoubleDict{V, DI, DO} <: AbstractDoubleDict{DataTypePair, V, Int64, DI, DO}
    dict::DO
    DoubleDict{V}() where V = new{V, Dict{Int64, V}, Dict{DataTypePair, Dict{Int64, V}}}(
        Dict{DataTypePair, Dict{Int64, V}}())
end

"""
    IndexDoubleDict

Specialized version of `DoubleDict` in which keys and values are of type
`ConstraintIndex` (`CI`).

This is an optimized dictionary to map `ConstraintIndex` (`CI`) to values of type `CI`.
Works as a `AbstractDict{CI, V}` with minimal differences.
Note that `CI` is not a concrete type, opposed to `CI{MOI.SingleVariable, MOI.Integers}`,
which is a concrete type.

When optimal performance or type stability is required its possible to obtain a
fully type stable dictionary with values of type
`CI{MOI.SingleVariable, MOI.Integers}` and keys of type
`CI{MOI.SingleVariable, MOI.Integers}` from the dictionary `dict`, for instance:

    ``inner = dict[MOI.SingleVariable, MOI.Integers]``
"""
struct IndexDoubleDict{DI, DO} <: AbstractDoubleDict{DataTypePair, CI, Int64, DI, DO}
    dict::DO
    IndexDoubleDict() = new{Dict{Int64, Int64}, Dict{DataTypePair, Dict{Int64, Int64}}}(
        Dict{DataTypePair, Dict{Int64, Int64}}())
end

struct FunctionSetDoubleDict{DI, DO} <: AbstractDoubleDict{DataTypePair, Tuple, Int64, DI, DO}
    dict::DO
    FunctionSetDoubleDict() = new{Dict{Int64}, Dict{DataTypePair, Dict{Int64}}}(
        Dict{DataTypePair, Dict{Int64}}())
end

MainIndexDoubleDict = IndexDoubleDict{Dict{Int64, Int64}, Dict{Tuple{DataType, DataType}, Dict{Int64, Int64}}}

@inline function typed_value(::DoubleDict{V}, v::V, ::Type{F}, ::Type{S})::V where {V, F, S}
    v
end
@inline function typed_value(::IndexDoubleDict, v::Int64, ::Type{F}, ::Type{S})::CI{F,S} where {F, S}
    CI{F,S}(v)
end
@inline function typed_value(::FunctionSetDoubleDict, v::Tuple{F,S}, ::Type{F}, ::Type{S})::Tuple{F,S} where {F, S}
    v
end

function Base.sizehint!(::AbstractDoubleDict, ::Integer)
    throw(ErrorException("sizehint!(d::DoubleDict, ::Integer) has no proper" *
    " meaning for DoubleDict, use sizehint!(d[F,S], n::Integer) " *
    "instead."))
end

function Base.length(d::AbstractDoubleDict)
    len = 0
    for inner in values(d.dict)
        len += length(inner)
    end
    return len
end

function Base.haskey(dict::AbstractDoubleDict, key::CI{F,S}) where {F,S}
    inner = get(dict.dict, (F,S), nothing)
    if inner !== nothing
        inner = dict.dict[(F,S)]
        return haskey(inner, key.value)
    else
        return false
    end
end

function Base.getindex(dict::AbstractDoubleDict, key::CI{F,S}) where {F,S}
    inner = dict.dict[(F,S)]
    k_value = key.value::Int64
    return typed_value(dict, inner[k_value], F, S)
end

function init_inner_dict(::AbstractDoubleDict{K, V}, ::Type{F}, ::Type{S}) where {K,V,F,S}
    return Dict{Int64, V}()
end
function init_inner_dict(::FunctionSetDoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    return Dict{Int64, Tuple{F,S}}()
end
function init_inner_dict(::IndexDoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    return Dict{Int64, Int64}()
end

"""
    lazy_get(dict::DoubleDict, ::Type{F}, ::Type{S})

description
"""
function lazy_get(dict::AbstractDoubleDict{K, V}, ::Type{F}, ::Type{S}) where {K,V,F,S}
    inner = get(dict.dict, (F,S), nothing)
    if inner === nothing
        return dict.dict[(F,S)] = init_inner_dict(dict, F, S)
    end
    return inner
end

function Base.setindex!(dict::AbstractDoubleDict{K, V}, value::V, key::CI{F,S}) where {K,V,F,S}
    v_value = value
    k_value = key.value::Int64
    inner = lazy_get(dict, F, S)
    inner[k_value] = v_value
    return value
end
function Base.setindex!(dict::DoubleDict{V}, value::V, key::CI{F,S}) where {V,F,S}
    v_value = value
    k_value = key.value::Int64
    inner = lazy_get(dict, F, S)::Dict{Int64, V}
    inner[k_value] = v_value
    return value
end
function Base.setindex!(dict::IndexDoubleDict, value::CI{F,S}, key::CI{F,S}) where {F,S}
    v_value = value.value::Int64
    k_value = key.value::Int64
    inner = lazy_get(dict, F, S)::Dict{Int64, Int64}
    inner[k_value] = v_value
    return value
end

function Base.empty!(d::AbstractDoubleDict)::Nothing
    Base.empty!(d.dict)
    return
end

function Base.delete!(d::AbstractDoubleDict, key::CI{F,S}) where {F,S}
    k_value = key.value::Int64
    inner = lazy_get(d, F, S)
    delete!(inner, k_value)
    return d
end

function Base.isempty(d::AbstractDoubleDict)
    if isempty(d.dict)
        return true
    end
    for val in values(d.dict)
        if !isempty(val)
            return false
        end
    end
    return true
end

function Base.values(d::IndexDoubleDict)::Vector{CI}
    out = CI[]
    for ((F, S), inner) in d.dict
        append!(out, CI{F,S}.(values(inner)))
    end
    return out
end
function Base.values(d::AbstractDoubleDict{K, V})::Vector{V} where {K, V}
    out = V[]
    for ((F, S), inner) in d.dict
        append!(out, values(inner))
    end
    return out
end

function Base.keys(d::AbstractDoubleDict)
    out = CI[]
    for ((F,S), inner) in d.dict
        append!(out, CI{F,S}.(keys(inner)))
    end
    return out
end

function Base.iterate(d::AbstractDoubleDict)
    o_next = iterate(d.dict)
    if o_next === nothing
        return nothing
    end
    (o_i, o_state) = o_next
    ((F,S), inner) = o_i
    i_next = iterate(inner)
    while i_next === nothing
        o_next = iterate(d.dict, o_state)
        if o_next === nothing
            return nothing
        end
        (o_i, o_state) = o_next
        ((F,S), inner) = o_i
        i_next = iterate(inner)
    end
    (i_i, i_state) = i_next
    return CI{F,S}(i_i[1]) => typed_value(d, i_i[2], F, S), (i_state, (o_i, o_state))
end
function Base.iterate(d::AbstractDoubleDict, state)
    (i_state, (o_i, o_state)) = state
    ((F,S), inner) = o_i
    i_next = iterate(inner, i_state)
    while i_next === nothing
        o_next = iterate(d.dict, o_state)
        if o_next === nothing
            return nothing
        end
        (o_i, o_state) = o_next
        ((F,S), inner) = o_i
        i_next = iterate(inner)
    end
    (i_i, i_state) = i_next
    return CI{F,S}(i_i[1]) => typed_value(d, i_i[2], F, S), (i_state, (o_i, o_state))
end

abstract type AbstractWithType{F, S, V, DI, DD} <: AD{CI{F,S}, V} end

"""
    WithType{D<:DoubleDict, F<:MOI.AbstractFunction, S<:MOI.AbstractSet}

Used to specialize methods and iterators for a given contraint type CI{F,S}.
"""
mutable struct WithType{F, S, V, DI, DD} <: AbstractWithType{F, S, V, DI, DD}
    dict::DD
    inner::Union{DI, Nothing}
    function WithType{F, S}(d::DD) where
        {V, DI<:AD{Int64, V}, DO<:AD{DataTypePair, DI}, DD<:DoubleDict{V, DI, DO}, F, S}
        inner = get(d.dict, (F, S), nothing)
        new{F, S, V, DI, DD}(d, inner)
    end
end
mutable struct IndexWithType{F, S, V, DI, DD} <: AbstractWithType{F, S, CI{F,S}, DI, DD}
    dict::DD
    inner::Union{DI, Nothing}
    function IndexWithType{F, S}(d::DD) where
        {DI<:AD{Int64, Int64}, DO<:AD{DataTypePair, DI}, DD<:IndexDoubleDict{DI, DO}, F, S}
        inner = get(d.dict, (F, S), nothing)::Union{DI, Nothing}
        new{F, S, CI{F,S}, DI, DD}(d, inner)
    end
end
mutable struct FunctionSetWithType{F, S, V, DI, DD} <: AbstractWithType{F, S, Tuple{F,S}, DI, DD}
    dict::DD
    inner::Union{DI, Nothing}
    function FunctionSetWithType{F, S}(d::DD) where
        {F, S, DI<:AD{Int64}, DO<:AD{DataTypePair, DI}, DD<:FunctionSetDoubleDict{DI, DO}}
        inner = get(d.dict, (F, S), nothing)::Union{DI{Tuple{F,S}}, Nothing}
        new{F, S, Tuple{F,S}, DI{Tuple{F,S}}, DD}(d, inner)
    end
end

function with_type(d::DoubleDict, ::Type{F}, ::Type{S})::WithType{F, S} where {F,S}
    return WithType{F, S}(d)::WithType{F, S}
end
function with_type(d::IndexDoubleDict, ::Type{F}, ::Type{S})::IndexWithType{F, S} where {F,S}
    return IndexWithType{F, S}(d)::IndexWithType{F, S}
end
function with_type(d::FunctionSetDoubleDict, ::Type{F}, ::Type{S})::FunctionSetWithType{F, S} where {F,S}
    return FunctionSetWithType{F, S}(d)::FunctionSetWithType{F, S}
end

function Base.getindex(d::DoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    return WithType{F, S}(d)
end
function Base.getindex(d::IndexDoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    return IndexWithType{F, S}(d)
end
function Base.getindex(d::FunctionSetDoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    return FunctionSetWithType{F, S}(d)
end

@inline function typed_value(::WithType{F, S, V}, v::V)::V where {V, F, S}
    v
end
@inline function typed_value(::IndexWithType{F, S}, v::Int)::CI{F,S} where {F, S}
    CI{F,S}(v)
end
@inline function typed_value(::FunctionSetWithType{F, S, V}, v::Tuple{F,S})::Tuple{F,S} where {V, F, S}
    v
end

function initialize_inner!(d::AbstractWithType{F, S, V, D}) where {F, S, V, D}
    d.inner = D()
end

inner_is_empty(d::AbstractWithType)::Bool = d.inner === nothing
function inner(d::AbstractWithType{F, S, V, D})::D where {F, S, V, D}
    d.inner::D
end

function Base.sizehint!(d::AbstractWithType{F, S}, n::Integer) where {F, S}
    if inner_is_empty(d)
        initialize_inner!(d)
    end
    sizehint!(inner(d), n)
end

function Base.length(d::AbstractWithType{F, S}) where {F, S}
    if inner_is_empty(d)
        return 0
    end
    return length(inner(d))
end

function Base.haskey(d::AbstractWithType{F, S}, key::CI{F,S}) where {F,S}
    if inner_is_empty(d)
        return false
    end
    return haskey(inner(d), key.value)
end

function Base.getindex(d::AbstractWithType{F, S},
    key::CI{F,S}) where {F, S}
    if inner_is_empty(d)
        throw(KeyError(key))
    end
    k_value = key.value::Int64
    return typed_value(d, inner(d)[k_value])
end

function Base.setindex!(d::WithType{F, S, V}, value::V,
    key::CI{F,S})::V where {F, S, V}
    if inner_is_empty(d)
        initialize_inner!(d)
    end
    v_value = value
    k_value = key.value::Int64
    inner(d)[k_value] = v_value
    return value
end
function Base.setindex!(d::IndexWithType{F, S, V}, value::CI{F,S},
    key::CI{F,S})::CI{F,S} where {F, S, V}
    if inner_is_empty(d)
        initialize_inner!(d)
    end
    _inner = inner(d)::Dict{Int64, Int64}
    v_value = value.value::Int64
    k_value = key.value::Int64
    _inner[k_value] = v_value
    return value
end
function Base.setindex!(d::FunctionSetWithType{F, S, V}, value::Tuple{F,S},
    key::CI{F,S})::Tuple{F,S} where {F, S, V}
    if inner_is_empty(d)
        initialize_inner!(d)
    end
    _inner = inner(d)::Dict{Int64, Tuple{F,S}}
    v_value = value
    k_value = key.value::Int64
    _inner[k_value] = v_value
    return value
end

function Base.empty!(d::AbstractWithType{F, S}) where {F,S}
    if inner_is_empty(d)
        return d
    end
    return empty!(inner(d))
end

function Base.delete!(d::AbstractWithType{F, S}, key::CI{F,S}) where {F,S}
    if inner_is_empty(d)
        return d
    end
    k_value = key.value::Int
    delete!(inner(d), k_value)
    return d
end

function Base.isempty(d::AbstractWithType{F, S}) where {F,S}
    if inner_is_empty(d)
        return true
    end
    return isempty(inner(d))
end

function Base.values(d::WithType{F, S, V})::Vector{V} where {F, S, V}
    if inner_is_empty(d)
        return V[]
    end
    return collect(values(inner(d)))
end
function Base.values(d::IndexWithType{F, S})::Vector{CI{F,S}} where {F,S}
    if inner_is_empty(d)
        return CI{F,S}[]
    end
    return CI{F,S}.(values(inner(d)))
end
function Base.values(d::FunctionSetWithType{F, S})::Vector{Tuple{F,S}} where {F,S}
    if inner_is_empty(d)
        return Tuple{F,S}[]
    end
    return collect(values(inner(d)))
end

function Base.keys(d::AbstractWithType{F, S}) where {F,S}
    if inner_is_empty(d)
        return CI{F,S}[]
    end
    return CI{F,S}.(keys(inner(d)))
end

function Base.iterate(d::AbstractWithType{F, S}) where {F, S}
    inner = d.inner
    if inner === nothing
        return nothing
    else
        next = iterate(inner)
        if next === nothing
            return nothing
        end
        (i, state) = next
        return CI{F,S}(i[1]) => typed_value(d, i[2]), (state, inner)
    end
end
function Base.iterate(d::AbstractWithType{F, S}, state) where {F, S}
    (istate, inner) = state
    next = iterate(inner, istate)
    if next === nothing
        return nothing
    end
    (i, state) = next
    return CI{F,S}(i[1]) => typed_value(d, i[2]), (state, inner)
end

end
