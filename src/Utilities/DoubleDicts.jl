module DoubleDicts

import MathOptInterface

const MOI = MathOptInterface
const CI{F,S} = MOI.ConstraintIndex{F,S}

abstract type AbstractDoubleDict{V, D<:AbstractDict{Int64, V}, D2<:AbstractDict{Tuple{DataType, DataType}, D}} end

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
struct DoubleDict{V, D<:AbstractDict{Int64, V}, D2<:AbstractDict{Tuple{DataType, DataType}, D}} <: AbstractDoubleDict{V, D, D2}
    dict::D2
    DoubleDict{V}() where V = new{V, Dict{Int64, V}, Dict{Tuple{DataType, DataType}, Dict{Int64, V}}}(
        Dict{Tuple{DataType, DataType}, Dict{Int64, V}}())
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
struct IndexDoubleDict{V<:Int64, D<:AbstractDict{Int64, Int64}, D2<:AbstractDict{Tuple{DataType, DataType}, D}} <: AbstractDoubleDict{V, D, D2}
    dict::D2
    IndexDoubleDict() = new{Int64, Dict{Int64, Int64}, Dict{Tuple{DataType, DataType}, Dict{Int64, Int64}}}(
        Dict{Tuple{DataType, DataType}, Dict{Int64, Int64}}())
end

MainIndexDoubleDict = IndexDoubleDict{Int, Dict{Int, Int}, Dict{Tuple{DataType, DataType}, Dict{Int, Int}}}

@inline function typed_value(::DoubleDict{V}, v::V, ::Type{F}, ::Type{S})::V where {V, F, S}
    v
end
@inline function typed_value(::IndexDoubleDict, v::Int64, ::Type{F}, ::Type{S})::CI{F,S} where {F, S}
    CI{F,S}(v)
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

"""
    lazy_get(dict::DoubleDict, ::Type{F}, ::Type{S})

description
"""
function lazy_get(dict::AbstractDoubleDict{V}, ::Type{F}, ::Type{S}) where {V,F,S}
    inner = get(dict.dict, (F,S), nothing)
    if inner === nothing
        return dict.dict[(F,S)] = Dict{Int64,V}()
    end
    return inner
end

function Base.setindex!(dict::AbstractDoubleDict{V}, value::V, key::CI{F,S}) where {V,F,S}
    v_value = value
    k_value = key.value::Int64
    inner = lazy_get(dict, F, S)::Dict{Int64,V}
    inner[k_value] = v_value
    return value
end
function Base.setindex!(dict::IndexDoubleDict, value::CI{F,S}, key::CI{F,S}) where {F,S}
    v_value = value.value::Int64
    k_value = key.value::Int64
    inner = lazy_get(dict, F, S)::Dict{Int64,Int64}
    inner[k_value] = v_value
    return value
end

function Base.empty!(d::AbstractDoubleDict)::Nothing
    Base.empty!(d.dict)
    return
end

function Base.delete!(d::AbstractDoubleDict{V}, key::CI{F,S}) where {V,F,S}
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
function Base.values(d::AbstractDoubleDict{V})::Vector{V} where V
    out = V[]
    for ((F, S), inner) in d.dict
        append!(out, V.(values(inner)))
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


abstract type AbstractWithType{F, S, V, D, D2} <: AbstractDict{CI{F,S}, V} end
"""
    WithType{D<:DoubleDict, F<:MOI.AbstractFunction, S<:MOI.AbstractSet}

Used to specialize methods and iterators for a given contraint type CI{F,S}.
"""
mutable struct WithType{F, S, V, D, D2} <: AbstractWithType{F, S, V, D, D2}
    dict::D2
    inner::Union{D, Nothing}
    function WithType{F, S}(d::D3) where
        {V, D<:AbstractDict{Int64, V}, D2<:AbstractDict{Tuple{DataType, DataType}, D},
         D3<:DoubleDict{V, D, D2}, F, S}
        inner = get(d.dict, (F, S), nothing)
        new{F, S, V, D, D3}(d, inner)
    end
end
mutable struct IndexWithType{F, S, V, D, D2} <: AbstractWithType{F, S, CI{F,S}, D, D2}
    dict::D2
    inner::Union{D, Nothing}
    function IndexWithType{F, S}(d::D3) where
        {V<:Int64, D<:AbstractDict{Int64, V}, D2<:AbstractDict{Tuple{DataType, DataType}, D},
         D3<:IndexDoubleDict{V, D, D2}, F, S}
        inner = get(d.dict, (F, S), nothing)::Union{D, Nothing}
        new{F, S, CI{F,S}, D, D3}(d, inner)
    end
end

function with_type(d::DoubleDict, ::Type{F}, ::Type{S})::WithType{F, S} where {F,S}
    return WithType{F, S}(d)::WithType{F, S}
end
function with_type(d::IndexDoubleDict, ::Type{F}, ::Type{S})::IndexWithType{F, S} where {F,S}
    return IndexWithType{F, S}(d)::IndexWithType{F, S}
end

function Base.getindex(d::DoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    return WithType{F, S}(d)
end
function Base.getindex(d::IndexDoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    return IndexWithType{F, S}(d)
end

@inline function typed_value(::WithType{F, S, V}, v::V)::V where {V, F, S}
    v
end
@inline function typed_value(::IndexWithType{F, S}, v::Int)::CI{F,S} where {F, S}
    CI{F,S}(v)
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
function Base.setindex!(d::IndexWithType{F, S, V, D, D2}, value::CI{F,S},
    key::CI{F,S})::CI{F,S} where {F, S, V, D, D2}
    if inner_is_empty(d)
        initialize_inner!(d)
    end
    _inner = inner(d)::Dict{Int64, Int64}
    v_value = value.value::Int64
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
    return V.(values(inner(d)))
end
function Base.values(d::IndexWithType{F, S})::Vector{CI{F,S}} where {F,S}
    if inner_is_empty(d)
        return CI{F,S}[]
    end
    return CI{F,S}.(values(inner(d)))
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
