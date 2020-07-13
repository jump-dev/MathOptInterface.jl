module DoubleDicts

import MathOptInterface

const MOI = MathOptInterface
const CI{F,S} = MOI.ConstraintIndex{F,S}

abstract type AbstractDoubleDict{V, D<:AbstractDict{Int, V}, D2<:AbstractDict{Tuple{DataType, DataType}, D}} end

"""
    DoubleDict

Special dictionary to map ConstraintIndex to ConstraintIndex with minimal
performance loss from the type instability due different constraint types.
"""
struct DoubleDict{V, D<:AbstractDict{Int, V}, D2<:AbstractDict{Tuple{DataType, DataType}, D}} <: AbstractDoubleDict{V, D, D2}
    dict::D2
    DoubleDict{V}() where V = new{V, Dict{Int, V}, Dict{Tuple{DataType, DataType}, Dict{Int, V}}}(
        Dict{Tuple{DataType, DataType}, Dict{Int, V}}())
end

struct IndexDoubleDict{V<:Int, D<:AbstractDict{Int, Int}, D2<:AbstractDict{Tuple{DataType, DataType}, D}} <: AbstractDoubleDict{V, D, D2}
    dict::D2
    IndexDoubleDict() = new{Int, Dict{Int, Int}, Dict{Tuple{DataType, DataType}, Dict{Int, Int}}}(
        Dict{Tuple{DataType, DataType}, Dict{Int, Int}}())
end

MainIndexDoubleDict = IndexDoubleDict{Int, Dict{Int, Int}, Dict{Tuple{DataType, DataType}, Dict{Int, Int}}}

@inline function typed_value(::DoubleDict{V}, v::V, ::Type{F}, ::Type{S})::V where {V, F, S}
    v
end
@inline function typed_value(::IndexDoubleDict, v::Int, ::Type{F}, ::Type{S})::CI{F,S} where {F, S}
    CI{F,S}(v)
end

function Base.sizehint!(::DoubleDict, ::Integer)
    error("sizehint!(d::DoubleDict, ::Integer) has no proper meaning for "*
        "DoubleDict, use sizehint!(WithType{F,S}(d), n::Integer) instead.")
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
    k_value = key.value::Int
    return typed_value(dict, inner[k_value], F, S)
end

"""
    lazy_get(dict::DoubleDict, ::Type{F}, ::Type{S})

description
"""
function lazy_get(dict::AbstractDoubleDict{V}, ::Type{F}, ::Type{S}) where {V,F,S}
    inner = get(dict.dict, (F,S), nothing)
    if inner === nothing
        return dict.dict[(F,S)] = Dict{Int,V}()
    end
    return inner
end

function Base.setindex!(dict::AbstractDoubleDict{V}, value::V, key::CI{F,S}) where {V,F,S}
    v_value = value
    k_value = key.value::Int
    inner = lazy_get(dict, F, S)::Dict{Int,V}
    inner[k_value] = v_value
    return dict
end
function Base.setindex!(dict::IndexDoubleDict, value::CI{F,S}, key::CI{F,S}) where {F,S}
    v_value = value.value::Int
    k_value = key.value::Int
    inner = lazy_get(dict, F, S)::Dict{Int,Int}
    inner[k_value] = v_value
    return dict
end

function Base.empty!(d::AbstractDoubleDict)::Nothing
    Base.empty!(d.dict)
    return
end

function Base.delete!(d::AbstractDoubleDict{V}, key::CI{F,S}) where {V,F,S}
    k_value = key.value::Int
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
        {V, D<:AbstractDict{Int, V}, D2<:AbstractDict{Tuple{DataType, DataType}, D},
         D3<:DoubleDict{V, D, D2}, F, S}
        # inner = get(d.dict, (F, S), D2())
        inner = get(d.dict, (F, S), nothing)
        new{F, S, V, D, D3}(d, inner)
    end
end
mutable struct IndexWithType{F, S, V, D, D2} <: AbstractWithType{F, S, CI{F,S}, D, D2}
    dict::D2
    inner::Union{D, Nothing}
    function IndexWithType{F, S}(d::D3) where
        {V<:Int, D<:AbstractDict{Int, V}, D2<:AbstractDict{Tuple{DataType, DataType}, D},
         D3<:IndexDoubleDict{V, D, D2}, F, S}
        # inner = get(d.dict, (F, S), D2())
        inner = get(d.dict, (F, S), nothing)
        new{F, S, CI{F,S}, D, D3}(d, inner)
    end
end

function with_type(d::DoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    return WithType{F, S}(d)
end
function with_type(d::IndexDoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    return IndexWithType{F, S}(d)
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

function Base.sizehint!(d::AbstractWithType{F, S}, n::Integer) where {F, S}
    if d.inner === nothing
        initialize_inner!(d)
    end
    sizehint!(d.inner, n)
end

function Base.length(d::AbstractWithType{F, S}) where {F, S}
    if d.inner === nothing
        return 0
    end
    return length(d.inner)
end

function Base.haskey(d::AbstractWithType{F, S}, key::CI{F,S}) where {F,S}
    if d.inner === nothing
        return false
    end
    return haskey(d.inner, key.value)
end

function Base.getindex(d::AbstractWithType{F, S},
    key::CI{F,S}) where {F, S}
    if d.inner === nothing
        error("No key")
    end
    inner = d.inner
    k_value = key.value::Int
    return typed_value(d, inner[k_value])
end

function Base.setindex!(d::WithType{F, S, V}, value::V,
    key::CI{F,S}) where {F, S, V}
    if d.inner === nothing
        initialize_inner!(d)
    end
    v_value = value#.value::Int
    k_value = key.value::Int
    d.inner[k_value] = v_value
    return d
end
function Base.setindex!(d::IndexWithType{F, S}, value::CI{F,S},
    key::CI{F,S}) where {F, S}
    if d.inner === nothing
        initialize_inner!(d)
    end
    v_value = value.value::Int
    k_value = key.value::Int
    d.inner[k_value] = v_value
    return d
end

function Base.empty!(d::AbstractWithType{F, S}) where {F,S}
    if d.inner === nothing
        return d
    end
    return empty!(d.inner)
end

function Base.delete!(d::AbstractWithType{F, S}, key::CI{F,S}) where {F,S}
    if d.inner === nothing
        return d
    end
    k_value = key.value::Int
    delete!(d.inner, k_value)
    return d
end

function Base.isempty(d::AbstractWithType{F, S}) where {F,S}
    if d.inner === nothing
        return true
    end
    return isempty(d.inner)
end

function Base.values(d::WithType{F, S, V})::Vector{V} where {F, S, V}
    if d.inner === nothing
        return V[]
    end
    return V.(values(d.inner))
end
function Base.values(d::IndexWithType{F, S})::Vector{CI{F,S}} where {F,S}
    if d.inner === nothing
        return CI{F,S}[]
    end
    return CI{F,S}.(values(d.inner))
end

function Base.keys(d::AbstractWithType{F, S}) where {F,S}
    if d.inner === nothing
        return CI{F,S}[]
    end
    return CI{F,S}.(keys(d.inner))
end

function Base.iterate(d::AbstractWithType{F, S}) where {F, S}
    # inner = get(d.dict, (F,S), nothing)
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
