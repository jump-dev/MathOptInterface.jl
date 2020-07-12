module DoubleDicts

import MathOptInterface

const MOI = MathOptInterface
const CI{F,S} = MOI.ConstraintIndex{F,S}

"""
    DoubleDict

Special dictionary to map ConstraintIndex to ConstraintIndex with minimal
performance loss from the type instability due different constraint types.
"""
struct DoubleDict{D<:AbstractDict{Int, Int}, D2<:AbstractDict{Tuple{DataType, DataType}, D}}
    dict::D2
    DoubleDict() = new{Dict{Int, Int}, Dict{Tuple{DataType, DataType}, Dict{Int, Int}}}(
        Dict{Tuple{DataType, DataType}, Dict{Int, Int}}())
end

MainDoubleDict = DoubleDict{Dict{Int, Int}, Dict{Tuple{DataType, DataType}, Dict{Int, Int}}}

function Base.sizehint!(::DoubleDict, ::Integer)
    error("sizehint!(::DoubleDict, ::Integer) has no proper meaning for "*
        "DoubleDict, use sizehint!(d::DoubleDict, ::Type{F}, ::Type{S}, n::Integer)"*
        " instead.")
end
# function Base.sizehint!(d::DoubleDict, ::Type{F}, ::Type{S}, n) where {F,S}
#     inner = lazy_get(d, F, S)::Dict{Int,Int}
#     sizehint!(inner, n)
# end

function Base.length(d::DoubleDict)
    len = 0
    for inner in values(d.dict)
        len += length(inner)
    end
    return len
end
# function Base.length(d::DoubleDict, ::Type{F}, ::Type{S}, n) where {F,S}
#     inner = get(d.dict, (F,S), nothing)::Union{Nothing, Dict{Int,Int}}
#     if inner === nothing
#         return 0
#     else
#         return length(inner)
#     end
# end

function Base.haskey(dict::DoubleDict, key::CI{F,S}) where {F,S}
    inner = get(dict.dict, (F,S), nothing)
    if inner !== nothing
        inner = dict.dict[(F,S)]
        return haskey(inner, key.value)
    else
        return false
    end
end

function Base.getindex(dict::DoubleDict, key::CI{F,S}) where {F,S}
    inner = dict.dict[(F,S)]
    k_value = key.value::Int
    return CI{F,S}(inner[k_value])
end

"""
    lazy_get(dict::DoubleDict, ::Type{F}, ::Type{S})

description
"""
function lazy_get(dict::DoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    inner = get(dict.dict, (F,S), nothing)
    if inner === nothing
        return dict.dict[(F,S)] = Dict{Int,Int}()
    end
    return inner
end

function Base.setindex!(dict::DoubleDict, value::CI{F,S}, key::CI{F,S}) where {F,S}
    v_value = value.value::Int
    k_value = key.value::Int
    inner = lazy_get(dict, F, S)::Dict{Int,Int}
    inner[k_value] = v_value
    return dict
end

function Base.empty!(d::DoubleDict)::Nothing
    Base.empty!(d.dict)
    return
end
# function Base.empty!(d::DoubleDict, ::Type{F}, ::Type{S}) where {F,S}
#     inner = get(d.dict, (F,S), nothing)
#     if inner === nothing
#         return
#     else
#         empty!(inner)
#     end
#     return
# end

function Base.delete!(d::DoubleDict, key::CI{F,S}) where {F,S}
    k_value = key.value::Int
    inner = lazy_get(d, F, S)::Dict{Int,Int}
    delete!(inner, k_value)
    return d
end

function Base.isempty(d::DoubleDict)
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
# function Base.isempty(d::DoubleDict, ::Type{F}, ::Type{S}) where {F,S}
#     inner = get(d.dict, (F,S), nothing)
#     if inner === nothing
#         return true
#     else
#         return isempty(inner)
#     end
# end

function Base.values(d::DoubleDict)
    out = CI[]
    for ((F, S), inner) in d.dict
        append!(out, CI{F,S}.(values(inner)))
    end
    return out
end
# function Base.values(d::DoubleDict, ::Type{F}, ::Type{S}) where {F,S}
#     inner = get(d.dict, (F,S), nothing)
#     if inner === nothing
#         return CI{F,S}[]
#     else
#         return CI{F,S}.(values(inner))
#     end
# end

function Base.keys(d::DoubleDict)
    out = CI[]
    for ((F,S), inner) in d.dict
        append!(out, CI{F,S}.(keys(inner)))
    end
    return out
end
# function Base.keys(d::DoubleDict, ::Type{F}, ::Type{S}) where {F,S}
#     inner = get(d.dict, (F,S), nothing)
#     if inner === nothing
#         return CI{F,S}[]
#     else
#         return CI{F,S}.(keys(inner))
#     end
# end

function Base.iterate(d::DoubleDict)
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
    return CI{F,S}(i_i[1]) => CI{F,S}(i_i[2]), (i_state, (o_i, o_state))
end
function Base.iterate(d::DoubleDict, state)
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
    return CI{F,S}(i_i[1]) => CI{F,S}(i_i[2]), (i_state, (o_i, o_state))
end

"""
    WithType{D<:DoubleDict, F<:MOI.AbstractFunction, S<:MOI.AbstractSet}

Used to specialize methods and iterators for a given contraint type CI{F,S}.
"""
struct WithType{F, S, D, D2} <: AbstractDict{CI{F,S}, CI{F,S}}
    dict::D
    inner::Union{D2, Nothing}
    function WithType{F, S}(d::D) where {D2, D<:DoubleDict{D2}, F, S}
        # inner = get(d.dict, (F, S), D2())
        inner = get(d.dict, (F, S), nothing)
        new{F, S, D, D2}(d, inner)
    end
end

function initialize_inner!(d::WithType{F, S, D, D2}) where {F, S, D, D2}
    d.inner = D2()
end

function Base.sizehint!(d::WithType{F, S}, n::Integer) where {F, S}
    if d.inner === nothing
        initialize_inner!(d)
    end
    sizehint!(d.inner, n)
end

function Base.length(d::WithType{F, S}) where {F, S}
    if d.inner === nothing
        return 0
    end
    return length(d.inner)
end

function Base.haskey(d::WithType{F, S}, key::CI{F,S}) where {F,S}
    if d.inner === nothing
        return false
    end
    return haskey(d.inner, key.value)
end

function Base.getindex(d::WithType{F, S},
    key::CI{F,S}) where {F, S}
    if d.inner === nothing
        error("No key")
    end
    inner = d.inner
    k_value = key.value::Int
    return CI{F,S}(inner[k_value])
end

function Base.setindex!(d::WithType{F, S}, value::CI{F,S},
    key::CI{F,S}) where {F, S}
    if d.inner === nothing
        initialize_inner!(d)
    end
    v_value = value.value::Int
    k_value = key.value::Int
    d.inner[k_value] = v_value
    return d
end

function Base.empty!(d::WithType{F, S}) where {F,S}
    if d.inner === nothing
        return d
    end
    return empty!(d.inner)
end

function Base.delete!(d::WithType{F, S}, key::CI{F,S}) where {F,S}
    if d.inner === nothing
        return d
    end
    k_value = key.value::Int
    delete!(d.inner, k_value)
    return d
end

function Base.isempty(d::WithType{F, S}) where {F,S}
    if d.inner === nothing
        return true
    end
    return isempty(d.inner)
end

function Base.values(d::WithType{F, S}) where {F,S}
    if d.inner === nothing
        return CI{F,S}[]
    end
    return CI{F,S}.(values(d.inner))
end

function Base.keys(d::WithType{F, S}) where {F,S}
    if d.inner === nothing
        return CI{F,S}[]
    end
    return  CI{F,S}.(keys(d.inner))
end

function Base.iterate(d::WithType{F, S}) where {F, S}
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
        return CI{F,S}(i[1]) => CI{F,S}(i[2]), (state, inner)
    end
end
function Base.iterate(::WithType{F, S}, state) where {F, S}
    (istate, inner) = state
    next = iterate(inner, istate)
    if next === nothing
        return nothing
    end
    (i, state) = next
    return CI{F,S}(i[1]) => CI{F,S}(i[2]), (state, inner)
end

end
