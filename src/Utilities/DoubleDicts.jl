module DoubleDicts

import MathOptInterface

const MOI = MathOptInterface

abstract type AbstractDoubleDict{V} <: AbstractDict{MOI.ConstraintIndex,V} end

abstract type AbstractDoubleDictInner{F,S,V} <:
              AbstractDict{MOI.ConstraintIndex{F,S},V} end

"""
    DoubleDict{V}

An optimized dictionary to map `MOI.ConstraintIndex` to values of type `V`.

Works as a `AbstractDict{MOI.ConstraintIndex,V}` with minimal differences.

If `V` is also a `MOI.ConstraintIndex`, use [`IndexDoubleDict`](@ref).

Note that `MOI.ConstraintIndex` is not a concrete type, opposed to
`MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integers}`, which is a concrete
type.

When looping through multiple keys of the same Function-in-Set type, use
```julia
inner = dict[F, S]
```
to return a type-stable [`DoubleDictInner`](@ref).
"""
struct DoubleDict{V} <: AbstractDoubleDict{V}
    dict::Dict{Tuple{DataType,DataType},Dict{Int64,V}}
    function DoubleDict{V}() where {V}
        return new{V}(Dict{Tuple{DataType,DataType},Dict{Int64,V}}())
    end
end

"""
    DoubleDictInner{F,S,V}

A type stable inner dictionary of [`DoubleDict`](@ref).
"""
mutable struct DoubleDictInner{F,S,V} <: AbstractDoubleDictInner{F,S,V}
    dict::DoubleDict{V}
    inner::Union{Dict{Int64,V},Nothing}
    function DoubleDictInner{F,S}(d::DoubleDict{V}) where {F,S,V}
        return new{F,S,V}(d, get(d.dict, (F, S), nothing))
    end
end

function _inner(
    d::DoubleDictInner{F,S,V};
    initialize::Bool = false,
) where {F,S,V}
    if initialize && d.inner === nothing
        d.inner = Dict{Int64,V}()
        d.dict.dict[(F, S)] = d.inner
    end
    return d.inner::Dict{Int64,V}
end

"""
    IndexDoubleDict

A specialized version of [`DoubleDict`] in which the values are of type
`MOI.ConstraintIndex`

When looping through multiple keys of the same Function-in-Set type, use
```julia
inner = dict[F, S]
```
to return a type-stable [`IndexDoubleDictInner`](@ref).
"""
struct IndexDoubleDict <: AbstractDoubleDict{MOI.ConstraintIndex}
    dict::Dict{Tuple{DataType,DataType},Dict{Int64,Int64}}
    function IndexDoubleDict()
        return new(Dict{Tuple{DataType,DataType},Dict{Int64,Int64}}())
    end
end

"""
    IndexDoubleDictInner{F,S}

A type stable inner dictionary of [`IndexDoubleDict`](@ref).
"""
mutable struct IndexDoubleDictInner{F,S} <:
               AbstractDoubleDictInner{F,S,MOI.ConstraintIndex{F,S}}
    dict::IndexDoubleDict
    inner::Union{Dict{Int64,Int64},Nothing}
    function IndexDoubleDictInner{F,S}(d::IndexDoubleDict) where {F,S}
        return new{F,S}(d, get(d.dict, (F, S), nothing))
    end
end

function _inner(
    d::IndexDoubleDictInner{F,S};
    initialize::Bool = false,
) where {F,S}
    if initialize && d.inner === nothing
        d.inner = Dict{Int64,Int64}()
        d.dict.dict[(F, S)] = d.inner
    end
    return d.inner::Dict{Int64,Int64}
end

# _typed_value

@inline function _typed_value(::DoubleDict{V}, v::V, ::Type, ::Type) where {V}
    return v
end

@inline function _typed_value(::DoubleDictInner{F,S,V}, v::V) where {F,S,V}
    return v
end

@inline function _typed_value(
    ::IndexDoubleDict,
    v::Int64,
    ::Type{F},
    ::Type{S},
) where {F,S}
    return MOI.ConstraintIndex{F,S}(v)
end

@inline function _typed_value(::IndexDoubleDictInner{F,S}, v::Int64) where {F,S}
    return MOI.ConstraintIndex{F,S}(v)
end

# _reverse_dict

# reversing IndexDoubleDict is ok because they map CI to CI
function MOI.Utilities._reverse_dict(
    dest::IndexDoubleDict,
    src::IndexDoubleDict,
)
    for (k, v) in src.dict
        dest.dict[k] = MOI.Utilities._reverse_dict(v)
    end
    return
end
# reversing other double dict types is not ok because the map CI fo K
# so it wont be a double dict anymore, double dict keys are always CIs.
# We keep the default fallback

# Base.sizehint!

function Base.sizehint!(::AbstractDoubleDict, ::Integer)
    return throw(
        ErrorException(
            "sizehint!(d::DoubleDict, ::Integer) has no proper" *
            " meaning for DoubleDict, use sizehint!(d[F,S], n::Integer) " *
            "instead.",
        ),
    )
end

function Base.sizehint!(d::AbstractDoubleDictInner, n::Integer)
    inner = _inner(d; initialize = true)
    return sizehint!(inner, n)
end

# Base.length

function Base.length(d::AbstractDoubleDict)
    len = 0
    for inner in values(d.dict)
        len += length(inner)
    end
    return len
end

function Base.length(d::AbstractDoubleDictInner)
    if d.inner === nothing
        return 0
    end
    return length(_inner(d))
end

# Base.haskey

function Base.haskey(
    dict::AbstractDoubleDict,
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    inner = get(dict.dict, (F, S), nothing)
    return inner !== nothing ? haskey(inner, key.value) : false
end

function Base.haskey(
    d::AbstractDoubleDictInner{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    if d.inner === nothing
        return false
    end
    return haskey(_inner(d), key.value)
end

# Base.get

function Base.get(
    dict::AbstractDoubleDict,
    key::MOI.ConstraintIndex{F,S},
    default,
) where {F,S}
    inner = get(dict.dict, (F, S), nothing)
    if inner !== nothing && haskey(inner, key.value)
        return _typed_value(dict, inner[key.value], F, S)
    end
    return default
end

# Base.getindex

function Base.getindex(
    dict::AbstractDoubleDict,
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    inner = dict.dict[(F, S)]
    return _typed_value(dict, inner[key.value], F, S)
end

function Base.getindex(d::DoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    return DoubleDictInner{F,S}(d)
end

function Base.getindex(
    d::AbstractDoubleDictInner{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    if d.inner === nothing
        throw(KeyError(key))
    end
    return _typed_value(d, _inner(d)[key.value])
end

function Base.getindex(d::IndexDoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    return IndexDoubleDictInner{F,S}(d)
end

# _initialize_and_get

function _initialize_and_get(
    dict::AbstractDoubleDict{V},
    ::Type{F},
    ::Type{S},
)::Dict{Int64,V} where {F,S,V}
    return get!(() -> Dict{Int64,V}(), dict.dict, (F, S))
end

function _initialize_and_get(
    dict::IndexDoubleDict,
    ::Type{F},
    ::Type{S},
)::Dict{Int64,Int64} where {F,S}
    return get!(() -> Dict{Int64,Int64}(), dict.dict, (F, S))
end

# Base.setindex!

function Base.setindex!(
    dict::AbstractDoubleDict{V},
    value::V,
    key::MOI.ConstraintIndex{F,S},
) where {F,S,V}
    inner = _initialize_and_get(dict, F, S)
    inner[key.value] = value
    return value
end

function Base.setindex!(
    d::DoubleDictInner{F,S,V},
    value::V,
    key::MOI.ConstraintIndex{F,S},
) where {F,S,V}
    inner = _inner(d; initialize = true)
    inner[key.value] = value
    return value
end

function Base.setindex!(
    dict::IndexDoubleDict,
    value::MOI.ConstraintIndex{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    inner = _initialize_and_get(dict, F, S)
    inner[key.value] = value.value
    return value
end

function Base.setindex!(
    d::IndexDoubleDictInner{F,S},
    value::MOI.ConstraintIndex{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    inner = _inner(d; initialize = true)
    inner[key.value] = value.value
    return value
end

# Base.empty!

function Base.empty!(d::AbstractDoubleDict)
    Base.empty!(d.dict)
    return d
end

function Base.empty!(d::AbstractDoubleDictInner)
    if d.inner === nothing
        return d
    end
    empty!(_inner(d))
    return d
end

# Base.delete!

function Base.delete!(
    d::AbstractDoubleDict,
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    inner = _initialize_and_get(d, F, S)
    delete!(inner, key.value)
    return d
end

function Base.delete!(
    d::AbstractDoubleDictInner{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    if !(d.inner === nothing)
        delete!(_inner(d), key.value)
    end
    return d
end

# Base.isempty

function Base.isempty(d::AbstractDoubleDict)
    return isempty(d.dict) || all(isempty, values(d.dict))
end

function Base.isempty(d::AbstractDoubleDictInner)
    return d.inner === nothing || isempty(_inner(d))
end

# Base.values

function Base.values(d::AbstractDoubleDict{V})::Vector{V} where {V}
    out = V[]
    for inner in values(d.dict)
        append!(out, values(inner))
    end
    return out
end

function Base.values(d::DoubleDictInner{F,S,V})::Vector{V} where {F,S,V}
    if d.inner === nothing
        return V[]
    end
    return collect(values(_inner(d)))
end

function Base.values(d::IndexDoubleDict)
    out = MOI.ConstraintIndex[]
    for ((F, S), inner) in d.dict
        append!(out, MOI.ConstraintIndex{F,S}.(values(inner)))
    end
    return out
end

function Base.values(d::IndexDoubleDictInner{F,S}) where {F,S}
    if d.inner === nothing
        return MOI.ConstraintIndex{F,S}[]
    end
    return MOI.ConstraintIndex{F,S}.(values(_inner(d)))
end

# Base.keys

function Base.keys(d::AbstractDoubleDict)
    out = MOI.ConstraintIndex[]
    for ((F, S), inner) in d.dict
        append!(out, MOI.ConstraintIndex{F,S}.(keys(inner)))
    end
    return out
end

function Base.keys(d::AbstractDoubleDictInner{F,S}) where {F,S}
    if d.inner === nothing
        return MOI.ConstraintIndex{F,S}[]
    end
    return MOI.ConstraintIndex{F,S}.(keys(_inner(d)))
end

# Base.iterate

function Base.iterate(d::AbstractDoubleDict)
    o_next = iterate(d.dict)
    if o_next === nothing
        return
    end
    (o_i, o_state) = o_next
    ((F, S), inner) = o_i
    i_next = iterate(inner)
    while i_next === nothing
        o_next = iterate(d.dict, o_state)
        if o_next === nothing
            return
        end
        (o_i, o_state) = o_next
        ((F, S), inner) = o_i
        i_next = iterate(inner)
    end
    (i_i, i_state) = i_next
    pair = MOI.ConstraintIndex{F,S}(i_i[1]) => _typed_value(d, i_i[2], F, S)
    return pair, (i_state, (o_i, o_state))
end

function Base.iterate(d::AbstractDoubleDictInner{F,S}) where {F,S}
    if d.inner === nothing
        return
    end
    next = iterate(d.inner)
    if next === nothing
        return
    end
    (i, state) = next
    value = MOI.ConstraintIndex{F,S}(i[1]) => _typed_value(d, i[2])
    return value, (state, d.inner)
end

function Base.iterate(d::AbstractDoubleDict, state)
    (i_state, (o_i, o_state)) = state
    ((F, S), inner) = o_i
    i_next = iterate(inner, i_state)
    while i_next === nothing
        o_next = iterate(d.dict, o_state)
        if o_next === nothing
            return nothing
        end
        (o_i, o_state) = o_next
        ((F, S), inner) = o_i
        i_next = iterate(inner)
    end
    (i_i, i_state) = i_next
    pair = MOI.ConstraintIndex{F,S}(i_i[1]) => _typed_value(d, i_i[2], F, S)
    return pair, (i_state, (o_i, o_state))
end

function Base.iterate(d::AbstractDoubleDictInner{F,S}, state) where {F,S}
    (istate, inner) = state
    next = iterate(inner, istate)
    if next === nothing
        return
    end
    (i, state) = next
    value = MOI.ConstraintIndex{F,S}(i[1]) => _typed_value(d, i[2])
    return value, (state, inner)
end

end
