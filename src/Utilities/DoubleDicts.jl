module DoubleDicts

import MathOptInterface

const MOI = MathOptInterface

abstract type AbstractDoubleDict{V} <: AbstractDict{MOI.ConstraintIndex,V} end

abstract type AbstractDoubleDictInner{F,S,V} <:
              AbstractDict{MOI.ConstraintIndex{F,S},V} end

_inner(d::AbstractDoubleDictInner) = d.inner

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
    inner::Dict{Int64,V}
    function DoubleDictInner{F,S}(d::DoubleDict{V}) where {F,S,V}
        return new{F,S,V}(d, get!(d.dict, (F, S), Dict{Int64,V}()))
    end
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
    inner::Dict{Int64,Int64}
    function IndexDoubleDictInner{F,S}(d::IndexDoubleDict) where {F,S}
        return new{F,S}(d, get!(d.dict, (F, S), Dict{Int64,Int64}()))
    end
end

# _typed_value

@inline function _typed_value(::DoubleDictInner{F,S,V}, v::V) where {F,S,V}
    return v
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
    for (key, value) in src.dict
        dest.dict[key] = MOI.Utilities._reverse_dict(value)
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
    return sizehint!(_inner(d), n)
end

# Base.length

function Base.length(d::AbstractDoubleDict)
    len = 0
    for inner in values(d.dict)
        len += length(inner)
    end
    return len
end

Base.length(d::AbstractDoubleDictInner) = length(_inner(d))

# Base.haskey

function Base.haskey(
    d::AbstractDoubleDict,
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    return haskey(getindex(d, F, S), key)
end

function Base.haskey(
    d::AbstractDoubleDictInner{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    return haskey(_inner(d), key.value)
end

# Base.get

function Base.get(
    d::AbstractDoubleDict,
    key::MOI.ConstraintIndex{F,S},
    default,
) where {F,S}
    inner = getindex(d, F, S)
    if haskey(inner, key)
        return inner[key]
    end
    return default
end

# Base.getindex

function Base.getindex(
    d::AbstractDoubleDict,
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    inner = getindex(d, F, S)
    return inner[key]
end

function Base.getindex(d::DoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    return DoubleDictInner{F,S}(d)
end

function Base.getindex(
    d::AbstractDoubleDictInner{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    inner = _inner(d)
    if !haskey(inner, key.value)
        throw(KeyError(key))
    end
    return _typed_value(d, inner[key.value])
end

function Base.getindex(d::IndexDoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    return IndexDoubleDictInner{F,S}(d)
end

# Base.setindex!

function Base.setindex!(
    d::AbstractDoubleDict{V},
    value::V,
    key::MOI.ConstraintIndex{F,S},
) where {F,S,V}
    inner = getindex(d, F, S)
    inner[key] = value
    return value
end

function Base.setindex!(
    d::DoubleDictInner{F,S,V},
    value::V,
    key::MOI.ConstraintIndex{F,S},
) where {F,S,V}
    _inner(d)[key.value] = value
    return value
end

function Base.setindex!(
    d::IndexDoubleDict,
    value::MOI.ConstraintIndex{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    inner = getindex(d, F, S)
    inner[key] = value
    return value
end

function Base.setindex!(
    d::IndexDoubleDictInner{F,S},
    value::MOI.ConstraintIndex{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    _inner(d)[key.value] = value.value
    return value
end

# Base.empty!

function Base.empty!(d::AbstractDoubleDict)
    Base.empty!(d.dict)
    return d
end

function Base.empty!(d::AbstractDoubleDictInner)
    empty!(_inner(d))
    return d
end

# Base.delete!

function Base.delete!(
    d::AbstractDoubleDict,
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    delete!(getindex(d, F, S), key)
    return d
end

function Base.delete!(
    d::AbstractDoubleDictInner{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    delete!(_inner(d), key.value)
    return d
end

# Base.isempty

function Base.isempty(d::AbstractDoubleDict)
    return isempty(d.dict) || all(isempty, values(d.dict))
end

Base.isempty(d::AbstractDoubleDictInner) = isempty(_inner(d))

# Base.values

function Base.values(d::AbstractDoubleDict{V})::Vector{V} where {V}
    out = V[]
    for inner in values(d.dict)
        append!(out, values(inner))
    end
    return out
end

function Base.values(d::DoubleDictInner{F,S,V})::Vector{V} where {F,S,V}
    return V[v for v in values(_inner(d))]
end

function Base.values(d::IndexDoubleDict)
    out = MOI.ConstraintIndex[]
    for ((F, S), inner) in d.dict
        append!(out, MOI.ConstraintIndex{F,S}.(values(inner)))
    end
    return out
end

function Base.values(d::IndexDoubleDictInner{F,S}) where {F,S}
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
    return MOI.ConstraintIndex{F,S}.(keys(_inner(d)))
end

# Base.iterate

function Base.iterate(d::AbstractDoubleDict)
    outer_next = iterate(d.dict)
    if outer_next === nothing
        return  # There are no keys.
    end
    # The result is a (F,S)=>inner pair.
    ((F, S), inner), outer_state = outer_next
    inner_next = iterate(inner)
    while inner_next === nothing
        # It may be that the inner dictionary is empty! If so, we should go to
        # the next element in the outer dictionary.
        outer_next = iterate(d.dict, outer_state)
        if outer_next === nothing
            return
        end
        ((F, S), inner), outer_state = outer_next
        # Start iterating from scratch on this new `inner` object.
        inner_next = iterate(inner)
    end
    (k, v), inner_state = inner_next
    result = MOI.ConstraintIndex{F,S}(k) => _typed_value(getindex(d, F, S), v)
    return result, (inner_state, outer_next)
end

function Base.iterate(d::AbstractDoubleDictInner{F,S}) where {F,S}
    next = iterate(_inner(d))
    if next === nothing
        return
    end
    (k, v), inner_state = next
    result = MOI.ConstraintIndex{F,S}(k) => _typed_value(d, v)
    return result, (_inner(d), inner_state)
end

function Base.iterate(d::AbstractDoubleDict, state)
    inner_state, outer_next = state
    ((F, S), inner), outer_state = outer_next
    inner_next = iterate(inner, inner_state)
    while inner_next === nothing
        # We may have reached the end of this inner dictionary. Get the next
        # element of the outer dictionary:
        outer_next = iterate(d.dict, outer_state)
        if outer_next === nothing
            return
        end
        ((F, S), inner), outer_state = outer_next
        # Start iterating from scratch on this new `inner` object.
        inner_next = iterate(inner)
    end
    (k, v), inner_state = inner_next
    result = MOI.ConstraintIndex{F,S}(k) => _typed_value(getindex(d, F, S), v)
    return result, (inner_state, outer_next)
end

function Base.iterate(d::AbstractDoubleDictInner{F,S}, state) where {F,S}
    inner, inner_state = state
    next = iterate(inner, inner_state)
    if next === nothing
        return
    end
    ((k, v), next_inner_state) = next
    result = MOI.ConstraintIndex{F,S}(k) => _typed_value(d, v)
    return result, (inner, next_inner_state)
end

end
