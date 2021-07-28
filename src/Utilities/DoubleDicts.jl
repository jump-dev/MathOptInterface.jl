module DoubleDicts

import MathOptInterface

const MOI = MathOptInterface

abstract type AbstractDoubleDict{V} <: AbstractDict{MOI.ConstraintIndex,V} end

abstract type AbstractDoubleDictInner{F,S,V} <:
              AbstractDict{MOI.ConstraintIndex{F,S},V} end

"""
    typed_value(dict::AbstractDoubleDictInner{F,S,V}, value) where {F,S,V}

Convert the `value` stored inside `dict` to the equivalent on the outer
`DoubleDict`. This is useful when the value type `V` of the inner dict is
different to the outer dict. (See, e.g., [`IndexDoubleDict`](@ref).)
"""
typed_value(::AbstractDoubleDictInner, value) = value

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
    dict::Dict{Tuple{Type,Type},Dict{Int64,V}}
    function DoubleDict{V}() where {V}
        return new{V}(Dict{Tuple{Type,Type},Dict{Int64,V}}())
    end
end

"""
    DoubleDictInner{F,S,V}

A type stable inner dictionary of [`DoubleDict`](@ref).
"""
mutable struct DoubleDictInner{F,S,V} <: AbstractDoubleDictInner{F,S,V}
    dict::Dict{Int64,V}
    function DoubleDictInner{F,S}(d::DoubleDict{V}) where {F,S,V}
        return new{F,S,V}(get!(d.dict, (F, S), Dict{Int64,V}()))
    end
end

function Base.getindex(d::DoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    return DoubleDictInner{F,S}(d)
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
    dict::Dict{Tuple{Type,Type},Dict{Int64,Int64}}
    function IndexDoubleDict()
        return new(Dict{Tuple{Type,Type},Dict{Int64,Int64}}())
    end
end

"""
    IndexDoubleDictInner{F,S}

A type stable inner dictionary of [`IndexDoubleDict`](@ref).
"""
mutable struct IndexDoubleDictInner{F,S} <:
               AbstractDoubleDictInner{F,S,MOI.ConstraintIndex{F,S}}
    dict::Dict{Int64,Int64}
    function IndexDoubleDictInner{F,S}(d::IndexDoubleDict) where {F,S}
        return new{F,S}(get!(d.dict, (F, S), Dict{Int64,Int64}()))
    end
end

function Base.getindex(d::IndexDoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    return IndexDoubleDictInner{F,S}(d)
end

function typed_value(::IndexDoubleDictInner{F,S}, v::Int64) where {F,S}
    return MOI.ConstraintIndex{F,S}(v)
end

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
    return sizehint!(d.dict, n)
end

# Base.length

function Base.length(d::AbstractDoubleDict)
    len = 0
    for inner in values(d.dict)
        len += length(inner)
    end
    return len
end

Base.length(d::AbstractDoubleDictInner) = length(d.dict)

# Base.haskey

function Base.haskey(
    d::AbstractDoubleDict,
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    return haskey(d[F, S], key)
end

function Base.haskey(
    d::AbstractDoubleDictInner{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    return haskey(d.dict, key.value)
end

# Base.get

function Base.get(
    d::AbstractDoubleDict,
    key::MOI.ConstraintIndex{F,S},
    default,
) where {F,S}
    inner = d[F, S]
    return get(inner, key, default)
end

function Base.get(
    d::AbstractDoubleDictInner,
    key::MOI.ConstraintIndex{F,S},
    default,
) where {F,S}
    return typed_value(d, get(d.dict, key.value, default))
end

# Base.getindex

function Base.getindex(
    d::AbstractDoubleDict,
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    inner = d[F, S]
    return inner[key]
end

function Base.getindex(
    d::AbstractDoubleDictInner{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    x = get(d.dict, key.value) do
        return throw(KeyError(key))
    end
    return typed_value(d, x)
end

# Base.setindex!

function Base.setindex!(
    d::AbstractDoubleDict{V},
    value::V,
    key::MOI.ConstraintIndex{F,S},
) where {F,S,V}
    inner = d[F, S]
    inner[key] = value
    return value
end

function Base.setindex!(
    d::AbstractDoubleDictInner{F,S,V},
    value::V,
    key::MOI.ConstraintIndex{F,S},
) where {F,S,V}
    d.dict[key.value] = value
    return value
end

function Base.setindex!(
    d::IndexDoubleDict,
    value::MOI.ConstraintIndex{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    inner = d[F, S]
    inner[key] = value
    return value
end

function Base.setindex!(
    d::IndexDoubleDictInner{F,S},
    value::MOI.ConstraintIndex{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    d.dict[key.value] = value.value
    return value
end

# Base.empty!

function Base.empty!(d::AbstractDoubleDict)
    Base.empty!(d.dict)
    return d
end

function Base.empty!(d::AbstractDoubleDictInner)
    empty!(d.dict)
    return d
end

# Base.delete!

function Base.delete!(
    d::AbstractDoubleDict,
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    delete!(d[F, S], key)
    return d
end

function Base.delete!(
    d::AbstractDoubleDictInner{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    delete!(d.dict, key.value)
    return d
end

# Base.isempty

function Base.isempty(d::AbstractDoubleDict)
    return isempty(d.dict) || all(isempty, values(d.dict))
end

Base.isempty(d::AbstractDoubleDictInner) = isempty(d.dict)

# Base.values

function Base.values(d::AbstractDoubleDict{V}) where {V}
    out = V[]
    for (F, S) in keys(d.dict)
        append!(out, values(d[F, S]))
    end
    return out
end

Base.values(d::AbstractDoubleDictInner) = typed_value.(Ref(d), values(d.dict))

# Base.keys

function Base.keys(d::AbstractDoubleDict)
    out = MOI.ConstraintIndex[]
    for (F, S) in keys(d.dict)
        append!(out, keys(d[F, S]))
    end
    return out
end

function Base.keys(d::AbstractDoubleDictInner{F,S}) where {F,S}
    return MOI.ConstraintIndex{F,S}.(keys(d.dict))
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
    result = MOI.ConstraintIndex{F,S}(k) => typed_value(d[F, S], v)
    return result, (inner_state, outer_next)
end

function Base.iterate(d::AbstractDoubleDictInner{F,S}) where {F,S}
    next = iterate(d.dict)
    if next === nothing
        return
    end
    (k, v), inner_state = next
    result = MOI.ConstraintIndex{F,S}(k) => typed_value(d, v)
    return result, (d.dict, inner_state)
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
    result = MOI.ConstraintIndex{F,S}(k) => typed_value(d[F, S], v)
    return result, (inner_state, outer_next)
end

function Base.iterate(d::AbstractDoubleDictInner{F,S}, state) where {F,S}
    inner, inner_state = state
    next = iterate(inner, inner_state)
    if next === nothing
        return
    end
    ((k, v), next_inner_state) = next
    result = MOI.ConstraintIndex{F,S}(k) => typed_value(d, v)
    return result, (inner, next_inner_state)
end

end
