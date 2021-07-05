module DoubleDicts

import MathOptInterface

const MOI = MathOptInterface

abstract type AbstractDoubleDict{
    K,
    V,
    IK,
    DI<:AbstractDict{IK},
    DO<:AbstractDict{K,DI},
} <: AbstractDict{MOI.ConstraintIndex,V} end

"""
    DoubleDict{V}

Optimized dictionary to map `MOI.ConstraintIndex` to values of type `V`.

Works as a `AbstractDict{MOI.ConstraintIndex, V}` with minimal differences.

Note that `MOI.ConstraintIndex` is not a concrete type, opposed to
`MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integers}`, which is a concrete
type.

When optimal performance or type stability is required it is possible to obtain
a fully type stable dictionary with values of type `V` and keys of type
`MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integers}` from the dictionary
`dict`, for instance:
```julia
inner = dict[MOI.SingleVariable, MOI.Integers]
```
"""
struct DoubleDict{V,DI,DO} <:
       AbstractDoubleDict{Tuple{DataType,DataType},V,Int64,DI,DO}
    dict::DO
    function DoubleDict{V}() where {V}
        return new{V,Dict{Int64,V},Dict{Tuple{DataType,DataType},Dict{Int64,V}}}(
            Dict{Tuple{DataType,DataType},Dict{Int64,V}}(),
        )
    end
end

"""
    IndexDoubleDict

Specialized version of `DoubleDict` in which keys and values are of type
`ConstraintIndex`

This is an optimized dictionary to map `MOI.ConstraintIndex` to values of
type `MOI.ConstraintIndex`.

Works as a `AbstractDict{MOI.ConstraintIndex, V}` with minimal differences.

Note that `MOI.ConstraintIndex` is not a concrete type, opposed to
`MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integers}`, which is a concrete
type.

When optimal performance or type stability is required its possible to obtain a
fully type stable dictionary with values of type
`MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integers}` and keys of type
`MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integers}` from the dictionary
`dict`, for instance:
```julia
inner = dict[MOI.SingleVariable, MOI.Integers]
```
"""
struct IndexDoubleDict{DI,DO} <: AbstractDoubleDict{
    Tuple{DataType,DataType},
    MOI.ConstraintIndex,
    Int64,
    DI,
    DO,
}
    dict::DO
    function IndexDoubleDict()
        return new{
            Dict{Int64,Int64},
            Dict{Tuple{DataType,DataType},Dict{Int64,Int64}},
        }(
            Dict{Tuple{DataType,DataType},Dict{Int64,Int64}}(),
        )
    end
end

struct FunctionSetDoubleDict{DI,DO} <:
       AbstractDoubleDict{Tuple{DataType,DataType},Tuple,Int64,DI,DO}
    dict::DO
    function FunctionSetDoubleDict()
        return new{Dict{Int64},Dict{Tuple{DataType,DataType},Dict{Int64}}}(
            Dict{Tuple{DataType,DataType},Dict{Int64}}(),
        )
    end
end

const MainIndexDoubleDict = IndexDoubleDict{
    Dict{Int64,Int64},
    Dict{Tuple{DataType,DataType},Dict{Int64,Int64}},
}

@inline function _typed_value(
    ::DoubleDict{V},
    v::V,
    ::Type{F},
    ::Type{S},
)::V where {V,F,S}
    return v
end

@inline function _typed_value(
    ::IndexDoubleDict,
    v::Int64,
    ::Type{F},
    ::Type{S},
)::MOI.ConstraintIndex{F,S} where {F,S}
    return MOI.ConstraintIndex{F,S}(v)
end

@inline function _typed_value(
    ::FunctionSetDoubleDict,
    v::Tuple{F,S},
    ::Type{F},
    ::Type{S},
)::Tuple{F,S} where {F,S}
    return v
end

# reversing IndexDoubleDict is ok because they map CI to CI
function MOI.Utilities._reverse_dict(
    dest::IndexDoubleDict{DI},
    src::IndexDoubleDict{DI},
) where {DI}
    for (k, v) in src.dict
        dest.dict[k] = MOI.Utilities._reverse_dict(v)
    end
end
# reversing other double dict types is not ok because the map CI fo K
# so it wont be a double dict anymore, double dict keys are always CIs.
# We keep the default fallback

function Base.sizehint!(::AbstractDoubleDict, ::Integer)
    return throw(
        ErrorException(
            "sizehint!(d::DoubleDict, ::Integer) has no proper" *
            " meaning for DoubleDict, use sizehint!(d[F,S], n::Integer) " *
            "instead.",
        ),
    )
end

function Base.length(d::AbstractDoubleDict)
    len = 0
    for inner in values(d.dict)
        len += length(inner)
    end
    return len
end

function Base.haskey(
    dict::AbstractDoubleDict,
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    inner = get(dict.dict, (F, S), nothing)
    return inner !== nothing ? haskey(inner, key.value) : false
end

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

function Base.getindex(
    dict::AbstractDoubleDict,
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    inner = dict.dict[(F, S)]
    return _typed_value(dict, inner[key.value], F, S)
end

function _init_inner_dict(
    ::AbstractDoubleDict{K,V},
    ::Type{F},
    ::Type{S},
) where {K,V,F,S}
    return Dict{Int64,V}()
end

function _init_inner_dict(
    ::FunctionSetDoubleDict,
    ::Type{F},
    ::Type{S},
) where {F,S}
    return Dict{Int64,Tuple{F,S}}()
end

function _init_inner_dict(::IndexDoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    return Dict{Int64,Int64}()
end

"""
    lazy_get(dict::DoubleDict, ::Type{F}, ::Type{S})

description
"""
function lazy_get(
    dict::AbstractDoubleDict{K,V},
    ::Type{F},
    ::Type{S},
) where {K,V,F,S}
    return get!(() -> _init_inner_dict(dict, F, S), dict.dict, (F, S))
end

function Base.setindex!(
    dict::AbstractDoubleDict{K,V},
    value::V,
    key::MOI.ConstraintIndex{F,S},
) where {K,V,F,S}
    inner = lazy_get(dict, F, S)
    inner[key.value] = value
    return value
end

function Base.setindex!(
    dict::DoubleDict{V},
    value::V,
    key::MOI.ConstraintIndex{F,S},
) where {V,F,S}
    inner = lazy_get(dict, F, S)::Dict{Int64,V}
    inner[key.value] = value
    return value
end
function Base.setindex!(
    dict::IndexDoubleDict,
    value::MOI.ConstraintIndex{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    inner = lazy_get(dict, F, S)::Dict{Int64,Int64}
    inner[key.value] = value.value
    return value
end

function Base.empty!(d::AbstractDoubleDict)
    Base.empty!(d.dict)
    return d
end

function Base.delete!(
    d::AbstractDoubleDict,
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    inner = lazy_get(d, F, S)
    delete!(inner, key.value)
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

function Base.values(d::IndexDoubleDict)::Vector{MOI.ConstraintIndex}
    out = MOI.ConstraintIndex[]
    for ((F, S), inner) in d.dict
        append!(out, MOI.ConstraintIndex{F,S}.(values(inner)))
    end
    return out
end

function Base.values(d::AbstractDoubleDict{K,V})::Vector{V} where {K,V}
    out = V[]
    for (_, inner) in d.dict
        append!(out, values(inner))
    end
    return out
end

function Base.keys(d::AbstractDoubleDict)
    out = MOI.ConstraintIndex[]
    for ((F, S), inner) in d.dict
        append!(out, MOI.ConstraintIndex{F,S}.(keys(inner)))
    end
    return out
end

function Base.iterate(d::AbstractDoubleDict)
    o_next = iterate(d.dict)
    if o_next === nothing
        return nothing
    end
    (o_i, o_state) = o_next
    ((F, S), inner) = o_i
    i_next = iterate(inner)
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
    return MOI.ConstraintIndex{F,S}(i_i[1]) => _typed_value(d, i_i[2], F, S),
    (i_state, (o_i, o_state))
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
    return MOI.ConstraintIndex{F,S}(i_i[1]) => _typed_value(d, i_i[2], F, S),
    (i_state, (o_i, o_state))
end

abstract type AbstractWithType{F,S,V,DI,DD} <:
              AbstractDict{MOI.ConstraintIndex{F,S},V} end

"""
    WithType{F,S,V,DI,DD}

Used to specialize methods and iterators for a given contraint type
`MOI.ConstraintIndex{F,S}` returning elements of type `V`.
"""
mutable struct WithType{F,S,V,DI,DD} <: AbstractWithType{F,S,V,DI,DD}
    dict::DD
    inner::Union{DI,Nothing}
    function WithType{F,S}(
        d::DD,
    ) where {
        V,
        DI<:AbstractDict{Int64,V},
        DO<:AbstractDict{Tuple{DataType,DataType},DI},
        DD<:DoubleDict{V,DI,DO},
        F,
        S,
    }
        inner = get(d.dict, (F, S), nothing)
        return new{F,S,V,DI,DD}(d, inner)
    end
end

mutable struct IndexWithType{F,S,V,DI,DD} <:
               AbstractWithType{F,S,MOI.ConstraintIndex{F,S},DI,DD}
    dict::DD
    inner::Union{DI,Nothing}
    function IndexWithType{F,S}(
        d::DD,
    ) where {
        DI<:AbstractDict{Int64,Int64},
        DO<:AbstractDict{Tuple{DataType,DataType},DI},
        DD<:IndexDoubleDict{DI,DO},
        F,
        S,
    }
        inner = get(d.dict, (F, S), nothing)::Union{DI,Nothing}
        return new{F,S,MOI.ConstraintIndex{F,S},DI,DD}(d, inner)
    end
end

mutable struct FunctionSetWithType{F,S,V,DI,DD} <:
               AbstractWithType{F,S,Tuple{F,S},DI,DD}
    dict::DD
    inner::Union{DI,Nothing}
    function FunctionSetWithType{F,S}(
        d::DD,
    ) where {
        F,
        S,
        DI<:AbstractDict{Int64},
        DO<:AbstractDict{Tuple{DataType,DataType},DI},
        DD<:FunctionSetDoubleDict{DI,DO},
    }
        inner = get(d.dict, (F, S), nothing)::Union{DI{Tuple{F,S}},Nothing}
        return new{F,S,Tuple{F,S},DI{Tuple{F,S}},DD}(d, inner)
    end
end

function with_type(
    d::DoubleDict,
    ::Type{F},
    ::Type{S},
)::WithType{F,S} where {F,S}
    return WithType{F,S}(d)::WithType{F,S}
end

function with_type(
    d::IndexDoubleDict,
    ::Type{F},
    ::Type{S},
)::IndexWithType{F,S} where {F,S}
    return IndexWithType{F,S}(d)::IndexWithType{F,S}
end

function with_type(
    d::FunctionSetDoubleDict,
    ::Type{F},
    ::Type{S},
)::FunctionSetWithType{F,S} where {F,S}
    return FunctionSetWithType{F,S}(d)::FunctionSetWithType{F,S}
end

function Base.getindex(d::DoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    return WithType{F,S}(d)
end

function Base.getindex(d::IndexDoubleDict, ::Type{F}, ::Type{S}) where {F,S}
    return IndexWithType{F,S}(d)
end

function Base.getindex(
    d::FunctionSetDoubleDict,
    ::Type{F},
    ::Type{S},
) where {F,S}
    return FunctionSetWithType{F,S}(d)
end

@inline function _typed_value(::WithType{F,S,V}, v::V)::V where {V,F,S}
    return v
end

@inline function _typed_value(
    ::IndexWithType{F,S},
    v::Int64,
)::MOI.ConstraintIndex{F,S} where {F,S}
    return MOI.ConstraintIndex{F,S}(v)
end

@inline function _typed_value(
    ::FunctionSetWithType{F,S,V},
    v::Tuple{F,S},
)::Tuple{F,S} where {V,F,S}
    return v
end

function _initialize_inner!(d::AbstractWithType{F,S,V,D}) where {F,S,V,D}
    d.inner = D()
    d.dict.dict[(F, S)] = d.inner
    return
end

_inner_is_empty(d::AbstractWithType)::Bool = d.inner === nothing

function _inner(d::AbstractWithType{F,S,V,D}) where {F,S,V,D}
    return d.inner::D
end

function Base.sizehint!(d::AbstractWithType{F,S}, n::Integer) where {F,S}
    if _inner_is_empty(d)
        _initialize_inner!(d)
    end
    return sizehint!(_inner(d), n)
end

function Base.length(d::AbstractWithType{F,S}) where {F,S}
    if _inner_is_empty(d)
        return 0
    end
    return length(_inner(d))
end

function Base.haskey(
    d::AbstractWithType{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    if _inner_is_empty(d)
        return false
    end
    return haskey(_inner(d), key.value)
end

function Base.getindex(
    d::AbstractWithType{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    if _inner_is_empty(d)
        throw(KeyError(key))
    end
    return _typed_value(d, _inner(d)[key.value])
end

function Base.setindex!(
    d::WithType{F,S,V},
    value::V,
    key::MOI.ConstraintIndex{F,S},
)::V where {F,S,V}
    if _inner_is_empty(d)
        _initialize_inner!(d)
    end
    inner = _inner(d)
    inner[key.value] = value
    return value
end

function Base.setindex!(
    d::IndexWithType{F,S,V},
    value::MOI.ConstraintIndex{F,S},
    key::MOI.ConstraintIndex{F,S},
)::MOI.ConstraintIndex{F,S} where {F,S,V}
    if _inner_is_empty(d)
        _initialize_inner!(d)
    end
    inner = _inner(d)::Dict{Int64,Int64}
    inner[key.value] = value.value
    return value
end

function Base.setindex!(
    d::FunctionSetWithType{F,S,V},
    value::Tuple{F,S},
    key::MOI.ConstraintIndex{F,S},
)::Tuple{F,S} where {F,S,V}
    if _inner_is_empty(d)
        _initialize_inner!(d)
    end
    inner = _inner(d)::Dict{Int64,Tuple{F,S}}
    inner[key.value] = value
    return value
end

function Base.empty!(d::AbstractWithType{F,S}) where {F,S}
    if _inner_is_empty(d)
        return d
    end
    return empty!(_inner(d))
end

function Base.delete!(
    d::AbstractWithType{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    if _inner_is_empty(d)
        return d
    end
    k_value = key.value::Int64
    delete!(_inner(d), k_value)
    return d
end

function Base.isempty(d::AbstractWithType{F,S}) where {F,S}
    if _inner_is_empty(d)
        return true
    end
    return isempty(_inner(d))
end

function Base.values(d::WithType{F,S,V})::Vector{V} where {F,S,V}
    if _inner_is_empty(d)
        return V[]
    end
    return collect(values(_inner(d)))
end

function Base.values(
    d::IndexWithType{F,S},
)::Vector{MOI.ConstraintIndex{F,S}} where {F,S}
    if _inner_is_empty(d)
        return MOI.ConstraintIndex{F,S}[]
    end
    return MOI.ConstraintIndex{F,S}.(values(_inner(d)))
end

function Base.values(
    d::FunctionSetWithType{F,S},
)::Vector{Tuple{F,S}} where {F,S}
    if _inner_is_empty(d)
        return Tuple{F,S}[]
    end
    return collect(values(_inner(d)))
end

function Base.keys(d::AbstractWithType{F,S}) where {F,S}
    if _inner_is_empty(d)
        return MOI.ConstraintIndex{F,S}[]
    end
    return MOI.ConstraintIndex{F,S}.(keys(_inner(d)))
end

function Base.iterate(d::AbstractWithType{F,S}) where {F,S}
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

function Base.iterate(d::AbstractWithType{F,S}, state) where {F,S}
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
