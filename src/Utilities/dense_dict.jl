"""
    struct DenseDict{K, V, F, I} <: AbstractDict{K, V}
        hash::F
        inverse_hash::I
        set::BitSet
        map::Vector{V}
        dict::Dict{K,V}
    end

Same as `Dict{K, V}` but very performant if `hash(key)` belongs to
`eachindex(map)`. If `hash(key) == length(map) + 1` the dictionary
grows continuosly in aperformant fashion. Otherwise, a regular `Dict{K, V}`
is used.
"""
struct DenseDict{K, V, F, I} <: AbstractDict{K, V}
    hash::F
    inverse_hash::I
    set::BitSet
    map::Vector{V}
    dict::Dict{K,V}
    function DenseDict{K, V}(hash, inverse_hash, n = 0) where {K, V}
        set = BitSet()
        sizehint!(set, n)
        return new{K, V, typeof(hash), typeof(inverse_hash)}(
            hash, inverse_hash, set, Vector{K}(undef, n), Dict{K,V}()
            )
    end
end

_is_dense(d::DenseDict) = !isempty(d.map)

# Implementation of the `AbstractDict` API.
function Base.empty!(d::DenseDict)
    if _is_dense(d)
        empty!(d.set)
        empty!(d.map)
    else
        empty!(d.dict)
    end
end
function Base.iterate(d::DenseDict{K,V}, args...) where {K,V}
    if _is_dense(d)
        itr = iterate(d.set, args...)
        if itr === nothing
            return nothing
        else
            el, i = itr
            return d.inverse_hash(el)::K => d.map[el]::V, i
        end
    else
        return Base.iterate(d.dict, args...)
    end
end
function Base.length(d::DenseDict)
    if _is_dense(d)
        return length(d.set)
    else
        return length(d.dict)
    end
end
function Base.haskey(d::DenseDict, key)
    if _is_dense(d)
        return d.hash(key) in d.set
    else
        return Base.haskey(d.dict, key)
    end
end
function Base.getindex(d::DenseDict, key)
    if _is_dense(d)
        return d.map[d.hash(key)]
    else
        return d.dict[key]
    end
end
function Base.setindex!(d::DenseDict, value, key)
    h = d.hash(key)
    if h <= length(d.map) && _is_dense(d)
        push!(d.set, h)
        d.map[h] = value
    elseif h == length(d.map) + 1 && _is_dense(d)
        push!(d.set, h)
        push!(d.map, value)
    else
        if _is_dense(d)
            _rehash(d)
        end
        d.dict[key] = value
    end
end
function Base.sizehint!(d::DenseDict, n)
    if _is_dense(d)
        sizehint!(d.set, n)
        sizehint!(d.map, n)
    else
        sizehint!(d.dict, n)
    end
end

function _rehash(d::DenseDict{K}) where K
    sizehint!(d.dict, length(d.set))
    # assumes dict is currently dense
    # iterator protocol from DenseDict is used
    for (k,v) in d
        d.dict[k] = v
    end
    empty!(d.set)
    empty!(d.map)
end

function Base.delete!(d::DenseDict{K}, k::K) where K
    if _is_dense(d)
        _rehash(d)
    end
    delete!(d.dict, k)
end
