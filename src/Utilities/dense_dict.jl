"""
    struct DenseDict{K, V, F, I} <: AbstractDict{K, V}
        hash::F
        inverse_hash::I
        set::BitSet
        map::Vector{V}
    end

Same as `Dict{K, V}` but `hash(key)` is assumed to belong to `eachindex(map)`.
`inverse_hash` maps `Int`s to `V`
"""
struct DenseDict{K, V, F, I} <: AbstractDict{K, V}
    hash::F
    inverse_hash::I
    set::BitSet
    map::Vector{V}
    function DenseDict{K, V}(hash, inverse_hash, n) where {K, V}
        set = BitSet()
        sizehint!(set, n)
        return new{K, V, typeof(hash), typeof(inverse_hash)}(
            hash, inverse_hash, set, Vector{K}(undef, n))
    end
end

# Implementation of the `AbstractDict` API.
# Base.empty(::DenseDict, ::Type{K}, ::Type{V}) not implemented
function Base.iterate(d::DenseDict{K,V}, args...) where {K,V}
    itr = iterate(d.set, args...)
    if itr === nothing
        return nothing
    else
        el, i = itr
        return d.inverse_hash(el)::K => d.map[el]::V, i
    end
end

Base.length(d::DenseDict) = length(d.set)
Base.haskey(dict::DenseDict, key) = dict.hash(key) in dict.set
Base.getindex(dict::DenseDict, key) = dict.map[dict.hash(key)]
function Base.setindex!(dict::DenseDict, value, key)
    h = dict.hash(key)
    push!(dict.set, h)
    dict.map[h] = value
end
