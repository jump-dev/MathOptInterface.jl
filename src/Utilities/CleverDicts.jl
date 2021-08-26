module CleverDicts

# The following two functions are overloaded for `MOI.VariableIndex` here
# it is the original use-case for `CleverDict`, and it would be type-piracy for
# solvers using `CleverDicts` to implement it themselves.

import MathOptInterface
import OrderedCollections

"""
    index_to_key(::Type{K}, index::Int)

Create a new key associated with the integer value `index`.
"""
function index_to_key end

"""
    key_to_index(key::K)

Map `key` to an integer valued index, assuming that there have been no
deletions.
"""
function key_to_index end

function index_to_key(::Type{MathOptInterface.VariableIndex}, index::Int64)
    return MathOptInterface.VariableIndex(index)
end

function index_to_key(
    ::Type{MathOptInterface.ConstraintIndex{F,S}},
    index::Int64,
) where {F,S}
    return MathOptInterface.ConstraintIndex{F,S}(index)
end

key_to_index(key::MathOptInterface.Index) = key.value

# Now, on with `CleverDicts`.

"""
    CleverDict{K, V}

A smart storage type for managing sequential objects with non-decreasing integer
indices. Note that the integers are `Int64`s.

Provided no keys are deleted, the backing storage is a `Vector{V}`. Once a key
has been deleted, the backing storage switches to an `OrderedDict{K, V}`.

Use the `add_item` to enforce adding item in sequence. Once an object is added
out of order `add_item` does not work anymore and the storage is switched to
`OrderedDict{K, V}` if it is not one already.

The i'th ordered element can be obtained with `c[LinearIndex(i)]`.

Note that querying a `LinearIndex` immediately after deleting a key via
`delete!` is very slow. (It requires a rebuild of an ordered list of variables.)

Store an item `val` using `add_item(c::CleverDict, val)`. `add_item` returns a
key corresponding to the stored item.

It is possible to initialize the CleverDict as `CleverDict{K, V}(n)` so that
`n` elements are pre-allocated and can be efficiently added even out of order
as long as their key hashes are between 1 and `n`.

Overload the functions `index_to_key` and `key_to_index` to enable mappings
between the integer index of the vector and the dictionary key.

## Example

```julia
struct MyKey
    x::Int64
end
index_to_key(::Type{MyKey}, i::Int64) = MyKey(i)
key_to_index(key::MyKey) = key.x
```
"""
mutable struct CleverDict{K,V,F<:Function,I<:Function} <: AbstractDict{K,V}
    last_index::Int64
    hash::F
    inverse_hash::I
    is_dense::Bool
    vector::Vector{V}
    dict::OrderedCollections.OrderedDict{K,V}
    function CleverDict{K,V}(
        hash::F = key_to_index,
        inverse_hash::I = index_to_key,
        n = nothing,
    ) where {K,V,F<:Function,I<:Function}
        if n !== nothing
            @warn("The `n` argument to `CleverDict` has been removed.")
        end
        return new{K,V,F,I}(
            0,
            hash,
            inverse_hash,
            true,
            K[],
            OrderedCollections.OrderedDict{K,V}(),
        )
    end
end

_is_dense(c::CleverDict) = c.is_dense

function _inverse_hash(c::CleverDict{K}, index::Integer) where {K}
    return c.inverse_hash(Int64(index))::K
end

function _inverse_hash(
    ::CleverDict{K,V,F,typeof(index_to_key)},
    index::Integer,
) where {K,V,F<:Function}
    return index_to_key(K, Int64(index))::K
end

"""
    add_item(c::CleverDict{K, V}, val::Val)::K where {K, V}

Set `val` in the next available key, and return that key.
"""
function add_item(c::CleverDict{K,V}, val::V)::K where {K,V}
    if c.last_index == -1
        error(
            "Keys were added out of order. `add_item` requires that keys are always added in order.",
        )
    end
    # adding a key in order
    key = _inverse_hash(c, c.last_index + 1)
    c[key] = val
    return key
end

function Base.empty!(c::CleverDict)
    empty!(c.vector)
    empty!(c.dict)
    c.last_index = 0
    c.is_dense = true
    return
end

function Base.length(c::CleverDict)
    return _is_dense(c) ? length(c.vector) : length(c.dict)
end

function Base.haskey(c::CleverDict{K}, key::K) where {K}
    if _is_dense(c)
        return 1 <= c.hash(key)::Int64 <= length(c.vector)
    end
    return haskey(c.dict, key)
end

function Base.keys(c::CleverDict{K}) where {K}
    if _is_dense(c)
        return K[_inverse_hash(c, i) for i in 1:length(c.vector)]
    end
    return collect(keys(c.dict))
end

function Base.get(c::CleverDict, key, default)
    if _is_dense(c)
        if !haskey(c, key)
            return default
        end
        return c.vector[c.hash(key)::Int64]
    end
    return get(c.dict, key, default)
end

function Base.getindex(c::CleverDict{K,V}, key::K) where {K,V}
    if _is_dense(c)
        if !haskey(c, key)
            throw(KeyError(key))
        end
        return c.vector[c.hash(key)::Int64]
    end
    return c.dict[key]
end

function Base.setindex!(c::CleverDict{K,V}, value::V, key::K)::V where {K,V}
    h = c.hash(key)::Int64
    if c.last_index != -1
        if h == c.last_index + 1
            c.last_index = h
        elseif h <= 0 || h > c.last_index
            c.last_index = -1
        end
    end
    if 1 <= h <= length(c.vector) && _is_dense(c)
        c.vector[h] = value
    elseif h == length(c.vector) + 1 && _is_dense(c)
        push!(c.vector, value)
    else
        if _is_dense(c)
            _rehash(c)
        end
        c.dict[key] = value
        # If there is a vector (e.g., because it has been rebuilt for
        # `LinearIndex`), clear it.
        if !isempty(c.vector)
            empty!(c.vector)
        end
    end
    return value
end

function _rehash(c::CleverDict{K}) where {K}
    sizehint!(c.dict, length(c.vector))
    @assert _is_dense(c)
    # Since c is currently dense, iterator protocol from CleverDict is used.
    for (k, v) in c
        c.dict[k] = v
    end
    empty!(c.vector)
    c.is_dense = false
    return
end

function Base.delete!(c::CleverDict{K}, k::K) where {K}
    if _is_dense(c)
        _rehash(c)
    end
    delete!(c.dict, k)
    if !isempty(c.vector)
        empty!(c.vector)
    end
    return c
end

struct LinearIndex
    i::Int64
end

function Base.getindex(c::CleverDict{K,V}, index::LinearIndex)::V where {K,V}
    if !(1 <= index.i <= length(c))
        throw(KeyError(index))
    end
    # Get the `index` linear element. If `c.vector` is currently `nothing`
    # (i.e., there has been a deletion), rebuild `c.vector`. This is a
    # trade-off: We could ensure `c.vector` is always updated, but this requires
    # a `splice!` in `delete!`, making deletions costly. However, it makes this
    # `getindex` operation trival because we would never have to rebuild the
    # vector.
    # The current implemented approach offers quick deletions, but an expensive
    # rebuild the first time you query a `LinearIndex` after a deletion or a new
    # key is added. Once the rebuild is done, there are quick queries until the
    # next deletion or addition. Thus, the worst-case is a user repeatedly
    # deleting a key and then querying a LinearIndex (e.g., getting the MOI
    # objective function).
    if !_is_dense(c) && length(c.dict) != length(c.vector)
        c.vector = Vector{V}(undef, length(c))
        for (i, val) in enumerate(values(c.dict))
            c.vector[i] = val
        end
    end
    return c.vector[index.i]::V
end

function Base.isempty(c::CleverDict)
    return _is_dense(c) ? isempty(c.vector) : isempty(c.dict)
end

Base.haskey(::CleverDict, key) = false

function Base.iterate(c::CleverDict{K,V}) where {K,V}
    if _is_dense(c)
        itr = iterate(c.vector)
        if itr === nothing
            return
        end
        return _inverse_hash(c, 1) => itr[1]::V, itr[2]::Int
    end
    itr = iterate(c.dict)
    if itr === nothing
        return
    end
    return convert(Pair{K,V}, itr[1]), itr[2]
end

function Base.iterate(c::CleverDict{K,V}, s::Int) where {K,V}
    if _is_dense(c)
        itr = iterate(c.vector, s)
        if itr === nothing
            return
        end
        return _inverse_hash(c, s) => itr[1]::V, itr[2]::Int
    end
    itr = iterate(c.dict, s)
    if itr === nothing
        return
    end
    return convert(Pair{K,V}, itr[1]), itr[2]
end

# we do not have to implement values and keys functions because they can rely
# on `Base`s fallback that uses the iterator protocol

function Base.sizehint!(c::CleverDict{K,V}, n) where {K,V}
    if _is_dense(c)
        sizehint!(c.vector, n)
    else
        sizehint!(c.dict, n)
    end
    return
end

function Base.resize!(c::CleverDict{K,V}, n) where {K,V}
    if _is_dense(c)
        if n < length(c.vector)
            error(
                "CleverDict cannot be resized to a size smaller than the current",
            )
        end
        resize!(c.vector, n)
    else
        sizehint!(c.dict, n)
    end
    return
end

Base.values(d::CleverDict) = _is_dense(d) ? d.vector : values(d.dict)

# TODO `map!(f, values(dict::AbstractDict))` requires Julia 1.2 or later,
#      use `map_values` once we drop Julia 1.1 and earlier.
function map_values!(f::Function, d::CleverDict)
    if _is_dense(d)
        map!(f, d.vector, d.vector)
    else
        for (k, v) in d.dict
            d.dict[k] = f(v)
        end
    end
    return
end

function Base.convert(
    ::Type{CleverDict{K,V,typeof(key_to_index),typeof(index_to_key)}},
    d::CleverDict{K,V,typeof(key_to_index),typeof(index_to_key)},
) where {K,V}
    return d
end

function Base.convert(
    ::Type{CleverDict{K,V,typeof(key_to_index),typeof(index_to_key)}},
    src::AbstractDict{K,V},
) where {K,V}
    dest = CleverDict{K,V}()
    for key in sort(collect(keys(src)), by = dest.hash)
        dest[key] = src[key]
    end
    return dest
end

end
