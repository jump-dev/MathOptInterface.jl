module CleverDicts

# The following two functions are overloaded for `MOI.VariableIndex` here
# it is the original use-case for `CleverDict`, and it would be type-piracy for
# solvers using `CleverDicts` to implement it themselves.

import MathOptInterface

function index_to_key(::Type{MathOptInterface.VariableIndex}, index::Int)
    return MathOptInterface.VariableIndex(index)
end

key_to_index(key::MathOptInterface.VariableIndex) = key.value

# Now, on with `CleverDicts`.

import OrderedCollections

"""
    CleverDict{K, V}

A smart storage type for managing sequential objects with non-decreasing integer
indices.

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
    x::Int
end
index_to_key(::Type{MyKey}, i::Int) = MyKey(i)
key_to_index(key::MyKey) = key.x
```
"""
mutable struct CleverDict{K, V, F<:Function, I<:Function} <: AbstractDict{K, V}
    last_index::Int64
    hash::F
    inverse_hash::I
    is_dense::Bool
    set::BitSet
    vector::Vector{V}
    dict::OrderedCollections.OrderedDict{K, V}
    function CleverDict{K, V}(
        n::Integer = 0
    ) where {K, V}
        set = BitSet()
        sizehint!(set, n)
        vec = Vector{K}(undef, n)
        inverse_hash = x -> index_to_key(K, x)
        hash = key_to_index
        new{K, V, typeof(hash), typeof(inverse_hash)}(
            0, hash, inverse_hash, true,
            set,
            vec,
            OrderedCollections.OrderedDict{K, V}())
    end
    function CleverDict{K, V}(hash::F, inverse_hash::I,
        n::Integer = 0
    ) where {K, V, F, I}
        set = BitSet()
        sizehint!(set, n)
        vec = Vector{K}(undef, n)
        new{K, V, F, I}(
            0, hash, inverse_hash, true,
            set,
            vec,
            OrderedCollections.OrderedDict{K, V}())
    end
end

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

_is_dense(c::CleverDict) = c.is_dense

"""
    add_item(c::CleverDict{K, V}, val::Val)::K where {K, V}

Set `val` in the next available key, and return that key.
"""
function add_item(c::CleverDict{K, V}, val::V)::K where {K, V}
    if c.last_index == -1
        error("Keys were added out of order. `add_item` requires that keys are always added in order.")
    end
    # adding a key in order
    key = c.inverse_hash(c.last_index + 1)::K
    c[key] = val
    return key
end

function Base.empty!(c::CleverDict, init_in_dense_mode = true)
    if _is_dense(c)
        empty!(c.vector)
        empty!(c.set)
    else
        empty!(c.dict)
    end
    c.last_index = 0
    c.is_dense = init_in_dense_mode
    return nothing
end

function Base.length(c::CleverDict)
    if _is_dense(c)
        # c.vector can be larger due to initial allocation
        return length(c.set)
    else
        return length(c.dict)
    end
end
function Base.haskey(c::CleverDict{K}, key::K) where K
    if _is_dense(c)
        return c.hash(key)::Int64 in c.set
    else
        return Base.haskey(c.dict, key)
    end
end
function Base.get(c::CleverDict, key, default)
    if _is_dense(c)
        if !haskey(c, key)
            return default
        end
        return c.vector[c.hash(key)::Int64]
    else
        return get(c.dict, key, default)
    end
end
function Base.getindex(c::CleverDict, key)
    if _is_dense(c)
        if !haskey(c, key)
            throw(KeyError(key))
        end
        return c.vector[c.hash(key)::Int64]
    else
        return c.dict[key]
    end
end
function Base.setindex!(c::CleverDict{K, V}, value, key)::V where {K, V}
    h = c.hash(key)::Int64
    if h > c.last_index
        c.last_index = ifelse(h == c.last_index + 1, h, -1)
    elseif h <= 0
        error("Invalid key, its hash must be > 0")
    end
    if 1 <= h <= length(c.vector) && _is_dense(c)
        c.vector[h] = value
        push!(c.set, h)
    elseif h == length(c.vector) + 1 && _is_dense(c)
        push!(c.vector, value)
        push!(c.set, h)
    else
        if _is_dense(c)
            _rehash(c)
        end
        c.dict[key] = value
        # If there is a vector (e.g., because it has been rebuild for
        # `LinearIndex`), clear it.
        if !isempty(c.vector)
            empty!(c.vector)
        end
    end
    return value
end

function _rehash(c::CleverDict{K}) where K
    sizehint!(c.dict, length(c.vector))
    @assert _is_dense(c)
    # since dict is currently dense
    # iterator protocol from CleverDict is used
    for (k,v) in c
        c.dict[k] = v
    end
    empty!(c.vector)
    empty!(c.set)
    c.is_dense = false
end

function Base.delete!(c::CleverDict{K}, k::K) where K
    if _is_dense(c)
        _rehash(c)
    end
    delete!(c.dict, k)
    if !isempty(c.vector)
        empty!(c.vector)
    end
    return nothing
end

struct LinearIndex
    i::Int64
end
function Base.getindex(c::CleverDict{K, V}, index::LinearIndex)::V where {K, V}
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
    return _is_dense(c) ? isempty(c.set) : isempty(c.dict)
end

Base.haskey(::CleverDict, key) = false

# Here, we implement the iterate functions for our `CleverDict`. For either
# backend (`OrderedDict` or `Vector`+`BitSet`) we return the same State type
# so that `iterate` returns the same
# type regardless of the backing datastructure. To help inference, we convert
# the return type. Don't use a type-assertion because this doesn't support `V`
# being an abstract type.

struct State
    int::Int64
    uint::UInt64
    State(i::Int64) = new(i, 0)
    State(i::Int64, j::UInt64) = new(i , j)
end

function Base.iterate(
    c::CleverDict{K, V}
)::Union{Nothing, Tuple{Pair{K, V}, State}} where {K, V}
    if _is_dense(c)
        itr = iterate(c.set)
        if itr === nothing
            return nothing
        else
            el, i = itr
            @static if VERSION >= v"1.4.0"
                return c.inverse_hash(el)::K => c.vector[el]::V, State(i[2], i[1])
            else
                return c.inverse_hash(el)::K => c.vector[el]::V, State(i)
            end
        end
    else
        itr = iterate(c.dict)
        if itr === nothing
            return nothing
        else
            el, i = itr
            return convert(Pair{K, V}, el), State(i)
        end
    end
end

function Base.iterate(
    c::CleverDict{K, V}, s::State
)::Union{Nothing, Tuple{Pair{K, V}, State}} where {K, V}
    if _is_dense(c)
        @static if VERSION >= v"1.4.0"
            itr = iterate(c.set, (s.uint, s.int))
        else
            itr = iterate(c.set, s.int)
        end
        if itr === nothing
            return nothing
        else
            el, i = itr
            @static if VERSION >= v"1.4.0"
                return c.inverse_hash(el)::K => c.vector[el]::V, State(i[2], i[1])
            else
                return c.inverse_hash(el)::K => c.vector[el]::V, State(i)
            end
        end
    else
        itr = iterate(c.dict, s.int)
        if itr === nothing
            return nothing
        else
            el, i = itr
            return convert(Pair{K, V}, el), State(i)
        end
    end
end

# we do not have to implement values and keys functions because they can rely
# on `Base`s fallback that uses the iterator protocol

function Base.sizehint!(c::CleverDict{K, V}, n) where {K, V}
    if _is_dense(c)
        sizehint!(c.vector, n)
        sizehint!(c.set, n)
    else
        sizehint!(c.dict, n)
    end
end

function Base.resize!(c::CleverDict{K, V}, n) where {K, V}
    if _is_dense(c)
        if n < length(c.vector)
            error("CleverDict cannot be resized to a size smaller than the current")
        end
        resize!(c.vector, n)
        sizehint!(c.set, n)
    else
        sizehint!(c.dict, n)
    end
end

end
