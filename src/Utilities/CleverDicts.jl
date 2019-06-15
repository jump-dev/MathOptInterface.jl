module CleverDicts

import OrderedCollections

"""
    CleverDict{K, V}

A smart storage type for managing sequential objects with non-decreasing integer
indices.

Provided no keys are deleted, the backing storage is a `Vector{V}`. Once a key
has been deleted, the backing storage switches to an `OrderedDict{K, V}`.

The i'th ordered element can be obtained with `c[LinearIndex(i)]`.

Note that querying a `LinearIndex` immediately after deleting a key via
`delete!` is very slow. (It requires a rebuild of an ordered list of variables.)

Store an item `val` using `add_item(c::CleverDict, val)`. `add_item` returns a
key corresponding to the stored item.

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
mutable struct CleverDict{K, V}
    last_index::Int
    vector::Union{Nothing, Vector{V}}
    dict::Union{Nothing, OrderedCollections.OrderedDict{K, V}}
    CleverDict{K, V}() where {K, V} = new{K, V}(0, V[], nothing)
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

"""
    add_item(c::CleverDict{K, V}, val::Val)::K where {K, V}

Set `val` in the next available key, and return that key.
"""
function add_item(c::CleverDict{K, V}, val::V)::K where {K, V}
    c.last_index += 1
    key = index_to_key(K, c.last_index)
    if c.dict === nothing
        push!(c.vector, val)
    else
        c.dict[key] = val
        # If there is a vector (e.g., because it has been rebuild for
        # `LinearIndex`), clear it.
        c.vector = nothing
    end
    return key
end

function Base.empty!(c::CleverDict{K, V})::Nothing where {K, V}
    c.vector = V[]
    c.last_index = 0
    c.dict = nothing
    return
end

function Base.getindex(c::CleverDict{K, V}, key::K)::V where {K, V}
    # Perform this `haskey` check up front to detect getting with keys that are
    # invalid (i.e., have previously been deleted).
    if !haskey(c, key)
        throw(KeyError(key))
    end
    # Case  I) no call to `Base.delete!`, so return the element:
    # Case II) `Base.delete!` must have been called, so return the element
    #          from the dictionary.
    return c.dict === nothing ? c.vector[key_to_index(key)] : c.dict[key]
end

function Base.setindex!(c::CleverDict{K, V}, val::V, key::K)::V where {K, V}
    # Perform this `haskey` check up front to detect setting with keys that are
    # invalid (i.e., have already been deleted). You can only call setindex!
    # with a key obtained from `new_key` that hasn't been deleted.
    if !haskey(c, key)
        throw(KeyError(key))
    elseif c.dict === nothing
        @assert c.vector !== nothing
        c.vector[key_to_index(key)] = val
    else
        c.dict[key] = val
    end
    return val
end

struct LinearIndex
    i::Int
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
    if c.vector === nothing
        c.vector = Vector{V}(undef, length(c))
        for (i, val) in enumerate(values(c.dict))
            c.vector[i] = val
        end
    end
    return c.vector[index.i]::V
end

function Base.delete!(c::CleverDict{K, V}, key::K)::Nothing where {K, V}
    if c.dict === nothing
        c.dict = OrderedCollections.OrderedDict{K, Union{Nothing, V}}()
        for (i, info) in enumerate(c.vector)
            c.dict[index_to_key(K, i)] = info
        end
    end
    delete!(c.dict, key)
    c.vector = nothing
    return
end

function Base.length(c::CleverDict)::Int
    return c.dict == nothing ? length(c.vector) : length(c.dict)
end

function Base.isempty(c::CleverDict)
    return c.dict !== nothing ? isempty(c.dict) : length(c.vector) == 0
end

Base.haskey(::CleverDict, key) = false
function Base.haskey(c::CleverDict{K, V}, key::K)::Bool where {K, V}
    if c.dict === nothing
        return 1 <= key_to_index(key) <= length(c.vector)
    else
        return haskey(c.dict, key)
    end
end

# Here, we implement the iterate functions for our `CleverDict`. If the backing
# datastructure is an `OrderedDict`, we just forward `iterate` to the dict. If
# it's the vector, we create a key-value pair so that `iterate` returns the same
# type regardless of the backing datastructure. To help inference, we annotate
# the return type.
#
# Also note that iterating an `OrderedDict` returns an `Int` state variable.
# This is identical to the type of the state variable that we return when
# iterating the vector, so we can add a type restriction on
# `iterate(c, s::Int)`.

function Base.iterate(
    c::CleverDict{K, V}
)::Union{Nothing, Tuple{Pair{K, V}, Int}} where {K, V}
    if c.dict !== nothing
        return iterate(c.dict)
    else
        @assert c.vector !== nothing
        if length(c.vector) == 0
            return nothing
        end
        key = index_to_key(K, 1)
        return key => c.vector[1], 2
    end
end

function Base.iterate(
    c::CleverDict{K, V}, s::Int
)::Union{Nothing, Tuple{Pair{K, V}, Int}} where {K, V}
    if c.dict !== nothing
        return iterate(c.dict, s)
    else
        @assert c.vector !== nothing
        if s > length(c.vector)
            return nothing
        end
        key = index_to_key(K, s)
        return key => c.vector[s], s + 1
    end
end

function Base.values(c::CleverDict{K, V}) where {K, V}
    return c.dict !== nothing ? values(c.dict) : c.vector
end

function Base.keys(c::CleverDict{K, V}) where {K, V}
    return c.dict !== nothing ? keys(c.dict) : index_to_key.(K, 1:length(c))
end

end
