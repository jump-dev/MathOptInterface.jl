# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module Coloring

"""
    mutable struct IndexedSet
        nzidx::Vector{Int}
        empty::BitVector
        nnz::Int
    end

Represent the set `nzidx[1:nnz]` by additionally setting `empty[i]` to `false`
for each element `i` of the set for fast membership check.
"""
mutable struct IndexedSet
    nzidx::Vector{Int}
    empty::BitVector
    nnz::Int
end

IndexedSet(n::Integer) = IndexedSet(zeros(Int, n), trues(n), 0)

function Base.push!(v::IndexedSet, i::Integer)
    if v.empty[i]  # new index
        v.nzidx[v.nnz+=1] = i
        v.empty[i] = false
    end
    return
end

function Base.empty!(v::IndexedSet)
    for i in 1:v.nnz
        v.empty[v.nzidx[i]] = true
    end
    v.nnz = 0
    return v
end

# Returns the maximum index that the set can contain,
# not the cardinality of the set like `length(::Base.Set)`
Base.length(v::IndexedSet) = length(v.nzidx)

function Base.resize!(v::IndexedSet, n::Integer)
    if n > length(v)
        @assert v.nnz == 0 # only resize empty vector
        resize!(v.nzidx, n)
        resize!(v.empty, n)
        fill!(v.empty, true)
    end
    return
end

Base.collect(v::IndexedSet) = v.nzidx[1:v.nnz]

function Base.union!(v::IndexedSet, s)
    for x in s
        push!(v, x)
    end
    return
end

end # module Coloring
