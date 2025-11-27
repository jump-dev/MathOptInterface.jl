# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
# Copyright (c) 2024: Guillaume Dalle and Alexis Montoison
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# The code in this file was taken from
# https://github.com/gdalle/SparseMatrixColorings.jl/blob/main/src/forest.jl
#
# It was copied at the suggestion of Alexis in his JuMP-dev 2025 talk.
#
# @odow made minor changes to match MOI coding styles.
#
# x-ref https://github.com/gdalle/SparseMatrixColorings.jl/pull/190

mutable struct _Forest
    # current number of distinct trees in the forest
    number_of_trees::Int
    # vector storing the index of a parent in the tree for each edge, used in
    # union-find operations
    parents::Vector{Int}
    # vector approximating the depth of each tree to optimize path compression
    ranks::Vector{Int}

    _Forest(n::Integer) = new(n, collect(Base.OneTo(n)), zeros(Int, n))
end

function _find_root!(parents::Vector{Int}, index_edge::Integer)
    p = parents[index_edge]
    if parents[p] != p
        parents[index_edge] = p = _find_root!(parents, p)
    end
    return p
end

function _find_root!(forest::_Forest, index_edge::Integer)
    return _find_root!(forest.parents, index_edge)
end

function _root_union!(forest::_Forest, index_edge1::Int, index_edge2::Int)
    rank1, rank2 = forest.ranks[index_edge1], forest.ranks[index_edge2]
    if rank1 < rank2
        index_edge1, index_edge2 = index_edge2, index_edge1
    elseif rank1 == rank2
        forest.ranks[index_edge1] += 1
    end
    forest.parents[index_edge2] = index_edge1
    forest.number_of_trees -= 1
    return
end
