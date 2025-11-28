# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
# Copyright (c) 2024: Guillaume Dalle and Alexis Montoison
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# The code in this file was taken from
# https://github.com/gdalle/SparseMatrixColorings.jl/blob/main/src/Forest.jl
#
# It was copied at the suggestion of Alexis in his JuMP-dev 2025 talk.
#
# @odow made minor changes to match MOI coding styles.
#
# x-ref https://github.com/gdalle/SparseMatrixColorings.jl/pull/190

mutable struct _IntDisjointSet
    # current number of distinct trees in the S
    number_of_trees::Int
    # vector storing the index of a parent in the tree for each edge, used in
    # union-find operations
    parents::Vector{Int}
    # vector approximating the depth of each tree to optimize path compression
    ranks::Vector{Int}

    _IntDisjointSet(n::Integer) = new(n, collect(1:n), zeros(Int, n))
end

function _find_root!(S::_IntDisjointSet, x::Integer)
    p = S.parents[x]
    if S.parents[p] != p
        S.parents[x] = p = _find_root!(S, p)
    end
    return p
end

function _root_union!(S::_IntDisjointSet, x::Int, y::Int)
    rank1, rank2 = S.ranks[x], S.ranks[y]
    if rank1 < rank2
        x, y = y, x
    elseif rank1 == rank2
        S.ranks[x] += 1
    end
    S.parents[y] = x
    S.number_of_trees -= 1
    return
end
