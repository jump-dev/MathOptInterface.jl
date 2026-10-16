# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    struct ColoringResult{R<:SMC.AbstractColoringResult}
        result::R
        local_indices::Vector{Int}  # map from local to global indices
    end

Wrapper around AbstractColoringResult that maps local Hessian indices back
to global variable indices.
"""
struct ColoringResult{R<:SMC.AbstractColoringResult}
    result::R
    local_indices::Vector{Int}   # map from local to global indices
end

"""
    _hessian_color_preprocess(
        edgelist,
        num_total_var,
        algo::SMC.GreedyColoringAlgorithm,
        seen_idx = MOI.Nonlinear.ReverseAD.Coloring.IndexedSet(0),
    )

`edgelist` contains the nonzeros in the Hessian, *including* nonzeros on the
diagonal. Diagonal entries absent from `edgelist` are not added to the pattern.

Returns `(colptr, I, J, result)` where `colptr` contains the local lower-
triangular CSC column pointers and `I`, `J` give the corresponding entries
in global variable indices. `result` wraps an SMC coloring result, or is
`nothing` if the pattern is empty.
"""
function _hessian_color_preprocess(
    edgelist,
    num_total_var,
    algo::SMC.GreedyColoringAlgorithm,
    seen_idx = MOI.Nonlinear.ReverseAD.Coloring.IndexedSet(0),
)
    resize!(seen_idx, num_total_var)
    I, J = Int[], Int[]
    for (i, j) in edgelist
        push!(seen_idx, i)
        push!(seen_idx, j)
        push!(I, max(i, j))
        push!(J, min(i, j))
    end
    local_indices = sort!(collect(seen_idx))
    empty!(seen_idx)
    # SMC's star coloring does not support an empty pattern.
    # TODO: remove when https://github.com/JuliaDiff/SparseMatrixColorings.jl/pull/352 lands
    if isempty(local_indices)
        return [1], Int[], Int[], nothing
    end

    global_to_local_idx = seen_idx.nzidx # steal for storage
    for k in eachindex(local_indices)
        global_to_local_idx[local_indices[k]] = k
    end
    for k in eachindex(I)
        I[k] = global_to_local_idx[I[k]]
        J[k] = global_to_local_idx[J[k]]
    end

    n = length(local_indices)
    mat = SparseArrays.sparse(I, J, trues(length(I)), n, n, |)
    # Color the full symmetric pattern, but recover only its lower triangle.
    S = SMC.SparsityPatternCSC(
        SparseArrays.sparse(LinearAlgebra.Symmetric(mat, :L)),
    )
    problem = SMC.ColoringProblem(; structure = :symmetric, partition = :column)
    tree_result = SMC.coloring(S, problem, algo)

    I, J, _ = SparseArrays.findnz(mat)
    I_global = local_indices[I]
    J_global = local_indices[J]
    result = ColoringResult(tree_result, local_indices)
    return mat.colptr, I_global, J_global, result
end

"""
    _seed_matrix(result::ColoringResult)

Allocate a seed matrix for the coloring result.
"""
function _seed_matrix(result::ColoringResult)
    n = length(result.local_indices)
    ncolors = SMC.ncolors(result.result)
    return Matrix{Float64}(undef, n, ncolors)
end

function _seed_matrix(::Nothing)
    return Matrix{Float64}(undef, 0, 0)
end

"""
    _prepare_seed_matrix!(R, result::ColoringResult)

Prepare the seed matrix R for Hessian computation.
"""
function _prepare_seed_matrix!(R, result::ColoringResult)
    color = SMC.column_colors(result.result)
    N = length(result.local_indices)
    @assert N == size(R, 1)
    @assert size(R, 2) == SMC.ncolors(result.result)
    fill!(R, 0.0)
    for i in 1:N
        if color[i] > 0
            R[i, color[i]] = 1
        end
    end
    return
end

"""
    _recover_from_matmat!(
        colptr::AbstractVector,
        V::AbstractVector{T},
        R::AbstractMatrix{T},
        result::ColoringResult,
    ) where {T}

Recover the Hessian values from the Hessian-matrix product H*R_seed.
R is the result of H*R_seed where R_seed is the seed matrix.
"""
function _recover_from_matmat!(
    colptr::AbstractVector,
    V::AbstractVector{T},
    R::AbstractMatrix{T},
    result::ColoringResult,
) where {T}
    SMC.decompress_csc!(V, colptr, R, result.result, :L)
    return
end

function _recover_from_matmat!(
    colptr::AbstractVector,
    V::AbstractVector{T},
    R::AbstractMatrix{T},
    result::ColoringResult{<:SMC.StarSetColoringResult},
) where {T}
    # SMC.decompress_csc! only supports substitution decompression. Mirror
    # SMC.decompress! for a lower-triangular CSC matrix without constructing it.
    S = SMC.sparsity_pattern(result.result)
    rows = SparseArrays.rowvals(S)
    compressed_indices = result.result.compressed_indices
    l = 0
    for j in axes(S, 2)
        for k in SparseArrays.nzrange(S, j)
            if rows[k] >= j
                l += 1
                V[l] = R[compressed_indices[k]]
            end
        end
    end
    return
end
