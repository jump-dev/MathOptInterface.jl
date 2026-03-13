# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

abstract type AbstractExt end

is_defined(::AbstractExt) = false

struct LDLFactorizationsExt <: AbstractExt end

struct CliqueTreesExt <: AbstractExt end

struct LinearAlgebraExt <: AbstractExt end

is_defined(::LinearAlgebraExt) = true

function compute_sparse_sqrt(::LinearAlgebraExt, Q::AbstractMatrix)
    factor = LinearAlgebra.cholesky(Q; check = false)
    if !LinearAlgebra.issuccess(factor)
        return nothing
    end
    L, p = SparseArrays.sparse(factor.L), factor.p
    # We have Q = P' * L * L' * P. We want to find Q = U' * U, so U = L' * P
    # First, compute L'. Note I and J are reversed
    J, I, V = SparseArrays.findnz(L)
    # Then, we want to permute the columns of L'. The rows stay in the same
    # order.
    return I, p[J], V
end

"""
    compute_sparse_sqrt(Q::AbstractMatrix)

Attempts to compute a sparse square root such that `Q = A' * A`.

## Return value

If successful, this function returns an `(I, J, V)` triplet of the sparse `A`
matrix.

If unsuccessful, this function returns `nothing`.

## Extensions

By default, this function attempts to use a Cholesky decomposition. If that
fails, it may optionally use various extension packages.

These extension packages must be loaded before calling `compute_sparse_sqrt`.

The extensions currently supported are:

 * The LDL routine in `LDLFactorizations.jl`
 * The pivoted Cholesky in `CliqueTrees.jl`
"""
function compute_sparse_sqrt(Q::AbstractMatrix)
    # There's a big try-catch here because Cholesky can fail even if
    # `check = false`. The try-catch isn't a performance concern because the
    # alternative is not being able to reformulate the problem.
    for ext in (LinearAlgebraExt(), LDLFactorizationsExt(), CliqueTreesExt())
        if is_defined(ext)
            try
                if (ret = compute_sparse_sqrt(ext, Q)) !== nothing
                    return ret
                end
            catch
            end
        end
    end
    return nothing
end
