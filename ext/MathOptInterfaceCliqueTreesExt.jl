# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module MathOptInterfaceCliqueTreesExt

import CliqueTrees
import LinearAlgebra
import MathOptInterface as MOI
import SparseArrays

MOI.Bridges.Constraint.is_defined(::MOI.Bridges.Constraint._CliqueTrees) = true

function MOI.Bridges.Constraint._compute_sparse_sqrt(
    ::MOI.Bridges.Constraint._CliqueTrees,
    Q::AbstractMatrix,
)
    G = LinearAlgebra.cholesky!(
        CliqueTrees.Multifrontal.ChordalCholesky(Q),
        LinearAlgebra.RowMaximum(),
    )
    U = SparseArrays.sparse(G.U) * G.P
    # Verify the factorization reconstructs Q. We don't have something like
    # LinearAlgebra.issuccess(G)
    if !isapprox(Q, U' * U; atol = 1e-10)
        return nothing
    end
    return SparseArrays.findnz(U)
end

end  # module
