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

MOI.Utilities.is_defined(::MOI.Utilities.CliqueTreesExt) = true

function MOI.Utilities.compute_sparse_sqrt(
    ::MOI.Utilities.CliqueTreesExt,
    Q::AbstractMatrix,
)
    G = LinearAlgebra.cholesky!(
        CliqueTrees.Multifrontal.ChordalCholesky(Q),
        LinearAlgebra.RowMaximum(),
    )
    U = SparseArrays.sparse(G.U) * G.P
    # Verify the factorization reconstructs Q. This catches indefinite
    # matrices where the diagonal is all zeros (e.g., [0 -1; -1 0]).
    if !isapprox(Q, U' * U; atol = 1e-10)
        return nothing
    end
    return SparseArrays.findnz(U)
end

end  # module
