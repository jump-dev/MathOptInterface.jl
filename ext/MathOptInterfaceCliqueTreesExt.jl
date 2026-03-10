# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module MathOptInterfaceCliqueTreesExt

import CliqueTrees.Multifrontal: ChordalCholesky
import LinearAlgebra: RowMaximum, cholesky!
import MathOptInterface as MOI
import SparseArrays: findnz, sparse

function MOI.Bridges.Constraint.compute_sparse_sqrt_fallback(
    Q::AbstractMatrix,
    ::F,
    ::S,
) where {F<:MOI.ScalarQuadraticFunction,S<:MOI.AbstractSet}
    G = cholesky!(ChordalCholesky(Q), RowMaximum())
    U = sparse(G.U) * G.P

    # Verify the factorization reconstructs Q. This catches indefinite
    # matrices where the diagonal is all zeros (e.g., [0 -1; -1 0]).
    if !isapprox(Q, U' * U; atol = 1e-10)
        msg = """
        Unable to transform a quadratic constraint into a SecondOrderCone
        constraint because the quadratic constraint is not convex.
        """
        throw(MOI.UnsupportedConstraint{F,S}(msg))
    end

    return findnz(U)
end

end  # module
