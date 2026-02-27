# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module MathOptInterfaceLDLFactorizationsExt

import LDLFactorizations
import LinearAlgebra
import MathOptInterface as MOI
import SparseArrays

# The type signature of this function is not important, so long as it is more
# specific than the (untyped) generic fallback with the error pointing to
# LDLFactorizations.jl
function MOI.Bridges.Constraint.compute_sparse_sqrt_fallback(
    Q::AbstractMatrix,
    ::F,
    ::S,
) where {F<:MOI.ScalarQuadraticFunction,S<:MOI.AbstractSet}
    n = LinearAlgebra.checksquare(Q)
    factor = LDLFactorizations.ldl(Q)
    if !LDLFactorizations.factorized(factor) || minimum(factor.D) < 0
        msg = """
        Unable to transform a quadratic constraint into a SecondOrderCone
        constraint because the quadratic constraint is not convex.
        """
        throw(MOI.UnsupportedConstraint{F,S}(msg))
    end
    # We have Q = P' * L * D * L' * P. We want to find Q = U' * U, so
    # U = sqrt(D) * L' * P. First, compute L'. Note I and J are reversed:
    J, I, V = SparseArrays.findnz(factor.L)
    # Except L doesn't include the identity along the diagonal. Add it back.
    append!(J, 1:n)
    append!(I, 1:n)
    append!(V, ones(n))
    # Now scale by sqrt(D)
    for (k, i) in enumerate(I)
        V[k] *= sqrt(factor.D[i, i])
    end
    # Finally, permute the columns of L'. The rows stay in the same order.
    return I, factor.P[J], V
end

end  # module
