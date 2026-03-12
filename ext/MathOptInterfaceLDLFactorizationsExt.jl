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

MOI.Utilities.is_defined(::MOI.Utilities.LDLFactorizationsExt) = true

function MOI.Utilities.compute_sparse_sqrt(
    ::MOI.Utilities.LDLFactorizationsExt,
    Q::AbstractMatrix,
)
    n = LinearAlgebra.checksquare(Q)
    factor = LDLFactorizations.ldl(Q)
    if !LDLFactorizations.factorized(factor) || minimum(factor.D) < 0
        return nothing
    end
    L = sqrt.(factor.D) * LinearAlgebra.UnitLowerTriangular(factor.L)
    J, I, V = SparseArrays.findnz(SparseArrays.sparse(L))
    return I, factor.P[J], V
end

end  # module
