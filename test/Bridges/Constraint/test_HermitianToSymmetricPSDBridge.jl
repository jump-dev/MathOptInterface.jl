# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintHermitianToSymmetricPSD

using Test

import MathOptInterface as MOI
import LinearAlgebra

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

function test_dimension_2()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.HermitianToSymmetricPSDBridge,
        """
        variables: a, b, c
        [1.0 * a + 2.0 * b, 3.0 * c, 4.0 * b, 5.0 * a] in HermitianPositiveSemidefiniteConeTriangle(2)
        """,
        """
        variables: a, b, c
        [1.0 * a + 2.0 * b, 3.0 * c, 4.0 * b, 0.0, -5.0 * a, 1.0 * a + 2.0 * b, 5.0 * a, 0.0, 3.0 * c, 4.0 * b] in PositiveSemidefiniteConeTriangle(4)
        """,
    )
    return
end

function test_dimension_3()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.HermitianToSymmetricPSDBridge,
        """
        variables: x11, x12, x22, x13, x23, x33, y12, y13, y23
        [x11, x12, x22, x13, x23, x33, y12, y13, y23] in HermitianPositiveSemidefiniteConeTriangle(3)
        """,
        """
        variables: x11, x12, x22, x13, x23, x33, y12, y13, y23
        [x11, x12, x22, x13, x23, x33, 0, -1 * y12, -1 * y13, x11, y12, 0, -1 * y23, x12, x22, y13, y23, 0, x13, x23, x33] in PositiveSemidefiniteConeTriangle(6)
        """,
    )
    return
end

function test_dimension_4()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.HermitianToSymmetricPSDBridge,
        """
        variables: x11, x12, x22, x13, x23, x33, x14, x24, x34, x44, y12, y13, y23, y14, y24, y34
        [x11, x12, x22, x13, x23, x33, x14, x24, x34, x44, y12, y13, y23, y14, y24, y34] in HermitianPositiveSemidefiniteConeTriangle(4)
        """,
        """
        variables: x11, x12, x22, x13, x23, x33, x14, x24, x34, x44, y12, y13, y23, y14, y24, y34
        [x11, x12, x22, x13, x23, x33, x14, x24, x34, x44, 0, -1 * y12, -1 * y13, -1 * y14, x11, y12, 0, -1 * y23, -1 * y24, x12, x22, y13, y23, 0, -1 * y34, x13, x23, x33, y14, y24, y34, 0, x14, x24, x34, x44] in PositiveSemidefiniteConeTriangle(8)
        """,
    )
    return
end

function test_inverse_adjoint_preserves_psd()
    BT = MOI.Bridges.Constraint.HermitianToSymmetricPSDBridge{Float64}
    for n in 1:4
        v = ComplexF64[complex(i, n - i) for i in 1:n]
        H = v * v'
        h = vcat(
            [real(H[i, j]) for j in 1:n for i in 1:j],
            [imag(H[i, j]) for j in 1:n for i in 1:(j-1)],
        )
        r = MOI.Bridges.inverse_adjoint_map_function(BT, h)
        R = [r[MOI.Utilities.trimap(i, j)] for i in 1:2n, j in 1:2n]
        @test R ≈ [real(H) imag(H); -imag(H) real(H)] / 2
        @test minimum(LinearAlgebra.eigvals(LinearAlgebra.Symmetric(R))) >=
              -1e-12
        @test MOI.Bridges.adjoint_map_function(BT, r) ≈ h
        # Check the adjoint using the cone inner products, including the
        # factors of two for off-diagonal entries in triangle storage.
        y = collect(1.0:length(r))
        @test MOI.Utilities.set_dot(
            MOI.Bridges.map_function(BT, h),
            y,
            MOI.PositiveSemidefiniteConeTriangle(2n),
        ) ≈ MOI.Utilities.set_dot(
            h,
            MOI.Bridges.adjoint_map_function(BT, y),
            MOI.HermitianPositiveSemidefiniteConeTriangle(n),
        )
    end
    return
end

end  # module

TestConstraintHermitianToSymmetricPSD.runtests()
