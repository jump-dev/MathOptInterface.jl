# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintHermitianToSymmetricPSD

using Test

import MathOptInterface as MOI

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

end  # module

TestConstraintHermitianToSymmetricPSD.runtests()
