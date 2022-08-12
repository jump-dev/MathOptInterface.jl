# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintHermitianToSymmetricPSD

using Test

using MathOptInterface
const MOI = MathOptInterface

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

function test_runtests()
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

end  # module

TestConstraintHermitianToSymmetricPSD.runtests()
