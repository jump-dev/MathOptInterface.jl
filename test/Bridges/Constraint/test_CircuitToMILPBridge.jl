# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintCircuit

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

function test_runtests_VectorOfVariables()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.CircuitToMILPBridge,
        """
        variables: x_bin, x_int, a, b, c
        [a, b, c] in Circuit(3)
        x_bin in ZeroOne()
        x_int in Integer()
        """,
        """
        variables: x_bin, x_int, a, b, c, z11, z21, z31, z12, z22, z32, z13, z23, z33, u2, u3
        1.0 * a + -2.0 * z12 + -3.0 * z13 == 0.0
        1.0 * b + -1.0 * z21 + -3.0 * z23 == 0.0
        1.0 * c + -1.0 * z31 + -2.0 * z32 == 0.0
        z12 + z13 == 1.0
        z21 + z23 == 1.0
        z31 + z32 == 1.0
        z21 + z31 == 1.0
        z12 + z32 == 1.0
        z13 + z23 == 1.0
        1.0 * u2 + -1.0 * u3 + 2.0 * z23 <= 1.0
        1.0 * u3 + -1.0 * u2 + 2.0 * z32 <= 1.0
        z11 in ZeroOne()
        z12 in ZeroOne()
        z13 in ZeroOne()
        z21 in ZeroOne()
        z22 in ZeroOne()
        z23 in ZeroOne()
        z31 in ZeroOne()
        z32 in ZeroOne()
        z33 in ZeroOne()
        u2 in Integer()
        u3 in Integer()
        u2 in Interval(1.0, 2.0)
        u3 in Interval(1.0, 2.0)
        x_bin in ZeroOne()
        x_int in Integer()
        """,
    )
    return
end

function test_runtests_VectorAffineFunction()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.CircuitToMILPBridge,
        """
        variables: b, c
        [2.0, b, c] in Circuit(3)
        """,
        """
        variables: b, c, z11, z21, z31, z12, z22, z32, z13, z23, z33, u2, u3
        -2.0 * z12 + -3.0 * z13 == -2.0
        1.0 * b + -1.0 * z21 + -3.0 * z23 == 0.0
        1.0 * c + -1.0 * z31 + -2.0 * z32 == 0.0
        z12 + z13 == 1.0
        z21 + z23 == 1.0
        z31 + z32 == 1.0
        z21 + z31 == 1.0
        z12 + z32 == 1.0
        z13 + z23 == 1.0
        1.0 * u2 + -1.0 * u3 + 2.0 * z23 <= 1.0
        1.0 * u3 + -1.0 * u2 + 2.0 * z32 <= 1.0
        z11 in ZeroOne()
        z12 in ZeroOne()
        z13 in ZeroOne()
        z21 in ZeroOne()
        z22 in ZeroOne()
        z23 in ZeroOne()
        z31 in ZeroOne()
        z32 in ZeroOne()
        z33 in ZeroOne()
        u2 in Integer()
        u3 in Integer()
        u2 in Interval(1.0, 2.0)
        u3 in Interval(1.0, 2.0)
        """,
    )
    return
end

end  # module

TestConstraintCircuit.runtests()
