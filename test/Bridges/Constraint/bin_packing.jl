# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintBinPacking

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
        MOI.Bridges.Constraint.BinPackingToMILPBridge,
        """
        variables: x, y, z
        [x, y, z] in BinPacking(3.0, [1.0, 2.0, 3.0])
        """,
        """
        variables: x, y, z, a11, a12, a13, a21, a22, a23, a31, a32, a33
        1.0 * a11 + 2.0 * a12 + 3.0 * a13 <= 3.0
        1.0 * a21 + 2.0 * a22 + 3.0 * a23 <= 3.0
        1.0 * a31 + 2.0 * a32 + 3.0 * a33 <= 3.0
        a11 + a21 + a31 == 1.0
        a12 + a22 + a32 == 1.0
        a13 + a23 + a33 == 1.0
        1.0 * a11 + 2.0 * a21 + 3.0 * a31 + -1.0 * x == 0.0
        1.0 * a12 + 2.0 * a22 + 3.0 * a32 + -1.0 * y == 0.0
        1.0 * a13 + 2.0 * a23 + 3.0 * a33 + -1.0 * z == 0.0
        a11 in ZeroOne()
        a12 in ZeroOne()
        a13 in ZeroOne()
        a21 in ZeroOne()
        a22 in ZeroOne()
        a23 in ZeroOne()
        a31 in ZeroOne()
        a32 in ZeroOne()
        a33 in ZeroOne()
        """,
    )
    return
end

end  # module

TestConstraintBinPacking.runtests()
