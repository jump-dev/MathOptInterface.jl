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

function test_runtests_VectorOfVariables()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.BinPackingToMILPBridge,
        """
        variables: x, y, z
        [x, y, z] in BinPacking(3.0, [1.1, 1.9, 2.8])
        x in Interval(1.0, 3.0)
        y >= 2.0
        y <= 3.0
        z == 3.0
        """,
        """
        variables: x, y, z, x1, x2, x3, y2, y3, z3
        1.1 * x1 <= 3.0
        1.1 * x2 + 1.9 * y2 <= 3.0
        1.1 * x3 + 1.9 * y3 + 2.8 * z3 <= 3.0
        1.0 * x + -1.0 * x1 + -2.0 * x2 + -3.0 * x3 == 0.0
        1.0 * y + -2.0 * y2 + -3.0 * y3 == 0.0
        1.0 * z + -3.0 * z3 == 0.0
        1.0 * x1 + 1.0 * x2 + 1.0 * x3 == 1.0
        1.0 * y2 + 1.0 * y3 == 1.0
        1.0 * z3 == 1.0
        x1 in ZeroOne()
        x2 in ZeroOne()
        x3 in ZeroOne()
        y2 in ZeroOne()
        y3 in ZeroOne()
        z3 in ZeroOne()
        x in Interval(1.0, 3.0)
        y >= 2.0
        y <= 3.0
        z == 3.0
        """,
    )
    return
end

function test_runtests_VectorAffineFunction()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.BinPackingToMILPBridge,
        """
        variables: x, y, z
        [1.0 * x + 1.0, 1.0 * y, z] in BinPacking(3.0, [1.1, 1.9, 2.8])
        x in Interval(1.0, 3.0)
        y >= 2.0
        y <= 3.0
        z == 3.0
        """,
        """
        variables: x, y, z, x2, x3, x4, y2, y3, z3
        1.1 * x2 + 1.9 * y2 <= 3.0
        1.1 * x3 + 1.9 * y3 + 2.8 * z3 <= 3.0
        1.1 * x4 <= 3.0
        1.0 * x + -2.0 * x2 + -3.0 * x3 + -4.0 * x4 == -1.0
        1.0 * y + -2.0 * y2 + -3.0 * y3 == 0.0
        1.0 * z + -3.0 * z3 == 0.0
        1.0 * x2 + 1.0 * x3 + 1.0 * x4 == 1.0
        1.0 * y2 + 1.0 * y3 == 1.0
        1.0 * z3 == 1.0
        x2 in ZeroOne()
        x3 in ZeroOne()
        x4 in ZeroOne()
        y2 in ZeroOne()
        y3 in ZeroOne()
        z3 in ZeroOne()
        x in Interval(1.0, 3.0)
        y >= 2.0
        y <= 3.0
        z == 3.0
        """,
    )
    return
end

end  # module

TestConstraintBinPacking.runtests()
