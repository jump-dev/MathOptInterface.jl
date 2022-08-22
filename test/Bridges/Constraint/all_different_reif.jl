# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintReifiedAllDifferent

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
        MOI.Bridges.Constraint.ReifiedAllDifferentToCountDistinctBridge,
        """
        variables: r, x, y, z
        [r, x, y, z] in Reified(AllDifferent(3))
        x in Interval(1.0, 2.0)
        y >= 2.0
        y <= 3.0
        z == 2.0
        r in ZeroOne()
        """,
        """
        variables: r, x, y, z, n
        [r, n, x, y, z] in Reified(CountDistinct(4))
        x in Interval(1.0, 2.0)
        y >= 2.0
        y <= 3.0
        z == 2.0
        n == 3.0
        r in ZeroOne()
        """,
    )
    return
end

function test_runtests_VectorAffineFunction()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ReifiedAllDifferentToCountDistinctBridge,
        """
        variables: r, x, y, z
        [r, 2.0 * x + -1.0, y, z] in Reified(AllDifferent(3))
        x in Interval(1.0, 2.0)
        y >= 2.0
        y <= 3.0
        z == 2.0
        r in ZeroOne()
        """,
        """
        variables: r, x, y, z
        [r, 3.0, 2.0 * x + -1.0, y, z] in Reified(CountDistinct(4))
        x in Interval(1.0, 2.0)
        y >= 2.0
        y <= 3.0
        z == 2.0
        r in ZeroOne()
        """,
    )
    return
end

end  # module

TestConstraintReifiedAllDifferent.runtests()
