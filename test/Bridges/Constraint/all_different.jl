# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintAllDifferent

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
        MOI.Bridges.Constraint.AllDifferentToCountDistinctBridge,
        """
        variables: x, y, z
        [x, y, z] in AllDifferent(3)
        x in Interval(1.0, 2.0)
        y >= 2.0
        y <= 3.0
        z == 2.0
        """,
        """
        variables: x, y, z, n
        [n, x, y, z] in CountDistinct(4)
        x in Interval(1.0, 2.0)
        y >= 2.0
        y <= 3.0
        z == 2.0
        n == 3.0
        """,
    )
    return
end

end  # module

TestConstraintAllDifferent.runtests()
