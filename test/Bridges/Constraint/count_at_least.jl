# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintCountAtLeast

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
        MOI.Bridges.Constraint.CountAtLeastToCountBelongsBridge,
        """
        variables: a, b, c
        [a, b, b, c] in CountAtLeast(1, [2, 2], Set([3]))
        a in Interval(1.0, 3.0)
        b in Interval(1.0, 3.0)
        c >= 1.0
        c <= 3.0
        """,
        """
        variables: a, b, c, y1, y2
        [y1, a, b] in CountBelongs(3, Set([3]))
        [y2, b, c] in CountBelongs(3, Set([3]))
        y1 >= 1.0
        y2 >= 1.0
        a in Interval(1.0, 3.0)
        b in Interval(1.0, 3.0)
        c >= 1.0
        c <= 3.0
        """,
    )
    return
end

function test_runtests_VectorAffineFunction()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.CountAtLeastToCountBelongsBridge,
        """
        variables: a, b, c
        [1.0 * a, b + 1.0, b, c] in CountAtLeast(1, [2, 2], Set([3]))
        a in Interval(1.0, 3.0)
        b in Interval(1.0, 3.0)
        c in Interval(1.0, 3.0)
        """,
        """
        variables: a, b, c, y1, y2
        [y1, 1.0 * a, b + 1.0] in CountBelongs(3, Set([3]))
        [y2, 1.0 * b, 1.0 * c] in CountBelongs(3, Set([3]))
        y1 >= 1.0
        y2 >= 1.0
        a in Interval(1.0, 3.0)
        b in Interval(1.0, 3.0)
        c in Interval(1.0, 3.0)
        """,
    )
    return
end

end  # module

TestConstraintCountAtLeast.runtests()
