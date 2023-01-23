# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestObjectiveVectorSlack

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
        MOI.Bridges.Objective.VectorSlackBridge,
        """
        variables: x
        minobjective: [1.1 * x + 2.2]
        """,
        """
        variables: x, y
        minobjective: [y]
        [1.0 * y + -1.1 * x + -2.2] in Nonnegatives(1)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.VectorSlackBridge,
        """
        variables: x
        maxobjective: [1.1 * x + 2.2]
        """,
        """
        variables: x, y
        maxobjective: [y]
        [-1.0 * y + 1.1 * x + 2.2] in Nonnegatives(1)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.VectorSlackBridge,
        """
        variables: x
        minobjective: [1.1 * x + 2.2, -1.0 * x]
        """,
        """
        variables: x, y, z
        minobjective: [y, z]
        [1.0 * y + -1.1 * x + -2.2, 1.0 * z + 1.0 * x] in Nonnegatives(2)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.VectorSlackBridge,
        """
        variables: x
        maxobjective: [1.1 * x + 2.2, -1.0 * x]
        """,
        """
        variables: x, y, z
        maxobjective: [y, z]
        [-1.0 * y + 1.1 * x + 2.2, -1.0 * z + -1.0 * x] in Nonnegatives(2)
        """,
    )
    return
end

end  # module

TestObjectiveVectorSlack.runtests()
