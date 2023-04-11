# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestObjectiveVectorFunctionize

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

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.VectorFunctionizeBridge,
        """
        variables: x, y
        minobjective: [x, y]
        """,
        """
        variables: x, y
        minobjective: [1.0 * x + 0.0, 1.0 * y + 0.0]
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.VectorFunctionizeBridge,
        """
        variables: x, y
        maxobjective: [x, y]
        """,
        """
        variables: x, y
        maxobjective: [1.0 * x + 0.0, 1.0 * y + 0.0]
        """,
    )
    return
end

end  # module

TestObjectiveVectorFunctionize.runtests()
