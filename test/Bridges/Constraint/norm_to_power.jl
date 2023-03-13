# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintNormToPower

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

function test_runtests_dimension_5()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.NormToPowerBridge,
        """
        variables: t, x1, x2, x3, x4
        [t, x1, x2, x3, x4] in NormCone(4, 5)
        """,
        """
        variables: t, x1, x2, x3, x4, r1, r2, r3, r4
        [r1, t, x1] in PowerCone(0.25)
        [r2, t, x2] in PowerCone(0.25)
        [r3, t, x3] in PowerCone(0.25)
        [r4, t, x4] in PowerCone(0.25)
        r1 + r2 + r3 + r4 + -1.0 * t == 0.0
        """,
    )
    return
end

function test_runtests_dimension_4()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.NormToPowerBridge,
        """
        variables: t, x1, x2, x3
        [t, 1.0 * x1, 2.0 * x2, x3] in NormCone(4, 4)
        """,
        """
        variables: t, x1, x2, x3, r1, r2, r3
        [r1, t, 1.0 * x1] in PowerCone(0.25)
        [r2, t, 2.0 * x2] in PowerCone(0.25)
        [r3, t, 1.0 * x3] in PowerCone(0.25)
        r1 + r2 + r3 + -1.0 * t == 0.0
        """,
    )
    return
end

function test_runtests_dimension_2()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.NormToPowerBridge,
        """
        variables: t, x1
        [t, x1] in NormCone(2, 2)
        """,
        """
        variables: t, x1, r1
        [r1, t, x1] in PowerCone(0.5)
        r1 + -1.0 * t == 0.0
        """,
    )
    return
end

end  # module

TestConstraintNormToPower.runtests()
