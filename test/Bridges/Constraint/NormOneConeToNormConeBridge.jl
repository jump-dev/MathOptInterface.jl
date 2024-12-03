# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintNormSpecialCase

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

function test_runtests_norm_1()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.NormOneConeToNormConeBridge,
        """
        variables: t, x1, x2, x3, x4
        [t, x1, x2, x3, x4] in NormOneCone(5)
        """,
        """
        variables: t, x1, x2, x3, x4
        [t, x1, x2, x3, x4] in NormCone(1.0, 5)
        """,
    )
    return
end

function test_runtests_norm_2()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SecondOrderConeToNormConeBridge,
        """
        variables: t, x1, x2, x3, x4
        [t, x1, x2, x3, x4] in SecondOrderCone(5)
        """,
        """
        variables: t, x1, x2, x3, x4
        [t, x1, x2, x3, x4] in NormCone(2.0, 5)
        """,
    )
    return
end

function test_runtests_norm_inf()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.NormInfinityConeToNormConeBridge,
        """
        variables: t, x1, x2, x3, x4
        [t, x1, x2, x3, x4] in NormInfinityCone(5)
        """,
        """
        variables: t, x1, x2, x3, x4
        [t, x1, x2, x3, x4] in NormCone(Inf, 5)
        """,
    )
    return
end
end  # module

TestConstraintNormSpecialCase.runtests()
