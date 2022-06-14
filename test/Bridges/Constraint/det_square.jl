# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintDetSquare

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

function test_runtests_symmetric_logdet()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.DetSquareBridge,
        """
        variables: t, u, x11, x21, x22
        [t, u, x11, x21, x21, x22] in LogDetConeSquare(2)
        """,
        """
        variables: t, u, x11, x21, x22
        [t, u, x11, x21, x22] in LogDetConeTriangle(2)
        """,
    )
    return
end

function test_runtests_non_symmetric_logdet()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.DetSquareBridge,
        """
        variables: t, u, x11, x12, x21, x22
        [t, u, x11, x21, x12, x22] in LogDetConeSquare(2)
        """,
        """
        variables: t, u, x11, x12, x21, x22
        [t, u, x11, x12, x22] in LogDetConeTriangle(2)
        1.0 * x12 + -1.0 * x21 == 0.0
        """,
    )
    return
end

function test_runtests_symmetric_rootdet()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.DetSquareBridge,
        """
        variables: t, x11, x21, x22
        [t, x11, x21, x21, x22] in RootDetConeSquare(2)
        """,
        """
        variables: t, x11, x21, x22
        [t, x11, x21, x22] in RootDetConeTriangle(2)
        """,
    )
    return
end

function test_runtests_non_symmetric_rootdet()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.DetSquareBridge,
        """
        variables: t, x11, x12, x21, x22
        [t, x11, x21, x12, x22] in RootDetConeSquare(2)
        """,
        """
        variables: t, x11, x12, x21, x22
        [t, x11, x12, x22] in RootDetConeTriangle(2)
        x12 + -1.0 * x21 == 0.0
        """,
    )
    return
end

end  # module

TestConstraintDetSquare.runtests()
