# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintFixParametricVariables

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

function test_runtests_x_fixed()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.FixParametricVariablesBridge,
        """
        variables: x, y
        c: 1.0 * x * y + x + y >= 1.0
        x == 2.0
        """,
        """
        variables: x, y
        c: 3.0 * y + x >= 1.0
        x == 2.0
        """,
    )
    return
end

function test_runtests_y_fixed()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.FixParametricVariablesBridge,
        """
        variables: x, y
        c: 1.0 * x * y + x + y >= 1.0
        y == 2.0
        """,
        """
        variables: x, y
        c: 3.0 * x + y >= 1.0
        y == 2.0
        """,
    )
    return
end

function test_runtests_both_fixed()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.FixParametricVariablesBridge,
        """
        variables: x, y
        c: 1.0 * x * y + x + y >= 1.0
        x == 3.0
        y == 2.0
        """,
        """
        variables: x, y
        c: 4.0 * y + x >= 1.0
        x == 3.0
        y == 2.0
        """,
    )
    return
end

function test_runtests_duplicates()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.FixParametricVariablesBridge,
        """
        variables: x, y
        c: 1.0 * x * y + 2.0 * x * y + x + y + x >= 1.0
        x == 3.0
        """,
        """
        variables: x, y
        c: 10.0 * y + 2.0 * x >= 1.0
        x == 3.0
        """,
    )
    return
end

end  # module

TestConstraintFixParametricVariables.runtests()
