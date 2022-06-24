# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintCountDistinct

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
        MOI.Bridges.Constraint.CountDistinctToMILPBridge,
        """
        variables: n, x, y
        [n, x, y] in CountDistinct(3)
        x in Interval(1.0, 2.0)
        y >= 2.0
        y <= 3.0
        """,
        """
        variables: n, x, y, z_x1, z_x2, z_y2, z_y3, a_1, a_2, a_3
        1.0 * x + -1.0 * z_x1 + -2.0 * z_x2 == 0.0
        1.0 * y + -2.0 * z_y2 + -3.0 * z_y3 == 0.0
        -1.0 * n + a_1 + a_2 + a_3 == 0.0
        z_x1 + -1.0 * a_1 <= 0.0
        z_x2 + z_y2 + -2.0 * a_2 <= 0.0
        z_y3 + -1.0 * a_3 <= 0.0
        x in Interval(1.0, 2.0)
        y >= 2.0
        y <= 3.0
        z_x1 in ZeroOne()
        z_x2 in ZeroOne()
        z_y2 in ZeroOne()
        z_y3 in ZeroOne()
        a_1 in ZeroOne()
        a_2 in ZeroOne()
        a_3 in ZeroOne()
        """,
    )
    return
end

function test_runtests_err()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.CountDistinctToMILP{Int}(inner)
    x = MOI.add_variables(model, 3)
    MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.CountDistinct(3))
    @test_throws(
        ErrorException(
            "Unable to use CountDistinctToMILPBridge because variable $(x[2]) " *
            "has a non-finite domain.",
        ),
        MOI.Utilities.final_touch(model),
    )
    return
end

end  # module

TestConstraintCountDistinct.runtests()
