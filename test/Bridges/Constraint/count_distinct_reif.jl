# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintReifiedCountDistinct

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
        MOI.Bridges.Constraint.ReifiedCountDistinctToMILPBridge,
        """
        variables: r, n, x, y
        [r, n, x, y] in Reified(CountDistinct(3))
        x in Interval(1.0, 2.0)
        y >= 2.0
        y <= 3.0
        r in ZeroOne()
        """,
        """
        variables: r, n, x, y, z_x1, z_x2, z_y2, z_y3, a_1, a_2, a_3, d
        1.0 * x + -1.0 * z_x1 + -2.0 * z_x2 == 0.0
        z_x1 + z_x2 == 1.0
        1.0 * y + -2.0 * z_y2 + -3.0 * z_y3 == 0.0
        z_y2 + z_y3 == 1.0
        -1.0 * n + a_1 + a_2 + a_3 + -1.0 * d == 0.0
        z_x1 + -1.0 * a_1 <= 0.0
        -1.0 * z_x1 + a_1 <= 0.0
        z_x2 + z_y2 + -2.0 * a_2 <= 0.0
        -1.0 * z_x2 + -1.0 * z_y2 + a_2 <= 0.0
        z_y3 + -1.0 * a_3 <= 0.0
        a_3 + -1.0 * z_y3 <= 0.0
        2.0 * r + 1.0 * d <= 2.0
        2.0 * r + -1.0 * d <= 2.0
        x in Interval(1.0, 2.0)
        y >= 2.0
        y <= 3.0
        r in ZeroOne()
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

function test_runtests_VectorAffineFunction()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ReifiedCountDistinctToMILPBridge,
        """
        variables: r, x, y
        [r, 2.0, 2.0 * x + -1.0, y] in Reified(CountDistinct(3))
        x in Interval(1.0, 2.0)
        y >= 2.0
        y <= 3.0
        r in ZeroOne()
        """,
        """
        variables: r, x, y, z_x1, z_x2, z_x3, z_y2, z_y3, a_1, a_2, a_3, d
        2.0 * x + -1.0 * z_x1 + -2.0 * z_x2 + -3.0 * z_x3 == 1.0
        1.0 * y + -2.0 * z_y2 + -3.0 * z_y3 == 0.0
        a_1 + a_2 + a_3 + -1.0 * d == 2.0
        z_x1 + z_x2 + z_x3 == 1.0
        z_y2 + z_y3 == 1.0
        z_x1 + -1.0 * a_1 <= 0.0
        z_x2 + z_y2 + -2.0 * a_2 <= 0.0
        z_x3 + z_y3 + -2.0 * a_3 <= 0.0
        a_1 + -1.0 * z_x1 <= 0.0
        a_2 + -1.0 * z_x2 + -1.0 * z_y2 <= 0.0
        a_3 + -1.0 * z_x3 + -1.0 * z_y3 <= 0.0
        2.0 * r + 1.0 * d <= 2.0
        2.0 * r + -1.0 * d <= 2.0
        x in Interval(1.0, 2.0)
        y >= 2.0
        y <= 3.0
        r in ZeroOne()
        z_x1 in ZeroOne()
        z_x2 in ZeroOne()
        z_x3 in ZeroOne()
        z_y2 in ZeroOne()
        z_y3 in ZeroOne()
        a_1 in ZeroOne()
        a_2 in ZeroOne()
        a_3 in ZeroOne()
        """,
    )
    return
end

function test_runtests_error_variable()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Int}())
    model = MOI.Bridges.Constraint.ReifiedCountDistinctToMILP{Int}(inner)
    x = MOI.add_variables(model, 4)
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.Reified(MOI.CountDistinct(3)),
    )
    @test_throws(
        ErrorException(
            "Unable to use ReifiedCountDistinctToMILPBridge because element " *
            "3 in the function has a non-finite domain: $(x[3])",
        ),
        MOI.Bridges.final_touch(model),
    )
    return
end

function test_runtests_error_affine()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Int}())
    model = MOI.Bridges.Constraint.ReifiedCountDistinctToMILP{Int}(inner)
    x = MOI.add_variables(model, 3)
    f = MOI.Utilities.operate(vcat, Int, x[1], 1, x[2], x[3])
    MOI.add_constraint(model, f, MOI.Reified(MOI.CountDistinct(3)))
    @test_throws(
        ErrorException(
            "Unable to use ReifiedCountDistinctToMILPBridge because element " *
            "3 in the function has a non-finite domain: $(1 * x[2])",
        ),
        MOI.Bridges.final_touch(model),
    )
    return
end

end  # module

TestConstraintReifiedCountDistinct.runtests()
