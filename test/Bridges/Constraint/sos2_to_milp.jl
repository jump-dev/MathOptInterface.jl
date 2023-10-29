# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintSOS2ToMILP

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

function test_runtests_VectorOfVariables()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SOS2ToMILPBridge,
        """
        variables: x, y, z
        [x, y, z] in SOS2([0.5, 1.0, 0.75])
        x in Interval(0.0, 2.0)
        y >= -1.0
        y <= 3.0
        z in ZeroOne()
        """,
        """
        variables: x, y, z, a1, a2
        x in Interval(0.0, 2.0)
        y >= -1.0
        y <= 3.0
        z in ZeroOne()
        a1 in ZeroOne()
        a2 in ZeroOne()
        1.0 * a1 + 1.0 * a2 == 1.0
        -1.0 * x <= 0.0
        1.0 * x + -2.0 * a1 <= 0.0
        -1.0 * z <= 0.0
        1.0 * z + -1.0 * a1 + -1.0 * a2 <= 0.0
        -1.0 * y + -1.0 * a2 <= 0.0
        1.0 * y + -3.0 * a2 <= 0.0
        """,
    )
    return
end

function test_runtests_VectorOfVariables()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SOS2ToMILPBridge,
        """
        variables: x, y, z
        [1.0 * x + 1.0, y, 2.0 * z] in SOS2([0.5, 1.0, 0.75])
        x in Interval(0.0, 2.0)
        y >= -1.0
        y <= 3.0
        z in ZeroOne()
        """,
        """
        variables: x, y, z, a1, a2
        x in Interval(0.0, 2.0)
        y >= -1.0
        y <= 3.0
        z in ZeroOne()
        a1 in ZeroOne()
        a2 in ZeroOne()
        1.0 * a1 + 1.0 * a2 == 1.0
        -1.0 * x + 1.0 * a1 <= 1.0
        1.0 * x + -3.0 * a1 <= -1.0
        -2.0 * z <= 0.0
        2.0 * z + -2.0 * a1 + -2.0 * a2 <= 0.0
        -1.0 * y + -1.0 * a2 <= 0.0
        1.0 * y + -3.0 * a2 <= 0.0
        """,
    )
    return
end

function test_resolve_with_modified()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.SOS2ToMILP{Int}(inner)
    x = MOI.add_variables(model, 3)
    c = MOI.add_constraint.(model, x, MOI.Interval(0, 2))
    MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.SOS2([1, 2, 3]))
    @test MOI.get(inner, MOI.NumberOfVariables()) == 3
    MOI.Bridges.final_touch(model)
    @test MOI.get(inner, MOI.NumberOfVariables()) == 5
    MOI.set(model, MOI.ConstraintSet(), c[3], MOI.Interval(0, 1))
    MOI.Bridges.final_touch(model)
    @test MOI.get(inner, MOI.NumberOfVariables()) == 5
    return
end

function test_runtests_error_variable()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.SOS2ToMILP{Int}(inner)
    x = MOI.add_variables(model, 3)
    MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.SOS2([1, 2, 3]))
    @test_throws(
        ErrorException(
            "Unable to use SOS2ToMILPBridge because element 1 in " *
            "the function has a non-finite domain: $(x[1])",
        ),
        MOI.Bridges.final_touch(model),
    )
    return
end

function test_runtests_error_affine()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.SOS2ToMILP{Int}(inner)
    x = MOI.add_variables(model, 2)
    f = MOI.Utilities.operate(vcat, Int, 2, 1 * x[1], x[2])
    MOI.add_constraint(model, f, MOI.SOS2([1, 2, 3]))
    @test_throws(
        ErrorException(
            "Unable to use SOS2ToMILPBridge because element 2 in " *
            "the function has a non-finite domain: $(1 * x[1])",
        ),
        MOI.Bridges.final_touch(model),
    )
    return
end

end  # module

TestConstraintSOS2ToMILP.runtests()
