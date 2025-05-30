# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintCountGreaterThan

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
        MOI.Bridges.Constraint.CountGreaterThanToMILPBridge,
        """
        variables: c, y, a, b
        [c, y, a, b] in CountGreaterThan(4)
        a in Interval(1.0, 3.0)
        b in Interval(1.0, 3.0)
        y >= 1.0
        y <= 2.0
        """,
        """
        variables: c, y, a, b, y1, y2, a1, a2, a3, b1, b2, b3
        y + -1.0 * y1 + -2.0 * y2 == 0.0
        y1 + y2 == 1.0
        a + -1.0 * a1 + -2.0 * a2 + -3.0 * a3 == 0.0
        a1 + a2 + a3 == 1.0
        b + -1.0 * b1 + -2.0 * b2 + -3.0 * b3 == 0.0
        b1 + b2 + b3 == 1.0
        c + -1.0 * a1 + -1.0 * b1 + -2.0 * y1 >= -1.0
        c + -1.0 * a2 + -1.0 * b2 + -2.0 * y2 >= -1.0
        y1 in ZeroOne()
        y2 in ZeroOne()
        a1 in ZeroOne()
        a2 in ZeroOne()
        a3 in ZeroOne()
        b1 in ZeroOne()
        b2 in ZeroOne()
        b3 in ZeroOne()
        a in Interval(1.0, 3.0)
        b in Interval(1.0, 3.0)
        y >= 1.0
        y <= 2.0

        """,
    )
    return
end

function test_runtests_VectorAffineFunction()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.CountGreaterThanToMILPBridge,
        """
        variables: z, c, y, a, b
        [2.0, y, a, b] in CountGreaterThan(4)
        a in Interval(1.0, 3.0)
        b in Interval(1.0, 3.0)
        y >= 1.0
        y <= 2.0
        z in ZeroOne()
        """,
        """
        variables: z, c, y, a, b, y1, y2, a1, a2, a3, b1, b2, b3
        y + -1.0 * y1 + -2.0 * y2 == 0.0
        y1 + y2 == 1.0
        a + -1.0 * a1 + -2.0 * a2 + -3.0 * a3 == 0.0
        a1 + a2 + a3 == 1.0
        b + -1.0 * b1 + -2.0 * b2 + -3.0 * b3 == 0.0
        b1 + b2 + b3 == 1.0
        -1.0 * a1 + -1.0 * b1 + -2.0 * y1 >= -3.0
        -1.0 * a2 + -1.0 * b2 + -2.0 * y2 >= -3.0
        y1 in ZeroOne()
        y2 in ZeroOne()
        a1 in ZeroOne()
        a2 in ZeroOne()
        a3 in ZeroOne()
        b1 in ZeroOne()
        b2 in ZeroOne()
        b3 in ZeroOne()
        a in Interval(1.0, 3.0)
        b in Interval(1.0, 3.0)
        y >= 1.0
        y <= 2.0
        z in ZeroOne()
        """,
    )
    return
end

function test_resolve_with_modified()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.CountGreaterThanToMILP{Int}(inner)
    x = MOI.add_variables(model, 3)
    c = MOI.add_constraint.(model, x, MOI.Interval(0, 2))
    MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.CountGreaterThan(3))
    @test MOI.get(inner, MOI.NumberOfVariables()) == 3
    MOI.Bridges.final_touch(model)
    @test MOI.get(inner, MOI.NumberOfVariables()) == 9
    MOI.set(model, MOI.ConstraintSet(), c[3], MOI.Interval(0, 1))
    MOI.Bridges.final_touch(model)
    @test MOI.get(inner, MOI.NumberOfVariables()) == 8
    return
end

function test_runtests_error_variable()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.CountGreaterThanToMILP{Int}(inner)
    x = MOI.add_variables(model, 3)
    f = MOI.VectorOfVariables(x)
    c = MOI.add_constraint(model, f, MOI.CountGreaterThan(3))
    BT = typeof(model.map[c])
    @test_throws(
        MOI.Bridges.BridgeRequiresFiniteDomainError{BT,MOI.VariableIndex},
        MOI.Bridges.final_touch(model),
    )
    return
end

function test_runtests_error_affine()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.CountGreaterThanToMILP{Int}(inner)
    x = MOI.add_variables(model, 2)
    f = MOI.Utilities.operate(vcat, Int, 2, x[1], 1 * x[1], x[2])
    c = MOI.add_constraint(model, f, MOI.CountGreaterThan(3))
    BT = typeof(model.map[c])
    F = MOI.ScalarAffineFunction{Int}
    @test_throws(
        MOI.Bridges.BridgeRequiresFiniteDomainError{BT,F},
        MOI.Bridges.final_touch(model),
    )
    return
end

end  # module

TestConstraintCountGreaterThan.runtests()
