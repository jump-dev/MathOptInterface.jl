# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintIntegerToZeroOne

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
        MOI.Bridges.Constraint.IntegerToZeroOneBridge,
        """
        variables: x, z
        x in Integer()
        x in Interval(1.0, 3.0)
        z in ZeroOne()
        """,
        """
        variables: x, z, y1, y2
        y1 in ZeroOne()
        y2 in ZeroOne()
        x + -1.0 * y1 + -2.0 * y2 == 1.0
        x in Interval(1.0, 3.0)
        z in ZeroOne()
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.IntegerToZeroOneBridge,
        """
        variables: x
        x in Integer()
        x in Interval(-1.0, 2.0)
        """,
        """
        variables: x, y1, y2
        y1 in ZeroOne()
        y2 in ZeroOne()
        x + -1.0 * y1 + -2.0 * y2 == -1.0
        x in Interval(-1.0, 2.0)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.IntegerToZeroOneBridge,
        """
        variables: x
        x in Integer()
        x in Interval(-2.0, 2.0)
        """,
        """
        variables: x, y1, y2, y3
        y1 in ZeroOne()
        y2 in ZeroOne()
        y3 in ZeroOne()
        x + -1.0 * y1 + -2.0 * y2 + -4.0 * y3 == -2.0
        x in Interval(-2.0, 2.0)
        """,
    )
    return
end

function test_finite_domain_error()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.IntegerToZeroOne{Int}(inner)
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    @test_throws(
        MOI.Bridges.BridgeRequiresFiniteDomainError,
        MOI.Bridges.final_touch(model),
    )
    return
end

function test_final_touch_twice()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.IntegerToZeroOne{Int}(inner)
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.Interval(1, 3))
    MOI.Bridges.final_touch(model)
    MOI.Bridges.final_touch(model)
    return
end

end  # module

TestConstraintIntegerToZeroOne.runtests()
