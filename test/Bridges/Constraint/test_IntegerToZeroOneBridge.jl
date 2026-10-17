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

function test_delete_before_final_touch()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.IntegerToZeroOne{Int}(inner)
    x, cx = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint(model, x, MOI.Interval(1, 3))
    MOI.delete(model, x)
    @test !MOI.is_valid(model, x)
    @test !MOI.is_valid(model, cx)
    MOI.Bridges.final_touch(model)
    return
end

function test_list_before_final_touch()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Constraint.IntegerToZeroOne{Float64}(inner)
    x = MOI.add_variables(model, 2)
    MOI.add_constraint(model, x[1], MOI.Interval(0.0, 3.0))
    c = MOI.add_constraint(model, 1.0 * x[1] + 1.0 * x[2], MOI.EqualTo(2.0))
    ci = MOI.add_constraint(model, x[1], MOI.Integer())
    F, S = MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}
    bridge = MOI.Bridges.bridge(model, ci)
    @test MOI.get(bridge, MOI.NumberOfConstraints{F,S}()) == 0
    @test isempty(MOI.get(bridge, MOI.ListOfConstraintIndices{F,S}()))
    @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 1
    @test MOI.get(model, MOI.ListOfConstraintIndices{F,S}()) == [c]
    dest = MOI.Utilities.Model{Float64}()
    MOI.copy_to(dest, model)
    @test MOI.get(dest, MOI.NumberOfConstraints{F,S}()) == 1
    MOI.Bridges.final_touch(model)
    @test MOI.get(bridge, MOI.NumberOfConstraints{F,S}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 1
    @test MOI.get(model, MOI.ListOfConstraintIndices{F,S}()) == [c]
    return
end

end  # module

TestConstraintIntegerToZeroOne.runtests()
