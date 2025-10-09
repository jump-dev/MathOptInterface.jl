# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintIndicatorToMILP

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

function test_runtests_VectorOfVariables_ON_ONE()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.IndicatorToMILPBridge,
        """
        variables: x, y
        [x, y] in Indicator{ACTIVATE_ON_ONE}(LessThan(2.0))
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        """,
        """
        variables: x, y, a
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        1.0 * y + 1.0 * a <= 2.0
        1.0 * x + -1.0 * a <= 1.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.IndicatorToMILPBridge,
        """
        variables: x, y
        [x, y] in Indicator{ACTIVATE_ON_ONE}(LessThan(4.0))
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        """,
        """
        variables: x, y, a
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        1.0 * y + 1.0 * a <= 4.0
        -1.0 * a <= 0.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.IndicatorToMILPBridge,
        """
        variables: x, y
        [x, y] in Indicator{ACTIVATE_ON_ONE}(GreaterThan(1.5))
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        """,
        """
        variables: x, y, a
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        1.0 * y + 1.0 * a >= 1.5
        0.5 * x + 1.0 * a <= 0.5
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.IndicatorToMILPBridge,
        """
        variables: x, y
        [x, y] in Indicator{ACTIVATE_ON_ONE}(EqualTo(1.25))
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        """,
        """
        variables: x, y, a
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        1.0 * y + 1.0 * a == 1.25
        0.25 * x + 1.0 * a <= 0.25
        1.75 * x + -1.0 * a <= 1.75
        """,
    )
    return
end

function test_runtests_VectorOfVariables_ON_ZERO()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.IndicatorToMILPBridge,
        """
        variables: x, y
        [x, y] in Indicator{ACTIVATE_ON_ZERO}(LessThan(2.0))
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        """,
        """
        variables: x, y, a
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        1.0 * y + 1.0 * a <= 2.0
        -1.0 * x + -1.0 * a <= 0.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.IndicatorToMILPBridge,
        """
        variables: x, y
        [x, y] in Indicator{ACTIVATE_ON_ZERO}(LessThan(4.0))
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        """,
        """
        variables: x, y, a
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        1.0 * y + 1.0 * a <= 4.0
        -1.0 * a <= 0.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.IndicatorToMILPBridge,
        """
        variables: x, y
        [x, y] in Indicator{ACTIVATE_ON_ZERO}(GreaterThan(1.5))
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        """,
        """
        variables: x, y, a
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        1.0 * y + 1.0 * a >= 1.5
        1.0 * a + -0.5 * x <= 0.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.IndicatorToMILPBridge,
        """
        variables: x, y
        [x, y] in Indicator{ACTIVATE_ON_ZERO}(EqualTo(1.25))
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        """,
        """
        variables: x, y, a
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        1.0 * y + 1.0 * a == 1.25
        -0.25 * x + 1.0 * a <= 0.0
        -1.75 * x + -1.0 * a <= 0.0
        """,
    )
    return
end

function test_runtests_VectorAffineFunction_ON_ZERO()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.IndicatorToMILPBridge,
        """
        variables: x, y
        [x, 2.0 * y + 1.0] in Indicator{ACTIVATE_ON_ZERO}(LessThan(2.0))
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        """,
        """
        variables: x, y, a
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        2.0 * y + 1.0 * a <= 1.0
        -5.0 * x + -1.0 * a <= 0.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.IndicatorToMILPBridge,
        """
        variables: x, y
        [x, 2.0 * y + 1.0] in Indicator{ACTIVATE_ON_ZERO}(LessThan(4.0))
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        """,
        """
        variables: x, y, a
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        2.0 * y + 1.0 * a <= 3.0
        -3.0 * x + -1.0 * a <= 0.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.IndicatorToMILPBridge,
        """
        variables: x, y
        [x, 2.0 * y + 1.0] in Indicator{ACTIVATE_ON_ZERO}(GreaterThan(1.5))
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        """,
        """
        variables: x, y, a
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        2.0 * y + 1.0 * a >= 0.5
        1.0 * a <= 0.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.IndicatorToMILPBridge,
        """
        variables: x, y
        [x, 0.5 * y + 0.25] in Indicator{ACTIVATE_ON_ZERO}(EqualTo(1.25))
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        """,
        """
        variables: x, y, a
        x in ZeroOne()
        y in Interval(1.0, 3.0)
        0.5 * y + 1.0 * a == 1.0
        -0.5 * x + 1.0 * a <= 0.0
        -0.5 * x + -1.0 * a <= 0.0
        """,
    )
    return
end

function test_resolve_with_modified()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.IndicatorToMILP{Int}(inner)
    x = MOI.add_variables(model, 2)
    MOI.add_constraint(model, x[1], MOI.ZeroOne())
    c = MOI.add_constraint(model, x[2], MOI.Interval(1, 2))
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.Indicator{MOI.ACTIVATE_ON_ZERO}(MOI.GreaterThan(2)),
    )
    @test MOI.get(inner, MOI.NumberOfVariables()) == 2
    MOI.Bridges.final_touch(model)
    @test MOI.get(inner, MOI.NumberOfVariables()) == 3
    MOI.set(model, MOI.ConstraintSet(), c, MOI.Interval(3, 4))
    MOI.Bridges.final_touch(model)
    @test MOI.get(inner, MOI.NumberOfVariables()) == 3
    return
end

function test_runtests_error_variable()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.IndicatorToMILP{Int}(inner)
    x = MOI.add_variables(model, 2)
    MOI.add_constraint(model, x[1], MOI.ZeroOne())
    c = MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.Indicator{MOI.ACTIVATE_ON_ZERO}(MOI.GreaterThan(2)),
    )
    BT = typeof(model.map[c])
    @test_throws(
        MOI.Bridges.BridgeRequiresFiniteDomainError{BT,MOI.VariableIndex},
        MOI.Bridges.final_touch(model),
    )
    return
end

function test_runtests_error_affine()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.IndicatorToMILP{Int}(inner)
    x = MOI.add_variables(model, 2)
    MOI.add_constraint(model, x[1], MOI.ZeroOne())
    c = MOI.add_constraint(
        model,
        MOI.Utilities.operate(vcat, Int, x[1], 2 * x[2]),
        MOI.Indicator{MOI.ACTIVATE_ON_ZERO}(MOI.GreaterThan(2)),
    )
    BT = typeof(model.map[c])
    F = MOI.ScalarAffineFunction{Int}
    @test_throws(
        MOI.Bridges.BridgeRequiresFiniteDomainError{BT,F},
        MOI.Bridges.final_touch(model),
    )
    return
end

function test_delete_before_final_touch()
    model = MOI.Bridges.Constraint.IndicatorToMILP{Float64}(
        MOI.Utilities.Model{Float64}(),
    )
    x = MOI.add_variables(model, 2)
    MOI.add_constraint(model, x[1], MOI.ZeroOne())
    c = MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.Indicator{MOI.ACTIVATE_ON_ZERO}(MOI.GreaterThan(2.0)),
    )
    MOI.delete(model, c)
    @test !MOI.is_valid(model, c)
    return
end

function test_runtests_error_not_binary()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.IndicatorToMILP{Int}(inner)
    x = MOI.add_variables(model, 2)
    MOI.add_constraint(model, x[2], MOI.Interval(0, 4))
    set = MOI.Indicator{MOI.ACTIVATE_ON_ZERO}(MOI.GreaterThan(2))
    c = MOI.add_constraint(model, MOI.VectorOfVariables(x), set)
    @test_throws(
        MOI.AddConstraintNotAllowed{MOI.VectorOfVariables,typeof(set)}(
            "Unable to reformulate indicator constraint to a MILP. The indicator variable must be binary.",
        ),
        MOI.Bridges.final_touch(model),
    )
    return
end

MOI.Utilities.@model(
    Model2867,
    (),
    (MOI.EqualTo, MOI.LessThan),
    (),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (),
    ()
)

function MOI.supports_constraint(
    model::Model2867,
    ::Type{MOI.VariableIndex},
    ::Type{<:Union{MOI.Integer,MOI.ZeroOne}},
)
    return get(model.ext, :supports, false)
end

function test_issue_2867()
    model = MOI.instantiate(Model2867{Float64}; with_bridge_type = Float64)
    @test !MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.EqualTo{Float64}},
    )
    model = MOI.instantiate(Model2867{Float64}; with_bridge_type = Float64)
    model.model.ext[:supports] = true
    @test MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.EqualTo{Float64}},
    )
    return
end

end  # module

TestConstraintIndicatorToMILP.runtests()
