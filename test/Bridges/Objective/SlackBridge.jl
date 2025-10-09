# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestObjectiveSlack

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

include("../utilities.jl")

function test_SlackBridge_ObjectiveSense_error()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], 1.2)
    attr = MOI.ObjectiveFunction{typeof(f)}()
    @test_throws(
        MOI.SetAttributeNotAllowed(
            attr,
            "Set `MOI.ObjectiveSense` before `MOI.ObjectiveFunction` when " *
            "using `MOI.Bridges.Objective.SlackBridge`.",
        ),
        MOI.set(model, attr, f),
    )
    return
end

function test_SlackBridge_ObjectiveSense_modify()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], -1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test_throws(
        MOI.SetAttributeNotAllowed,
        MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test MOI.get(model, MOI.ObjectiveFunction{typeof(f)}()) ≈ f
    return
end

function test_SlackBridge_ObjectiveFunction_modify()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], -1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test MOI.get(model, MOI.ObjectiveFunction{typeof(f)}()) ≈ f
    g = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(-1.1, x)], 1.2)
    MOI.set(model, MOI.ObjectiveFunction{typeof(g)}(), g)
    @test MOI.get(model, MOI.ObjectiveFunction{typeof(g)}()) ≈ g
    return
end

function test_SlackBridge_get_ObjectiveFunction_MIN()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], -1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test MOI.get(model, MOI.ObjectiveFunction{typeof(f)}()) ≈ f
    return
end

function test_SlackBridge_get_ObjectiveFunction_MAX()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], -1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test MOI.get(model, MOI.ObjectiveFunction{typeof(f)}()) ≈ f
    return
end

function test_SlackBridge_get_ObjectiveSense()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    return
end

function test_SlackBridge_NumberOfVariables()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], 1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    @test MOI.get(inner, MOI.NumberOfVariables()) == 2
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    @test MOI.get(inner, MOI.NumberOfVariables()) == 1
    return
end

function test_SlackBridge_ListOfModelAttributesSet()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], 1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    attr = MOI.get(model, MOI.ListOfModelAttributesSet())
    @test length(attr) == 2
    @test MOI.ObjectiveSense() in attr
    @test MOI.ObjectiveFunction{typeof(f)}() in attr
    attr = MOI.get(inner, MOI.ListOfModelAttributesSet())
    @test length(attr) == 2
    @test MOI.ObjectiveSense() in attr
    @test MOI.ObjectiveFunction{MOI.VariableIndex}() in attr
    return
end

function test_SlackBridge_ListOfVariableIndices()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], 1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test MOI.get(model, MOI.ListOfVariableIndices()) == [x]
    @test length(MOI.get(inner, MOI.ListOfVariableIndices())) == 2
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    @test MOI.get(model, MOI.ListOfVariableIndices()) == [x]
    @test MOI.get(inner, MOI.ListOfVariableIndices()) == [x]
    return
end

function test_SlackBridge_ListOfConstraintTypesPresent()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], 1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test MOI.get(model, MOI.ListOfConstraintTypesPresent()) == []
    @test MOI.get(inner, MOI.ListOfConstraintTypesPresent()) ==
          [(MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})]
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    @test MOI.get(model, MOI.ListOfConstraintTypesPresent()) == []
    @test MOI.get(inner, MOI.ListOfConstraintTypesPresent()) == []
    return
end

function test_SlackBridge_ListOfConstraintIndices()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], 1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    attr = MOI.ListOfConstraintIndices{
        MOI.ScalarAffineFunction{Float64},
        MOI.GreaterThan{Float64},
    }()
    @test MOI.get(model, attr) == []
    @test length(MOI.get(inner, attr)) == 1
    attr = MOI.ListOfConstraintIndices{
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    }()
    @test MOI.get(model, attr) == []
    @test length(MOI.get(inner, attr)) == 0
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    @test MOI.get(model, attr) == []
    @test length(MOI.get(inner, attr)) == 0
    return
end

function test_SlackBridge_ObjectiveFunctionValue()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.GreaterThan(2.0))
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], -1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.Utilities.set_mock_optimize!(
        inner,
        mock -> MOI.Utilities.mock_optimize!(mock, [2.0, 1.0]),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1.0
    return
end

function test_SlackBridge_ObjectiveFunctionValue_2()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.GreaterThan(1.0))
    f = 1.1 * x - 1.2
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.Utilities.set_mock_optimize!(
        inner,
        mock -> begin
            MOI.set(mock, MOI.ResultCount(), 2)
            MOI.set(mock, MOI.TerminationStatus(), MOI.OPTIMAL)
            MOI.set(mock, MOI.PrimalStatus(1), MOI.FEASIBLE_POINT)
            MOI.set(mock, MOI.PrimalStatus(2), MOI.FEASIBLE_POINT)
            y = MOI.get(mock, MOI.ListOfVariableIndices())
            MOI.set.(mock, MOI.VariablePrimal(1), y, [1.0, -0.1])
            MOI.set.(mock, MOI.VariablePrimal(2), y, [2.0, 1.0])
        end,
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.ObjectiveValue(1)) ≈ -0.1
    @test MOI.get(model, MOI.VariablePrimal(1), x) ≈ 1.0
    @test MOI.get(model, MOI.ObjectiveValue(2)) ≈ 1.0
    @test MOI.get(model, MOI.VariablePrimal(2), x) ≈ 2.0
    return
end

function test_original()
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    bridged_mock = MOI.Bridges.Objective.Slack{Float64}(mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 2.0, 5.0]),
        ),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 2.0, 7.0]),
        ),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 2.0, 2.0]),
        ),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 2.0, 7.0]),
        ),
    )
    MOI.Test.test_objective_qp_ObjectiveFunction_edge_cases(
        bridged_mock,
        MOI.Test.Config(),
    )

    @test MOI.Bridges.is_objective_bridged(bridged_mock)
    @test MOI.get(bridged_mock, MOI.ObjectiveFunctionType()) ==
          MOI.ScalarQuadraticFunction{Float64}
    @test MOI.get(bridged_mock, MOI.ListOfModelAttributesSet()) == [
        MOI.ObjectiveSense(),
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
    ]

    var_names = ["x", "y"]
    xy = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
    MOI.set(bridged_mock, MOI.VariableName(), xy, var_names)
    abs = MOI.get(mock, MOI.ListOfVariableIndices())
    @test length(abs) == 3
    MOI.set(mock, MOI.VariableName(), abs[3], "s")
    cquad = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.ScalarQuadraticFunction{Float64},
            MOI.LessThan{Float64},
        }(),
    )
    MOI.set(bridged_mock, MOI.ConstraintName(), cquad[1], "quad")

    s = """
    variables: x, y, s
    x >= 1.0
    y >= 2.0
    quad: 1.0 * x * x + 1.0 * x * y + 1.0 * y * y + -1.0 * s <= 0.0
    minobjective: s
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        mock,
        model,
        [var_names; "s"],
        ["quad"],
        [
            ("x", MOI.GreaterThan{Float64}(1.0)),
            ("y", MOI.GreaterThan{Float64}(2.0)),
        ],
    )
    bridged_var_names = ["x", "y"]
    s = """
    variables: x, y
    x >= 1.0
    y >= 2.0
    minobjective: 1.0 * x * x + 1.0 * x * y + 1.0 * y * y
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        bridged_mock,
        model,
        var_names,
        String[],
        [
            ("x", MOI.GreaterThan{Float64}(1.0)),
            ("y", MOI.GreaterThan{Float64}(2.0)),
        ],
    )
    err = MOI.SetAttributeNotAllowed(
        MOI.ObjectiveSense(),
        "Objective bridge of type `$(MOI.Bridges.Objective.SlackBridge{Float64,MOI.ScalarQuadraticFunction{Float64},MOI.ScalarQuadraticFunction{Float64}})`" *
        " does not support modifying the objective sense. As a workaround, set" *
        " the sense to `MOI.FEASIBILITY_SENSE` to clear the objective function" *
        " and bridges.",
    )
    @test_throws err MOI.set(bridged_mock, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    obj = MOI.get(
        bridged_mock,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
    )
    MOI.set(bridged_mock, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    @test !MOI.Bridges.is_objective_bridged(bridged_mock)
    @test MOI.get(bridged_mock, MOI.ObjectiveSense()) == MOI.FEASIBILITY_SENSE
    @test MOI.get(bridged_mock, MOI.ListOfModelAttributesSet()) ==
          [MOI.ObjectiveSense()]
    MOI.set(bridged_mock, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        bridged_mock,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
        obj,
    )
    abs = MOI.get(mock, MOI.ListOfVariableIndices())
    @test length(abs) == 3
    MOI.set(mock, MOI.VariableName(), abs[3], "s")
    cquad = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.ScalarQuadraticFunction{Float64},
            MOI.GreaterThan{Float64},
        }(),
    )
    MOI.set(bridged_mock, MOI.ConstraintName(), cquad[1], "quad")

    s = """
    variables: x, y, s
    x >= 1.0
    y >= 2.0
    quad: 1.0 * x * x + 1.0 * x * y + 1.0 * y * y + -1.0 * s >= 0.0
    maxobjective: s
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        mock,
        model,
        [var_names; "s"],
        ["quad"],
        [
            ("x", MOI.GreaterThan{Float64}(1.0)),
            ("y", MOI.GreaterThan{Float64}(2.0)),
        ],
    )
    s = """
    variables: x, y
    x >= 1.0
    y >= 2.0
    maxobjective: 1.0 * x * x + 1.0 * x * y + 1.0 * y * y
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        bridged_mock,
        model,
        var_names,
        String[],
        [
            ("x", MOI.GreaterThan{Float64}(1.0)),
            ("y", MOI.GreaterThan{Float64}(2.0)),
        ],
    )
    _test_delete_objective(
        bridged_mock,
        2,
        (
            (MOI.ScalarQuadraticFunction{Float64}, MOI.GreaterThan{Float64}, 0),
            (MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}, 0),
        ),
    )
    return
end

function test_quadratic_integration()
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    bridged_mock = MOI.Bridges.Objective.Slack{Float64}(mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [4 / 7, 3 / 7, 6 / 7, 13 / 7],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [5 / 7, 6 / 7],
        ),
    )
    MOI.Test.test_quadratic_integration(bridged_mock, MOI.Test.Config())
    return
end

function test_quadratic_duplicate_terms()
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    bridged_mock = MOI.Bridges.Objective.Slack{Float64}(mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [4 / 7, 3 / 7, 6 / 7, 13 / 7],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [5 / 7, 6 / 7],
        ),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [4 / 7, 3 / 7, 6 / 7, -2 * 13 / 7],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [10 / 7, 12 / 7],
        ),
    )
    MOI.Test.test_quadratic_duplicate_terms(bridged_mock, MOI.Test.Config())
    return
end

function test_quadratic_nonhomogeneous()
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    bridged_mock = MOI.Bridges.Objective.Slack{Float64}(mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1 / 4, 3 / 4, 2.875],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [11 / 4],
            (MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1.0],
        ),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1.0, 0.0, 3.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [-2.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [1.0],
        ),
    )
    MOI.Test.test_quadratic_nonhomogeneous(bridged_mock, MOI.Test.Config())
    return
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.SlackBridge,
        """
        variables: x
        minobjective: 1.1 * x + 2.2
        """,
        """
        variables: x, y
        minobjective: y
        1.1 * x + -1.0 * y <= -2.2
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.SlackBridge,
        """
        variables: x
        maxobjective: 1.1 * x + 2.2
        """,
        """
        variables: x, y
        maxobjective: y
        1.1 * x + -1.0 * y >= -2.2
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.SlackBridge,
        """
        variables: x
        maxobjective: ScalarNonlinearFunction(log(x))
        """,
        """
        variables: x, y
        maxobjective: y
        ScalarNonlinearFunction(log(x) - y) >= 0.0
        """,
    )
    return
end

function test_complex_objective()
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x = MOI.add_variable(model)
    for f in ((1.0 + 1.0im) * x, (1.0 + 1.0im) * x * x)
        attr = MOI.ObjectiveFunction{typeof(f)}()
        MOI.set(model, attr, f)
        dest = MOI.Bridges.full_bridge_optimizer(
            MOI.Utilities.Model{Float64}(),
            Float64,
        )
        @test_throws MOI.UnsupportedAttribute(attr) MOI.copy_to(dest, model)
    end
    return
end

function test_deletion_of_variable_in_slacked_objective()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.VariableName(), x, ["x", "y"])
    f = 1.0 * x[1] * x[1] + 1.0 * x[2] * x[2]
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.delete(model, x[1])
    @test MOI.get(model, MOI.ObjectiveFunction{typeof(f)}()) ≈ 1.0 * x[2] * x[2]
    return
end

function test_SlackBridgePrimalDualStart_non_slack()
    inner = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    # Should ignore without erroring
    MOI.set(inner, MOI.Bridges.Objective.SlackBridgePrimalDualStart(), nothing)
    model = MOI.Bridges.Objective.Functionize{Float64}(inner)
    x = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveFunction{typeof(x)}(), x)
    # Should ignore without erroring
    MOI.set(model, MOI.Bridges.Objective.SlackBridgePrimalDualStart(), nothing)
    return
end

function test_SlackBridgePrimalDualStart()
    inner = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.GreaterThan(2.0))
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], -1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.set(model, MOI.VariablePrimalStart(), x, 2.0)
    attr = MOI.Bridges.Objective.SlackBridgePrimalDualStart()
    @test MOI.supports(model, attr)
    MOI.set(model, attr, nothing)
    vars = MOI.get(inner, MOI.ListOfVariableIndices())
    primal_start = MOI.get.(inner, MOI.VariablePrimalStart(), vars)
    @test primal_start[1] ≈ 2.0
    @test primal_start[2] ≈ 1.1 * 2.0 - 1.2
    F = MOI.ScalarAffineFunction{Float64}
    cis = MOI.get(inner, MOI.ListOfConstraintIndices{F,MOI.LessThan{Float64}}())
    @test length(cis) == 1
    @test MOI.get(inner, MOI.ConstraintPrimalStart(), cis[1]) ≈ 0.0
    @test MOI.get(inner, MOI.ConstraintDualStart(), cis[1]) ≈ -1.0
    return
end

function test_SlackBridgePrimalDualStart_unsupported()
    attr = MOI.Bridges.Objective.SlackBridgePrimalDualStart()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    # Check that setting on blank model doesn't error.
    @test MOI.supports(inner, attr)
    MOI.set(inner, attr, nothing)
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    @test MOI.supports(model, attr)
    x = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], -1.2)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    # Unsupported. Should silently skip without error.
    MOI.set(model, attr, nothing)
    return
end

end  # module

TestObjectiveSlack.runtests()
