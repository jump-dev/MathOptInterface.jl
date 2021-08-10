"""
    test_solve_ObjectiveBound_MIN_SENSE_IP(model::MOI.ModelLike, config::Config)

Test a variety of edge cases related to the ObjectiveBound attribute.
"""
function test_solve_ObjectiveBound_MIN_SENSE_IP(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ObjectiveBound)
    MOIU.loadfromstring!(
        model,
        """
variables: x
minobjective: 2.0x + -1.0
x >= 1.5
x in Integer()
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    _test_model_solution(
        model,
        config;
        objective_value = 3.0,
        variable_primal = [(x, 2.0)],
    )
    @test MOI.get(model, MOI.ObjectiveBound()) <= 3.0
    return
end

function setup_test(
    ::typeof(test_solve_ObjectiveBound_MIN_SENSE_IP),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 3.0)
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [2.0]))
        end,
    )
    return
end

"""
    test_solve_ObjectiveBound_MAX_SENSE_IP(model::MOI.ModelLike, config::Config)

Test a variety of edge cases related to the ObjectiveBound attribute.
"""
function test_solve_ObjectiveBound_MAX_SENSE_IP(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ObjectiveBound)
    MOIU.loadfromstring!(
        model,
        """
variables: x
maxobjective: 2.0x + 1.0
x <= 1.5
x in Integer()
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    _test_model_solution(
        model,
        config;
        objective_value = 3.0,
        variable_primal = [(x, 1.0)],
    )
    @test MOI.get(model, MOI.ObjectiveBound()) >= 3.0
    return
end

function setup_test(
    ::typeof(test_solve_ObjectiveBound_MAX_SENSE_IP),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 3.0)
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [1.0]))
        end,
    )
    return
end

"""
    test_solve_ObjectiveBound_MIN_SENSE_LP(model::MOI.ModelLike, config::Config)

Test a variety of edge cases related to the ObjectiveBound attribute.
"""
function test_solve_ObjectiveBound_MIN_SENSE_LP(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ObjectiveBound)
    MOIU.loadfromstring!(
        model,
        """
variables: x
minobjective: 2.0x + -1.0
x >= 1.5
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    _test_model_solution(
        model,
        config;
        objective_value = 2.0,
        variable_primal = [(x, 1.5)],
    )
    @test MOI.get(model, MOI.ObjectiveBound()) <= 2.0
end

function setup_test(
    ::typeof(test_solve_ObjectiveBound_MIN_SENSE_LP),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 2.0)
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [1.5]))
        end,
    )
    return
end

"""
    test_solve_ObjectiveBound_MIN_SENSE_LP(model::MOI.ModelLike, config::Config)

Test a variety of edge cases related to the ObjectiveBound attribute.
"""
function test_solve_ObjectiveBound_MAX_SENSE_LP(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ObjectiveBound)
    MOIU.loadfromstring!(
        model,
        """
variables: x
maxobjective: 2.0x + 1.0
x <= 1.5
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    _test_model_solution(
        model,
        config;
        objective_value = 4.0,
        variable_primal = [(x, 1.5)],
    )
    @test MOI.get(model, MOI.ObjectiveBound()) >= 4.0
    return
end

function setup_test(
    ::typeof(test_solve_ObjectiveBound_MAX_SENSE_LP),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 4.0)
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [1.5]))
        end,
    )
    return
end

"""
    test_solve_TerminationStatus_DUAL_INFEASIBLE(
        model::MOI.ModelLike,
        config::Config,
    )

Test an unbounded linear program.
"""
function test_solve_TerminationStatus_DUAL_INFEASIBLE(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    x = MOI.add_variables(model, 5)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x), 0.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.DUAL_INFEASIBLE
    return
end

function setup_test(
    ::typeof(test_solve_TerminationStatus_DUAL_INFEASIBLE),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.DUAL_INFEASIBLE),
    )
    return
end

"""
    test_solve_SingleVariable_ConstraintDual_MIN_SENSE(
        model::MOI.ModelLike,
        config::Config,
    )

Test `ConstraintDual` of a `SingleVariable` constraint when minimizing.
"""
function test_solve_SingleVariable_ConstraintDual_MIN_SENSE(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variable(model)
    xl = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(1.0))
    xu = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.LessThan(1.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x),
    )
    MOI.optimize!(model)
    @test isapprox(
        MOI.get(model, MOI.VariablePrimal(), x),
        1.0,
        atol = config.atol,
    )
    sl = MOI.get(model, MOI.ConstraintDual(), xl)
    su = MOI.get(model, MOI.ConstraintDual(), xu)
    @test isapprox(sl + su, 1.0, atol = config.atol)
    @test sl >= -config.atol
    @test su <= config.atol
    return
end

function setup_test(
    ::typeof(test_solve_SingleVariable_ConstraintDual_MIN_SENSE),
    model::MOIU.MockOptimizer,
    ::Config,
)
    flag = model.eval_variable_constraint_dual
    model.eval_variable_constraint_dual = false
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0]),
            MOI.FEASIBLE_POINT,
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [1.0],
            (MOI.SingleVariable, MOI.LessThan{Float64}) => [0.0],
        ),
    )
    return () -> model.eval_variable_constraint_dual = flag
end

"""
    test_solve_SingleVariable_ConstraintDual_MAX_SENSE(
        model::MOI.ModelLike,
        config::Config,
    )

Test `ConstraintDual` of a `SingleVariable` constraint when maximizing.
"""
function test_solve_SingleVariable_ConstraintDual_MAX_SENSE(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variable(model)
    xl = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(1.0))
    xu = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.LessThan(1.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x),
    )
    MOI.optimize!(model)
    @test isapprox(
        MOI.get(model, MOI.VariablePrimal(), x),
        1.0,
        atol = config.atol,
    )
    sl = MOI.get(model, MOI.ConstraintDual(), xl)
    su = MOI.get(model, MOI.ConstraintDual(), xu)
    @test isapprox(sl + su, -1.0, atol = config.atol)
    @test sl >= -config.atol
    @test su <= config.atol
    return
end

function setup_test(
    ::typeof(test_solve_SingleVariable_ConstraintDual_MAX_SENSE),
    model::MOIU.MockOptimizer,
    ::Config,
)
    flag = model.eval_variable_constraint_dual
    model.eval_variable_constraint_dual = false
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0]),
            MOI.FEASIBLE_POINT,
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [0.0],
            (MOI.SingleVariable, MOI.LessThan{Float64}) => [-1.0],
        ),
    )
    return () -> model.eval_variable_constraint_dual = flag
end

"""
    test_solve_result_index(model::MOI.ModelLike, config::Config)

Test that various attributes implement `.result_index` correctly.
"""
function test_solve_result_index(model::MOI.ModelLike, config::Config)
    @requires _supports(config, MOI.optimize!)
    atol = config.atol
    rtol = config.rtol
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(1.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x),
    )
    MOI.optimize!(model)
    result_count = MOI.get(model, MOI.ResultCount())
    _test_attribute_value_type(model, MOI.ResultCount())
    function result_err(attr)
        return MOI.ResultIndexBoundsError{typeof(attr)}(attr, result_count)
    end
    result_index = result_count + 1
    @test isapprox(MOI.get(model, MOI.ObjectiveValue(1)), 1.0, config)
    @test_throws result_err(MOI.ObjectiveValue(result_index)) MOI.get(
        model,
        MOI.ObjectiveValue(result_index),
    )
    if _supports(config, MOI.DualObjectiveValue)
        @test isapprox(MOI.get(model, MOI.DualObjectiveValue(1)), 1.0, config)
        @test_throws(
            result_err(MOI.DualObjectiveValue(result_index)),
            MOI.get(model, MOI.DualObjectiveValue(result_index)),
        )
    end
    @test MOI.get(model, MOI.PrimalStatus(1)) == MOI.FEASIBLE_POINT
    @test MOI.get(model, MOI.PrimalStatus(result_index)) == MOI.NO_SOLUTION
    @test MOI.get(model, MOI.VariablePrimal(1), x) ≈ 1.0 atol = atol rtol = rtol
    @test_throws result_err(MOI.VariablePrimal(result_index)) MOI.get(
        model,
        MOI.VariablePrimal(result_index),
        x,
    )
    @test MOI.get(model, MOI.ConstraintPrimal(1), c) ≈ 1.0 atol = atol rtol =
        rtol
    @test_throws result_err(MOI.ConstraintPrimal(result_index)) MOI.get(
        model,
        MOI.ConstraintPrimal(result_index),
        c,
    )
    if _supports(config, MOI.ConstraintDual)
        @test MOI.get(model, MOI.DualStatus(1)) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.DualStatus(result_index)) == MOI.NO_SOLUTION
        @test MOI.get(model, MOI.ConstraintDual(1), c) ≈ 1.0 atol = atol rtol =
            rtol
        @test_throws result_err(MOI.ConstraintDual(result_index)) MOI.get(
            model,
            MOI.ConstraintDual(result_index),
            c,
        )
    end
    return
end

function setup_test(
    ::typeof(test_solve_result_index),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0]),
            MOI.FEASIBLE_POINT,
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [1.0],
        ),
    )
    return
end

"""
    test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_EqualTo_upper(
        model::MOI.ModelLike,
        config::Config,
    )

Test the Farkas dual of an equality constraint violated above.
"""
function test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_EqualTo_upper(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variables(model, 2)
    clb =
        MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.GreaterThan(0.0))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0, 1.0], x), 0.0),
        MOI.EqualTo(-1.0),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
    if MOI.get(model, MOI.DualStatus()) != MOI.INFEASIBILITY_CERTIFICATE
        return
    end
    clb_dual = MOI.get.(model, MOI.ConstraintDual(), clb)
    c_dual = MOI.get(model, MOI.ConstraintDual(), c)
    @test clb_dual[1] > config.atol
    @test clb_dual[2] > config.atol
    @test c_dual[1] < -config.atol
    @test clb_dual ≈ [2, 1] .* -c_dual atol = config.atol rtol = config.rtol
    return
end

function setup_test(
    ::typeof(test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_EqualTo_upper),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            (MOI.NO_SOLUTION, [NaN, NaN]),
            MOI.INFEASIBILITY_CERTIFICATE,
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [2.0, 1.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [-1.0],
        ),
    )
    return
end

"""
    test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_EqualTo_upper(
        model::MOI.ModelLike,
        config::Config,
    )

Test the Farkas dual of an equality constraint violated below.
"""
function test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_EqualTo_lower(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variables(model, 2)
    clb =
        MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.GreaterThan(0.0))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-2.0, -1.0], x), 0.0),
        MOI.EqualTo(1.0),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
    if MOI.get(model, MOI.DualStatus()) != MOI.INFEASIBILITY_CERTIFICATE
        return
    end
    clb_dual = MOI.get.(model, MOI.ConstraintDual(), clb)
    c_dual = MOI.get(model, MOI.ConstraintDual(), c)
    @test clb_dual[1] > config.atol
    @test clb_dual[2] > config.atol
    @test c_dual[1] > config.atol
    @test clb_dual ≈ [2, 1] .* c_dual atol = config.atol rtol = config.rtol
end

function setup_test(
    ::typeof(test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_EqualTo_lower),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            (MOI.NO_SOLUTION, [NaN, NaN]),
            MOI.INFEASIBILITY_CERTIFICATE,
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [2.0, 1.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [1.0],
        ),
    )
    return
end

"""
    test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_EqualTo_upper(
        model::MOI.ModelLike,
        config::Config,
    )

Test the Farkas dual of a less-than constraint.
"""
function test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_LessThan(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variables(model, 2)
    clb =
        MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.GreaterThan(0.0))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0, 1.0], x), 0.0),
        MOI.LessThan(-1.0),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
    if MOI.get(model, MOI.DualStatus()) != MOI.INFEASIBILITY_CERTIFICATE
        return
    end
    clb_dual = MOI.get.(model, MOI.ConstraintDual(), clb)
    c_dual = MOI.get(model, MOI.ConstraintDual(), c)
    @test clb_dual[1] > config.atol
    @test clb_dual[2] > config.atol
    @test c_dual[1] < -config.atol
    @test clb_dual ≈ [2, 1] .* -c_dual atol = config.atol rtol = config.rtol
    return
end

function setup_test(
    ::typeof(test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_LessThan),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            (MOI.NO_SOLUTION, [NaN, NaN]),
            MOI.INFEASIBILITY_CERTIFICATE,
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [2.0, 1.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1.0],
        ),
    )
    return
end

"""
    test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_EqualTo_upper(
        model::MOI.ModelLike,
        config::Config,
    )

Test the Farkas dual of a greater-than constraint.
"""
function test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_GreaterThan(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variables(model, 2)
    clb =
        MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.GreaterThan(0.0))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-2.0, -1.0], x), 0.0),
        MOI.GreaterThan(1.0),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
    if MOI.get(model, MOI.DualStatus()) != MOI.INFEASIBILITY_CERTIFICATE
        return
    end
    clb_dual = MOI.get.(model, MOI.ConstraintDual(), clb)
    c_dual = MOI.get(model, MOI.ConstraintDual(), c)
    @test clb_dual[1] > config.atol
    @test clb_dual[2] > config.atol
    @test c_dual[1] > config.atol
    @test clb_dual ≈ [2, 1] .* c_dual atol = config.atol rtol = config.rtol
    return
end

function setup_test(
    ::typeof(test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_GreaterThan),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            (MOI.NO_SOLUTION, [NaN, NaN]),
            MOI.INFEASIBILITY_CERTIFICATE,
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [2.0, 1.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [1.0],
        ),
    )
    return
end

"""
    test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_EqualTo_upper(
        model::MOI.ModelLike,
        config::Config,
    )

Test the Farkas dual of an interval constraint violated above.
"""
function test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_Interval_upper(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variables(model, 2)
    clb =
        MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.GreaterThan(0.0))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0, 1.0], x), 0.0),
        MOI.Interval(-2.0, -1.0),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
    if MOI.get(model, MOI.DualStatus()) != MOI.INFEASIBILITY_CERTIFICATE
        return
    end
    clb_dual = MOI.get.(model, MOI.ConstraintDual(), clb)
    c_dual = MOI.get(model, MOI.ConstraintDual(), c)
    @test clb_dual[1] > config.atol
    @test clb_dual[2] > config.atol
    @test c_dual[1] < -config.atol
    @test clb_dual ≈ [2, 1] .* -c_dual atol = config.atol rtol = config.rtol
    return
end

function setup_test(
    ::typeof(test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_Interval_upper),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            (MOI.NO_SOLUTION, [NaN, NaN]),
            MOI.INFEASIBILITY_CERTIFICATE,
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [2.0, 1.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [-1.0],
        ),
    )
    return
end

"""
    test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_EqualTo_upper(
        model::MOI.ModelLike,
        config::Config,
    )

Test the Farkas dual of an interval constraint violated below.
"""
function test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_Interval_lower(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variables(model, 2)
    clb =
        MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.GreaterThan(0.0))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-2.0, -1.0], x), 0.0),
        MOI.Interval(1.0, 2.0),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
    if MOI.get(model, MOI.DualStatus()) != MOI.INFEASIBILITY_CERTIFICATE
        return
    end
    clb_dual = MOI.get.(model, MOI.ConstraintDual(), clb)
    c_dual = MOI.get(model, MOI.ConstraintDual(), c)
    @test clb_dual[1] > config.atol
    @test clb_dual[2] > config.atol
    @test c_dual[1] > config.atol
    @test clb_dual ≈ [2, 1] .* c_dual atol = config.atol rtol = config.rtol
    return
end

function setup_test(
    ::typeof(test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_Interval_lower),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            (MOI.NO_SOLUTION, [NaN, NaN]),
            MOI.INFEASIBILITY_CERTIFICATE,
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [2.0, 1.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [1.0],
        ),
    )
    return
end

"""
    test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_EqualTo_upper(
        model::MOI.ModelLike,
        config::Config,
    )

Test the Farkas dual of a variable upper bound violated above when minimizing.
"""
function test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_SingleVariable_LessThan(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variables(model, 2)
    clb = MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.LessThan(0.0))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0, 1.0], x), 0.0),
        MOI.GreaterThan(1.0),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
    if MOI.get(model, MOI.DualStatus()) != MOI.INFEASIBILITY_CERTIFICATE
        return
    end
    clb_dual = MOI.get.(model, MOI.ConstraintDual(), clb)
    c_dual = MOI.get(model, MOI.ConstraintDual(), c)
    @test clb_dual[1] < -config.atol
    @test clb_dual[2] < -config.atol
    @test c_dual[1] > config.atol
    @test clb_dual ≈ [2, 1] .* -c_dual atol = config.atol rtol = config.rtol
    return
end

function setup_test(
    ::typeof(
        test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_SingleVariable_LessThan,
    ),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            (MOI.NO_SOLUTION, [NaN, NaN]),
            MOI.INFEASIBILITY_CERTIFICATE,
            (MOI.SingleVariable, MOI.LessThan{Float64}) => [-2.0, -1.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [1.0],
        ),
    )
    return
end

"""
    test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_EqualTo_upper(
        model::MOI.ModelLike,
        config::Config,
    )

Test the Farkas dual of a variable upper bound violated above when maximizing.
"""
function test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_SingleVariable_LessThan_max(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variables(model, 2)
    clb = MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.LessThan(0.0))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0, 1.0], x), 0.0),
        MOI.GreaterThan(1.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x[1]),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
    if MOI.get(model, MOI.DualStatus()) != MOI.INFEASIBILITY_CERTIFICATE
        return
    end
    clb_dual = MOI.get.(model, MOI.ConstraintDual(), clb)
    c_dual = MOI.get(model, MOI.ConstraintDual(), c)
    @test clb_dual[1] < -config.atol
    @test clb_dual[2] < -config.atol
    @test c_dual[1] > config.atol
    @test clb_dual ≈ [2, 1] .* -c_dual atol = config.atol rtol = config.rtol
    return
end

function setup_test(
    ::typeof(
        test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_SingleVariable_LessThan_max,
    ),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            (MOI.NO_SOLUTION, [NaN, NaN]),
            MOI.INFEASIBILITY_CERTIFICATE,
            (MOI.SingleVariable, MOI.LessThan{Float64}) => [-2.0, -1.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [1.0],
        ),
    )
    return
end

"""
    test_solve_optimize_twice(
        model::MOI.ModelLike,
        config::Config,
    )

Test that calling `optimize!` twice in succession does not error.
"""
function test_solve_optimize_twice(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(one(T)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x),
    )
    MOI.optimize!(model)
    MOI.optimize!(model)
    MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    MOI.get(model, MOI.VariablePrimal(), x) == one(T)
    return
end

function setup_test(
    ::typeof(test_solve_optimize_twice),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [1.0])),
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [1.0])),
    )
    return
end
