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
    test_solve_VariableIndex_ConstraintDual_MIN_SENSE(
        model::MOI.ModelLike,
        config::Config,
    )

Test `ConstraintDual` of a `VariableIndex` constraint when minimizing.
"""
function test_solve_VariableIndex_ConstraintDual_MIN_SENSE(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variable(model)
    xl = MOI.add_constraint(model, x, MOI.GreaterThan(1.0))
    xu = MOI.add_constraint(model, x, MOI.LessThan(1.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
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
    ::typeof(test_solve_VariableIndex_ConstraintDual_MIN_SENSE),
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
            (MOI.VariableIndex, MOI.GreaterThan{Float64}) => [1.0],
            (MOI.VariableIndex, MOI.LessThan{Float64}) => [0.0],
        ),
    )
    return () -> model.eval_variable_constraint_dual = flag
end

"""
    test_solve_VariableIndex_ConstraintDual_MAX_SENSE(
        model::MOI.ModelLike,
        config::Config,
    )

Test `ConstraintDual` of a `VariableIndex` constraint when maximizing.
"""
function test_solve_VariableIndex_ConstraintDual_MAX_SENSE(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variable(model)
    xl = MOI.add_constraint(model, x, MOI.GreaterThan(1.0))
    xu = MOI.add_constraint(model, x, MOI.LessThan(1.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
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
    ::typeof(test_solve_VariableIndex_ConstraintDual_MAX_SENSE),
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
            (MOI.VariableIndex, MOI.GreaterThan{Float64}) => [0.0],
            (MOI.VariableIndex, MOI.LessThan{Float64}) => [-1.0],
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
    c = MOI.add_constraint(model, x, MOI.GreaterThan(1.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
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
            (MOI.VariableIndex, MOI.GreaterThan{Float64}) => [1.0],
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
    clb = MOI.add_constraint.(model, x, MOI.GreaterThan(0.0))
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
            (MOI.VariableIndex, MOI.GreaterThan{Float64}) => [2.0, 1.0],
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
    clb = MOI.add_constraint.(model, x, MOI.GreaterThan(0.0))
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
            (MOI.VariableIndex, MOI.GreaterThan{Float64}) => [2.0, 1.0],
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
    clb = MOI.add_constraint.(model, x, MOI.GreaterThan(0.0))
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
            (MOI.VariableIndex, MOI.GreaterThan{Float64}) => [2.0, 1.0],
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
    clb = MOI.add_constraint.(model, x, MOI.GreaterThan(0.0))
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
            (MOI.VariableIndex, MOI.GreaterThan{Float64}) => [2.0, 1.0],
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
    clb = MOI.add_constraint.(model, x, MOI.GreaterThan(0.0))
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
            (MOI.VariableIndex, MOI.GreaterThan{Float64}) => [2.0, 1.0],
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
    clb = MOI.add_constraint.(model, x, MOI.GreaterThan(0.0))
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
            (MOI.VariableIndex, MOI.GreaterThan{Float64}) => [2.0, 1.0],
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
function test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_VariableIndex_LessThan(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variables(model, 2)
    clb = MOI.add_constraint.(model, x, MOI.LessThan(0.0))
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
        test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_VariableIndex_LessThan,
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
            (MOI.VariableIndex, MOI.LessThan{Float64}) => [-2.0, -1.0],
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
function test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_VariableIndex_LessThan_max(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variables(model, 2)
    clb = MOI.add_constraint.(model, x, MOI.LessThan(0.0))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0, 1.0], x), 0.0),
        MOI.GreaterThan(1.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x[1])
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
        test_solve_DualStatus_INFEASIBILITY_CERTIFICATE_VariableIndex_LessThan_max,
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
            (MOI.VariableIndex, MOI.LessThan{Float64}) => [-2.0, -1.0],
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
    MOI.add_constraint(model, x, MOI.GreaterThan(one(T)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
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

"""
    test_solve_conflict_bound_bound(model::MOI.ModelLike, config::Config)

Test the ConflictStatus API when two variable bounds are in the conflict.
"""
function test_solve_conflict_bound_bound(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.compute_conflict!)
    @requires _supports(config, MOI.optimize!)
    try
        MOI.get(model, MOI.ConflictStatus())
    catch
        return  # If this fails, skip the test.
    end
    x = MOI.add_variable(model)
    c1 = MOI.add_constraint(model, x, MOI.GreaterThan(T(2)))
    c2 = MOI.add_constraint(model, x, MOI.LessThan(one(T)))
    @test MOI.get(model, MOI.ConflictStatus()) ==
          MOI.COMPUTE_CONFLICT_NOT_CALLED
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
    MOI.compute_conflict!(model)
    @test MOI.get(model, MOI.ConflictStatus()) == MOI.CONFLICT_FOUND
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c1) == MOI.IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c2) == MOI.IN_CONFLICT
    return
end

function setup_test(
    ::typeof(test_solve_conflict_bound_bound),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        mock::MOIU.MockOptimizer -> begin
            MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                MOI.NO_SOLUTION,
                MOI.NO_SOLUTION;
                constraint_conflict_status = [
                    (MOI.VariableIndex, MOI.LessThan{Float64}) =>
                        [MOI.IN_CONFLICT],
                    (MOI.VariableIndex, MOI.GreaterThan{Float64}) =>
                        [MOI.IN_CONFLICT],
                ],
            )
            MOI.set(mock, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
        end,
    )
    return
end

"""
    test_solve_conflict_two_affine(model::MOI.ModelLike, config::Config)

Test the ConflictStatus API when two affine constraints are in the conflict.
"""
function test_solve_conflict_two_affine(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.compute_conflict!)
    @requires _supports(config, MOI.optimize!)
    try
        MOI.get(model, MOI.ConflictStatus())
    catch
        return  # If this fails, skip the test.
    end
    x = MOI.add_variable(model)
    c1 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([one(T)], [x]), zero(T)),
        MOI.GreaterThan(T(2)),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([one(T)], [x]), zero(T)),
        MOI.LessThan(T(1)),
    )
    @test MOI.get(model, MOI.ConflictStatus()) ==
          MOI.COMPUTE_CONFLICT_NOT_CALLED
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
    MOI.compute_conflict!(model)
    @test MOI.get(model, MOI.ConflictStatus()) == MOI.CONFLICT_FOUND
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c1) == MOI.IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c2) == MOI.IN_CONFLICT
    return
end

function setup_test(
    ::typeof(test_solve_conflict_two_affine),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        mock::MOIU.MockOptimizer -> begin
            MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                MOI.NO_SOLUTION,
                MOI.NO_SOLUTION;
                constraint_conflict_status = [
                    (
                        MOI.ScalarAffineFunction{Float64},
                        MOI.LessThan{Float64},
                    ) => [MOI.IN_CONFLICT],
                    (
                        MOI.ScalarAffineFunction{Float64},
                        MOI.GreaterThan{Float64},
                    ) => [MOI.IN_CONFLICT],
                ],
            )
            MOI.set(mock, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
        end,
    )
    return
end

"""
    test_solve_conflict_invalid_interval(model::MOI.ModelLike, config::Config)

Test the ConflictStatus API when an interval bound has upper < lower.
"""
function test_solve_conflict_invalid_interval(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.compute_conflict!)
    @requires _supports(config, MOI.optimize!)
    try
        MOI.get(model, MOI.ConflictStatus())
    catch
        return  # If this fails, skip the test.
    end
    x = MOI.add_variable(model)
    c1 = MOI.add_constraint(model, x, MOI.Interval(one(T), zero(T)))
    @test MOI.get(model, MOI.ConflictStatus()) ==
          MOI.COMPUTE_CONFLICT_NOT_CALLED
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
    MOI.compute_conflict!(model)
    @test MOI.get(model, MOI.ConflictStatus()) == MOI.CONFLICT_FOUND
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c1) == MOI.IN_CONFLICT
    return
end

function setup_test(
    ::typeof(test_solve_conflict_invalid_interval),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        mock::MOIU.MockOptimizer -> begin
            MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                MOI.NO_SOLUTION,
                MOI.NO_SOLUTION;
                constraint_conflict_status = [
                    (MOI.VariableIndex, MOI.Interval{Float64}) =>
                        [MOI.IN_CONFLICT],
                ],
            )
            MOI.set(mock, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
        end,
    )
    return
end

"""
    test_solve_conflict_affine_affine(model::MOI.ModelLike, config::Config)

Test the ConflictStatus API when two constraints are in the conflict.
"""
function test_solve_conflict_affine_affine(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.compute_conflict!)
    @requires _supports(config, MOI.optimize!)
    try
        MOI.get(model, MOI.ConflictStatus())
    catch
        return  # If this fails, skip the test.
    end
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    b1 = MOI.add_constraint(model, x, MOI.GreaterThan(zero(T)))
    b2 = MOI.add_constraint(model, y, MOI.GreaterThan(zero(T)))
    cf1 =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(one(T), [x, y]), zero(T))
    c1 = MOI.add_constraint(model, cf1, MOI.LessThan(-one(T)))
    cf2 = MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.([one(T), -one(T)], [x, y]),
        zero(T),
    )
    c2 = MOI.add_constraint(model, cf2, MOI.GreaterThan(one(T)))
    @test MOI.get(model, MOI.ConflictStatus()) ==
          MOI.COMPUTE_CONFLICT_NOT_CALLED
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
    MOI.compute_conflict!(model)
    @test MOI.get(model, MOI.ConflictStatus()) == MOI.CONFLICT_FOUND
    @test MOI.get(model, MOI.ConstraintConflictStatus(), b1) == MOI.IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), b2) == MOI.IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c1) == MOI.IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c2) ==
          MOI.NOT_IN_CONFLICT
    return
end

function setup_test(
    ::typeof(test_solve_conflict_affine_affine),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        mock::MOIU.MockOptimizer -> begin
            MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                MOI.NO_SOLUTION,
                MOI.NO_SOLUTION;
                constraint_conflict_status = [
                    (MOI.VariableIndex, MOI.GreaterThan{Float64}) =>
                        [MOI.IN_CONFLICT, MOI.IN_CONFLICT],
                    (
                        MOI.ScalarAffineFunction{Float64},
                        MOI.LessThan{Float64},
                    ) => [MOI.IN_CONFLICT],
                    (
                        MOI.ScalarAffineFunction{Float64},
                        MOI.GreaterThan{Float64},
                    ) => [MOI.NOT_IN_CONFLICT],
                ],
            )
            MOI.set(mock, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
        end,
    )
    return
end

"""
    test_solve_conflict_EqualTo(model::MOI.ModelLike, config::Config)

Test the ConflictStatus API when some constraints are EqualTo.
"""
function test_solve_conflict_EqualTo(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.compute_conflict!)
    @requires _supports(config, MOI.optimize!)
    try
        MOI.get(model, MOI.ConflictStatus())
    catch
        return  # If this fails, skip the test.
    end
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    b1 = MOI.add_constraint(model, x, MOI.GreaterThan(zero(T)))
    b2 = MOI.add_constraint(model, y, MOI.GreaterThan(zero(T)))
    cf1 =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(one(T), [x, y]), zero(T))
    c1 = MOI.add_constraint(model, cf1, MOI.EqualTo(-one(T)))
    cf2 = MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.([one(T), -one(T)], [x, y]),
        zero(T),
    )
    c2 = MOI.add_constraint(model, cf2, MOI.GreaterThan(one(T)))
    @test MOI.get(model, MOI.ConflictStatus()) ==
          MOI.COMPUTE_CONFLICT_NOT_CALLED
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
    MOI.compute_conflict!(model)
    @test MOI.get(model, MOI.ConflictStatus()) == MOI.CONFLICT_FOUND
    @test MOI.get(model, MOI.ConstraintConflictStatus(), b1) == MOI.IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), b2) == MOI.IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c1) == MOI.IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c2) ==
          MOI.NOT_IN_CONFLICT
    return
end

function setup_test(
    ::typeof(test_solve_conflict_EqualTo),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        mock::MOIU.MockOptimizer -> begin
            MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                MOI.NO_SOLUTION,
                MOI.NO_SOLUTION;
                constraint_conflict_status = [
                    (MOI.VariableIndex, MOI.GreaterThan{Float64}) =>
                        [MOI.IN_CONFLICT, MOI.IN_CONFLICT],
                    (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) => [MOI.IN_CONFLICT],
                    (
                        MOI.ScalarAffineFunction{Float64},
                        MOI.GreaterThan{Float64},
                    ) => [MOI.NOT_IN_CONFLICT],
                ],
            )
            MOI.set(mock, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
        end,
    )
    return
end

"""
    test_solve_conflict_NOT_IN_CONFLICT(model::MOI.ModelLike, config::Config)

Test the ConflictStatus API when some constraints are not in the conlict.
"""
function test_solve_conflict_NOT_IN_CONFLICT(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.compute_conflict!)
    @requires _supports(config, MOI.optimize!)
    try
        MOI.get(model, MOI.ConflictStatus())
    catch
        return  # If this fails, skip the test.
    end
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    z = MOI.add_variable(model)
    S = MOI.GreaterThan(zero(T))
    b1 = MOI.add_constraint(model, x, S)
    b2 = MOI.add_constraint(model, y, S)
    b3 = MOI.add_constraint(model, z, S)
    cf1 =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(one(T), [x, y]), zero(T))
    c1 = MOI.add_constraint(model, cf1, MOI.LessThan(-one(T)))
    cf2 = MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.([one(T), -one(T), one(T)], [x, y, z]),
        zero(T),
    )
    c2 = MOI.add_constraint(model, cf2, MOI.GreaterThan(one(T)))
    @test MOI.get(model, MOI.ConflictStatus()) ==
          MOI.COMPUTE_CONFLICT_NOT_CALLED
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
    MOI.compute_conflict!(model)
    @test MOI.get(model, MOI.ConflictStatus()) == MOI.CONFLICT_FOUND
    @test MOI.get(model, MOI.ConstraintConflictStatus(), b1) == MOI.IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), b2) == MOI.IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), b3) ==
          MOI.NOT_IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c1) == MOI.IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c2) ==
          MOI.NOT_IN_CONFLICT
    return
end

function setup_test(
    ::typeof(test_solve_conflict_NOT_IN_CONFLICT),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        mock::MOIU.MockOptimizer -> begin
            MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                MOI.NO_SOLUTION,
                MOI.NO_SOLUTION;
                constraint_conflict_status = [
                    (MOI.VariableIndex, MOI.GreaterThan{Float64}) => [
                        MOI.IN_CONFLICT,
                        MOI.IN_CONFLICT,
                        MOI.NOT_IN_CONFLICT,
                    ],
                    (
                        MOI.ScalarAffineFunction{Float64},
                        MOI.LessThan{Float64},
                    ) => [MOI.IN_CONFLICT],
                    (
                        MOI.ScalarAffineFunction{Float64},
                        MOI.GreaterThan{Float64},
                    ) => [MOI.NOT_IN_CONFLICT],
                ],
            )
            MOI.set(mock, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
        end,
    )
    return
end

"""
    test_solve_conflict_feasible(model::MOI.ModelLike, config::Config)

Test the ConflictStatus API when the problem is feasible.
"""
function test_solve_conflict_feasible(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.compute_conflict!)
    @requires _supports(config, MOI.optimize!)
    try
        MOI.get(model, MOI.ConflictStatus())
    catch
        return  # If this fails, skip the test.
    end
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(one(T), x)], zero(T))
    _ = MOI.add_constraint(model, f, MOI.GreaterThan(one(T)))
    _ = MOI.add_constraint(model, f, MOI.LessThan(T(2)))
    @test MOI.get(model, MOI.ConflictStatus()) ==
          MOI.COMPUTE_CONFLICT_NOT_CALLED
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    MOI.compute_conflict!(model)
    @test MOI.get(model, MOI.ConflictStatus()) == MOI.NO_CONFLICT_EXISTS
    return
end

function setup_test(
    ::typeof(test_solve_conflict_feasible),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        mock::MOIU.MockOptimizer -> begin
            MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                MOI.NO_SOLUTION,
                MOI.NO_SOLUTION,
            )
            MOI.set(mock, MOI.ConflictStatus(), MOI.NO_CONFLICT_EXISTS)
        end,
    )
    return
end

"""
    test_solve_conflict_zeroone(model::MOI.ModelLike, config::Config)

Test the ConflictStatus API when an integrality is in the conflict.
"""
function test_solve_conflict_zeroone(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.compute_conflict!)
    @requires _supports(config, MOI.optimize!)
    try
        MOI.get(model, MOI.ConflictStatus())
    catch
        return  # If this fails, skip the test.
    end
    x, c1 = MOI.add_constrained_variable(model, MOI.ZeroOne())
    c2 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(one(T), [x]), zero(T)),
        MOI.GreaterThan(T(2)),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
    MOI.compute_conflict!(model)
    @test MOI.get(model, MOI.ConflictStatus()) == MOI.CONFLICT_FOUND
    zeroone_conflict = MOI.get(model, MOI.ConstraintConflictStatus(), c1)
    @test zeroone_conflict == MOI.MAYBE_IN_CONFLICT ||
          zeroone_conflict == MOI.IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c2) == MOI.IN_CONFLICT
    return
end

function setup_test(
    ::typeof(test_solve_conflict_zeroone),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        mock::MOIU.MockOptimizer -> begin
            MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                MOI.NO_SOLUTION,
                MOI.NO_SOLUTION;
                constraint_conflict_status = [
                    (MOI.VariableIndex, MOI.ZeroOne) =>
                        [MOI.MAYBE_IN_CONFLICT],
                    (
                        MOI.ScalarAffineFunction{Float64},
                        MOI.GreaterThan{Float64},
                    ) => [MOI.IN_CONFLICT],
                ],
            )
            MOI.set(mock, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
        end,
    )
    return
end

"""
    test_solve_conflict_zeroone_ii(model::MOI.ModelLike, config::Config)

Test the ConflictStatus API when an integrality is in the conflict.
In this test, integrality is the conflict and no the upper bound == 1.
"""
function test_solve_conflict_zeroone_ii(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires !(T <: Integer)
    @requires _supports(config, MOI.compute_conflict!)
    @requires _supports(config, MOI.optimize!)
    try
        MOI.get(model, MOI.ConflictStatus())
    catch
        return  # If this fails, skip the test.
    end
    x, c1 = MOI.add_constrained_variable(model, MOI.ZeroOne())
    c2 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(one(T), [x]), zero(T)),
        MOI.EqualTo(div(one(T), T(2))),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
    MOI.compute_conflict!(model)
    @test MOI.get(model, MOI.ConflictStatus()) == MOI.CONFLICT_FOUND
    zeroone_conflict = MOI.get(model, MOI.ConstraintConflictStatus(), c1)
    @test zeroone_conflict == MOI.MAYBE_IN_CONFLICT ||
          zeroone_conflict == MOI.IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c2) == MOI.IN_CONFLICT
    return
end

function setup_test(
    ::typeof(test_solve_conflict_zeroone_ii),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        mock::MOIU.MockOptimizer -> begin
            MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                MOI.NO_SOLUTION,
                MOI.NO_SOLUTION;
                constraint_conflict_status = [
                    (MOI.VariableIndex, MOI.ZeroOne) => [MOI.IN_CONFLICT],
                    (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) => [MOI.IN_CONFLICT],
                ],
            )
            MOI.set(mock, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
        end,
    )
    return
end

function test_solve_SOS2_add_and_delete(model::MOI.ModelLike, config::Config)
    @requires _supports(config, MOI.optimize!)
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.SOS2{Float64},
    )
    function _add_SOS2(model, x, y, xp, yp)
        λ = MOI.add_variables(model, length(xp))
        # 0 <= λ <= 1
        MOI.add_constraint.(model, λ, MOI.LessThan(1.0))
        MOI.add_constraint.(model, λ, MOI.GreaterThan(0.0))
        # Σλᵢ == 1
        MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, λ), 0.0),
            MOI.EqualTo(1.0),
        )
        # Σλᵢxᵢ - x == 0
        MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction(
                MOI.ScalarAffineTerm.([xp; -1.0], [λ; x]),
                0.0,
            ),
            MOI.EqualTo(0.0),
        )
        # Σλᵢyᵢ - y  == 0
        MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction(
                MOI.ScalarAffineTerm.([yp; -1.0], [λ; y]),
                0.0,
            ),
            MOI.EqualTo(0.0),
        )
        # λ ∈ SOS2()
        c = MOI.add_constraint(
            model,
            MOI.VectorOfVariables(λ),
            MOI.SOS2{Float64}(collect(1.0:length(xp))),
        )
        return λ, c
    end
    x = MOI.add_variables(model, 2)
    y = MOI.add_variables(model, 2)
    # !!! warning
    #     Make sure the variable returned from `_add_SOS2` is named something
    #     other than λ to avoid Julia's `Box`ing issue with closed over
    #     variables.
    vλ, c1 = _add_SOS2(model, x[1], y[1], [1.0, 2.0, 3.0], [2.0, 2.0, 1.0])
    MOI.add_constraint(model, x[1], MOI.LessThan(2.5))
    vη, c2 =
        _add_SOS2(model, x[2], y[2], [1.0, 2.0, 3.0, 4.0], [0.5, 1.0, 0.2, 2.0])
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, y), 0.0)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    attr = MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SOS2{Float64}}()
    @test MOI.get(model, attr) == 2
    MOI.optimize!(model)
    #  x[1] == 2.5
    #  y[1] == 1.5
    #  x[2] == 3.0
    #  y[2] == 0.2
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), x[1]), 2.5, config)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), x[2]), 3.0, config)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), y[1]), 1.5, config)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), y[2]), 0.2, config)
    @test isapprox(MOI.get(model, MOI.ObjectiveValue()), 1.7, config)
    MOI.delete(model, c1)
    MOI.delete(model, c2)
    @test MOI.get(model, attr) == 0
    MOI.optimize!(model)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), x[1]), 2.5, config)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), x[2]), 3.0, config)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), y[1]), 1.25, config)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), y[2]), 0.2, config)
    @test isapprox(MOI.get(model, MOI.ObjectiveValue()), 1.45, config)
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(vλ),
        MOI.SOS2{Float64}([1.0, 2.0, 3.0]),
    )
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(vη),
        MOI.SOS2{Float64}([1.0, 2.0, 3.0, 4.0]),
    )
    @test MOI.get(model, attr) == 2
    MOI.optimize!(model)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), x[1]), 2.5, config)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), x[2]), 3.0, config)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), y[1]), 1.5, config)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), y[2]), 0.2, config)
    @test isapprox(MOI.get(model, MOI.ObjectiveValue()), 1.7, config)
    return
end

function setup_test(
    ::typeof(test_solve_SOS2_add_and_delete),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            [2.5, 3.0, 1.5, 0.2, 0.0, 0.5, 0.5, 0.0, 0.0, 1.0, 0.0],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            [2.5, 3.0, 1.25, 0.2, 0.25, 0.0, 0.75, 0.0, 0.0, 1.0, 0.0],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            [2.5, 3.0, 1.5, 0.2, 0.0, 0.5, 0.5, 0.0, 0.0, 1.0, 0.0],
        ),
    )
    return
end
