# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    test_solve_ObjectiveBound_MIN_SENSE_IP(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test a variety of edge cases related to the ObjectiveBound attribute.
"""
function test_solve_ObjectiveBound_MIN_SENSE_IP(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ObjectiveBound)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.GreaterThan(T(3 // 2)))
    MOI.add_constraint(model, x, MOI.Integer())
    f = T(2) * x + T(-1)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    _test_model_solution(
        model,
        config;
        objective_value = T(3),
        variable_primal = [(x, T(2))],
    )
    @test MOI.get(model, MOI.ObjectiveBound()) <= T(3)
    return
end

function setup_test(
    ::typeof(test_solve_ObjectiveBound_MIN_SENSE_IP),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), T(3))
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, T[2]))
        end,
    )
    return
end

"""
    test_solve_ObjectiveBound_MAX_SENSE_IP(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test a variety of edge cases related to the ObjectiveBound attribute.
"""
function test_solve_ObjectiveBound_MAX_SENSE_IP(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ObjectiveBound)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.LessThan(T(3 // 2)))
    MOI.add_constraint(model, x, MOI.Integer())
    f = T(2) * x + T(1)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    _test_model_solution(
        model,
        config;
        objective_value = T(3),
        variable_primal = [(x, T(1))],
    )
    @test MOI.get(model, MOI.ObjectiveBound()) >= T(3)
    return
end

function setup_test(
    ::typeof(test_solve_ObjectiveBound_MAX_SENSE_IP),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), T(3))
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, T[1]))
        end,
    )
    return
end

"""
    test_solve_ObjectiveBound_MIN_SENSE_LP(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test a variety of edge cases related to the ObjectiveBound attribute.
"""
function test_solve_ObjectiveBound_MIN_SENSE_LP(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ObjectiveBound)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.GreaterThan(T(3 // 2)))
    f = T(2) * x + T(-1)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    _test_model_solution(
        model,
        config;
        objective_value = T(2),
        variable_primal = [(x, T(3 // 2))],
    )
    @test MOI.get(model, MOI.ObjectiveBound()) <= T(2)
end

function setup_test(
    ::typeof(test_solve_ObjectiveBound_MIN_SENSE_LP),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), T(2))
            MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, T[3//2]),
            )
        end,
    )
    return
end

"""
    test_solve_ObjectiveBound_MIN_SENSE_LP(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test a variety of edge cases related to the ObjectiveBound attribute.
"""
function test_solve_ObjectiveBound_MAX_SENSE_LP(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ObjectiveBound)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.LessThan(T(3 // 2)))
    f = T(2) * x + T(1)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    _test_model_solution(
        model,
        config;
        objective_value = T(4),
        variable_primal = [(x, T(3 // 2))],
    )
    @test MOI.get(model, MOI.ObjectiveBound()) >= T(4)
    return
end

function setup_test(
    ::typeof(test_solve_ObjectiveBound_MAX_SENSE_LP),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), T(4))
            MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, T[3//2]),
            )
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
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    x = MOI.add_variables(model, 5)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T(1), x), T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.optimize!(model)
    status = MOI.get(model, MOI.TerminationStatus())
    @test in(
        status,
        (MOI.DUAL_INFEASIBLE, MOI.NORM_LIMIT, MOI.INFEASIBLE_OR_UNBOUNDED),
    )
    return
end

function setup_test(
    ::typeof(test_solve_TerminationStatus_DUAL_INFEASIBLE),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
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
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variable(model)
    xl = MOI.add_constraint(model, x, MOI.GreaterThan(T(1)))
    xu = MOI.add_constraint(model, x, MOI.LessThan(T(1)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    MOI.optimize!(model)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(1), atol = config.atol)
    sl = MOI.get(model, MOI.ConstraintDual(), xl)
    su = MOI.get(model, MOI.ConstraintDual(), xu)
    @test ≈(sl + su, T(1), atol = config.atol)
    @test sl >= -config.atol
    @test su <= config.atol
    return
end

function setup_test(
    ::typeof(test_solve_VariableIndex_ConstraintDual_MIN_SENSE),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    flag = model.eval_variable_constraint_dual
    model.eval_variable_constraint_dual = false
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1]),
            MOI.FEASIBLE_POINT,
            (MOI.VariableIndex, MOI.GreaterThan{T}) => T[1],
            (MOI.VariableIndex, MOI.LessThan{T}) => T[0],
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
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variable(model)
    xl = MOI.add_constraint(model, x, MOI.GreaterThan(T(1)))
    xu = MOI.add_constraint(model, x, MOI.LessThan(T(1)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    MOI.optimize!(model)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(1), atol = config.atol)
    sl = MOI.get(model, MOI.ConstraintDual(), xl)
    su = MOI.get(model, MOI.ConstraintDual(), xu)
    @test ≈(sl + su, T(-1), atol = config.atol)
    @test sl >= -config.atol
    @test su <= config.atol
    return
end

function setup_test(
    ::typeof(test_solve_VariableIndex_ConstraintDual_MAX_SENSE),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    flag = model.eval_variable_constraint_dual
    model.eval_variable_constraint_dual = false
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1]),
            MOI.FEASIBLE_POINT,
            (MOI.VariableIndex, MOI.GreaterThan{T}) => T[0],
            (MOI.VariableIndex, MOI.LessThan{T}) => T[-1],
        ),
    )
    return () -> model.eval_variable_constraint_dual = flag
end

"""
    test_solve_result_index(model::MOI.ModelLike, config::Config{T}) where {T}

Test that various attributes implement `.result_index` correctly.
"""
function test_solve_result_index(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    atol = config.atol
    rtol = config.rtol
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, x, MOI.GreaterThan(T(1)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    MOI.optimize!(model)
    result_count = MOI.get(model, MOI.ResultCount())
    _test_attribute_value_type(model, MOI.ResultCount())
    function result_err(attr)
        return MOI.ResultIndexBoundsError{typeof(attr)}(attr, result_count)
    end
    result_index = result_count + 1
    @test ≈(MOI.get(model, MOI.ObjectiveValue(1)), T(1), config)
    @test_throws result_err(MOI.ObjectiveValue(result_index)) MOI.get(
        model,
        MOI.ObjectiveValue(result_index),
    )
    if _supports(config, MOI.DualObjectiveValue)
        @test ≈(MOI.get(model, MOI.DualObjectiveValue(1)), T(1), config)
        @test_throws(
            result_err(MOI.DualObjectiveValue(result_index)),
            MOI.get(model, MOI.DualObjectiveValue(result_index)),
        )
    end
    @test MOI.get(model, MOI.PrimalStatus(1)) == MOI.FEASIBLE_POINT
    @test MOI.get(model, MOI.PrimalStatus(result_index)) == MOI.NO_SOLUTION
    @test MOI.get(model, MOI.VariablePrimal(1), x) ≈ T(1) atol = atol rtol =
        rtol
    @test_throws result_err(MOI.VariablePrimal(result_index)) MOI.get(
        model,
        MOI.VariablePrimal(result_index),
        x,
    )
    @test MOI.get(model, MOI.ConstraintPrimal(1), c) ≈ T(1) atol = atol rtol =
        rtol
    @test_throws result_err(MOI.ConstraintPrimal(result_index)) MOI.get(
        model,
        MOI.ConstraintPrimal(result_index),
        c,
    )
    if _supports(config, MOI.ConstraintDual)
        @test MOI.get(model, MOI.DualStatus(1)) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.DualStatus(result_index)) == MOI.NO_SOLUTION
        @test MOI.get(model, MOI.ConstraintDual(1), c) ≈ T(1) atol = atol rtol =
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
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1]),
            MOI.FEASIBLE_POINT,
            (MOI.VariableIndex, MOI.GreaterThan{T}) => T[1],
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
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variables(model, 2)
    clb = MOI.add_constraint.(model, x, MOI.GreaterThan(T(0)))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[2, 1], x), T(0)),
        MOI.EqualTo(T(-1)),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.infeasible_status
    @requires MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
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
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            (MOI.NO_SOLUTION, [NaN, NaN]),
            MOI.INFEASIBILITY_CERTIFICATE,
            (MOI.VariableIndex, MOI.GreaterThan{T}) => T[2, 1],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => T[-1],
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
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variables(model, 2)
    clb = MOI.add_constraint.(model, x, MOI.GreaterThan(T(0)))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[-2, -1], x), T(0)),
        MOI.EqualTo(T(1)),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.infeasible_status
    @requires MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
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
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            MOI.NO_SOLUTION,
            MOI.INFEASIBILITY_CERTIFICATE,
            (MOI.VariableIndex, MOI.GreaterThan{T}) => T[2, 1],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => T[1],
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
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variables(model, 2)
    clb = MOI.add_constraint.(model, x, MOI.GreaterThan(T(0)))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[2, 1], x), T(0)),
        MOI.LessThan(T(-1)),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.infeasible_status
    @requires MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
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
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            (MOI.NO_SOLUTION, [NaN, NaN]),
            MOI.INFEASIBILITY_CERTIFICATE,
            (MOI.VariableIndex, MOI.GreaterThan{T}) => T[2, 1],
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-1],
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
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variables(model, 2)
    clb = MOI.add_constraint.(model, x, MOI.GreaterThan(T(0)))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[-2, -1], x), T(0)),
        MOI.GreaterThan(T(1)),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.infeasible_status
    @requires MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
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
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            (MOI.NO_SOLUTION, [NaN, NaN]),
            MOI.INFEASIBILITY_CERTIFICATE,
            (MOI.VariableIndex, MOI.GreaterThan{T}) => T[2, 1],
            (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}) => T[1],
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
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variables(model, 2)
    clb = MOI.add_constraint.(model, x, MOI.GreaterThan(T(0)))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[2, 1], x), T(0)),
        MOI.Interval(T(-2), T(-1)),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.infeasible_status
    @requires MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
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
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            (MOI.NO_SOLUTION, [NaN, NaN]),
            MOI.INFEASIBILITY_CERTIFICATE,
            (MOI.VariableIndex, MOI.GreaterThan{T}) => T[2, 1],
            (MOI.ScalarAffineFunction{T}, MOI.Interval{T}) => T[-1],
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
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variables(model, 2)
    clb = MOI.add_constraint.(model, x, MOI.GreaterThan(T(0)))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[-2, -1], x), T(0)),
        MOI.Interval(T(1), T(2)),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.infeasible_status
    @requires MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
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
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            (MOI.NO_SOLUTION, [NaN, NaN]),
            MOI.INFEASIBILITY_CERTIFICATE,
            (MOI.VariableIndex, MOI.GreaterThan{T}) => T[2, 1],
            (MOI.ScalarAffineFunction{T}, MOI.Interval{T}) => T[1],
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
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variables(model, 2)
    clb = MOI.add_constraint.(model, x, MOI.LessThan(T(0)))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[2, 1], x), T(0)),
        MOI.GreaterThan(T(1)),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.infeasible_status
    @requires MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
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
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            (MOI.NO_SOLUTION, [NaN, NaN]),
            MOI.INFEASIBILITY_CERTIFICATE,
            (MOI.VariableIndex, MOI.LessThan{T}) => T[-2, -1],
            (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}) => T[1],
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
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    x = MOI.add_variables(model, 2)
    clb = MOI.add_constraint.(model, x, MOI.LessThan(T(0)))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[2, 1], x), T(0)),
        MOI.GreaterThan(T(1)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x[1])
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.infeasible_status
    @requires MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
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
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            (MOI.NO_SOLUTION, [NaN, NaN]),
            MOI.INFEASIBILITY_CERTIFICATE,
            (MOI.VariableIndex, MOI.LessThan{T}) => T[-2, -1],
            (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}) => T[1],
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
    MOI.add_constraint(model, x, MOI.GreaterThan(T(1)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    MOI.optimize!(model)
    MOI.optimize!(model)
    MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    MOI.get(model, MOI.VariablePrimal(), x) == T(1)
    return
end

function setup_test(
    ::typeof(test_solve_optimize_twice),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, T[1])),
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, T[1])),
    )
    return
end

"""
    test_solve_conflict_bound_bound(model::MOI.ModelLike, config::Config{T}) where {T}

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
    c2 = MOI.add_constraint(model, x, MOI.LessThan(T(1)))
    @test MOI.get(model, MOI.ConflictStatus()) ==
          MOI.COMPUTE_CONFLICT_NOT_CALLED
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.infeasible_status
    MOI.compute_conflict!(model)
    @test MOI.get(model, MOI.ConflictStatus()) == MOI.CONFLICT_FOUND
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c1) == MOI.IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c2) == MOI.IN_CONFLICT
    return
end

function setup_test(
    ::typeof(test_solve_conflict_bound_bound),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        mock::MOIU.MockOptimizer -> begin
            MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                MOI.NO_SOLUTION,
                MOI.NO_SOLUTION;
                constraint_conflict_status = [
                    (MOI.VariableIndex, MOI.LessThan{T}) =>
                        [MOI.IN_CONFLICT],
                    (MOI.VariableIndex, MOI.GreaterThan{T}) =>
                        [MOI.IN_CONFLICT],
                ],
            )
            MOI.set(mock, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
        end,
    )
    return
end

"""
    test_solve_conflict_two_affine(model::MOI.ModelLike, config::Config{T}) where {T}

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
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T(1), [x]), T(0)),
        MOI.GreaterThan(T(2)),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T(1), [x]), T(0)),
        MOI.LessThan(T(1)),
    )
    @test MOI.get(model, MOI.ConflictStatus()) ==
          MOI.COMPUTE_CONFLICT_NOT_CALLED
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.infeasible_status
    MOI.compute_conflict!(model)
    @test MOI.get(model, MOI.ConflictStatus()) == MOI.CONFLICT_FOUND
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c1) == MOI.IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c2) == MOI.IN_CONFLICT
    return
end

function setup_test(
    ::typeof(test_solve_conflict_two_affine),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        mock::MOIU.MockOptimizer -> begin
            MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                MOI.NO_SOLUTION,
                MOI.NO_SOLUTION;
                constraint_conflict_status = [
                    (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) =>
                        [MOI.IN_CONFLICT],
                    (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}) =>
                        [MOI.IN_CONFLICT],
                ],
            )
            MOI.set(mock, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
        end,
    )
    return
end

"""
    test_solve_conflict_invalid_interval(model::MOI.ModelLike, config::Config{T}) where {T}

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
    c1 = MOI.add_constraint(model, x, MOI.Interval(T(1), T(0)))
    @test MOI.get(model, MOI.ConflictStatus()) ==
          MOI.COMPUTE_CONFLICT_NOT_CALLED
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.infeasible_status
    MOI.compute_conflict!(model)
    @test MOI.get(model, MOI.ConflictStatus()) == MOI.CONFLICT_FOUND
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c1) == MOI.IN_CONFLICT
    return
end

function setup_test(
    ::typeof(test_solve_conflict_invalid_interval),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        mock::MOIU.MockOptimizer -> begin
            MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                MOI.NO_SOLUTION,
                MOI.NO_SOLUTION;
                constraint_conflict_status = [
                    (MOI.VariableIndex, MOI.Interval{T}) =>
                        [MOI.IN_CONFLICT],
                ],
            )
            MOI.set(mock, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
        end,
    )
    return
end

"""
    test_solve_conflict_affine_affine(model::MOI.ModelLike, config::Config{T}) where {T}

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
    b1 = MOI.add_constraint(model, x, MOI.GreaterThan(T(0)))
    b2 = MOI.add_constraint(model, y, MOI.GreaterThan(T(0)))
    cf1 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T(1), [x, y]), T(0))
    c1 = MOI.add_constraint(model, cf1, MOI.LessThan(T(-1)))
    cf2 =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[1, -1], [x, y]), T(0))
    c2 = MOI.add_constraint(model, cf2, MOI.GreaterThan(T(1)))
    @test MOI.get(model, MOI.ConflictStatus()) ==
          MOI.COMPUTE_CONFLICT_NOT_CALLED
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.infeasible_status
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
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        mock::MOIU.MockOptimizer -> begin
            MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                MOI.NO_SOLUTION,
                MOI.NO_SOLUTION;
                constraint_conflict_status = [
                    (MOI.VariableIndex, MOI.GreaterThan{T}) =>
                        [MOI.IN_CONFLICT, MOI.IN_CONFLICT],
                    (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) =>
                        [MOI.IN_CONFLICT],
                    (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}) =>
                        [MOI.NOT_IN_CONFLICT],
                ],
            )
            MOI.set(mock, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
        end,
    )
    return
end

"""
    test_solve_conflict_EqualTo(model::MOI.ModelLike, config::Config{T}) where {T}

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
    b1 = MOI.add_constraint(model, x, MOI.GreaterThan(T(0)))
    b2 = MOI.add_constraint(model, y, MOI.GreaterThan(T(0)))
    cf1 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T(1), [x, y]), T(0))
    c1 = MOI.add_constraint(model, cf1, MOI.EqualTo(T(-1)))
    cf2 =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[1, -1], [x, y]), T(0))
    c2 = MOI.add_constraint(model, cf2, MOI.GreaterThan(T(1)))
    @test MOI.get(model, MOI.ConflictStatus()) ==
          MOI.COMPUTE_CONFLICT_NOT_CALLED
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.infeasible_status
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
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        mock::MOIU.MockOptimizer -> begin
            MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                MOI.NO_SOLUTION,
                MOI.NO_SOLUTION;
                constraint_conflict_status = [
                    (MOI.VariableIndex, MOI.GreaterThan{T}) =>
                        [MOI.IN_CONFLICT, MOI.IN_CONFLICT],
                    (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) =>
                        [MOI.IN_CONFLICT],
                    (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}) =>
                        [MOI.NOT_IN_CONFLICT],
                ],
            )
            MOI.set(mock, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
        end,
    )
    return
end

"""
    test_solve_conflict_NOT_IN_CONFLICT(model::MOI.ModelLike, config::Config{T}) where {T}

Test the ConflictStatus API when some constraints are not in the conflict.
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
    S = MOI.GreaterThan(T(0))
    b1 = MOI.add_constraint(model, x, S)
    b2 = MOI.add_constraint(model, y, S)
    b3 = MOI.add_constraint(model, z, S)
    cf1 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T(1), [x, y]), T(0))
    c1 = MOI.add_constraint(model, cf1, MOI.LessThan(T(-1)))
    cf2 = MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.(T[1, -1, 1], [x, y, z]),
        T(0),
    )
    c2 = MOI.add_constraint(model, cf2, MOI.GreaterThan(T(1)))
    @test MOI.get(model, MOI.ConflictStatus()) ==
          MOI.COMPUTE_CONFLICT_NOT_CALLED
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.infeasible_status
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
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        mock::MOIU.MockOptimizer -> begin
            MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                MOI.NO_SOLUTION,
                MOI.NO_SOLUTION;
                constraint_conflict_status = [
                    (MOI.VariableIndex, MOI.GreaterThan{T}) => [
                        MOI.IN_CONFLICT,
                        MOI.IN_CONFLICT,
                        MOI.NOT_IN_CONFLICT,
                    ],
                    (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) =>
                        [MOI.IN_CONFLICT],
                    (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}) =>
                        [MOI.NOT_IN_CONFLICT],
                ],
            )
            MOI.set(mock, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
        end,
    )
    return
end

"""
    test_solve_conflict_feasible(model::MOI.ModelLike, config::Config{T}) where {T}

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
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x)], T(0))
    _ = MOI.add_constraint(model, f, MOI.GreaterThan(T(1)))
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
    ::Config{T},
) where {T}
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
    test_solve_conflict_zeroone(model::MOI.ModelLike, config::Config{T}) where {T}

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
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T(1), [x]), T(0)),
        MOI.GreaterThan(T(2)),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.infeasible_status
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
    ::Config{T},
) where {T}
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
                    (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}) =>
                        [MOI.IN_CONFLICT],
                ],
            )
            MOI.set(mock, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
        end,
    )
    return
end

"""
    test_solve_conflict_zeroone_2(model::MOI.ModelLike, config::Config{T}) where {T}

Test the ConflictStatus API when an integrality is in the conflict.
In this test, integrality is the conflict and no the upper bound == 1.
"""
function test_solve_conflict_zeroone_2(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires (T(1) / T(2)) isa T
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
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T(1), [x]), T(0)),
        MOI.EqualTo(T(1) / T(2)),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.infeasible_status
    MOI.compute_conflict!(model)
    @test MOI.get(model, MOI.ConflictStatus()) == MOI.CONFLICT_FOUND
    zeroone_conflict = MOI.get(model, MOI.ConstraintConflictStatus(), c1)
    @test zeroone_conflict == MOI.MAYBE_IN_CONFLICT ||
          zeroone_conflict == MOI.IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c2) == MOI.IN_CONFLICT
    return
end

version_added(::typeof(test_solve_conflict_zeroone_2)) = v"0.10.6"

function setup_test(
    ::typeof(test_solve_conflict_zeroone_2),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
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
                    (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) =>
                        [MOI.IN_CONFLICT],
                ],
            )
            MOI.set(mock, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
        end,
    )
    return
end

function test_solve_SOS2_add_and_delete(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.SOS2{T})
    function _add_SOS2(model, x, y, xp, yp)
        λ = MOI.add_variables(model, length(xp))
        # 0 <= λ <= 1
        MOI.add_constraint.(model, λ, MOI.LessThan(T(1)))
        MOI.add_constraint.(model, λ, MOI.GreaterThan(T(0)))
        # Σλᵢ == 1
        MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T(1), λ), T(0)),
            MOI.EqualTo(T(1)),
        )
        # Σλᵢxᵢ - x == 0
        MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction(
                MOI.ScalarAffineTerm.([xp; T(-1)], [λ; x]),
                T(0),
            ),
            MOI.EqualTo(T(0)),
        )
        # Σλᵢyᵢ - y  == 0
        MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction(
                MOI.ScalarAffineTerm.([yp; T(-1)], [λ; y]),
                T(0),
            ),
            MOI.EqualTo(T(0)),
        )
        # λ ∈ SOS2()
        c = MOI.add_constraint(
            model,
            MOI.VectorOfVariables(λ),
            MOI.SOS2{T}(collect(T(1):length(xp))),
        )
        return λ, c
    end
    x = MOI.add_variables(model, 2)
    y = MOI.add_variables(model, 2)
    # !!! warning
    #     Make sure the variable returned from `_add_SOS2` is named something
    #     other than λ to avoid Julia's `Box`ing issue with closed over
    #     variables.
    vλ, c1 = _add_SOS2(model, x[1], y[1], T[1, 2, 3], T[2, 2, 1])
    MOI.add_constraint(model, x[1], MOI.LessThan(T(5 // 2)))
    vη, c2 = _add_SOS2(model, x[2], y[2], T[1, 2, 3, 4], T[1//2, 1, 1//5, 2])
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T(1), y), T(0))
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    attr = MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SOS2{T}}()
    @test MOI.get(model, attr) == 2
    MOI.optimize!(model)
    #  x[1] == 2.5
    #  y[1] == 1.5
    #  x[2] == 3.0
    #  y[2] == 0.2
    @test ≈(MOI.get(model, MOI.VariablePrimal(), x[1]), T(5 // 2), config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), x[2]), T(3), config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), y[1]), T(3 // 2), config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), y[2]), T(1 // 5), config)
    @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(17 // 10), config)
    MOI.delete(model, c1)
    MOI.delete(model, c2)
    @test MOI.get(model, attr) == 0
    MOI.optimize!(model)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), x[1]), T(5 // 2), config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), x[2]), T(3), config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), y[1]), T(5 // 4), config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), y[2]), T(1 // 5), config)
    @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(29 // 20), config)
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(vλ),
        MOI.SOS2{T}(T[1, 2, 3]),
    )
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(vη),
        MOI.SOS2{T}(T[1, 2, 3, 4]),
    )
    @test MOI.get(model, attr) == 2
    MOI.optimize!(model)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), x[1]), T(5 // 2), config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), x[2]), T(3), config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), y[1]), T(3 // 2), config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), y[2]), T(1 // 5), config)
    @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(17 // 10), config)
    return
end

function setup_test(
    ::typeof(test_solve_SOS2_add_and_delete),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            T[5//2, 3, 3//2, 1//5, 0, 1//2, 1//2, 0, 0, 1, 0],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            T[5//2, 3, 5//4, 1//5, 1//4, 0, 3//4, 0, 0, 1, 0],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            T[5//2, 3, 3//2, 1//5, 0, 1//2, 1//2, 0, 0, 1, 0],
        ),
    )
    return
end

function _test_matrix_norm_cone(
    model::MOI.ModelLike,
    config::Config{T},
    set::Union{MOI.NormSpectralCone,MOI.NormNuclearCone},
    b::Union{Nothing,Vector{T}},
) where {T}
    @requires _supports(config, MOI.optimize!)
    F = b == nothing ? MOI.VectorOfVariables : MOI.VectorAffineFunction{T}
    @requires MOI.supports_constraint(model, F, typeof(set))
    t = MOI.add_variable(model)
    X = MOI.add_variables(model, 6)
    f = if b === nothing
        MOI.VectorOfVariables([t; X])
    else
        fx = MOI.Utilities.operate.(+, T, X, b)
        MOI.Utilities.operate(vcat, T, t, fx...)
    end
    MOI.add_constraint(model, f, set)
    for i in 1:6
        MOI.add_constraint(model, X[i], MOI.EqualTo{T}(i))
    end
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.optimize!(model)
    x_result = MOI.get(model, MOI.VariablePrimal(), X)
    @test ≈(x_result, 1:6, config)
    Y = reshape(x_result .+ something(b, T(0)), set.row_dim, set.column_dim)
    # svdvals doesn't work for BigFloat etc. So don't use `T` as the element
    # type when computing the singular values.
    σ = LinearAlgebra.svdvals(convert(Matrix{Float64}, Y))
    result = set isa MOI.NormSpectralCone ? maximum(σ) : sum(σ)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), t), result, config)
    return
end

function test_NormSpectralCone_VectorOfVariables_without_transform(
    model::MOI.ModelLike,
    config::Config,
)
    _test_matrix_norm_cone(model, config, MOI.NormSpectralCone(3, 2), nothing)
    return
end

function setup_test(
    ::typeof(test_NormSpectralCone_VectorOfVariables_without_transform),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    # svdvals doesn't work for BigFloat etc. So don't use `T` as the element
    # type when computing the result.
    result = [1, 2, 3, 4, 5, 6]
    t = LinearAlgebra.opnorm(reshape(result, 3, 2))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, T[t; result]),
    )
    return
end

function test_NormSpectralCone_VectorOfVariables_with_transform(
    model::MOI.ModelLike,
    config::Config,
)
    _test_matrix_norm_cone(model, config, MOI.NormSpectralCone(2, 3), nothing)
    return
end

function setup_test(
    ::typeof(test_NormSpectralCone_VectorOfVariables_with_transform),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    # svdvals doesn't work for BigFloat etc. So don't use `T` as the element
    # type when computing the result.
    result = [1, 2, 3, 4, 5, 6]
    t = LinearAlgebra.opnorm(reshape(result, 2, 3))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, T[t; result]),
    )
    return
end

function test_NormSpectralCone_VectorAffineFunction_without_transform(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    b = convert(Vector{T}, 7:12)
    _test_matrix_norm_cone(model, config, MOI.NormSpectralCone(3, 2), b)
    return
end

function setup_test(
    ::typeof(test_NormSpectralCone_VectorAffineFunction_without_transform),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    # svdvals doesn't work for BigFloat etc. So don't use `T` as the element
    # type when computing the result.
    result = [1, 2, 3, 4, 5, 6]
    t = LinearAlgebra.opnorm(reshape(result + collect(7:12), 3, 2))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, T[t; result]),
    )
    return
end

function test_NormSpectralCone_VectorAffineFunction_with_transform(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    b = convert(Vector{T}, 7:12)
    _test_matrix_norm_cone(model, config, MOI.NormSpectralCone(2, 3), b)
    return
end

function setup_test(
    ::typeof(test_NormSpectralCone_VectorAffineFunction_with_transform),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    # svdvals doesn't work for BigFloat etc. So don't use `T` as the element
    # type when computing the result.
    result = [1, 2, 3, 4, 5, 6]
    t = LinearAlgebra.opnorm(reshape(result + collect(7:12), 2, 3))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, T[t; result]),
    )
    return
end

function test_NormNuclearCone_VectorOfVariables_without_transform(
    model::MOI.ModelLike,
    config::Config,
)
    _test_matrix_norm_cone(model, config, MOI.NormNuclearCone(3, 2), nothing)
    return
end

function setup_test(
    ::typeof(test_NormNuclearCone_VectorOfVariables_without_transform),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    # svdvals doesn't work for BigFloat etc. So don't use `T` as the element
    # type when computing the result.
    result = [1, 2, 3, 4, 5, 6]
    t = sum(LinearAlgebra.svdvals(reshape(result, 3, 2)))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, T[t; result]),
    )
    return
end

function test_NormNuclearCone_VectorOfVariables_with_transform(
    model::MOI.ModelLike,
    config::Config,
)
    _test_matrix_norm_cone(model, config, MOI.NormNuclearCone(2, 3), nothing)
    return
end

function setup_test(
    ::typeof(test_NormNuclearCone_VectorOfVariables_with_transform),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    # svdvals doesn't work for BigFloat etc. So don't use `T` as the element
    # type when computing the result.
    result = [1, 2, 3, 4, 5, 6]
    t = sum(LinearAlgebra.svdvals(reshape(result, 2, 3)))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, T[t; result]),
    )
    return
end

function test_NormNuclearCone_VectorAffineFunction_without_transform(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    b = convert(Vector{T}, 7:12)
    _test_matrix_norm_cone(model, config, MOI.NormNuclearCone(3, 2), b)
    return
end

function setup_test(
    ::typeof(test_NormNuclearCone_VectorAffineFunction_without_transform),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    # svdvals doesn't work for BigFloat etc. So don't use `T` as the element
    # type when computing the result.
    result = [1, 2, 3, 4, 5, 6]
    t = sum(LinearAlgebra.svdvals(reshape(result + collect(7:12), 3, 2)))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, T[t; result]),
    )
    return
end

function test_NormNuclearCone_VectorAffineFunction_with_transform(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    b = convert(Vector{T}, 7:12)
    _test_matrix_norm_cone(model, config, MOI.NormNuclearCone(2, 3), b)
    return
end

function setup_test(
    ::typeof(test_NormNuclearCone_VectorAffineFunction_with_transform),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    # svdvals doesn't work for BigFloat etc. So don't use `T` as the element
    # type when computing the result.
    result = [1, 2, 3, 4, 5, 6]
    t = sum(LinearAlgebra.svdvals(reshape(result + collect(7:12), 2, 3)))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, T[t; result]),
    )
    return
end

function test_HermitianPSDCone_basic(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    S = MOI.HermitianPositiveSemidefiniteConeTriangle
    @requires MOI.supports_constraint(model, MOI.VectorAffineFunction{T}, S)
    x = MOI.add_variables(model, 9)
    for i in eachindex(x)
        MOI.add_constraint(model, x[i], MOI.EqualTo{T}(i))
    end
    b = T[12, 0, 12, 0, 0, 12, 0, 0, 0]
    f = MOI.Utilities.operate(vcat, T, MOI.Utilities.operate.(+, T, x, b)...)
    set = MOI.HermitianPositiveSemidefiniteConeTriangle(3)
    MOI.add_constraint(model, f, set)
    MOI.optimize!(model)
    MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test ≈(MOI.get(model, MOI.VariablePrimal(), x), 1:9, config)
    return
end

function setup_test(
    ::typeof(test_HermitianPSDCone_basic),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            T[1, 2, 3, 4, 5, 6, 7, 8, 9],
        ),
    )
    return
end

function test_HermitianPSDCone_min_t(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    S = MOI.HermitianPositiveSemidefiniteConeTriangle
    @requires MOI.supports_constraint(model, MOI.VectorAffineFunction{T}, S)
    t = MOI.add_variable(model)
    x = MOI.add_variables(model, 9)
    for (i, xi) in enumerate(x)
        MOI.add_constraint(model, xi, MOI.EqualTo{T}(i))
    end
    bt = Union{MOI.VariableIndex,T}[t, T(0), t, T(0), T(0), t, T(0), T(0), T(0)]
    f = MOI.Utilities.operate(vcat, T, MOI.Utilities.operate.(+, T, x, bt)...)
    set = MOI.HermitianPositiveSemidefiniteConeTriangle(3)
    MOI.add_constraint(model, f, set)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.optimize!(model)
    MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    x_result = MOI.get(model, MOI.VariablePrimal(), x)
    @test ≈(x_result, 1:9, config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), t), 11.034899152582094, config)
    return
end

function setup_test(
    ::typeof(test_HermitianPSDCone_min_t),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    x = T[11.034899152582094, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.OPTIMAL, x),
    )
    return
end

function test_DualObjectiveValue_Min_VariableIndex_GreaterThan(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.DualObjectiveValue)
    x, _ = MOI.add_constrained_variable(model, MOI.GreaterThan(T(2)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = T(3) * x
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.optimize!(model)
    MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
    @test ≈(MOI.get(model, MOI.DualObjectiveValue()), T(6), config)
    return
end

function setup_test(
    ::typeof(test_DualObjectiveValue_Min_VariableIndex_GreaterThan),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        mock -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[2]),
            MOI.FEASIBLE_POINT,
            (MOI.VariableIndex, MOI.GreaterThan{T}) => T[3],
        ),
    )
    return
end

function test_DualObjectiveValue_Max_VariableIndex_LessThan(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.DualObjectiveValue)
    x, _ = MOI.add_constrained_variable(model, MOI.LessThan(T(2)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = T(3) * x
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.optimize!(model)
    MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
    @test ≈(MOI.get(model, MOI.DualObjectiveValue()), T(6), config)
    return
end

function setup_test(
    ::typeof(test_DualObjectiveValue_Max_VariableIndex_LessThan),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        mock -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[2]),
            MOI.FEASIBLE_POINT,
            (MOI.VariableIndex, MOI.LessThan{T}) => T[-3],
        ),
    )
    return
end

function test_DualObjectiveValue_Min_ScalarAffine_GreaterThan(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.DualObjectiveValue)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, one(T) * x, MOI.GreaterThan(T(2)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = T(3) * x
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.optimize!(model)
    MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
    @test ≈(MOI.get(model, MOI.DualObjectiveValue()), T(6), config)
    return
end

function setup_test(
    ::typeof(test_DualObjectiveValue_Min_ScalarAffine_GreaterThan),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        mock -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[2]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}) => T[3],
        ),
    )
    return
end

function test_DualObjectiveValue_Max_ScalarAffine_LessThan(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.DualObjectiveValue)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, one(T) * x, MOI.LessThan(T(2)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = T(3) * x
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.optimize!(model)
    MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
    @test ≈(MOI.get(model, MOI.DualObjectiveValue()), T(6), config)
    return
end

function setup_test(
    ::typeof(test_DualObjectiveValue_Max_ScalarAffine_LessThan),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        mock -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[2]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-3],
        ),
    )
    return
end
