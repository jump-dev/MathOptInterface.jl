"""
    test_variable_add_variable(model::MOI.ModelLike, config::Config)

Test adding a single variable.
"""
function test_variable_add_variable(model::MOI.ModelLike, ::Config)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    v = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    _test_attribute_value_type(model, MOI.NumberOfVariables())
    return
end

"""
    test_variable_add_variables(model::MOI.ModelLike, config::Config)

Test adding multiple variables.
"""
function test_variable_add_variables(model::MOI.ModelLike, ::Config)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    v = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    return
end

"""
    test_variable_delete(model::MOI.ModelLike, config::Config)

Tess adding, and then deleting, a single variable.
"""
function test_variable_delete(model::MOI.ModelLike, ::Config)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    v = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    MOI.delete(model, v)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    return
end

"""
    test_variable_delete_variables(model::MOI.ModelLike, config::Config)

Test adding, and then deleting, multiple variables.
"""
function test_variable_delete_variables(model::MOI.ModelLike, ::Config)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    v = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    MOI.delete(model, v)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    v = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    MOI.delete(model, v[1])
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    @test_throws MOI.InvalidIndex{MOI.VariableIndex} MOI.delete(model, v[1])
    try
        MOI.delete(model, v[1])
    catch err
        @test err.index == v[1]
    end
    @test !MOI.is_valid(model, v[1])
    @test MOI.is_valid(model, v[2])
    return
end

"""
    test_variable_delete_Nonnegatives(model::MOI.ModelLike, config::Config)

Test adding, and then deleting, nonnegative variables.
"""
function test_variable_delete_Nonnegatives(model::MOI.ModelLike, ::Config)
    @requires MOI.supports_add_constrained_variables(model, MOI.Nonnegatives)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    v, cv = MOI.add_constrained_variables(model, MOI.Nonnegatives(2))
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    MOI.delete(model, v)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    @test !MOI.is_valid(model, v[1])
    @test_throws MOI.InvalidIndex(v[1]) MOI.delete(model, v[1])
    @test !MOI.is_valid(model, v[2])
    @test_throws MOI.InvalidIndex(v[2]) MOI.delete(model, v[2])
    @test !MOI.is_valid(model, cv)
    v, cv = MOI.add_constrained_variables(model, MOI.Nonnegatives(1))
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    MOI.delete(model, v[1])
    @test !MOI.is_valid(model, v[1])
    @test_throws MOI.InvalidIndex(v[1]) MOI.delete(model, v[1])
    @test !MOI.is_valid(model, cv)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    return
end

"""
    test_variable_delete_Nonnegatives_row(model::MOI.ModelLike, ::Config)

Test adding, and then deleting one by one, nonnegative variables.
"""
function test_variable_delete_Nonnegatives_row(model::MOI.ModelLike, ::Config)
    @requires MOI.supports_add_constrained_variables(model, MOI.Nonnegatives)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    v, cv = MOI.add_constrained_variables(model, MOI.Nonnegatives(2))
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    MOI.delete(model, v[1])
    @test !MOI.is_valid(model, v[1])
    @test_throws MOI.InvalidIndex(v[1]) MOI.delete(model, v[1])
    @test MOI.is_valid(model, cv)
    @test MOI.is_valid(model, v[2])
    MOI.delete(model, v[2])
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    @test !MOI.is_valid(model, v[1])
    @test_throws MOI.InvalidIndex(v[1]) MOI.delete(model, v[1])
    @test !MOI.is_valid(model, v[2])
    @test_throws MOI.InvalidIndex(v[2]) MOI.delete(model, v[2])
    @test !MOI.is_valid(model, cv)
    return
end

"""
    test_variable_delete_SecondOrderCone(model::MOI.ModelLike, config::Config)

Test adding, and then deleting, second-order cone variables.
"""
function test_variable_delete_SecondOrderCone(model::MOI.ModelLike, ::Config)
    @requires MOI.supports_add_constrained_variables(model, MOI.SecondOrderCone)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    v, cv = MOI.add_constrained_variables(model, MOI.SecondOrderCone(3))
    @test MOI.get(model, MOI.NumberOfVariables()) == 3
    MOI.delete(model, v)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    @test !MOI.is_valid(model, v[1])
    @test_throws MOI.InvalidIndex(v[1]) MOI.delete(model, v[1])
    @test !MOI.is_valid(model, v[2])
    @test_throws MOI.InvalidIndex(v[2]) MOI.delete(model, v[2])
    @test !MOI.is_valid(model, cv)
    v, cv = MOI.add_constrained_variables(model, MOI.SecondOrderCone(3))
    @test MOI.get(model, MOI.NumberOfVariables()) == 3
    @test_throws MOI.DeleteNotAllowed MOI.delete(model, v[1])
    return
end

"""
    test_variable_get_VariableIndex(model::MOI.ModelLike, config::Config)

Test getting variables by name.
"""
function test_variable_get_VariableIndex(model::MOI.ModelLike, ::Config)
    @requires MOI.supports(model, MOI.VariableName(), MOI.VariableIndex)
    variable = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), variable, "x")
    x = MOI.get(model, MOI.VariableIndex, "x")
    @test MOI.is_valid(model, x)
    @test MOI.get(model, MOI.VariableIndex, "y") === nothing
    return
end

"""
    test_variable_VariableName(model::MOI.ModelLike, config::Config)

Test getting and setting variable names.
"""
function test_variable_VariableName(model::MOI.ModelLike, ::Config)
    @requires MOI.supports(model, MOI.VariableName(), MOI.VariableIndex)
    v = MOI.add_variable(model)
    @test MOI.get(model, MOI.VariableName(), v) == ""
    MOI.set(model, MOI.VariableName(), v, "x")
    @test MOI.get(model, MOI.VariableName(), v) == "x"
    MOI.set(model, MOI.VariableName(), v, "y")
    @test MOI.get(model, MOI.VariableName(), v) == "y"
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    @test MOI.get(model, MOI.VariableName(), x) == "x"
    return
end

"""
    test_variable_solve_with_upperbound(model::MOI.ModelLike, config::Config)

Test setting the upper bound of a variable, confirm that it solves correctly.
"""
function test_variable_solve_with_upperbound(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    c1 = MOI.add_constraint(model, x, MOI.LessThan(T(1)))
    @test x.value == c1.value
    c2 = MOI.add_constraint(model, x, MOI.GreaterThan(T(0)))
    @test x.value == c2.value
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(2), x)], T(0))
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    _test_model_solution(
        model,
        config;
        objective_value = T(2),
        variable_primal = [(x, T(1))],
        constraint_primal = [(c1, T(1)), (c2, T(1))],
        constraint_dual = [(c1, T(-2)), (c2, T(0))],
    )
    return
end

function setup_test(
    ::typeof(test_variable_solve_with_upperbound),
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
            (MOI.VariableIndex, MOI.LessThan{T}) => T[-2],
            (MOI.VariableIndex, MOI.GreaterThan{T}) => T[0],
        ),
    )
    model.eval_variable_constraint_dual = false
    return () -> model.eval_variable_constraint_dual = true
end

"""
    test_variable_solve_with_lowerbound(model::MOI.ModelLike, config::Config)

Test setting the lower bound of a variable, confirm that it solves correctly.
"""
function test_variable_solve_with_lowerbound(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    c1 = MOI.add_constraint(model, x, MOI.GreaterThan(T(1)))
    c2 = MOI.add_constraint(model, x, MOI.LessThan(T(2)))
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(2), x)], T(0))
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    _test_model_solution(
        model,
        config;
        objective_value = T(2),
        variable_primal = [(x, T(1))],
        constraint_primal = [(c1, T(1)), (c2, T(1))],
        constraint_dual = [(c1, T(2)), (c2, T(0))],
    )
    return
end

function setup_test(
    ::typeof(test_variable_solve_with_lowerbound),
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
            (MOI.VariableIndex, MOI.GreaterThan{T}) => T[2],
            (MOI.VariableIndex, MOI.LessThan{T}) => T[0],
        ),
    )
    model.eval_variable_constraint_dual = false
    return () -> model.eval_variable_constraint_dual = true
end

"""
    test_variable_solve_Integer_with_lower_bound(
        model::MOI.ModelLike,
        config::Config,
    )

Test an integer variable with fractional lower bound.
"""
function test_variable_solve_Integer_with_lower_bound(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.GreaterThan(T(3 // 2)))
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(2), x)], T(0))
    MOI.add_constraint(model, x, MOI.Integer())
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    _test_model_solution(
        model,
        config;
        objective_value = T(4),
        variable_primal = [(x, T(2))],
    )
    return
end

function setup_test(
    ::typeof(test_variable_solve_Integer_with_lower_bound),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, T[2])),
    )
    return
end

"""
    test_variable_solve_Integer_with_upper_bound(
        model::MOI.ModelLike,
        config::Config,
    )

Test an integer variable with fractional upper bound.
"""
function test_variable_solve_Integer_with_upper_bound(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.LessThan(T(3 // 2)))
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(-2), x)], T(0))
    MOI.add_constraint(model, x, MOI.Integer())
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    _test_model_solution(
        model,
        config;
        objective_value = T(-2),
        variable_primal = [(x, T(1))],
    )
    return
end

function setup_test(
    ::typeof(test_variable_solve_Integer_with_upper_bound),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, T[1])),
    )
    return
end

"""
    test_variable_solve_ZeroOne_with_upper_bound(
        model::MOI.ModelLike,
        config::Config,
    )

Test a binary variable `<= 2`.
"""
function test_variable_solve_ZeroOne_with_upper_bound(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.LessThan(T(2)))
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(-2), x)], T(0))
    MOI.add_constraint(model, x, MOI.ZeroOne())
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    _test_model_solution(
        model,
        config;
        objective_value = T(-2),
        variable_primal = [(x, T(1))],
    )
    return
end

function setup_test(
    ::typeof(test_variable_solve_ZeroOne_with_upper_bound),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, T[1])),
    )
    return
end

"""
    test_variable_solve_ZeroOne_with_0_upper_bound(
        model::MOI.ModelLike,
        config::Config,
    )

Test a binary variable `<= 0`.
"""
function test_variable_solve_ZeroOne_with_0_upper_bound(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.LessThan(T(0)))
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x)], T(0))
    MOI.add_constraint(model, x, MOI.ZeroOne())
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    _test_model_solution(
        model,
        config;
        objective_value = T(0),
        variable_primal = [(x, T(0))],
    )
    return
end

function setup_test(
    ::typeof(test_variable_solve_ZeroOne_with_0_upper_bound),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, T[0])),
    )
    return
end

"""
    test_variable_VariablePrimalStart(model::MOI.ModelLike, ::Config)

Test VariablePrimalStart
"""
function test_model_VariablePrimalStart(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    @requires MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.VariablePrimalStart(), x) === nothing
    MOI.set(model, MOI.VariablePrimalStart(), x, T(1))
    @test MOI.get(model, MOI.VariablePrimalStart(), x) == T(1)
    MOI.set(model, MOI.VariablePrimalStart(), x, nothing)
    @test MOI.get(model, MOI.VariablePrimalStart(), x) === nothing
    return
end

"""
    test_add_constrained_variables(model::MOI.ModelLike, config::Config)

Test vector method of `add_constrained_variables`.
"""
function test_add_constrained_variables_vector(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    @requires MOI.supports_add_constrained_variable(model, MOI.GreaterThan{T})
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    sets = [MOI.GreaterThan(zero(T)), MOI.GreaterThan(one(T))]
    v, cv = MOI.add_constrained_variables(model, sets)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    @test length(v) == 2
    @test length(cv) == 2
    @test MOI.get(model, MOI.ConstraintSet(), cv[1]) == sets[1]
    @test MOI.get(model, MOI.ConstraintSet(), cv[2]) == sets[2]
    return
end
