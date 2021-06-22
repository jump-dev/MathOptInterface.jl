"""
    test_add_variable(model::MOI.ModelLike, config::Config)

Test adding a single variable.
"""
function test_add_variable(model::MOI.ModelLike, ::Config)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    v = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    return
end

"""
    test_add_variables(model::MOI.ModelLike, config::Config)

Test adding multiple variables.
"""
function test_add_variables(model::MOI.ModelLike, ::Config)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    v = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    return
end

"""
    test_delete_variable(model::MOI.ModelLike, config::Config)

Tess adding, and then deleting, a single variable.
"""
function test_delete_variable(model::MOI.ModelLike, ::Config)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    v = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    MOI.delete(model, v)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    return
end

"""
    test_delete_variables(model::MOI.ModelLike, config::Config)

Test adding, and then deleting, multiple variables.
"""
function test_delete_variables(model::MOI.ModelLike, ::Config)
    MOI.empty!(model)
    @test MOI.is_empty(model)
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
    test_delete_nonnegative_variables(model::MOI.ModelLike, config::Config)

Test adding, and then deleting, nonnegative variables.
"""
function test_delete_nonnegative_variables(model::MOI.ModelLike, ::Config)
    MOI.empty!(model)
    @test MOI.is_empty(model)
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
    test_update_dimension_nonnegative_variables(model::MOI.ModelLike, ::Config)

Test adding, and then deleting one by one, nonnegative variables.
"""
function test_update_dimension_nonnegative_variables(
    model::MOI.ModelLike,
    ::Config,
)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    v, cv = MOI.add_constrained_variables(model, MOI.Nonnegatives(2))
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    MOI.delete(model, v[1])
    @test !MOI.is_valid(model, v[1])
    @test_throws MOI.InvalidIndex(v[1]) MOI.delete(model, v[1])
    @test MOI.is_valid(model, cv)
    @test MOI.is_valid(model, v[2])
    @test MOI.get(model, MOI.ConstraintFunction(), cv) ==
          MOI.VectorOfVariables([v[2]])
    @test MOI.get(model, MOI.ConstraintSet(), cv) == MOI.Nonnegatives(1)
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
    test_delete_soc_variables(model::MOI.ModelLike, config::Config)

Test adding, and then deleting, second-order cone variables.
"""
function test_delete_soc_variables(model::MOI.ModelLike, config::Config)
    if !MOI.supports_add_constrained_variables(model, MOI.SecondOrderCone)
        return
    end
    MOI.empty!(model)
    @test MOI.is_empty(model)
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
    test_get_variable(model::MOI.ModelLike, config::Config)

Test getting variables by name.
"""
function test_get_variable_by_name(model::MOI.ModelLike, ::Config)
    MOI.empty!(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    minobjective: 2.0x
    x >= 1.0
    x <= 2.0
""",
    )
    @test MOI.get(model, MOI.VariableIndex, "y") === nothing
    x = MOI.get(model, MOI.VariableIndex, "x")
    @test MOI.is_valid(model, x)
    return
end

"""
    test_VariableName(model::MOI.ModelLike, config::Config)

Test getting and setting variable names.
"""
function test_VariableName(model::MOI.ModelLike, ::Config)
    MOI.empty!(model)
    v = MOI.add_variable(model)
    @test MOI.get(model, MOI.VariableName(), v) == ""
    @test MOI.supports(model, MOI.VariableName(), typeof(v))
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
    test_solve_with_upperbound(model::MOI.ModelLike, config::Config)

Test setting the upper bound of a variable, confirm that it solves correctly,
and if `config.duals=true`, check that the dual is computed correctly.
"""
function test_solve_with_upperbound(model::MOI.ModelLike, config::Config)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    maxobjective: 2.0x
    x <= 1.0
    x >= 0.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    c1 = MOI.ConstraintIndex{MOI.SingleVariable,MOI.LessThan{Float64}}(x.value)
    # We test this after the creation of every `SingleVariable` constraint
    # to ensure a good coverage of corner cases.
    @test c1.value == x.value
    c2 = MOI.ConstraintIndex{MOI.SingleVariable,MOI.GreaterThan{Float64}}(
        x.value,
    )
    @test c2.value == x.value
    _test_model_solution(
        model,
        config;
        objective_value = 2.0,
        variable_primal = [(x, 1.0)],
        constraint_primal = [(c1, 1.0), (c2, 1.0)],
        constraint_dual = [(c1, -2.0), (c2, 0.0)],
    )
    return
end

function setup_test(
    ::typeof(test_solve_with_upperbound),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1]),
            MOI.FEASIBLE_POINT,
            (MOI.SingleVariable, MOI.LessThan{Float64}) => [-2.0],
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [0.0],
        ),
    )
    model.eval_variable_constraint_dual = false
    return () -> model.eval_variable_constraint_dual = true
end

"""
    test_solve_with_lowerbound(model::MOI.ModelLike, config::Config)

Test setting the lower bound of a variable, confirm that it solves correctly,
and if `config.duals=true`, check that the dual is computed correctly.
"""
function test_solve_with_lowerbound(model::MOI.ModelLike, config::Config)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    minobjective: 2.0x
    x >= 1.0
    x <= 2.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    c1 = MOI.ConstraintIndex{MOI.SingleVariable,MOI.GreaterThan{Float64}}(
        x.value,
    )
    @test c1.value == x.value
    c2 = MOI.ConstraintIndex{MOI.SingleVariable,MOI.LessThan{Float64}}(x.value)
    @test c2.value == x.value
    _test_model_solution(
        model,
        config;
        objective_value = 2.0,
        variable_primal = [(x, 1.0)],
        constraint_primal = [(c1, 1.0), (c2, 1.0)],
        constraint_dual = [(c1, 2.0), (c2, 0.0)],
    )
    return
end

function setup_test(
    ::typeof(test_solve_with_lowerbound),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1]),
            MOI.FEASIBLE_POINT,
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [2.0],
            (MOI.SingleVariable, MOI.LessThan{Float64}) => [0.0],
        ),
    )
    model.eval_variable_constraint_dual = false
    return () -> model.eval_variable_constraint_dual = true
end

"""
    test_solve_Integer_with_lower_bound(
        model::MOI.ModelLike,
        config::Config,
    )

Test an integer variable with fractional lower bound.
"""
function test_solve_Integer_with_lower_bound(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    MOIU.loadfromstring!(
        model,
        """
variables: x
minobjective: 2.0x
x >= 1.5
x in Integer()
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    _test_model_solution(
        model,
        config;
        objective_value = 4.0,
        variable_primal = [(x, 2.0)],
    )
    return
end

function setup_test(
    ::typeof(test_solve_Integer_with_lower_bound),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [2.0])),
    )
    return
end

"""
    test_solve_Integer_with_upper_bound(
        model::MOI.ModelLike,
        config::Config,
    )

Test an integer variable with fractional upper bound.
"""
function test_solve_Integer_with_upper_bound(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    MOIU.loadfromstring!(
        model,
        """
variables: x
minobjective: -2.0x
x <= 1.5
x in Integer()
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    _test_model_solution(
        model,
        config;
        objective_value = -2.0,
        variable_primal = [(x, 1.0)],
    )
    return
end

function setup_test(
    ::typeof(test_solve_Integer_with_upper_bound),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [1.0])),
    )
    return
end

"""
    test_solve_Integer_with_0_upper_bound(
        model::MOI.ModelLike,
        config::Config,
    )

Test a binary variable `<= 2`.
"""
function test_solve_ZeroOne_with_upper_bound(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    MOIU.loadfromstring!(
        model,
        """
variables: x
minobjective: -2.0x
x <= 2.0
x in ZeroOne()
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    _test_model_solution(
        model,
        config;
        objective_value = -2.0,
        variable_primal = [(x, 1.0)],
    )
    return
end

function setup_test(
    ::typeof(test_solve_ZeroOne_with_upper_bound),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [1.0])),
    )
    return
end

"""
    test_solve_ZeroOne_with_0_upper_bound(
        model::MOI.ModelLike,
        config::Config,
    )

Test a binary variable `<= 0`.
"""
function test_solve_ZeroOne_with_0_upper_bound(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    MOIU.loadfromstring!(
        model,
        """
variables: x
minobjective: 1.0x
x <= 0.0
x in ZeroOne()
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    _test_model_solution(
        model,
        config;
        objective_value = 0.0,
        variable_primal = [(x, 0.0)],
    )
    return
end

function setup_test(
    ::typeof(test_solve_ZeroOne_with_0_upper_bound),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [0.0])),
    )
    return
end
