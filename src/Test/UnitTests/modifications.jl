"""
    test_modification_set_function_single_variable(
        model::MOI.ModelLike,
        config::Config,
    )

Test that modifying the function of a `SingleVariable`-in-`LessThan` constraint
throws a [`SettingSingleVariableFunctionNotAllowed`](@ref) error.
"""
function test_modification_set_function_single_variable(
    model::MOI.ModelLike,
    ::Config,
)
    MOI.empty!(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x, y
    maxobjective: 1.0x + 1.0y
    x <= 1.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    y = MOI.get(model, MOI.VariableIndex, "y")
    c = MOI.ConstraintIndex{MOI.SingleVariable,MOI.LessThan{Float64}}(x.value)
    # We test this after the creation of every `SingleVariable` constraint
    # to ensure a good coverage of corner cases.
    @test c.value == x.value
    err = MOI.SettingSingleVariableFunctionNotAllowed()
    func = MOI.SingleVariable(y)
    @test_throws err MOI.set(model, MOI.ConstraintFunction(), c, func)
    return
end

"""
    test_modification_set_singlevariable_lessthan(
        model::MOI.ModelLike,
        config::Config,
    )

Test set modification SingleVariable-in-LessThan constraint. If
`config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function test_modification_set_singlevariable_lessthan(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    maxobjective: 1.0x
    x <= 1.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.ConstraintIndex{MOI.SingleVariable,MOI.LessThan{Float64}}(x.value)
    @test c.value == x.value
    _test_model_solution(
        model,
        config;
        objective_value = 1.0,
        variable_primal = [(x, 1.0)],
        constraint_primal = [(c, 1.0)],
        constraint_dual = [(c, -1.0)],
    )
    MOI.set(model, MOI.ConstraintSet(), c, MOI.LessThan(2.0))
    @test MOI.get(model, MOI.ConstraintSet(), c) == MOI.LessThan(2.0)
    _test_model_solution(
        model,
        config;
        objective_value = 2.0,
        variable_primal = [(x, 2.0)],
        constraint_primal = [(c, 2.0)],
        constraint_dual = [(c, -1.0)],
    )
    return
end

function setup_test(
    ::typeof(test_modification_set_singlevariable_lessthan),
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
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [2.0]),
            MOI.FEASIBLE_POINT,
        ),
    )
    return
end

"""
    test_modification_transform_singlevariable_lessthan(
        model::MOI.ModelLike,
        config::Config,
    )

Test set transformation of a SingleVariable-in-LessThan constraint. If
`config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function test_modification_transform_singlevariable_lessthan(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    maxobjective: 1.0x
    x <= 1.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.ConstraintIndex{MOI.SingleVariable,MOI.LessThan{Float64}}(x.value)
    @test c.value == x.value
    _test_model_solution(
        model,
        config;
        objective_value = 1.0,
        variable_primal = [(x, 1.0)],
        constraint_primal = [(c, 1.0)],
        constraint_dual = [(c, -1.0)],
    )
    c2 = MOI.transform(model, c, MOI.GreaterThan(2.0))
    @test !MOI.is_valid(model, c)
    @test MOI.is_valid(model, c2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    _test_model_solution(
        model,
        config;
        objective_value = 2.0,
        variable_primal = [(x, 2.0)],
        constraint_primal = [(c2, 2.0)],
        constraint_dual = [(c2, 1.0)],
    )
    return
end

function setup_test(
    ::typeof(test_modification_transform_singlevariable_lessthan),
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
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [2.0]),
            MOI.FEASIBLE_POINT,
        ),
    )
    return
end

"""
    test_modification_set_scalaraffine_lessthan(
        model::MOI.ModelLike,
        config::Config,
    )

Test modifying set of ScalarAffineFunction-in-LessThan constraint. If
`config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function test_modification_set_scalaraffine_lessthan(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    maxobjective: 1.0x
    c: 1.0x <= 1.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(model, MOI.ConstraintIndex, "c")
    _test_model_solution(
        model,
        config;
        objective_value = 1.0,
        variable_primal = [(x, 1.0)],
        constraint_primal = [(c, 1.0)],
        constraint_dual = [(c, -1.0)],
    )
    MOI.set(model, MOI.ConstraintSet(), c, MOI.LessThan(2.0))
    @test MOI.get(model, MOI.ConstraintSet(), c) == MOI.LessThan(2.0)
    _test_model_solution(
        model,
        config;
        objective_value = 2.0,
        variable_primal = [(x, 2.0)],
        constraint_primal = [(c, 2.0)],
        constraint_dual = [(c, -1.0)],
    )
    return
end

function setup_test(
    ::typeof(test_modification_set_scalaraffine_lessthan),
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
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1.0],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [2.0]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1.0],
        ),
    )
    return
end

"""
    test_modification_coef_scalaraffine_lessthan(
        model::MOI.ModelLike,
        config::Config,
    )

Test modifying a variable coefficient in a ScalarAffineFunction-in-LessThan
constraint. If `config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function test_modification_coef_scalaraffine_lessthan(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    maxobjective: 1.0x
    c: 1.0x <= 1.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(model, MOI.ConstraintIndex, "c")
    _test_model_solution(
        model,
        config;
        objective_value = 1.0,
        variable_primal = [(x, 1.0)],
        constraint_primal = [(c, 1.0)],
        constraint_dual = [(c, -1.0)],
    )
    MOI.modify(model, c, MOI.ScalarCoefficientChange(x, 2.0))
    _test_model_solution(
        model,
        config;
        objective_value = 0.5,
        variable_primal = [(x, 0.5)],
        constraint_primal = [(c, 1.0)],
        constraint_dual = [(c, -0.5)],
    )
    return
end

function setup_test(
    ::typeof(test_modification_coef_scalaraffine_lessthan),
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
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1.0],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.5]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-0.5],
        ),
    )
    return
end

"""
    test_modification_func_scalaraffine_lessthan(
        model::MOI.ModelLike,
        config::Config,
    )

Test setting the function in a ScalarAffineFunction-in-LessThan
constraint. If `config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function test_modification_func_scalaraffine_lessthan(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    maxobjective: 1.0x
    c: 1.0x <= 1.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(model, MOI.ConstraintIndex, "c")
    _test_model_solution(
        model,
        config;
        objective_value = 1.0,
        variable_primal = [(x, 1.0)],
        constraint_primal = [(c, 1.0)],
        constraint_dual = [(c, -1.0)],
    )
    MOI.set(
        model,
        MOI.ConstraintFunction(),
        c,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 0.0),
    )
    foo = MOI.get(model, MOI.ConstraintFunction(), c)
    @test foo ≈ MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 0.0)
    _test_model_solution(
        model,
        config;
        objective_value = 0.5,
        variable_primal = [(x, 0.5)],
        # constraint_primal = [(c, 1.0)],
        constraint_dual = [(c, -0.5)],
    )
    return
end

function setup_test(
    ::typeof(test_modification_func_scalaraffine_lessthan),
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
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1.0],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.5]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-0.5],
        ),
    )
    return
end

"""
    test_modification_func_vectoraffine_nonneg(
        model::MOI.ModelLike,
        config::Config,
    )

Test setting the function in a VectorAffineFunction-in-Nonnegatives
constraint. If `config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function test_modification_func_vectoraffine_nonneg(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x, y
    minobjective: 1.0x + 2.0y
    c: [1.0x, 2.0y] in Nonnegatives(2)
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    y = MOI.get(model, MOI.VariableIndex, "y")
    c = MOI.get(model, MOI.ConstraintIndex, "c")
    _test_model_solution(
        model,
        config;
        objective_value = 0.0,
        variable_primal = [(x, 0.0), (y, 0.0)],
        constraint_primal = [(c, [0.0, 0.0])],
    )
    MOI.set(
        model,
        MOI.ConstraintFunction(),
        c,
        MOI.VectorAffineFunction(
            [
                MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x)),
                MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(2.0, y)),
            ],
            [-1.0, -1.5],
        ),
    )
    foo = MOI.get(model, MOI.ConstraintFunction(), c)
    @test foo ≈ MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(2.0, y)),
        ],
        [-1.0, -1.5],
    )
    _test_model_solution(
        model,
        config;
        objective_value = 2.5,
        variable_primal = [(x, 1.0), (y, 0.75)],
        constraint_primal = [(c, [0.0, 0.0])],
    )
    return
end

function setup_test(
    ::typeof(test_modification_func_vectoraffine_nonneg),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.0, 0.0]),
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 0.75]),
        ),
    )
    return
end

"""
    test_modification_const_vectoraffine_nonpos(
        model::MOI.ModelLike,
        config::Config,
    )

Test modifying the constant term in a VectorAffineFunction-in-Nonpositives
constraint. If `config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function test_modification_const_vectoraffine_nonpos(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x, y
    maxobjective: 1.0x + 2.0y
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    y = MOI.get(model, MOI.VariableIndex, "y")
    c = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [
                MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x)),
                MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(2.0, y)),
            ],
            [0.0, 0.0],
        ),
        MOI.Nonpositives(2),
    )
    _test_model_solution(
        model,
        config;
        objective_value = 0.0,
        variable_primal = [(x, 0.0), (y, 0.0)],
        constraint_primal = [(c, [0.0, 0.0])],
    )
    MOI.modify(model, c, MOI.VectorConstantChange([-1.0, -1.5]))
    _test_model_solution(
        model,
        config;
        objective_value = 2.5,
        variable_primal = [(x, 1.0), (y, 0.75)],
        constraint_primal = [(c, [0.0, 0.0])],
    )
    return
end

function setup_test(
    ::typeof(test_modification_const_vectoraffine_nonpos),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.0, 0.0]),
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 0.75]),
        ),
    )
    return
end

"""
    test_modification_multirow_vectoraffine_nonpos(
        model::MOI.ModelLike,
        config::Config,
    )

Test modifying the variable coefficients in a
VectorAffineFunction-in-Nonpositives constraint. If `config.solve=true` confirm
that it solves correctly.
"""
function test_modification_multirow_vectoraffine_nonpos(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    maxobjective: 1.0x
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [
                MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x)),
                MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(2.0, x)),
            ],
            [-1.0, -1.0],
        ),
        MOI.Nonpositives(2),
    )
    _test_model_solution(
        model,
        config;
        objective_value = 0.5,
        variable_primal = [(x, 0.5)],
        constraint_primal = [(c, [-0.5, 0.0])],
    )
    MOI.modify(model, c, MOI.MultirowChange(x, [(1, 4.0), (2, 3.0)]))
    _test_model_solution(
        model,
        config;
        objective_value = 0.25,
        variable_primal = [(x, 0.25)],
        constraint_primal = [(c, [0.0, -0.25])],
    )
    return
end

function setup_test(
    ::typeof(test_modification_multirow_vectoraffine_nonpos),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [0.5])),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.25]),
        ),
    )
    return
end

"""
    test_modification_const_scalar_objective(
        model::MOI.ModelLike,
        config::Config,
    )

Test the constant of a scalaraffine objective. If `config.solve=true` confirm
that it solves correctly.
"""
function test_modification_const_scalar_objective(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    maxobjective: 1.0x + 2.0
    c1: 1.0x <= 1.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    _test_model_solution(
        model,
        config;
        objective_value = 3.0,
        variable_primal = [(x, 1.0)],
    )
    MOI.modify(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarConstantChange(3.0),
    )
    _test_model_solution(
        model,
        config;
        objective_value = 4.0,
        variable_primal = [(x, 1.0)],
    )
    return
end

function setup_test(
    ::typeof(test_modification_const_scalar_objective),
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
    test_modification_coef_scalar_objective(
        model::MOI.ModelLike,
        config::Config
    )

Test modifying a variable coefficient in a scalaraffine objective. If
`config.solve=true` confirm that it solves correctly.
"""
function test_modification_coef_scalar_objective(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    maxobjective: 1.0x
    c1: 1.0x <= 1.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    _test_model_solution(
        model,
        config;
        objective_value = 1.0,
        variable_primal = [(x, 1.0)],
    )
    MOI.modify(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarCoefficientChange(x, 3.0),
    )
    _test_model_solution(
        model,
        config;
        objective_value = 3.0,
        variable_primal = [(x, 1.0)],
    )
    return
end

function setup_test(
    ::typeof(test_modification_coef_scalar_objective),
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
    test_modification_delete_variable_with_single_variable_obj(
        model::MOI.ModelLike,
        config::Config,
    )

Test deleting a variable that is the objective function.
"""
function test_modification_delete_variable_with_single_variable_obj(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x, y
    minobjective: x
    x >= 1.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    y = MOI.get(model, MOI.VariableIndex, "y")
    c = MOI.ConstraintIndex{MOI.SingleVariable,MOI.GreaterThan{Float64}}(
        x.value,
    )
    @test c.value == x.value
    MOI.delete(model, y)
    _test_model_solution(
        model,
        config;
        objective_value = 1.0,
        variable_primal = [(x, 1.0)],
        constraint_primal = [(c, 1.0)],
        constraint_dual = [(c, 1.0)],
    )
    return
end

function setup_test(
    ::typeof(test_modification_delete_variable_with_single_variable_obj),
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
    test_modification_delete_variables_in_a_batch(
        model::MOI.ModelLike,
        config::Config,
    )

Test deleting many variables in a batch (i.e. using the delete method which
takes a vector of variable references). If `config.solve=true` confirm that it
solves correctly.
"""
function test_modification_delete_variables_in_a_batch(
    model::MOI.ModelLike,
    config::Config,
)
    atol = config.atol
    rtol = config.rtol
    MOI.empty!(model)
    @test MOI.is_empty(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x, y, z
    minobjective: 1.0 * x + 2.0 * y + 3.0 * z
    x >= 1.0
    y >= 1.0
    z >= 1.0
""",
    )
    x, y, z = MOI.get(model, MOI.ListOfVariableIndices())
    @test MOI.is_valid(model, x)
    @test MOI.is_valid(model, y)
    @test MOI.is_valid(model, z)
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 6.0 atol = atol rtol = rtol
    end
    MOI.delete(model, [x, z])
    @test !MOI.is_valid(model, x)
    @test MOI.is_valid(model, y)
    @test !MOI.is_valid(model, z)
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2.0 atol = atol rtol = rtol
    end
    return
end

function setup_test(
    ::typeof(test_modification_delete_variables_in_a_batch),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 1.0, 1.0]),
            MOI.FEASIBLE_POINT,
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0]),
            MOI.FEASIBLE_POINT,
        ),
    )
    return
end
