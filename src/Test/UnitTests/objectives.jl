"""
    test_ObjectiveSense_MAX_SENSE(model::MOI.ModelLike, config::Config)

Test setting objective sense to MAX_SENSE.
"""
function test_ObjectiveSense_MAX_SENSE(model::MOI.ModelLike, ::Config)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test MOI.supports(model, MOI.ObjectiveSense())
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    return
end

"""
    test_ObjectiveSense_MIN_SENSE(model::MOI.ModelLike, config::Config)

Test setting objective sense to MIN_SENSE.
"""
function test_ObjectiveSense_MIN_SENSE(model::MOI.ModelLike, ::Config)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test MOI.supports(model, MOI.ObjectiveSense())
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    return
end

"""
    test_ObjectiveSense_FEASIBILITY_SENSE(model::MOI.ModelLike, config::Config)

Test setting objective sense to FEASIBILITY_SENSE.
"""
function test_ObjectiveSense_FEASIBILITY_SENSE(model::MOI.ModelLike, ::Config)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test MOI.supports(model, MOI.ObjectiveSense())
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.FEASIBILITY_SENSE
end

"""
    test_get_ObjectiveFunction(model::MOI.ModelLike, config::Config)

Test get objective function.
"""
function test_get_ObjectiveFunction(model::MOI.ModelLike, ::Config)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    obj_attr = MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()
    @test MOI.supports(model, obj_attr)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    minobjective: 2.0x + 1.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    expected_obj_fun =
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 1.0)
    @test_throws(
        InexactError,
        MOI.get(model, MOI.ObjectiveFunction{MOI.SingleVariable}()),
    )
    obj_fun = MOI.get(model, obj_attr)
    @test obj_fun ≈ expected_obj_fun
    quad_obj_attr =
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}()
    quad_obj_fun = MOI.get(model, quad_obj_attr)
    @test convert(MOI.ScalarAffineFunction{Float64}, quad_obj_fun) ≈
          expected_obj_fun
    return
end

"""
    test_ObjectiveFunction_constant(model::MOI.ModelLike, config::Config)

Test constant in linear objective,  if `config.solve=true` confirm that it
solves correctly, and if `config.duals=true`, check that the duals are computed
correctly.
"""
function test_ObjectiveFunction_constant(model::MOI.ModelLike, config::Config)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    minobjective: 2.0x + 1.0
    x >= 1.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.ConstraintIndex{MOI.SingleVariable,MOI.GreaterThan{Float64}}(
        x.value,
    )
    # We test this after the creation of every `SingleVariable` constraint
    # to ensure a good coverage of corner cases.
    @test c.value == x.value
    _test_model_solution(
        model,
        config;
        objective_value = 3.0,
        variable_primal = [(x, 1.0)],
        constraint_primal = [(c, 1.0)],
        constraint_dual = [(c, 2.0)],
    )
    return
end

function setup_test(
    ::typeof(test_ObjectiveFunction_constant),
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
        ),
    )
    return
end

"""
    test_ObjectiveFunction_blank(model::MOI.ModelLike, config::Config)

Test blank linear objective,  if `config.solve=true` confirm that it
solves correctly, and if `config.duals=true`, check that the duals are computed
correctly.
"""
function test_ObjectiveFunction_blank(model::MOI.ModelLike, config::Config)
    if !config.solve
        return
    end
    atol, rtol = config.atol, config.rtol
    MOI.empty!(model)
    @test MOI.is_empty(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    minobjective: 0.0x + 0.0
    x >= 1.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.ConstraintIndex{MOI.SingleVariable,MOI.GreaterThan{Float64}}(
        x.value,
    )
    @test c.value == x.value
    _test_model_solution(
        model,
        config;
        objective_value = 0.0,
        constraint_dual = [(c, 0.0)],
    )
    # The objective is blank so any primal value ≥ 1 is correct
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
    @test MOI.get(model, MOI.VariablePrimal(), x) + atol + rtol >= 1.0
    @test MOI.get(model, MOI.ConstraintPrimal(), c) + atol + rtol >= 1.0
    return
end

function setup_test(
    ::typeof(test_ObjectiveFunction_blank),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.2]),
            MOI.FEASIBLE_POINT,
        ),
    )
    return
end

"""
    test_ObjectiveFunction_SingleVariable(model::MOI.ModelLike, config::Config)

Test SingleVariable objective,  if `config.solve=true` confirm that it
solves correctly, and if `config.duals=true`, check that the duals are computed
correctly.
"""
function test_ObjectiveFunction_SingleVariable(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    minobjective: x
    x >= 1.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.ConstraintIndex{MOI.SingleVariable,MOI.GreaterThan{Float64}}(
        x.value,
    )
    @test c.value == x.value
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
    ::typeof(test_ObjectiveFunction_SingleVariable),
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
        ),
    )
    return
end

"""
    test_qp_ObjectiveFunction_edge_cases(model::MOI.ModelLike, config::Config)

Test various edge cases relating to quadratic programs (i.e., with a quadratic
objective function).

If `config.solve=true` confirm that it solves correctly.
"""
function test_qp_ObjectiveFunction_edge_cases(
    model::MOI.ModelLike,
    config::Config,
)
    obj_attr = MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}()
    if !MOI.supports(model, obj_attr)
        return
    end
    MOI.empty!(model)
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    vc1 = MOI.add_constraint(
        model,
        MOI.SingleVariable(x[1]),
        MOI.GreaterThan(1.0),
    )
    @test vc1.value == x[1].value
    vc2 = MOI.add_constraint(
        model,
        MOI.SingleVariable(x[2]),
        MOI.GreaterThan(2.0),
    )
    @test vc2.value == x[2].value
    # Basic model
    # min x^2 + y^2 | x>=1, y>=2
    MOI.set(
        model,
        obj_attr,
        MOI.ScalarQuadraticFunction(
            MOI.ScalarAffineTerm{Float64}[],  # affine terms
            MOI.ScalarQuadraticTerm.([2.0, 2.0], x, x),  # quad
            0.0,  # constant
        ),
    )
    _test_model_solution(
        model,
        config;
        objective_value = 5.0,
        variable_primal = [(x[1], 1.0), (x[2], 2.0)],
    )
    # Duplicate linear terms
    # min x + x + x^2 + y^2 | x>=1, y>=2
    MOI.set(
        model,
        obj_attr,
        MOI.ScalarQuadraticFunction(
            MOI.ScalarAffineTerm.([1.0, 1.0], [x[1], x[1]]),  # affine terms
            MOI.ScalarQuadraticTerm.([2.0, 2.0], x, x),  # quad
            0.0,  # constant
        ),
    )
    _test_model_solution(
        model,
        config;
        objective_value = 7.0,
        variable_primal = [(x[1], 1.0), (x[2], 2.0)],
    )
    # Duplicate diagonal terms
    # min x^2 + x^2 | x>=1, y>=2
    MOI.set(
        model,
        obj_attr,
        MOI.ScalarQuadraticFunction(
            MOI.ScalarAffineTerm{Float64}[],  # affine terms
            MOI.ScalarQuadraticTerm.([2.0, 2.0], [x[1], x[1]], [x[1], x[1]]),  # quad
            0.0,  # constant
        ),
    )
    _test_model_solution(
        model,
        config;
        objective_value = 2.0,
        variable_primal = [(x[1], 1.0)],
    )
    # Duplicate off-diagonal terms" begin
    # min x^2 + 0.25x*y + 0.25y*x + 0.5x*y + y^2 | x>=1, y>=2
    MOI.set(
        model,
        obj_attr,
        MOI.ScalarQuadraticFunction(
            MOI.ScalarAffineTerm{Float64}[],  # affine terms
            MOI.ScalarQuadraticTerm.(
                [2.0, 0.25, 0.25, 0.5, 2.0],
                [x[1], x[1], x[2], x[1], x[2]],
                [x[1], x[2], x[1], x[2], x[2]],
            ),  # quad
            0.0,  # constant
        ),
    )
    _test_model_solution(
        model,
        config;
        objective_value = 7.0,
        variable_primal = [(x[1], 1.0), (x[2], 2.0)],
    )
    return
end

function setup_test(
    ::typeof(test_qp_ObjectiveFunction_edge_cases),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 2.0]),
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 2.0]),
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 2.0]),
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 2.0]),
        ),
    )
    return
end

"""
    test_qp_ObjectiveFunction_zero_ofdiag(model::MOI.ModelLike, config::Config)

Test quadratic program with a zero off-diagonal term.

If `config.solve=true` confirm that it solves correctly.
"""
function test_qp_ObjectiveFunction_zero_ofdiag(
    model::MOI.ModelLike,
    config::Config,
)
    obj_attr = MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}()
    if !MOI.supports(model, obj_attr)
        return
    end
    MOI.empty!(model)
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    vc1 = MOI.add_constraint(
        model,
        MOI.SingleVariable(x[1]),
        MOI.GreaterThan(1.0),
    )
    @test vc1.value == x[1].value
    vc2 = MOI.add_constraint(
        model,
        MOI.SingleVariable(x[2]),
        MOI.GreaterThan(2.0),
    )
    @test vc2.value == x[2].value
    MOI.set(
        model,
        obj_attr,
        MOI.ScalarQuadraticFunction(
            MOI.ScalarAffineTerm{Float64}[],  # affine terms
            MOI.ScalarQuadraticTerm.(
                [2.0, 0.0, 2.0],
                [x[1], x[1], x[2]],
                [x[1], x[2], x[2]],
            ),  # quad
            0.0,  # constant
        ),
    )
    _test_model_solution(
        model,
        config;
        objective_value = 5.0,
        variable_primal = [(x[1], 1.0), (x[2], 2.0)],
    )
    return
end

function setup_test(
    ::typeof(test_qp_ObjectiveFunction_zero_ofdiag),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 2.0]),
        ),
    )
    return
end

"""
    test_ObjectiveFunction_duplicate_terms(model::MOI.ModelLike, config::Config)

Test duplicate terms in linear objective, if `config.solve=true` confirm that it
solves correctly.
"""
function test_ObjectiveFunction_duplicate_terms(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(1.0))
    @test c.value == x.value
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(2.0, x), MOI.ScalarAffineTerm(1.0, x)],
            0.0,
        ),
    )
    _test_model_solution(
        model,
        config;
        objective_value = 3.0,
        variable_primal = [(x, 1.0)],
        constraint_primal = [(c, 1.0)],
        constraint_dual = [(c, 3.0)],
    )
    return
end

function setup_test(
    ::typeof(test_ObjectiveFunction_duplicate_terms),
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
        ),
    )
    return
end
