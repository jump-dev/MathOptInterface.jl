"""
    test_objective_ObjectiveSense_MAX_SENSE(
        model::MOI.ModelLike,
        config::Config,
    )

Test setting objective sense to MAX_SENSE.
"""
function test_objective_ObjectiveSense_MAX_SENSE(model::MOI.ModelLike, ::Config)
    @requires MOI.supports(model, MOI.ObjectiveSense())
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    return
end

"""
    test_objective_ObjectiveSense_MIN_SENSE(
        model::MOI.ModelLike,
        config::Config,
    )

Test setting objective sense to MIN_SENSE.
"""
function test_objective_ObjectiveSense_MIN_SENSE(model::MOI.ModelLike, ::Config)
    @requires MOI.supports(model, MOI.ObjectiveSense())
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    return
end

"""
    test_objective_ObjectiveSense_FEASIBILITY_SENSE(
        model::MOI.ModelLike,
        config::Config,
    )

Test setting objective sense to FEASIBILITY_SENSE.
"""
function test_objective_ObjectiveSense_FEASIBILITY_SENSE(
    model::MOI.ModelLike,
    ::Config,
)
    @requires MOI.supports(model, MOI.ObjectiveSense())
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.FEASIBILITY_SENSE
end

"""
    test_objective_get_ObjectiveFunction_ScalarAffineFunction(
        model::MOI.ModelLike,
        config::Config,
    )

Test get objective function.
"""
function test_objective_get_ObjectiveFunction_ScalarAffineFunction(
    model::MOI.ModelLike,
    ::Config,
)
    obj_attr = MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()
    @requires MOI.supports(model, obj_attr)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 1.0)
    MOI.set(model, obj_attr, f)
    @test_throws(
        InexactError,
        MOI.get(model, MOI.ObjectiveFunction{MOI.SingleVariable}()),
    )
    obj_fun = MOI.get(model, obj_attr)
    @test obj_fun ≈ f
    quad_obj_fun = MOI.get(
        model,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
    )
    @test convert(MOI.ScalarAffineFunction{Float64}, quad_obj_fun) ≈ f
    return
end

"""
    test_objective_ObjectiveFunction_constant(
        model::MOI.ModelLike,
        config::Config,
    )

Test constant in linear objective,  if `config.solve=true` confirm that it
solves correctly, and if `config.duals=true`, check that the duals are computed
correctly.
"""
function test_objective_ObjectiveFunction_constant(
    model::MOI.ModelLike,
    config::Config,
)
    obj_attr = MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()
    @requires MOI.supports(model, obj_attr)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 1.0)
    MOI.set(model, obj_attr, f)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    c = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(1.0))
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
    ::typeof(test_objective_ObjectiveFunction_constant),
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
    test_objective_ObjectiveFunction_blank(model::MOI.ModelLike, config::Config)

Test blank linear objective,  if `config.solve=true` confirm that it
solves correctly, and if `config.duals=true`, check that the duals are computed
correctly.
"""
function test_objective_ObjectiveFunction_blank(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    obj_attr = MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()
    @requires MOI.supports(model, obj_attr)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(0.0, x)], 0.0)
    MOI.set(model, obj_attr, f)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test MOI.get(model, MOI.ObjectiveValue()) == 0.0
    return
end

function setup_test(
    ::typeof(test_objective_ObjectiveFunction_blank),
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
    test_objective_ObjectiveFunction_SingleVariable(
        model::MOI.ModelLike,
        config::Config,
    )

Test SingleVariable objective,  if `config.solve=true` confirm that it
solves correctly, and if `config.duals=true`, check that the duals are computed
correctly.
"""
function test_objective_ObjectiveFunction_SingleVariable(
    model::MOI.ModelLike,
    config::Config,
)
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.SingleVariable}())
    x = MOI.add_variable(model)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    c = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(1.0))
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
    ::typeof(test_objective_ObjectiveFunction_SingleVariable),
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
    test_objective_qp_ObjectiveFunction_edge_cases(
        model::MOI.ModelLike,
        config::Config,
    )

Test various edge cases relating to quadratic programs (i.e., with a quadratic
objective function).

If `config.solve=true` confirm that it solves correctly.
"""
function test_objective_qp_ObjectiveFunction_edge_cases(
    model::MOI.ModelLike,
    config::Config,
)
    obj_attr = MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}()
    @requires MOI.supports(model, obj_attr)
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
    ::typeof(test_objective_qp_ObjectiveFunction_edge_cases),
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
    test_objective_qp_ObjectiveFunction_zero_ofdiag(
        model::MOI.ModelLike,
        config::Config,
    )

Test quadratic program with a zero off-diagonal term.

If `config.solve=true` confirm that it solves correctly.
"""
function test_objective_qp_ObjectiveFunction_zero_ofdiag(
    model::MOI.ModelLike,
    config::Config,
)
    obj_attr = MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}()
    @requires MOI.supports(model, obj_attr)
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
    ::typeof(test_objective_qp_ObjectiveFunction_zero_ofdiag),
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
    test_objective_ObjectiveFunction_duplicate_terms(
        model::MOI.ModelLike,
        config::Config,
    )

Test duplicate terms in linear objective, if `config.solve=true` confirm that it
solves correctly.
"""
function test_objective_ObjectiveFunction_duplicate_terms(
    model::MOI.ModelLike,
    config::Config,
)
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
    ::typeof(test_objective_ObjectiveFunction_duplicate_terms),
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
    test_objective_set_via_modify(model::MOI.ModelLike, config::Config)

Test that a SclaarAffineFunction can be set via modification without setting an
objective prior.
"""
function test_objective_set_via_modify(model::MOI.ModelLike, config::Config)
    attr = MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()
    @requires MOI.supports(model, attr)
    @requires _supports(config, MOI.modify)
    @requires _supports(config, MOI.ScalarCoefficientChange)
    @test MOI.get(model, MOI.ListOfModelAttributesSet()) == []
    x = MOI.add_variable(model)
    MOI.modify(model, attr, MOI.ScalarCoefficientChange(x, 1.0))
    @test MOI.get(model, MOI.ListOfModelAttributesSet()) == [attr]
    return
end

"""
    test_incorrect_modifications(model::MOI.ModelLike, config::Config)

Test that constraint sets cannot be set for the wrong set type, and that
SingleVariable functions cannot be modified.
"""
function test_incorrect_modifications(model::MOI.ModelLike, ::Config)
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )
    x = MOI.add_variable(model)
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
        MOI.EqualTo(1.0),
    )
    @test_throws(
        ArgumentError,
        MOI.set(model, MOI.ConstraintSet(), c, MOI.LessThan(1.0)),
    )
    @test_throws(
        ArgumentError,
        MOI.set(model, MOI.ConstraintFunction(), c, MOI.SingleVariable(x)),
    )
    return
end
