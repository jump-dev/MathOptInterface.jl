"""
    test_objective_ObjectiveSense_MAX_SENSE(
        model::MOI.ModelLike,
        config::Config,
    )

Test setting objective sense to MAX_SENSE.
"""
function test_objective_ObjectiveSense_MAX_SENSE(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
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
function test_objective_ObjectiveSense_MIN_SENSE(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
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
    ::Config{T},
) where {T}
    @requires MOI.supports(model, MOI.ObjectiveSense())
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.FEASIBILITY_SENSE
end

"""
    test_objective_FEASIBILITY_SENSE_clears_objective(
        model::MOI.ModelLike,
        config::Config,
    )

Test setting objective sense to FEASIBILITY_SENSE  clears previous objective.
"""
function test_objective_FEASIBILITY_SENSE_clears_objective(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.GreaterThan(T(1)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x)], T(0))
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.optimize!(model)
    @test isapprox(MOI.get(model, MOI.ObjectiveValue()), T(1), config)
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    MOI.optimize!(model)
    @test isapprox(MOI.get(model, MOI.ObjectiveValue()), T(0), config)
    return
end

function setup_test(
    ::typeof(test_objective_FEASIBILITY_SENSE_clears_objective),
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
    test_objective_get_ObjectiveFunction_ScalarAffineFunction(
        model::MOI.ModelLike,
        config::Config,
    )

Test get objective function.
"""
function test_objective_get_ObjectiveFunction_ScalarAffineFunction(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    obj_attr = MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}()
    @requires MOI.supports(model, obj_attr)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(2), x)], T(1))
    MOI.set(model, obj_attr, f)
    @test_throws(
        InexactError,
        MOI.get(model, MOI.ObjectiveFunction{MOI.VariableIndex}()),
    )
    obj_fun = MOI.get(model, obj_attr)
    @test obj_fun ≈ f
    quad_obj_fun =
        MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}}())
    @test convert(MOI.ScalarAffineFunction{T}, quad_obj_fun) ≈ f
    _test_attribute_value_type(model, MOI.ObjectiveFunctionType())
    _test_attribute_value_type(model, obj_attr)
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
    config::Config{T},
) where {T}
    obj_attr = MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}()
    @requires MOI.supports(model, obj_attr)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(2), x)], T(1))
    MOI.set(model, obj_attr, f)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    c = MOI.add_constraint(model, x, MOI.GreaterThan(T(1)))
    _test_model_solution(
        model,
        config;
        objective_value = T(3),
        variable_primal = [(x, T(1))],
        constraint_primal = [(c, T(1))],
        constraint_dual = [(c, T(2))],
    )
    return
end

function setup_test(
    ::typeof(test_objective_ObjectiveFunction_constant),
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
        ),
    )
    return
end

"""
    test_objective_ObjectiveFunction_blank(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test blank linear objective,  if `config.solve=true` confirm that it
solves correctly, and if `config.duals=true`, check that the duals are computed
correctly.
"""
function test_objective_ObjectiveFunction_blank(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    obj_attr = MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}()
    @requires MOI.supports(model, obj_attr)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(0), x)], T(0))
    MOI.set(model, obj_attr, f)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test MOI.get(model, MOI.ObjectiveValue()) == T(0)
    return
end

function setup_test(
    ::typeof(test_objective_ObjectiveFunction_blank),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[6//5]),
            MOI.FEASIBLE_POINT,
        ),
    )
    return
end

"""
    test_objective_ObjectiveFunction_VariableIndex(
        model::MOI.ModelLike,
        config::Config,
    )

Test VariableIndex objective,  if `config.solve=true` confirm that it
solves correctly, and if `config.duals=true`, check that the duals are computed
correctly.
"""
function test_objective_ObjectiveFunction_VariableIndex(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    x = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    c = MOI.add_constraint(model, x, MOI.GreaterThan(T(1)))
    _test_attribute_value_type(model, MOI.ObjectiveFunctionType())
    _test_attribute_value_type(
        model,
        MOI.ObjectiveFunction{MOI.VariableIndex}(),
    )
    _test_model_solution(
        model,
        config;
        objective_value = T(1),
        variable_primal = [(x, T(1))],
        constraint_primal = [(c, T(1))],
        constraint_dual = [(c, T(1))],
    )
    return
end

function setup_test(
    ::typeof(test_objective_ObjectiveFunction_VariableIndex),
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
    config::Config{T},
) where {T}
    obj_attr = MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}}()
    @requires MOI.supports(model, obj_attr)
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    vc1 = MOI.add_constraint(model, x[1], MOI.GreaterThan(T(1)))
    @test vc1.value == x[1].value
    vc2 = MOI.add_constraint(model, x[2], MOI.GreaterThan(T(2)))
    @test vc2.value == x[2].value
    # Basic model
    # min x^2 + y^2 | x>=1, y>=2
    MOI.set(
        model,
        obj_attr,
        MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.(T(2), x, x),  # quad
            MOI.ScalarAffineTerm{T}[],  # affine terms
            T(0),  # constant
        ),
    )
    _test_model_solution(
        model,
        config;
        objective_value = T(5),
        variable_primal = [(x[1], T(1)), (x[2], T(2))],
    )
    # Duplicate linear terms
    # min x + x + x^2 + y^2 | x>=1, y>=2
    MOI.set(
        model,
        obj_attr,
        MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.(T(2), x, x),  # quad
            MOI.ScalarAffineTerm.(T(1), [x[1], x[1]]),  # affine terms
            T(0),  # constant
        ),
    )
    _test_model_solution(
        model,
        config;
        objective_value = T(7),
        variable_primal = [(x[1], T(1)), (x[2], T(2))],
    )
    # Duplicate diagonal terms
    # min x^2 + x^2 | x>=1, y>=2
    MOI.set(
        model,
        obj_attr,
        MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.(T(2), [x[1], x[1]], [x[1], x[1]]),  # quad
            MOI.ScalarAffineTerm{T}[],  # affine terms
            T(0),  # constant
        ),
    )
    _test_model_solution(
        model,
        config;
        objective_value = T(2),
        variable_primal = [(x[1], T(1))],
    )
    # Duplicate off-diagonal terms" begin
    # min x^2 + 0.25x*y + 0.25y*x + 0.5x*y + y^2 | x>=1, y>=2
    MOI.set(
        model,
        obj_attr,
        MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.(
                T[2, 1//4, 1//4, 1//2, 2],
                [x[1], x[1], x[2], x[1], x[2]],
                [x[1], x[2], x[1], x[2], x[2]],
            ),  # quad
            MOI.ScalarAffineTerm{T}[],  # affine terms
            T(0),  # constant
        ),
    )
    _test_model_solution(
        model,
        config;
        objective_value = T(7),
        variable_primal = [(x[1], T(1)), (x[2], T(2))],
    )
    return
end

function setup_test(
    ::typeof(test_objective_qp_ObjectiveFunction_edge_cases),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1, 2]),
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1, 2]),
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1, 2]),
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1, 2]),
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
    config::Config{T},
) where {T}
    obj_attr = MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}}()
    @requires MOI.supports(model, obj_attr)
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    vc1 = MOI.add_constraint(model, x[1], MOI.GreaterThan(T(1)))
    @test vc1.value == x[1].value
    vc2 = MOI.add_constraint(model, x[2], MOI.GreaterThan(T(2)))
    @test vc2.value == x[2].value
    MOI.set(
        model,
        obj_attr,
        MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.(
                T[2, 0, 2],
                [x[1], x[1], x[2]],
                [x[1], x[2], x[2]],
            ),  # quad
            MOI.ScalarAffineTerm{T}[],  # affine terms
            T(0),  # constant
        ),
    )
    _test_model_solution(
        model,
        config;
        objective_value = T(5),
        variable_primal = [(x[1], T(1)), (x[2], T(2))],
    )
    return
end

function setup_test(
    ::typeof(test_objective_qp_ObjectiveFunction_zero_ofdiag),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1, 2]),
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
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, x, MOI.GreaterThan(T(1)))
    @test c.value == x.value
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[2, 1], x), T(0)),
    )
    _test_model_solution(
        model,
        config;
        objective_value = T(3),
        variable_primal = [(x, T(1))],
        constraint_primal = [(c, T(1))],
        constraint_dual = [(c, T(3))],
    )
    return
end

function setup_test(
    ::typeof(test_objective_ObjectiveFunction_duplicate_terms),
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
        ),
    )
    return
end

"""
    test_objective_set_via_modify(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test that a SclaarAffineFunction can be set via modification without setting an
objective prior.
"""
function test_objective_set_via_modify(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    attr = MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}()
    @requires MOI.supports(model, attr)
    @requires _supports(config, MOI.modify)
    @requires _supports(config, MOI.ScalarCoefficientChange)
    @test MOI.get(model, MOI.ListOfModelAttributesSet()) == []
    x = MOI.add_variable(model)
    MOI.modify(model, attr, MOI.ScalarCoefficientChange(x, T(1)))
    @test MOI.get(model, MOI.ListOfModelAttributesSet()) == [attr]
    return
end
