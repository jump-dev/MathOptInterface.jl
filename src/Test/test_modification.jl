"""
    test_modification_set_function_single_variable(
        model::MOI.ModelLike,
        config::Config,
    )

Test that modifying the function of a `VariableIndex`-in-`LessThan` constraint
throws a [`SettingVariableIndexNotAllowed`](@ref) error.
"""
function test_modification_set_function_single_variable(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    x, y = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = T(1) * x + T(1) * y
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    c = MOI.add_constraint(model, x, MOI.LessThan(T(1)))
    err = MOI.SettingVariableIndexNotAllowed()
    @test_throws err MOI.set(model, MOI.ConstraintFunction(), c, y)
    return
end

"""
    test_modification_set_singlevariable_lessthan(
        model::MOI.ModelLike,
        config::Config,
    )

Test set modification VariableIndex-in-LessThan constraint. If
`config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function test_modification_set_singlevariable_lessthan(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = T(1) * x
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    c = MOI.add_constraint(model, x, MOI.LessThan(T(1)))
    _test_model_solution(
        model,
        config;
        objective_value = T(1),
        variable_primal = [(x, T(1))],
        constraint_primal = [(c, T(1))],
        constraint_dual = [(c, T(-1))],
    )
    MOI.set(model, MOI.ConstraintSet(), c, MOI.LessThan(T(2)))
    @test MOI.get(model, MOI.ConstraintSet(), c) == MOI.LessThan(T(2))
    _test_model_solution(
        model,
        config;
        objective_value = T(2),
        variable_primal = [(x, T(2))],
        constraint_primal = [(c, T(2))],
        constraint_dual = [(c, T(-1))],
    )
    return
end

function setup_test(
    ::typeof(test_modification_set_singlevariable_lessthan),
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
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[2]),
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

Test set transformation of a VariableIndex-in-LessThan constraint. If
`config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function test_modification_transform_singlevariable_lessthan(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = T(1) * x
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    c = MOI.add_constraint(model, x, MOI.LessThan(T(1)))
    _test_model_solution(
        model,
        config;
        objective_value = T(1),
        variable_primal = [(x, T(1))],
        constraint_primal = [(c, T(1))],
        constraint_dual = [(c, T(-1))],
    )
    c2 = MOI.transform(model, c, MOI.GreaterThan(T(2)))
    @test !MOI.is_valid(model, c)
    @test MOI.is_valid(model, c2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    _test_model_solution(
        model,
        config;
        objective_value = T(2),
        variable_primal = [(x, T(2))],
        constraint_primal = [(c2, T(2))],
        constraint_dual = [(c2, T(1))],
    )
    return
end

function setup_test(
    ::typeof(test_modification_transform_singlevariable_lessthan),
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
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[2]),
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
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = T(1) * x
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    c = MOI.add_constraint(model, f, MOI.LessThan(T(1)))
    _test_model_solution(
        model,
        config;
        objective_value = T(1),
        variable_primal = [(x, T(1))],
        constraint_primal = [(c, T(1))],
        constraint_dual = [(c, T(-1))],
    )
    MOI.set(model, MOI.ConstraintSet(), c, MOI.LessThan(T(2)))
    @test MOI.get(model, MOI.ConstraintSet(), c) == MOI.LessThan(T(2))
    _test_model_solution(
        model,
        config;
        objective_value = T(2),
        variable_primal = [(x, T(2))],
        constraint_primal = [(c, T(2))],
        constraint_dual = [(c, T(-1))],
    )
    return
end

function setup_test(
    ::typeof(test_modification_set_scalaraffine_lessthan),
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
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-1],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[2]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-1],
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
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = T(1) * x
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    c = MOI.add_constraint(model, f, MOI.LessThan(T(1)))
    _test_model_solution(
        model,
        config;
        objective_value = T(1),
        variable_primal = [(x, T(1))],
        constraint_primal = [(c, T(1))],
        constraint_dual = [(c, T(-1))],
    )
    MOI.modify(model, c, MOI.ScalarCoefficientChange(x, T(2)))
    _test_model_solution(
        model,
        config;
        objective_value = T(1 // 2),
        variable_primal = [(x, T(1 // 2))],
        constraint_primal = [(c, T(1))],
        constraint_dual = [(c, T(-1 // 2))],
    )
    return
end

function setup_test(
    ::typeof(test_modification_coef_scalaraffine_lessthan),
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
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-1],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1//2]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-1//2],
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
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = T(1) * x
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    c = MOI.add_constraint(model, f, MOI.LessThan(T(1)))
    _test_model_solution(
        model,
        config;
        objective_value = T(1),
        variable_primal = [(x, T(1))],
        constraint_primal = [(c, T(1))],
        constraint_dual = [(c, T(-1))],
    )
    MOI.set(
        model,
        MOI.ConstraintFunction(),
        c,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(2), x)], T(0)),
    )
    foo = MOI.get(model, MOI.ConstraintFunction(), c)
    @test foo ≈ MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(2), x)], T(0))
    _test_model_solution(
        model,
        config;
        objective_value = T(1 // 2),
        variable_primal = [(x, T(1 // 2))],
        # constraint_primal = [(c, T(1))],
        constraint_dual = [(c, T(-1 // 2))],
    )
    return
end

function setup_test(
    ::typeof(test_modification_func_scalaraffine_lessthan),
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
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-1],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1//2]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-1//2],
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
    config::Config{T},
) where {T}
    x, y = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = T(1) * x + T(2) * y
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    g = MOI.Utilities.operate(vcat, T, T(1) * x, T(2) * y)
    c = MOI.add_constraint(model, g, MOI.Nonnegatives(2))
    _test_model_solution(
        model,
        config;
        objective_value = T(0),
        variable_primal = [(x, T(0)), (y, T(0))],
        constraint_primal = [(c, T[0, 0])],
    )
    MOI.set(
        model,
        MOI.ConstraintFunction(),
        c,
        MOI.VectorAffineFunction(
            [
                MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x)),
                MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(2), y)),
            ],
            T[-1, -3//2],
        ),
    )
    foo = MOI.get(model, MOI.ConstraintFunction(), c)
    @test foo ≈ MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(2), y)),
        ],
        T[-1, -3//2],
    )
    _test_model_solution(
        model,
        config;
        objective_value = T(5 // 2),
        variable_primal = [(x, T(1)), (y, T(3 // 4))],
        constraint_primal = [(c, T[0, 0])],
    )
    return
end

function setup_test(
    ::typeof(test_modification_func_vectoraffine_nonneg),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[0, 0]),
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1, 3//4]),
        ),
    )
    return
end

"""
    test_modification_const_vectoraffine_zeros(
        model::MOI.ModelLike,
        config::Config,
    )

Test modifying the constant term in a VectorAffineFunction-in-Zeros constraint.
If `config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function test_modification_const_vectoraffine_zeros(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    x, y = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = T(1) * x + T(2) * y
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    c = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [
                MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x)),
                MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(2), y)),
            ],
            T[0, 0],
        ),
        MOI.Zeros(2),
    )
    _test_model_solution(
        model,
        config;
        objective_value = T(0),
        variable_primal = [(x, T(0)), (y, T(0))],
        constraint_primal = [(c, T[0, 0])],
    )
    MOI.modify(model, c, MOI.VectorConstantChange(T[-1, -3//2]))
    _test_model_solution(
        model,
        config;
        objective_value = T(5 // 2),
        variable_primal = [(x, T(1)), (y, T(3 // 4))],
        constraint_primal = [(c, T[0, 0])],
    )
    return
end

version_added(::typeof(test_modification_const_vectoraffine_zeros)) = v"0.10.7"

function setup_test(
    ::typeof(test_modification_const_vectoraffine_zeros),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[0, 0]),
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1, 3//4]),
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
    config::Config{T},
) where {T}
    x, y = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = T(1) * x + T(2) * y
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    c = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [
                MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x)),
                MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(2), y)),
            ],
            T[0, 0],
        ),
        MOI.Nonpositives(2),
    )
    _test_model_solution(
        model,
        config;
        objective_value = T(0),
        variable_primal = [(x, T(0)), (y, T(0))],
        constraint_primal = [(c, T[0, 0])],
    )
    MOI.modify(model, c, MOI.VectorConstantChange(T[-1, -3//2]))
    _test_model_solution(
        model,
        config;
        objective_value = T(5 // 2),
        variable_primal = [(x, T(1)), (y, T(3 // 4))],
        constraint_primal = [(c, T[0, 0])],
    )
    return
end

function setup_test(
    ::typeof(test_modification_const_vectoraffine_nonpos),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[0, 0]),
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1, 3//4]),
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
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x)], T(0))
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    c = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [
                MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x)),
                MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(2), x)),
            ],
            T[-1, -1],
        ),
        MOI.Nonpositives(2),
    )
    _test_model_solution(
        model,
        config;
        objective_value = T(1 // 2),
        variable_primal = [(x, T(1 // 2))],
        constraint_primal = [(c, T[-1//2, 0])],
    )
    MOI.modify(model, c, MOI.MultirowChange(x, [(1, T(4)), (2, T(3))]))
    _test_model_solution(
        model,
        config;
        objective_value = T(1 // 4),
        variable_primal = [(x, T(1 // 4))],
        constraint_primal = [(c, T[0, -1//4])],
    )
    return
end

function setup_test(
    ::typeof(test_modification_multirow_vectoraffine_nonpos),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1//2]),
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1//4]),
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
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x)], T(2))
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.add_constraint(model, x, MOI.LessThan(T(1)))
    _test_model_solution(
        model,
        config;
        objective_value = T(3),
        variable_primal = [(x, T(1))],
    )
    MOI.modify(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarConstantChange(T(3)),
    )
    _test_model_solution(
        model,
        config;
        objective_value = T(4),
        variable_primal = [(x, T(1))],
    )
    return
end

function setup_test(
    ::typeof(test_modification_const_scalar_objective),
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
    test_modification_coef_scalar_objective(
        model::MOI.ModelLike,
        config::Config
    )

Test modifying a variable coefficient in a scalaraffine objective. If
`config.solve=true` confirm that it solves correctly.
"""
function test_modification_coef_scalar_objective(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x)], T(0))
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.add_constraint(model, x, MOI.LessThan(T(1)))
    _test_model_solution(
        model,
        config;
        objective_value = T(1),
        variable_primal = [(x, T(1))],
    )
    MOI.modify(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarCoefficientChange(x, T(3)),
    )
    _test_model_solution(
        model,
        config;
        objective_value = T(3),
        variable_primal = [(x, T(1))],
    )
    return
end

function setup_test(
    ::typeof(test_modification_coef_scalar_objective),
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
    test_modification_delete_variable_with_single_variable_obj(
        model::MOI.ModelLike,
        config::Config,
    )

Test deleting a variable that is the objective function.
"""
function test_modification_delete_variable_with_single_variable_obj(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    c = MOI.add_constraint(model, x, MOI.GreaterThan(T(1)))
    MOI.delete(model, y)
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
    ::typeof(test_modification_delete_variable_with_single_variable_obj),
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
    config::Config{T},
) where {T}
    x, y, z = MOI.add_variables(model, 3)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = T(1) * x + T(2) * y + T(3) * z
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.add_constraint.(model, [x, y, z], MOI.GreaterThan(T(1)))
    @test MOI.is_valid(model, x)
    @test MOI.is_valid(model, y)
    @test MOI.is_valid(model, z)
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test isapprox(MOI.get(model, MOI.ObjectiveValue()), T(6), config)
    end
    MOI.delete(model, [x, z])
    @test !MOI.is_valid(model, x)
    @test MOI.is_valid(model, y)
    @test !MOI.is_valid(model, z)
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test isapprox(MOI.get(model, MOI.ObjectiveValue()), T(2), config)
    end
    return
end

function setup_test(
    ::typeof(test_modification_delete_variables_in_a_batch),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1, 1, 1]),
            MOI.FEASIBLE_POINT,
        ),
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
    test_modification_affine_deletion_edge_cases(
        model::MOI.ModelLike,
        config::Config,
    )

Test various edge cases relating to deleting affine constraints. This requires
    + ScalarAffineFunction-in-LessThan; and
    + VectorAffineFunction-in-Nonpositives.
"""
function test_modification_affine_deletion_edge_cases(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    # helpers. The function 1.0x + 0.0
    saf = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x)], T(0))
    vaf = MOI.VectorAffineFunction(
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x))],
        T[0],
    )
    vaf2 = MOI.VectorAffineFunction(
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x))],
        T[-2],
    )
    # max x
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), saf)
    # test adding a VectorAffineFunction -in- LessThan
    c1 = MOI.add_constraint(model, vaf, MOI.Nonpositives(1))
    _test_model_solution(
        model,
        config;
        objective_value = T(0),
        constraint_primal = [(c1, T[0])],
    )
    # test adding a ScalarAffineFunction -in- LessThan
    c2 = MOI.add_constraint(model, saf, MOI.LessThan(T(1)))
    _test_model_solution(
        model,
        config;
        objective_value = T(0),
        constraint_primal = [(c1, T[0]), (c2, T(0))],
    )
    # now delete the VectorAffineFunction
    MOI.delete(model, c1)
    @test_throws MOI.InvalidIndex{typeof(c1)} MOI.delete(model, c1)
    try
        MOI.delete(model, c1)
    catch err
        @test err.index == c1
    end
    _test_model_solution(
        model,
        config;
        objective_value = T(1),
        constraint_primal = [(c2, T(1))],
    )
    # add a different VectorAffineFunction constraint
    c3 = MOI.add_constraint(model, vaf2, MOI.Nonpositives(1))
    _test_model_solution(
        model,
        config;
        objective_value = T(1),
        constraint_primal = [(c2, T(1)), (c3, T[-1])],
    )
    # delete the ScalarAffineFunction
    MOI.delete(model, c2)
    _test_model_solution(
        model,
        config;
        objective_value = T(2),
        constraint_primal = [(c3, T[0])],
    )
    return
end

function setup_test(
    ::typeof(test_modification_affine_deletion_edge_cases),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, T[0])),
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, T[0])),
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, T[1])),
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, T[1])),
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, T[2])),
    )
    return
end

"""
    test_modification_incorrect(model::MOI.ModelLike, config::Config{T}) where {T}

Test that constraint sets and functions cannot be set for the wrong type.
"""
function test_modification_incorrect(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.EqualTo{T},
    )
    x = MOI.add_variable(model)
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x)], T(0)),
        MOI.EqualTo(T(1)),
    )
    @test_throws(
        MOI.SetTypeMismatch{MOI.EqualTo{T},MOI.LessThan{T}},
        MOI.set(model, MOI.ConstraintSet(), c, MOI.LessThan(T(1))),
    )
    @test_throws(
        MOI.FunctionTypeMismatch{MOI.ScalarAffineFunction{T},MOI.VariableIndex},
        MOI.set(model, MOI.ConstraintFunction(), c, x),
    )
    return
end

"""
    test_modification_incorrect_VariableIndex(
        model::MOI.ModelLike,
        config::Config,
    )

Test that constraint sets cannot be set for the wrong set type, and that
VariableIndex functions cannot be modified.
"""
function test_modification_incorrect_VariableIndex(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.LessThan{T})
    @requires(
        MOI.supports_constraint(model, MOI.VariableIndex, MOI.GreaterThan{T}),
    )
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, x, MOI.GreaterThan(T(0)))
    @test_throws(
        MOI.SetTypeMismatch{MOI.GreaterThan{T},MOI.LessThan{T}},
        MOI.set(model, MOI.ConstraintSet(), c, MOI.LessThan(T(1))),
    )
    @test_throws(
        MOI.FunctionTypeMismatch{MOI.VariableIndex,MOI.ScalarAffineFunction{T}},
        MOI.set(model, MOI.ConstraintFunction(), c, T(1) * x),
    )
    y = MOI.add_variable(model)
    @test_throws(
        MOI.SettingVariableIndexNotAllowed(),
        MOI.set(model, MOI.ConstraintFunction(), c, y),
    )
    return
end
