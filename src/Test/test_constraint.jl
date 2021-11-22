"""
    test_constraint_get_ConstraintIndex(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test getting constraints by name.
"""
function test_constraint_get_ConstraintIndex(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    x = MOI.add_variable(model)
    c1 = MOI.add_constraint(model, T(1) * x, MOI.GreaterThan(T(1)))
    MOI.set(model, MOI.ConstraintName(), c1, "c1")
    c2 = MOI.add_constraint(model, T(1) * x, MOI.LessThan(T(2)))
    MOI.set(model, MOI.ConstraintName(), c2, "c2")
    F = MOI.ScalarAffineFunction{T}
    @test MOI.get(model, MOI.ConstraintIndex, "c3") === nothing
    @test MOI.get(model, MOI.ConstraintIndex{F,MOI.LessThan{T}}, "c1") ===
          nothing
    @test MOI.get(model, MOI.ConstraintIndex{F,MOI.GreaterThan{T}}, "c2") ===
          nothing
    c1 = MOI.get(model, MOI.ConstraintIndex{F,MOI.GreaterThan{T}}, "c1")
    @test MOI.get(model, MOI.ConstraintIndex, "c1") == c1
    @test MOI.is_valid(model, c1)
    c2 = MOI.get(model, MOI.ConstraintIndex{F,MOI.LessThan{T}}, "c2")
    @test MOI.get(model, MOI.ConstraintIndex, "c2") == c2
    @test MOI.is_valid(model, c2)
    return
end

"""
    test_constraint_ScalarAffineFunction_LessThan(
        model::MOI.ModelLike,
        config::Config,
    )

Add an ScalarAffineFunction-in-LessThan constraint.
"""
function test_constraint_ScalarAffineFunction_LessThan(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.LessThan{T},
    )
    x = MOI.add_variable(model)
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(2), x)], T(0)),
        MOI.LessThan(T(1)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x)], T(0)),
    )
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
    ::typeof(test_constraint_ScalarAffineFunction_LessThan),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
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
    test_constraint_ScalarAffineFunction_GreaterThan(
        model::MOI.ModelLike,
        config::Config,
    )

Add an ScalarAffineFunction-in-GreaterThan constraint.
"""
function test_constraint_ScalarAffineFunction_GreaterThan(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.GreaterThan{T},
    )
    x = MOI.add_variable(model)
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(2), x)], T(0)),
        MOI.GreaterThan(T(1)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x)], T(0)),
    )
    _test_model_solution(
        model,
        config;
        objective_value = T(1 // 2),
        variable_primal = [(x, T(1 // 2))],
        constraint_primal = [(c, T(1))],
        constraint_dual = [(c, T(1 // 2))],
    )
    return
end

function setup_test(
    ::typeof(test_constraint_ScalarAffineFunction_GreaterThan),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1//2]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}) => T[1//2],
        ),
    )
    return
end

"""
    test_constraint_ScalarAffineFunction_EqualTo(
        model::MOI.ModelLike,
        config::Config,
    )

Add an ScalarAffineFunction-in-EqualTo constraint.
"""
function test_constraint_ScalarAffineFunction_EqualTo(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.EqualTo{T},
    )
    x = MOI.add_variable(model)
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(2), x)], T(0)),
        MOI.EqualTo(T(1)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x)], T(0)),
    )
    _test_model_solution(
        model,
        config;
        objective_value = T(1 // 2),
        variable_primal = [(x, T(1 // 2))],
        constraint_primal = [(c, T(1))],
        constraint_dual = [(c, T(1 // 2))],
    )
    return
end

function setup_test(
    ::typeof(test_constraint_ScalarAffineFunction_EqualTo),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1//2]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => T[1//2],
        ),
    )
    return
end

"""
    test_constraint_ScalarAffineFunction_Interval(
        model::MOI.ModelLike,
        config::Config,
    )

Add an ScalarAffineFunction-in-Interval constraint.
"""
function test_constraint_ScalarAffineFunction_Interval(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.Interval{T},
    )
    x = MOI.add_variable(model)
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(2), x)], T(0)),
        MOI.Interval(T(1), T(4)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(3), x)], T(0)),
    )
    _test_model_solution(
        model,
        config;
        objective_value = T(6),
        variable_primal = [(x, T(2))],
        constraint_primal = [(c, T(4))],
        constraint_dual = [(c, T(-3 // 2))],
    )
    return
end

function setup_test(
    ::typeof(test_constraint_ScalarAffineFunction_Interval),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[2]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{T}, MOI.Interval{T}) => T[-3//2],
        ),
    )
    return
end

"""
    test_constraint_ScalarAffineFunction_duplicate(
        model::MOI.ModelLike,
        config::Config,
    )

Add a `ScalarAffineFunction`-in-`LessThan` constraint with duplicate terms in
the function.

Taken from https://github.com/JuliaOpt/MathOptInterfaceMosek.jl/issues/41
"""
function test_constraint_ScalarAffineFunction_duplicate(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    objective_function =
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x)], T(0))
    MOI.set(
        model,
        MOI.ObjectiveFunction{typeof(objective_function)}(),
        objective_function,
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T(1), [x, x]), T(0))
    c = MOI.add_constraint(model, f, MOI.LessThan(T(1)))
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
    ::typeof(test_constraint_ScalarAffineFunction_duplicate),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
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
    test_constraint_VectorAffineFunction_duplicate(
        model::MOI.ModelLike,
        config::Config,
    )

Add a `VectorAffineFunction`-in-`Nonpositives` constraint with duplicate terms
in the function.
"""
function test_constraint_VectorAffineFunction_duplicate(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    x = MOI.add_variable(model)
    objective_function =
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x)], T(0))
    MOI.set(
        model,
        MOI.ObjectiveFunction{typeof(objective_function)}(),
        objective_function,
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(1, MOI.ScalarAffineTerm.(T(1), [x, x])),
        T[-1],
    )
    c = MOI.add_constraint(model, f, MOI.Nonpositives(1))
    _test_model_solution(
        model,
        config;
        objective_value = T(1 // 2),
        variable_primal = [(x, T(1 // 2))],
        constraint_primal = [(c, T[0])],
        constraint_dual = [(c, T[-1//2])],
    )
    return
end

function setup_test(
    ::typeof(test_constraint_VectorAffineFunction_duplicate),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1//2]),
            MOI.FEASIBLE_POINT,
            (MOI.VectorAffineFunction{T}, MOI.Nonpositives) => [T[-1//2]],
        ),
    )
    return
end

"""
    test_constraint_qcp_duplicate_diagonal(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test a QCP problem with a duplicate diagonal term.

The problem is `max x + 2y | y + x^2 + x^2 <= 1, x >= 0.5, y >= 0.5`.
"""
function test_constraint_qcp_duplicate_diagonal(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarQuadraticFunction{T},
        MOI.LessThan{T},
    )
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[1, 2], x), T(0)),
    )
    vc1 = MOI.add_constraint(model, x[1], MOI.GreaterThan(T(1 // 2)))
    # We test this after the creation of every `VariableIndex` constraint
    # to ensure a good coverage of corner cases.
    @test vc1.value == x[1].value
    vc2 = MOI.add_constraint(model, x[2], MOI.GreaterThan(T(1 // 2)))
    @test vc2.value == x[2].value
    MOI.add_constraint(
        model,
        MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.(T(2), [x[1], x[1]], [x[1], x[1]]),  # quad
            MOI.ScalarAffineTerm.(T(1), [x[2]]),  # affine terms
            T(0),  # constant
        ),
        MOI.LessThan(T(1)),
    )
    _test_model_solution(
        model,
        config;
        objective_value = T(3 // 2),
        variable_primal = [(x[1], T(1 // 2)), (x[2], T(1 // 2))],
    )
    return
end

function setup_test(
    ::typeof(test_constraint_qcp_duplicate_diagonal),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1//2, 1//2]),
        ),
    )
    return
end

"""
    test_constraint_qcp_duplicate_off_diagonal(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test a QCP problem with a duplicate off-diagonal term.

The problem is
`max x + 2y | x^2 + 0.25y*x + 0.25x*y + 0.5x*y + y^2 <= 1, x >= 0.5, y >= 0.5`.

!!! warn
    This problem has an irrational solution! Solvers using rational arithmetic
    should exclude it.
"""
function test_constraint_qcp_duplicate_off_diagonal(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarQuadraticFunction{T},
        MOI.LessThan{T},
    )
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[1, 2], x), T(0)),
    )
    vc1 = MOI.add_constraint(model, x[1], MOI.GreaterThan{T}(T(1 // 2)))
    @test vc1.value == x[1].value
    vc2 = MOI.add_constraint(model, x[2], MOI.GreaterThan{T}(T(1 // 2)))
    @test vc2.value == x[2].value
    MOI.add_constraint(
        model,
        MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.(
                T[2, 1//4, 1//4, 1//2, 2],
                [x[1], x[1], x[2], x[1], x[2]],
                [x[1], x[2], x[1], x[2], x[2]],
            ),  # quad
            MOI.ScalarAffineTerm{T}[],  # affine terms
            T(0),  # constant
        ),
        MOI.LessThan(T(1)),
    )
    x2_solution = (sqrt(T(13)) - T(1)) / T(4)
    _test_model_solution(
        model,
        config;
        objective_value = T(1 // 2) + T(2) * x2_solution,
        variable_primal = [(x[1], T(1 // 2)), (x[2], x2_solution)],
    )
    return
end

function setup_test(
    ::typeof(test_constraint_qcp_duplicate_off_diagonal),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1//2, (sqrt(T(13))-T(1))/T(4)]),
        ),
    )
    return
end

"""
    test_constraint_ZeroOne_bounds(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test a problem with a bounded ZeroOne variable.
"""
function test_constraint_ZeroOne_bounds(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.ZeroOne)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.ZeroOne())
    MOI.add_constraint(model, x, MOI.GreaterThan(T(0)))
    MOI.add_constraint(model, x, MOI.LessThan(T(1)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = T(2) * x
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    _test_model_solution(
        model,
        config;
        objective_value = T(2),
        variable_primal = [(x, T(1))],
    )
    return
end

function setup_test(
    ::typeof(test_constraint_ZeroOne_bounds),
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
    test_constraint_ZeroOne_bounds_2(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test a problem with a ZeroOne and binding fractional upper bound.
"""
function test_constraint_ZeroOne_bounds_2(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.ZeroOne)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.ZeroOne())
    MOI.add_constraint(model, x, MOI.GreaterThan(T(0)))
    MOI.add_constraint(model, x, MOI.LessThan(T(1 // 2)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = T(2) * x
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    _test_model_solution(
        model,
        config;
        objective_value = T(0),
        variable_primal = [(x, T(0))],
    )
    return
end

function setup_test(
    ::typeof(test_constraint_ZeroOne_bounds_2),
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
    test_constraint_ZeroOne_bounds_3(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test a problem with a ZeroOne and infeasible fractional bounds.
"""
function test_constraint_ZeroOne_bounds_3(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.ZeroOne)
    @requires _supports(config, MOI.optimize!)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.ZeroOne())
    MOI.add_constraint(model, x, MOI.GreaterThan(T(1 // 5)))
    MOI.add_constraint(model, x, MOI.LessThan(T(1 // 2)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = T(2) * x
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.infeasible_status
    return
end

function setup_test(
    ::typeof(test_constraint_ZeroOne_bounds_3),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.INFEASIBLE),
    )
    return
end

"""
    test_constraint_PrimalStart_DualStart_SecondOrderCone(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test combining the [`MOI.VariablePrimalStart`](@ref),
[`MOI.ConstraintPrimalStart`](@ref) and [`MOI.ConstraintDualStart`](@ref)
attributes with a `MOI.VectorAffineFunction{T}`-in-`MOI.SecondOrderCone`.
"""
function test_constraint_PrimalStart_DualStart_SecondOrderCone(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.SecondOrderCone,
    )
    @requires _supports(config, MOI.optimize!)
    x = MOI.add_variable(model)
    c = MOI.add_constraint(
        model,
        MOIU.operate(vcat, T, x, T(1)),
        MOI.SecondOrderCone(2),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    if MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
        MOI.set(model, MOI.VariablePrimalStart(), x, T(2))
    end
    if MOI.supports(model, MOI.ConstraintPrimalStart(), typeof(c))
        MOI.set(model, MOI.ConstraintPrimalStart(), c, T[2, 2])
    end
    if MOI.supports(model, MOI.ConstraintDualStart(), typeof(c))
        MOI.set(model, MOI.ConstraintDualStart(), c, T[2, -2])
    end
    _test_model_solution(
        model,
        config;
        objective_value = T(1),
        variable_primal = [(x, T(1))],
        constraint_primal = [(c, T[1, 1])],
        constraint_dual = [(c, T[1, -1])],
    )
    return
end

function setup_test(
    ::typeof(test_constraint_PrimalStart_DualStart_SecondOrderCone),
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
            (MOI.VectorAffineFunction{T}, MOI.SecondOrderCone) =>
                [T[1, -1]],
        ),
    )
    return
end

"""
    test_constraint_ConstraintPrimalStart(
        model::MOI.ModelLike,
        ::Config{T},
    ) where {T}

Test the `ConstraintPrimalStart` attribute.
"""
function test_constraint_ConstraintPrimalStart(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Nonnegatives,
    )
    @requires MOI.supports(
        model,
        MOI.ConstraintPrimalStart(),
        MOI.ConstraintIndex{MOI.VectorAffineFunction{T},MOI.Nonnegatives},
    )
    x = MOI.add_variable(model)
    f = MOI.VectorAffineFunction(
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x))],
        T[0],
    )
    c = MOI.add_constraint(model, f, MOI.Nonnegatives(1))
    @test MOI.get(model, MOI.ConstraintPrimalStart(), c) === nothing
    MOI.set(model, MOI.ConstraintPrimalStart(), c, T[-1])
    @test MOI.get(model, MOI.ConstraintPrimalStart(), c) == T[-1]
    MOI.set(model, MOI.ConstraintPrimalStart(), c, nothing)
    @test MOI.get(model, MOI.ConstraintPrimalStart(), c) === nothing
    return
end

"""
    test_constraint_ConstraintDualStart(
        model::MOI.ModelLike,
        ::Config{T},
    ) where {T}

Test the `ConstraintDualStart` attribute.
"""
function test_constraint_ConstraintDualStart(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Nonnegatives,
    )
    @requires MOI.supports(
        model,
        MOI.ConstraintDualStart(),
        MOI.ConstraintIndex{MOI.VectorAffineFunction{T},MOI.Nonnegatives},
    )
    x = MOI.add_variable(model)
    f = MOI.VectorAffineFunction(
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x))],
        T[0],
    )
    c = MOI.add_constraint(model, f, MOI.Nonnegatives(1))
    @test MOI.get(model, MOI.ConstraintDualStart(), c) === nothing
    MOI.set(model, MOI.ConstraintDualStart(), c, T[-1])
    @test MOI.get(model, MOI.ConstraintDualStart(), c) == T[-1]
    MOI.set(model, MOI.ConstraintDualStart(), c, nothing)
    @test MOI.get(model, MOI.ConstraintDualStart(), c) === nothing
    return
end

"""
    test_constraint_Indicator_ConstraintName(
        model::MOI.ModelLike,
        ::Config{T},
    ) where {T}

Test ConstraintName for indicator sets.
"""
function test_constraint_Indicator_ConstraintName(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    @requires(
        MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.GreaterThan{T}},
        ),
    )
    x = MOI.add_variables(model, 2)
    MOI.add_constraint(model, x[1], MOI.ZeroOne())
    f = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x[1])),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(2), x[2])),
        ],
        T[0, 0],
    )
    s = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.GreaterThan(T(1)))
    c = MOI.add_constraint(model, f, s)
    MOI.set(model, MOI.ConstraintName(), c, "my_indicator")
    @test MOI.get(model, MOI.ConstraintName(), c) == "my_indicator"
    return
end

"""
    test_constraint_Indicator_ACTIVATE_ON_ONE(
        model::MOI.ModelLike,
        ::Config{T},
    ) where {T}

Test ACTIVATE_ON_ONE for indicator sets.
"""
function test_constraint_Indicator_ACTIVATE_ON_ONE(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    @requires(
        MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.GreaterThan{T}},
        ),
    )
    x = MOI.add_variables(model, 2)
    MOI.add_constraint(model, x[1], MOI.ZeroOne())
    f = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x[1])),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(2), x[2])),
        ],
        T[0, 0],
    )
    s = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.GreaterThan(T(1)))
    c = MOI.add_constraint(model, f, s)
    @test MOI.get(model, MOI.ConstraintSet(), c) == s
    @test isapprox(MOI.get(model, MOI.ConstraintFunction(), c), f)
    return
end

"""
    test_constraint_Indicator_ACTIVATE_ON_ZERO(
        model::MOI.ModelLike,
        ::Config{T},
    ) where {T}

Test ACTIVATE_ON_ZERO for indicator sets.
"""
function test_constraint_Indicator_ACTIVATE_ON_ZERO(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    @requires(
        MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.GreaterThan{T}},
        ),
    )
    x = MOI.add_variables(model, 2)
    MOI.add_constraint(model, x[1], MOI.ZeroOne())
    f = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x[1])),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(2), x[2])),
        ],
        T[0, 0],
    )
    s = MOI.Indicator{MOI.ACTIVATE_ON_ZERO}(MOI.GreaterThan(T(1)))
    c = MOI.add_constraint(model, f, s)
    @test MOI.get(model, MOI.ConstraintSet(), c) == s
    @test isapprox(MOI.get(model, MOI.ConstraintFunction(), c), f)
    return
end
