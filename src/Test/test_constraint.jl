"""
    test_constraint_get_ConstraintIndex(model::MOI.ModelLike, config::Config)

Test getting constraints by name.
"""
function test_constraint_get_ConstraintIndex(model::MOI.ModelLike, ::Config)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    minobjective: 2.0x
    c1: 1.0 * x >= 1.0
    c2: 1.0 * x <= 2.0
""",
    )
    F = MOI.ScalarAffineFunction{Float64}
    @test MOI.get(model, MOI.ConstraintIndex, "c3") === nothing
    @test MOI.get(model, MOI.ConstraintIndex{F,MOI.LessThan{Float64}}, "c1") ===
          nothing
    @test MOI.get(
        model,
        MOI.ConstraintIndex{F,MOI.GreaterThan{Float64}},
        "c2",
    ) === nothing
    c1 = MOI.get(model, MOI.ConstraintIndex{F,MOI.GreaterThan{Float64}}, "c1")
    @test MOI.get(model, MOI.ConstraintIndex, "c1") == c1
    @test MOI.is_valid(model, c1)
    c2 = MOI.get(model, MOI.ConstraintIndex{F,MOI.LessThan{Float64}}, "c2")
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
    config::Config,
)
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    x = MOI.add_variable(model)
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 0.0),
        MOI.LessThan(1.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
    )
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
    ::typeof(test_constraint_ScalarAffineFunction_LessThan),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
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
    test_constraint_ScalarAffineFunction_GreaterThan(
        model::MOI.ModelLike,
        config::Config,
    )

Add an ScalarAffineFunction-in-GreaterThan constraint.
"""
function test_constraint_ScalarAffineFunction_GreaterThan(
    model::MOI.ModelLike,
    config::Config,
)
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.GreaterThan{Float64},
    )
    x = MOI.add_variable(model)
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 0.0),
        MOI.GreaterThan(1.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
    )
    _test_model_solution(
        model,
        config;
        objective_value = 0.5,
        variable_primal = [(x, 0.5)],
        constraint_primal = [(c, 1.0)],
        constraint_dual = [(c, 0.5)],
    )
    return
end

function setup_test(
    ::typeof(test_constraint_ScalarAffineFunction_GreaterThan),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.5]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [0.5],
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
    config::Config,
)
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )
    x = MOI.add_variable(model)
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 0.0),
        MOI.EqualTo(1.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
    )
    _test_model_solution(
        model,
        config;
        objective_value = 0.5,
        variable_primal = [(x, 0.5)],
        constraint_primal = [(c, 1.0)],
        constraint_dual = [(c, 0.5)],
    )
    return
end

function setup_test(
    ::typeof(test_constraint_ScalarAffineFunction_EqualTo),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.5]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [0.5],
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
    config::Config,
)
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.Interval{Float64},
    )
    x = MOI.add_variable(model)
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 0.0),
        MOI.Interval(1.0, 4.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(3.0, x)], 0.0),
    )
    _test_model_solution(
        model,
        config;
        objective_value = 6.0,
        variable_primal = [(x, 2.0)],
        constraint_primal = [(c, 4.0)],
        constraint_dual = [(c, -1.5)],
    )
    return
end

function setup_test(
    ::typeof(test_constraint_ScalarAffineFunction_Interval),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [2.0]),
            MOI.FEASIBLE_POINT,
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [-1.5],
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
    config::Config,
)
    x = MOI.add_variable(model)
    objective_function =
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0)
    MOI.set(
        model,
        MOI.ObjectiveFunction{typeof(objective_function)}(),
        objective_function,
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 1.0], [x, x]), 0.0)
    c = MOI.add_constraint(model, f, MOI.LessThan(1.0))
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
    ::typeof(test_constraint_ScalarAffineFunction_duplicate),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
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
    test_constraint_VectorAffineFunction_duplicate(
        model::MOI.ModelLike,
        config::Config,
    )

Add a `VectorAffineFunction`-in-`Nonpositives` constraint with duplicate terms
in the function.
"""
function test_constraint_VectorAffineFunction_duplicate(
    model::MOI.ModelLike,
    config::Config,
)
    x = MOI.add_variable(model)
    objective_function =
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0)
    MOI.set(
        model,
        MOI.ObjectiveFunction{typeof(objective_function)}(),
        objective_function,
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(1, MOI.ScalarAffineTerm.([1.0, 1.0], [x, x])),
        [-1.0],
    )
    c = MOI.add_constraint(model, f, MOI.Nonpositives(1))
    _test_model_solution(
        model,
        config;
        objective_value = 0.5,
        variable_primal = [(x, 0.5)],
        constraint_primal = [(c, [0.0])],
        constraint_dual = [(c, [-0.5])],
    )
    return
end

function setup_test(
    ::typeof(test_constraint_VectorAffineFunction_duplicate),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.5]),
            MOI.FEASIBLE_POINT,
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) =>
                [[-0.5]],
        ),
    )
    return
end

"""
    test_constraint_qcp_duplicate_diagonal(model::MOI.ModelLike, config::Config)

Test a QCP problem with a duplicate diagonal term.

The problem is `max x + 2y | y + x^2 + x^2 <= 1, x >= 0.5, y >= 0.5`.
"""
function test_constraint_qcp_duplicate_diagonal(
    model::MOI.ModelLike,
    config::Config,
)
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarQuadraticFunction{Float64},
        MOI.LessThan{Float64},
    )
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 2.0], x), 0.0),
    )
    vc1 = MOI.add_constraint(model, x[1], MOI.GreaterThan(0.5))
    # We test this after the creation of every `VariableIndex` constraint
    # to ensure a good coverage of corner cases.
    @test vc1.value == x[1].value
    vc2 = MOI.add_constraint(model, x[2], MOI.GreaterThan(0.5))
    @test vc2.value == x[2].value
    MOI.add_constraint(
        model,
        MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.([2.0, 2.0], [x[1], x[1]], [x[1], x[1]]),  # quad
            MOI.ScalarAffineTerm.([1.0], [x[2]]),  # affine terms
            0.0,  # constant
        ),
        MOI.LessThan(1.0),
    )
    _test_model_solution(
        model,
        config;
        objective_value = 1.5,
        variable_primal = [(x[1], 0.5), (x[2], 0.5)],
    )
    return
end

function setup_test(
    ::typeof(test_constraint_qcp_duplicate_diagonal),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.5, 0.5]),
        ),
    )
    return
end

"""
    test_constraint_qcp_duplicate_off_diagonal(model::MOI.ModelLike, config::Config)

Test a QCP problem with a duplicate off-diagonal term.

The problem is
`max x + 2y | x^2 + 0.25y*x + 0.25x*y + 0.5x*y + y^2 <= 1, x >= 0.5, y >= 0.5`.
"""
function test_constraint_qcp_duplicate_off_diagonal(
    model::MOI.ModelLike,
    config::Config,
)
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarQuadraticFunction{Float64},
        MOI.LessThan{Float64},
    )
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 2.0], x), 0.0),
    )
    vc1 = MOI.add_constraint(model, x[1], MOI.GreaterThan{Float64}(0.5))
    @test vc1.value == x[1].value
    vc2 = MOI.add_constraint(model, x[2], MOI.GreaterThan{Float64}(0.5))
    @test vc2.value == x[2].value
    MOI.add_constraint(
        model,
        MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.(
                [2.0, 0.25, 0.25, 0.5, 2.0],
                [x[1], x[1], x[2], x[1], x[2]],
                [x[1], x[2], x[1], x[2], x[2]],
            ),  # quad
            MOI.ScalarAffineTerm{Float64}[],  # affine terms
            0.0,  # constant
        ),
        MOI.LessThan(1.0),
    )
    _test_model_solution(
        model,
        config;
        objective_value = 0.5 + (√13 - 1) / 2,
        variable_primal = [(x[1], 0.5), (x[2], (√13 - 1) / 4)],
    )
    return
end

function setup_test(
    ::typeof(test_constraint_qcp_duplicate_off_diagonal),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.5, (√13 - 1) / 4]),
        ),
    )
    return
end

"""
    test_constraint_ZeroOne_bounds(model::MOI.ModelLike, config::Config)

Test a problem with a bounded ZeroOne variable.
"""
function test_constraint_ZeroOne_bounds(model::MOI.ModelLike, config::Config)
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.ZeroOne)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    maxobjective: 2.0x
    x in ZeroOne()
    x >= 0.0
    x <= 1.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    _test_model_solution(
        model,
        config;
        objective_value = 2.0,
        variable_primal = [(x, 1.0)],
    )
    return
end

function setup_test(
    ::typeof(test_constraint_ZeroOne_bounds),
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
    test_constraint_ZeroOne_bounds_2(model::MOI.ModelLike, config::Config)

Test a problem with a ZeroOne and binding fractional upper bound.
"""
function test_constraint_ZeroOne_bounds_2(model::MOI.ModelLike, config::Config)
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.ZeroOne)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    maxobjective: 2.0x
    x in ZeroOne()
    x >= 0.0
    x <= 0.5
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
    ::typeof(test_constraint_ZeroOne_bounds_2),
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

"""
    test_constraint_ZeroOne_bounds_3(model::MOI.ModelLike, config::Config)

Test a problem with a ZeroOne and infeasible fractional bounds.
"""
function test_constraint_ZeroOne_bounds_3(model::MOI.ModelLike, config::Config)
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.ZeroOne)
    @requires _supports(config, MOI.optimize!)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    maxobjective: 2.0x
    x in ZeroOne()
    x >= 0.2
    x <= 0.5
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
    return
end

function setup_test(
    ::typeof(test_constraint_ZeroOne_bounds_3),
    model::MOIU.MockOptimizer,
    ::Config,
)
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
    o = one(T)
    c = MOI.add_constraint(
        model,
        MOIU.operate(vcat, T, x, o),
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
        objective_value = o,
        variable_primal = [(x, o)],
        constraint_primal = [(c, [o, o])],
        constraint_dual = [(c, [o, -o])],
    )
    return
end

function setup_test(
    ::typeof(test_constraint_PrimalStart_DualStart_SecondOrderCone),
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
            (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone) =>
                [[1.0, -1.0]],
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
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(one(T), x))],
        [0.0],
    )
    c = MOI.add_constraint(model, f, MOI.Nonnegatives(1))
    @test MOI.get(model, MOI.ConstraintPrimalStart(), c) === nothing
    MOI.set(model, MOI.ConstraintPrimalStart(), c, [-one(T)])
    @test MOI.get(model, MOI.ConstraintPrimalStart(), c) == [-one(T)]
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
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(one(T), x))],
        [0.0],
    )
    c = MOI.add_constraint(model, f, MOI.Nonnegatives(1))
    @test MOI.get(model, MOI.ConstraintDualStart(), c) === nothing
    MOI.set(model, MOI.ConstraintDualStart(), c, [-one(T)])
    @test MOI.get(model, MOI.ConstraintDualStart(), c) == [-one(T)]
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
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x[1])),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(2.0, x[2])),
        ],
        [0.0, 0.0],
    )
    s = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.GreaterThan(1.0))
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
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x[1])),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(2.0, x[2])),
        ],
        [0.0, 0.0],
    )
    s = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.GreaterThan(1.0))
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
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x[1])),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(2.0, x[2])),
        ],
        [0.0, 0.0],
    )
    s = MOI.Indicator{MOI.ACTIVATE_ON_ZERO}(MOI.GreaterThan(1.0))
    c = MOI.add_constraint(model, f, s)
    @test MOI.get(model, MOI.ConstraintSet(), c) == s
    @test isapprox(MOI.get(model, MOI.ConstraintFunction(), c), f)
    return
end
