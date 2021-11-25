"""
    _test_conic_linear_helper(
        model::MOI.ModelLike,
        config::Config,
        use_VectorOfVariables::Bool,
    )

A helper function for writing other conic tests.

Constructs the problem:
```
min -3x - 2y - 4z
st    x +  y +  z == 3
           y +  z == 2
      x>=0 y>=0 z>=0
Opt obj = -11, soln x = 1, y = 0, z = 2
```
"""
function _test_conic_linear_helper(
    model::MOI.ModelLike,
    config::Config{T},
    use_VectorOfVariables::Bool,
) where {T}
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.Nonnegatives,
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.Nonnegatives,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Zeros,
    )
    v = MOI.add_variables(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3
    vov = MOI.VectorOfVariables(v)
    if use_VectorOfVariables
        vc = MOI.add_constraint(model, vov, MOI.Nonnegatives(3))
    else
        vc = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{T}(vov),
            MOI.Nonnegatives(3),
        )
    end
    c = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 1, 1, 2, 2],
                MOI.ScalarAffineTerm.(T(1), [v; v[2]; v[3]]),
            ),
            T[-3, -2],
        ),
        MOI.Zeros(2),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{T},
                MOI.Nonnegatives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorAffineFunction{T},MOI.Zeros}(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (
        use_VectorOfVariables ? MOI.VectorOfVariables :
        MOI.VectorAffineFunction{T},
        MOI.Nonnegatives,
    ) in loc
    @test (MOI.VectorAffineFunction{T}, MOI.Zeros) in loc
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[-3, -2, -4], v), T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(-11), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), T(-11), config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), T[1, 0, 2], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), vc), T[1, 0, 2], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c), T[0, 0], config)
        if _supports(config, MOI.ConstraintDual)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), vc),
                T[0, 2, 0],
                config,
            )
            @test ≈(MOI.get(model, MOI.ConstraintDual(), c), T[-3, -1], config)
        end
    end
    return
end

"""
    test_conic_linear_VectorOfVariables(model::MOI.ModelLike, config::Config)

Test a conic formulation of a linear program using standard conic form.
"""
function test_conic_linear_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_linear_helper(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_linear_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 0, 2],
            (MOI.VectorAffineFunction{T}, MOI.Zeros) => [T[-3, -1]],
        ),
    )
    return
end

"""
    test_conic_linear_VectorAffineFunction(model::MOI.ModelLike, config::Config)

Test a conic formulation of a linear program using geometric conic form.
"""
function test_conic_linear_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_linear_helper(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_linear_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 0, 2],
            (MOI.VectorAffineFunction{T}, MOI.Nonnegatives) => [T[0, 2, 0]],
            (MOI.VectorAffineFunction{T}, MOI.Zeros) => [T[-3, -1]],
        ),
    )
    return
end

"""
    _test_conic_linear_helper_2(
        model::MOI.ModelLike,
        config::Config,
        use_VectorOfVariables::Bool,
    )

Another helper for linear conic problems.

Builds the problem:
```
min  3x + 2y - 4z + 0s
st    x           -  s  == -4    (i.e. x >= -4)
           y            == -3
      x      +  z       == 12
      x free
      y <= 0
      z >= 0
      s zero
Opt solution = -82
x = -4, y = -3, z = 16, s == 0
```
"""
function _test_conic_linear_helper_2(
    model::MOI.ModelLike,
    config::Config{T},
    use_VectorOfVariables::Bool,
) where {T}
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.Nonnegatives,
        )
        @requires MOI.supports_add_constrained_variables(
            model,
            MOI.Nonpositives,
        )
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.Zeros,
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.Nonnegatives,
        )
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.Nonpositives,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Zeros,
    )
    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    if use_VectorOfVariables
        ys, vc = MOI.add_constrained_variables(model, MOI.Nonpositives(1))
        y = ys[1]
    else
        y = MOI.add_variable(model)
        func = MOI.VectorAffineFunction{T}(MOI.VectorOfVariables([y]))
        vc = MOI.add_constraint(model, func, MOI.Nonpositives(1))
    end
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    z, s = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 4
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[3, 2, -4], [x, y, z]),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    c = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 1, 2, 3, 3],
                MOI.ScalarAffineTerm.(T[1, -1, 1, 1, 1], [x, s, y, x, z]),
            ),
            T[4, 3, -12],
        ),
        MOI.Zeros(3),
    )
    if use_VectorOfVariables
        # test fallback
        vz = MOI.add_constraint(model, [z], MOI.Nonnegatives(1))
    else
        vz = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction(
                [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), z))],
                T[0],
            ),
            MOI.Nonnegatives(1),
        )
    end
    vov = MOI.VectorOfVariables([s])
    if use_VectorOfVariables
        vs = MOI.add_constraint(model, vov, MOI.Zeros(1))
    else
        vs = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{T}(vov),
            MOI.Zeros(1),
        )
    end
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorAffineFunction{T},MOI.Zeros}(),
        ) == 2 - use_VectorOfVariables
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{T},
                MOI.Nonpositives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{T},
                MOI.Nonnegatives,
            }(),
        ) == 1
    end
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(-82), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), T(-82), config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(-4), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), y), T(-3), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), z), T(16), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), s), T(0), config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c), T[0, 0, 0], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), vc), T[-3], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), vz), T[16], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), vs), T[0], config)
        if _supports(config, MOI.ConstraintDual)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), c),
                T[7, 2, -4],
                config,
            )
            @test ≈(MOI.get(model, MOI.ConstraintDual(), vc), T[0], config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), vz), T[0], config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), vs), T[7], config)
        end
    end
    return
end

"""
    test_conic_linear_VectorOfVariables_2(
        model::MOI.ModelLike,
        config::Config,
    )

Test a linear program in standard conic form.
"""
function test_conic_linear_VectorOfVariables_2(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_linear_helper_2(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_linear_VectorOfVariables_2),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[-4, -3, 16, 0],
            (MOI.VectorAffineFunction{T}, MOI.Zeros) => [T[7, 2, -4]],
        ),
    )
    return
end

"""
    test_conic_linear_VectorAffineFunction_2(
        model::MOI.ModelLike,
        config::Config,
    )

Test a linear program in geometric conic form.
"""
function test_conic_linear_VectorAffineFunction_2(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_linear_helper_2(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_linear_VectorAffineFunction_2),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[-4, -3, 16, 0],
            (MOI.VectorAffineFunction{T}, MOI.Nonnegatives) => [T[0]],
            (MOI.VectorAffineFunction{T}, MOI.Nonpositives) => [T[0]],
            (MOI.VectorAffineFunction{T}, MOI.Zeros) => [T[7, 2, -4], T[7]],
        ),
    )
    return
end

"""
    test_conic_linear_INFEASIBLE(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test an infeasible linear program in conic form.

The problem is:
```
min 0
s.t. -1 + x ∈ R₊
      1 + x ∈ R₋
```
"""
function test_conic_linear_INFEASIBLE(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Nonpositives,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Nonnegatives,
    )
    x = MOI.add_variable(model)
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x))],
            T[-1],
        ),
        MOI.Nonnegatives(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x))],
            T[1],
        ),
        MOI.Nonpositives(1),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.Nonnegatives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.Nonpositives,
            }(),
        ) == 1
    end
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) in
              [config.infeasible_status, MOI.INFEASIBLE_OR_UNBOUNDED]
        # TODO test dual feasibility and objective sign
    end
    return
end

function setup_test(
    ::typeof(test_conic_linear_INFEASIBLE),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            MOI.INFEASIBLE_POINT,
            MOI.INFEASIBILITY_CERTIFICATE,
        ),
    )
    return
end

"""
    test_conic_linear_INFEASIBLE_2(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test an infeasible linear program in conic form.

The problem is:
```
min 0
s.t. -1 + x ∈ R₊
          x ∈ R₋
```
"""
function test_conic_linear_INFEASIBLE_2(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Nonnegatives,
    )
    @requires MOI.supports_add_constrained_variables(model, MOI.Nonpositives)
    xs, cx = MOI.add_constrained_variables(model, MOI.Nonpositives(1))
    x = xs[1]
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x))],
            T[-1],
        ),
        MOI.Nonnegatives(1),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.Nonnegatives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonpositives}(),
        ) == 1
    end
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) in
              [config.infeasible_status, MOI.INFEASIBLE_OR_UNBOUNDED]
        # TODO test dual feasibility and objective sign
    end
    return
end

function setup_test(
    ::typeof(test_conic_linear_INFEASIBLE_2),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            MOI.INFEASIBLE_POINT,
            MOI.INFEASIBILITY_CERTIFICATE,
        ),
    )
    return
end

"""
    _test_conic_NormInfinityCone_helper(
        model::MOI.ModelLike,
        config::Config,
        use_VectorOfVariables::Bool,
    )

A helper function for testing NormInfinityCone.
"""
function _test_conic_NormInfinityCone_helper(
    model::MOI.ModelLike,
    config::Config{T},
    use_VectorOfVariables::Bool,
) where {T}
    F = if use_VectorOfVariables
        MOI.VectorOfVariables
    else
        MOI.VectorAffineFunction{T}
    end
    @requires MOI.supports_constraint(model, F, MOI.NormInfinityCone)
    # Problem NormInf1
    # max 0x + 1y + 1z
    #  st  x == 1
    #      y == 1/2
    #      x >= ||(y,z)||_∞
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.Zeros,
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.Zeros,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.NormInfinityCone,
    )
    x, y, z = MOI.add_variables(model, 3)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[1, 1], [y, z]), T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    ceq1 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x))],
            T[-1],
        ),
        MOI.Zeros(1),
    )
    ceq2 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), y))],
            T[-1//2],
        ),
        MOI.Zeros(1),
    )
    vov = MOI.VectorOfVariables([x, y, z])
    if use_VectorOfVariables
        ccone = MOI.add_constraint(model, vov, MOI.NormInfinityCone(3))
    else
        ccone = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{T}(vov),
            MOI.NormInfinityCone(3),
        )
    end
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorAffineFunction{T},MOI.Zeros}(),
        ) == 2
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{T},
                MOI.NormInfinityCone,
            }(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{T}, MOI.Zeros) in loc
    @test (
        use_VectorOfVariables ? MOI.VectorOfVariables :
        MOI.VectorAffineFunction{T},
        MOI.NormInfinityCone,
    ) in loc
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(3 // 2), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), T(3 // 2), config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(1), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), y), T(1 // 2), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), z), T(1), config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), ceq1), T[0], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), ceq2), T[0], config)
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), ccone),
            T[1, 1//2, 1],
            config,
        )
        if _supports(config, MOI.ConstraintDual)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), ceq1), T[-1], config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), ceq2), T[-1], config)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), ccone),
                T[1, 0, -1],
                config,
            )
        end
    end
    return
end

"""
    test_conic_NormInfinityCone_VectorOfVariables(
        model::MOI.ModelLike,
        config::Config,
    )

Test a NormInfinityCone in standard conic form.
"""
function test_conic_NormInfinityCone_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_NormInfinityCone_helper(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_NormInfinityCone_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 1//2, 1],
            (MOI.VectorAffineFunction{T}, MOI.Zeros) => [T[-1], T[-1]],
            (MOI.VectorOfVariables, MOI.NormInfinityCone) => [T[1, 0, -1]],
        ),
    )
    return
end

"""
    test_conic_NormInfinityCone_VectorAffineFunction(
        model::MOI.ModelLike,
        config::Config,
    )

Test a NormInfinityCone in geometric conic form.
"""
function test_conic_NormInfinityCone_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_NormInfinityCone_helper(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_NormInfinityCone_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 1//2, 1],
            (MOI.VectorAffineFunction{T}, MOI.Zeros) => [T[-1], T[-1]],
            (MOI.VectorAffineFunction{T}, MOI.NormInfinityCone) =>
                [T[1, 0, -1]],
        ),
    )
    return
end

"""
    test_conic_NormInfinityCone_INFEASIBLE(
        model::MOI.ModelLike,
        config::Config,
    )

Test the problem:
```
min 0
s.t. -2 + y ∈ R₊
     -1 + x ∈ R₋
      (x,y) ∈ NormInf₂
```
"""
function test_conic_NormInfinityCone_INFEASIBLE(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Nonnegatives,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Nonpositives,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.NormInfinityCone,
    )
    x, y = MOI.add_variables(model, 2)
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), y))],
            T[-2],
        ),
        MOI.Nonnegatives(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x))],
            T[-1],
        ),
        MOI.Nonpositives(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.(T(1), [x, y])),
            zeros(T, 2),
        ),
        MOI.NormInfinityCone(2),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.Nonnegatives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.Nonpositives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.NormInfinityCone,
            }(),
        ) == 1
    end
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) ==
              config.infeasible_status
        @test MOI.get(model, MOI.PrimalStatus()) in
              (MOI.NO_SOLUTION, MOI.INFEASIBLE_POINT)
        # TODO test dual feasibility and objective sign
    end
    return
end

function setup_test(
    ::typeof(test_conic_NormInfinityCone_INFEASIBLE),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            MOI.INFEASIBLE_POINT,
            MOI.INFEASIBILITY_CERTIFICATE,
        ),
    )
    return
end

"""
    test_conic_NormInfinityCone_3(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test the problem:
```
min x
 st  (-1 + x, 2 .+ y) in NormInf(1 + n)
     (1 .+ y) in Nonnegatives(n)
let n = 3. optimal solution: y .= -1, x = 2
```
"""
function test_conic_NormInfinityCone_3(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.NormInfinityCone,
    )
    x = MOI.add_variable(model)
    y = MOI.add_variables(model, 3)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    norminf_vaf = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(1:4, MOI.ScalarAffineTerm.(T(1), vcat(x, y))),
        T[-1, 2, 2, 2],
    )
    norminf = MOI.add_constraint(model, norminf_vaf, MOI.NormInfinityCone(4))
    nonneg_vaf = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(1:3, MOI.ScalarAffineTerm.(T(1), y)),
        ones(T, 3),
    )
    nonneg = MOI.add_constraint(model, nonneg_vaf, MOI.Nonnegatives(3))
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.NormInfinityCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.Nonnegatives,
            }(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{T}, MOI.NormInfinityCone) in loc
    @test (MOI.VectorAffineFunction{T}, MOI.Nonnegatives) in loc
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(2), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), T(2), config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(2), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), y), fill(T(-1), 3), config)
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), norminf),
            ones(T, 4),
            config,
        )
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), nonneg),
            zeros(T, 3),
            config,
        )
        if _supports(config, MOI.ConstraintDual)
            dual_nonneg = MOI.get(model, MOI.ConstraintDual(), nonneg)
            @test minimum(dual_nonneg) >= -config.atol
            @test ≈(sum(dual_nonneg), T(1), config)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), norminf),
                vcat(1, -dual_nonneg),
                config,
            )
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_NormInfinityCone_3),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[2, -1, -1, -1],
            (MOI.VectorAffineFunction{T}, MOI.NormInfinityCone) =>
                [vcat(1, fill(-inv(T(3)), 3))],
            (MOI.VectorAffineFunction{T}, MOI.Nonnegatives) =>
                [fill(inv(T(3)), 3)],
        ),
    )
    return
end

function _test_conic_NormOneCone_helper(
    model::MOI.ModelLike,
    config::Config{T},
    use_VectorOfVariables::Bool,
) where {T}
    # Problem NormOne1
    # max 0x + 1y + 1z
    #  st  x == 1
    #      y == 1/2
    #      x >= ||(y,z)||_1
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.Zeros,
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.Zeros,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.NormOneCone,
    )
    x, y, z = MOI.add_variables(model, 3)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[1, 1], [y, z]), T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    ceq1 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x))],
            T[-1],
        ),
        MOI.Zeros(1),
    )
    ceq2 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), y))],
            T[-1//2],
        ),
        MOI.Zeros(1),
    )
    vov = MOI.VectorOfVariables([x, y, z])
    if use_VectorOfVariables
        ccone = MOI.add_constraint(model, vov, MOI.NormOneCone(3))
    else
        ccone = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{T}(vov),
            MOI.NormOneCone(3),
        )
    end
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorAffineFunction{T},MOI.Zeros}(),
        ) == 2
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{T},
                MOI.NormOneCone,
            }(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{T}, MOI.Zeros) in loc
    @test (
        use_VectorOfVariables ? MOI.VectorOfVariables :
        MOI.VectorAffineFunction{T},
        MOI.NormOneCone,
    ) in loc
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(1), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), T(1), config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(1), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), y), T(1 // 2), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), z), T(1 // 2), config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), ceq1), T[0], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), ceq2), T[0], config)
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), ccone),
            T[1, 1//2, 1//2],
            config,
        )
        if _supports(config, MOI.ConstraintDual)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), ceq1), T[-1], config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), ceq2), T[0], config)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), ccone),
                T[1, -1, -1],
                config,
            )
        end
    end
    return
end

"""
    test_conic_NormOneCone_VectorOfVariables(
        model::MOI.ModelLike,
        config::Config,
    )

Test NormOneCone in standard conic form.
"""
function test_conic_NormOneCone_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    return _test_conic_NormOneCone_helper(model, config, true)
end

function setup_test(
    ::typeof(test_conic_NormOneCone_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 1//2, 1//2],
            (MOI.VectorAffineFunction{T}, MOI.Zeros) => [T[-1], T[0]],
            (MOI.VectorOfVariables, MOI.NormOneCone) => [T[1, -1, -1]],
        ),
    )
    return
end

"""
    test_conic_NormOneCone_VectorAffineFunction(
        model::MOI.ModelLike,
        config::Config,
    )

Test NormOneCone in geometric conic form.
"""
function test_conic_NormOneCone_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    return _test_conic_NormOneCone_helper(model, config, false)
end

function setup_test(
    ::typeof(test_conic_NormOneCone_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 1//2, 1//2],
            (MOI.VectorAffineFunction{T}, MOI.Zeros) => [T[-1], T[0]],
            (MOI.VectorAffineFunction{T}, MOI.NormOneCone) =>
                [T[1, -1, -1]],
        ),
    )
    return
end

"""
    test_conic_NormOneCone_INFEASIBLE(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test and infeasible problem with NormOneCone.

Problem NormOne2 - Infeasible
min 0
s.t. y ≥ 2
     x ≤ 1
     |y| ≤ x
in conic form:
min 0
s.t. -2 + y ∈ R₊
     -1 + x ∈ R₋
      (x,y) ∈ NormOne₂
"""
function test_conic_NormOneCone_INFEASIBLE(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Nonnegatives,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Nonpositives,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.NormOneCone,
    )
    x, y = MOI.add_variables(model, 2)
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), y))],
            T[-2],
        ),
        MOI.Nonnegatives(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x))],
            T[-1],
        ),
        MOI.Nonpositives(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.(T(1), [x, y])),
            zeros(T, 2),
        ),
        MOI.NormOneCone(2),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.Nonnegatives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.Nonpositives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.NormOneCone,
            }(),
        ) == 1
    end
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) ==
              config.infeasible_status
        @test MOI.get(model, MOI.PrimalStatus()) in
              (MOI.NO_SOLUTION, MOI.INFEASIBLE_POINT)
        # TODO test dual feasibility and objective sign
    end
    return
end

function setup_test(
    ::typeof(test_conic_NormOneCone_INFEASIBLE),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            MOI.INFEASIBLE_POINT,
            MOI.INFEASIBILITY_CERTIFICATE,
        ),
    )
    return
end

"""
    test_conic_NormOneCone(model::MOI.ModelLike, config::Config{T}) where {T}

Test the following problem:
```
min x
 st  (-1 + x, 2 .+ y) in NormOne(1 + n)
     (1 .+ y) in Nonnegatives(n)
let n = 3. optimal solution: y .= -1, x = 4
```
"""
function test_conic_NormOneCone(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.NormOneCone,
    )
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.NormOneCone,
    )
    x = MOI.add_variable(model)
    y = MOI.add_variables(model, 3)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    norminf_vaf = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(1:4, MOI.ScalarAffineTerm.(T(1), vcat(x, y))),
        T[-1, 2, 2, 2],
    )
    norminf = MOI.add_constraint(model, norminf_vaf, MOI.NormOneCone(4))
    nonneg_vaf = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(1:3, MOI.ScalarAffineTerm.(T(1), y)),
        ones(T, 3),
    )
    nonneg = MOI.add_constraint(model, nonneg_vaf, MOI.Nonnegatives(3))
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.NormOneCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.Nonnegatives,
            }(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{T}, MOI.NormOneCone) in loc
    @test (MOI.VectorAffineFunction{T}, MOI.Nonnegatives) in loc
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(4), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), T(4), config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(4), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), y), fill(T(-1), 3), config)
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), norminf),
            T[3, 1, 1, 1],
            config,
        )
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), nonneg),
            zeros(T, 3),
            config,
        )
        if _supports(config, MOI.ConstraintDual)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), norminf),
                T[1, -1, -1, -1],
                config,
            )
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), nonneg),
                ones(T, 3),
                config,
            )
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_NormOneCone),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[4, -1, -1, -1],
            (MOI.VectorAffineFunction{T}, MOI.NormOneCone) =>
                [T[1, -1, -1, -1]],
            (MOI.VectorAffineFunction{T}, MOI.Nonnegatives) => [ones(T, 3)],
        ),
    )
    return
end

"""
Problem SOC1
max 0x + 1y + 1z
 st  x == 1
     x >= ||(y,z)||
"""
function _test_conic_SecondOrderCone_helper(
    model::MOI.ModelLike,
    config::Config{T},
    use_VectorOfVariables::Bool,
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.SecondOrderCone,
    )
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_add_constrained_variables(
            model,
            MOI.SecondOrderCone,
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.SecondOrderCone,
        )
    end
    @requires MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.Zeros)
    if use_VectorOfVariables
        xyz, csoc = MOI.add_constrained_variables(model, MOI.SecondOrderCone(3))
        x, y, z = xyz
    else
        x, y, z = MOI.add_variables(model, 3)
        vov = MOI.VectorOfVariables([x, y, z])
        csoc = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{T}(vov),
            MOI.SecondOrderCone(3),
        )
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[1, 1], [y, z]), T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    ceq = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x))],
            T[-1],
        ),
        MOI.Zeros(1),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorAffineFunction{T},MOI.Zeros}(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{T},
                MOI.SecondOrderCone,
            }(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{T}, MOI.Zeros) in loc
    @test (
        use_VectorOfVariables ? MOI.VectorOfVariables :
        MOI.VectorAffineFunction{T},
        MOI.SecondOrderCone,
    ) in loc
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), √T(2), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), √T(2), config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(1), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), y), 1 / √T(2), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), z), 1 / √T(2), config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), ceq), T[0], config)
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), csoc),
            T[1, 1/√T(2), 1/√T(2)],
            config,
        )
        if _supports(config, MOI.ConstraintDual)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), ceq), [-√T(2)], config)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), csoc),
                T[√T(2), -1, -1],
                config,
            )
        end
    end
end

"""
    test_conic_SecondOrderCone_VectorOfVariables(
        model::MOI.ModelLike,
        config::Config,
    )

Test a SecondOrderCone in standard conic form.
"""
function test_conic_SecondOrderCone_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_SecondOrderCone_helper(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 1/√T(2), 1/√T(2)],
            (MOI.VectorAffineFunction{T}, MOI.Zeros) => [T[-√T(2)]],
        ),
    )
    return
end

"""
    test_conic_SecondOrderCone_VectorAffineFunction(
        model::MOI.ModelLike,
        config::Config,
    )

Test a SecondOrderCone in geometric conic form.
"""
function test_conic_SecondOrderCone_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_SecondOrderCone_helper(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 1/√T(2), 1/√T(2)],
            (MOI.VectorAffineFunction{T}, MOI.SecondOrderCone) =>
                [T[√T(2), -1, -1]],
            (MOI.VectorAffineFunction{T}, MOI.Zeros) => [T[-√T(2)]],
        ),
    )
    return
end

function _test_conic_SecondOrderCone_helper_2(
    model::MOI.ModelLike,
    config::Config{T},
    nonneg::Bool,
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.SecondOrderCone,
    )
    # Problem SOC2
    # min  x
    # s.t. y ≥ 1/√T(2)
    #      x² + y² ≤ 1
    # in conic form:
    # min  x
    # s.t.  -1/√T(2) + y ∈ R₊
    #        1 - t ∈ {0}
    #      (t,x,y) ∈ SOC₃
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Zeros,
    )
    if nonneg
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.Nonnegatives,
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.Nonpositives,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.SecondOrderCone,
    )
    x, y, t = MOI.add_variables(model, 3)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x)], T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if nonneg
        cnon = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction(
                [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), y))],
                T[-1/√T(2)],
            ),
            MOI.Nonnegatives(1),
        )
    else
        cnon = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction(
                [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(-1), y))],
                T[1/√T(2)],
            ),
            MOI.Nonpositives(1),
        )
    end
    ceq = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(-1), t))],
            T[1],
        ),
        MOI.Zeros(1),
    )
    csoc = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 2, 3],
                MOI.ScalarAffineTerm.(T(1), [t, x, y]),
            ),
            zeros(T, 3),
        ),
        MOI.SecondOrderCone(3),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                nonneg ? MOI.Nonnegatives : MOI.Nonpositives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorAffineFunction{T},MOI.Zeros}(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.SecondOrderCone,
            }(),
        ) == 1
    end
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), -1 / √T(2), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(
                MOI.get(model, MOI.DualObjectiveValue()),
                -1 / √T(2),
                config,
            )
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), -1 / √T(2), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), y), 1 / √T(2), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), t), 1, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cnon), T[0], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), ceq), T[0], config)
        p = [1, -1 / √T(2), 1 / √T(2)]
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), csoc), p, config)
        if _supports(config, MOI.ConstraintDual)
            d = [nonneg ? 1 : -1]
            @test ≈(MOI.get(model, MOI.ConstraintDual(), cnon), d, config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), ceq), [√T(2)], config)
            d2 = [√T(2), 1, -1]
            @test ≈(MOI.get(model, MOI.ConstraintDual(), csoc), d2, config)
        end
    end
    return
end

"""
    test_conic_SecondOrderCone_Nonnegatives(
        model::MOI.ModelLike,
        config::Config,
    )

Test a SecondOrderCone with Nonnegatives constraints.
"""
function test_conic_SecondOrderCone_Nonnegatives(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_SecondOrderCone_helper_2(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_Nonnegatives),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[-1/√T(2), 1/√T(2), 1],
            (MOI.VectorAffineFunction{T}, MOI.SecondOrderCone) =>
                [T[√T(2), 1, -1]],
            (MOI.VectorAffineFunction{T}, MOI.Zeros) => [T[√T(2)]],
            (MOI.VectorAffineFunction{T}, MOI.Nonnegatives) => [T[1]],
        ),
    )
    return
end

"""
    test_conic_SecondOrderCone_Nonpositives(
        model::MOI.ModelLike,
        config::Config,
    )

Test a SecondOrderCone with Nonpositives constraints.
"""
function test_conic_SecondOrderCone_Nonpositives(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_SecondOrderCone_helper_2(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_Nonpositives),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[-1/√T(2), 1/√T(2), 1],
            (MOI.VectorAffineFunction{T}, MOI.SecondOrderCone) =>
                [T[√T(2), 1, -1]],
            (MOI.VectorAffineFunction{T}, MOI.Zeros) => [T[√T(2)]],
            (MOI.VectorAffineFunction{T}, MOI.Nonpositives) => [T[-1]],
        ),
    )
    return
end

"""
    test_conic_SecondOrderCone_INFEASIBLE(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Problem SOC3 - Infeasible
min 0
s.t. y ≥ 2
     x ≤ 1
     |y| ≤ x
in conic form:
min 0
s.t. -2 + y ∈ R₊
     -1 + x ∈ R₋
      (x,y) ∈ SOC₂
"""
function test_conic_SecondOrderCone_INFEASIBLE(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.SecondOrderCone,
    )
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Nonnegatives,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Nonpositives,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.SecondOrderCone,
    )
    x, y = MOI.add_variables(model, 2)
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), y))],
            T[-2],
        ),
        MOI.Nonnegatives(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), x))],
            T[-1],
        ),
        MOI.Nonpositives(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.(T(1), [x, y])),
            zeros(T, 2),
        ),
        MOI.SecondOrderCone(2),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.Nonnegatives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.Nonpositives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.SecondOrderCone,
            }(),
        ) == 1
    end
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) ==
              config.infeasible_status
        @test MOI.get(model, MOI.PrimalStatus()) in
              (MOI.NO_SOLUTION, MOI.INFEASIBLE_POINT)
        # TODO test dual feasibility and objective sign
    end
    return
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_INFEASIBLE),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            MOI.INFEASIBLE_POINT,
            MOI.INFEASIBILITY_CERTIFICATE,
        ),
    )
    return
end

"""
    test_conic_SecondOrderCone_out_of_order(
        model::MOI.ModelLike,
        config::Config,
    )

Problem SOC4
min 0x[1] - 2x[2] - 1x[3]
 st  x[1]                                == 1 (c1a)
             x[2]         - x[4]         == 0 (c1b)
                     x[3]         - x[5] == 0 (c1c)
     x[1] >= ||(x[4],x[5])||                  (c2)
in conic form:
min  c^Tx
s.t. Ax + b ∈ {0}₃
     (x[1],x[4],x[5]) ∈ SOC₃
Like SOCINT1 but with copies of variables and integrality relaxed
Tests out-of-order indices in cones
"""
function test_conic_SecondOrderCone_out_of_order(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.SecondOrderCone,
    )
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Zeros,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.SecondOrderCone,
    )
    x = MOI.add_variables(model, 5)
    c1 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 2, 3, 2, 3],
                MOI.ScalarAffineTerm.(T[1, 1, 1, -1, -1], x),
            ),
            T[-1, 0, 0],
        ),
        MOI.Zeros(3),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.VectorOfVariables([x[1], x[4], x[5]]),
        MOI.SecondOrderCone(3),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorAffineFunction{T},MOI.Zeros}(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SecondOrderCone}(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[0, -2, -1, 0, 0], x),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), -√T(5), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), -√T(5), config)
        end
        @test ≈(
            MOI.get(model, MOI.VariablePrimal(), x),
            T[1, 2/√T(5), 1/√T(5), 2/√T(5), 1/√T(5)],
            config,
        )
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c1), zeros(T, 3), config)
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), c2),
            T[1, 2/√T(5), 1/√T(5)],
            config,
        )
        if _supports(config, MOI.ConstraintDual)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), c1),
                T[-√T(5), -2, -1],
                config,
            )
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), c2),
                T[√T(5), -2, -1],
                config,
            )
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_out_of_order),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 2/√T(5), 1/√T(5), 2/√T(5), 1/√T(5)],
            (MOI.VectorAffineFunction{T}, MOI.Zeros) => [T[-√T(5), -2, -1]],
        ),
    )
    return
end

"""
Problem SOCRotated1v
min 0a + 0b - 1x - 1y
 st  a            == 1/2
 st  b            == 1
     2a*b >= x^2+y^2
Problem SOCRotated1f - Problem SOCRotated1v with a and b substituted
min          -y - z
 st T[1//2] - [      ] SOCRotated
    T[1] - [      ] SOCRotated
    T[0] - [-y    ] SOCRotated
    T[0] - [    -z] SOCRotated
"""
function _test_conic_RotatedSecondOrderCone_helper(
    model::MOI.ModelLike,
    config::Config{T},
    abvars::Bool,
) where {T}
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if abvars
        @requires MOI.supports_constraint(
            model,
            MOI.VariableIndex,
            MOI.EqualTo{T},
        )
        @requires MOI.supports_add_constrained_variables(
            model,
            MOI.RotatedSecondOrderCone,
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.RotatedSecondOrderCone,
        )
    end
    if abvars
        abx, rsoc =
            MOI.add_constrained_variables(model, MOI.RotatedSecondOrderCone(4))
        a, b, x1, x2 = abx
        x = [x1, x2]
        vc1 = MOI.add_constraint(model, a, MOI.EqualTo(T(1 // 2)))
        # We test this after the creation of every `VariableIndex` constraint
        # to ensure a good coverage of corner cases.
        @test vc1.value == a.value
        vc2 = MOI.add_constraint(model, b, MOI.EqualTo(T(1)))
        @test vc2.value == b.value
    else
        x = MOI.add_variables(model, 2)
        a = T(1 // 2)
        b = T(1)
        rsoc = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction(
                MOI.VectorAffineTerm.(
                    [3, 4],
                    MOI.ScalarAffineTerm.(T[1, 1], x),
                ),
                T[a, b, 0, 0],
            ),
            MOI.RotatedSecondOrderCone(4),
        )
    end
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.EqualTo{T}}(),
        ) == (abvars ? 2 : 0)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                abvars ? MOI.VectorOfVariables : MOI.VectorAffineFunction{T},
                MOI.RotatedSecondOrderCone,
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T(1), x), T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), √T(2), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), √T(2), config)
        end
        if abvars
            @test ≈(MOI.get(model, MOI.VariablePrimal(), a), T(1 // 2), config)
            @test ≈(MOI.get(model, MOI.VariablePrimal(), b), 1, config)
        end
        @test ≈(
            MOI.get(model, MOI.VariablePrimal(), x),
            [1 / √T(2), 1 / √T(2)],
            config,
        )
        if abvars
            @test ≈(
                MOI.get(model, MOI.ConstraintPrimal(), vc1),
                T(1 // 2),
                config,
            )
            @test ≈(MOI.get(model, MOI.ConstraintPrimal(), vc2), T(1), config)
        end
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), rsoc),
            T[1//2, 1, 1/√T(2), 1/√T(2)],
            config,
        )
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus(1)) == MOI.FEASIBLE_POINT
            if abvars
                @test ≈(
                    MOI.get(model, MOI.ConstraintDual(), vc1),
                    -√T(2),
                    config,
                )
                @test ≈(
                    MOI.get(model, MOI.ConstraintDual(), vc2),
                    -1 / √T(2),
                    config,
                )
            end
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), rsoc),
                T[√T(2), 1/√T(2), -1, -1],
                config,
            )
        end
    end
    return
end

function test_conic_RotatedSecondOrderCone_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_RotatedSecondOrderCone_helper(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_RotatedSecondOrderCone_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1//2, 1, 1/√T(2), 1/√T(2)],
            (MOI.VariableIndex, MOI.EqualTo{T}) => T[-√T(2), -1/√T(2)],
            (MOI.VectorOfVariables, MOI.RotatedSecondOrderCone) =>
                [T[√T(2), 1/√T(2), -1, -1]],
        ),
    )
    model.eval_variable_constraint_dual = false
    return () -> model.eval_variable_constraint_dual = true
end

function test_conic_RotatedSecondOrderCone_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_RotatedSecondOrderCone_helper(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_RotatedSecondOrderCone_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1/√T(2), 1/√T(2)],
            (MOI.VectorAffineFunction{T}, MOI.RotatedSecondOrderCone) =>
                [T[√T(2), 1/√T(2), -1, -1]],
        ),
    )
    return
end

"""
    test_conic_RotatedSecondOrderCone_INFEASIBLE(
        model::MOI.ModelLike,
        config::Config,
    )

Problem SOCRotated2 - Infeasible
min 0
s.t.
     x ≤ 1
     y = 1/2
     z ≥ 2
     z^2 ≤ 2x*y
in conic form:
min 0
s.t.
     -1 + x ∈ R₋
    1/2 - y ∈ {0}
     -2 + z ∈ R₊
      (x,y,z) ∈ SOCRotated
"""
function test_conic_RotatedSecondOrderCone_INFEASIBLE(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    b = T[-2, -1, 1//2]
    c = T[0, 0, 0]
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.EqualTo{T})
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.LessThan{T})
    @requires MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{T},
    )
    @requires MOI.supports_add_constrained_variables(
        model,
        MOI.RotatedSecondOrderCone,
    )
    x, rsoc =
        MOI.add_constrained_variables(model, MOI.RotatedSecondOrderCone(3))
    vc1 = MOI.add_constraint(model, x[1], MOI.LessThan(T(1)))
    @test vc1.value == x[1].value
    vc2 = MOI.add_constraint(model, x[2], MOI.EqualTo(T(1 // 2)))
    @test vc2.value == x[2].value
    vc3 = MOI.add_constraint(model, x[3], MOI.GreaterThan(T(2)))
    @test vc3.value == x[3].value
    if _supports(config, MOI.NumberOfConstraints)
        attr = MOI.NumberOfConstraints{MOI.VariableIndex,MOI.LessThan{T}}()
        @test MOI.get(model, attr) == 1
        attr = MOI.NumberOfConstraints{MOI.VariableIndex,MOI.EqualTo{T}}()
        @test MOI.get(model, attr) == 1
        attr = MOI.NumberOfConstraints{MOI.VariableIndex,MOI.GreaterThan{T}}()
        @test MOI.get(model, attr) == 1
        attr = MOI.NumberOfConstraints{
            MOI.VectorOfVariables,
            MOI.RotatedSecondOrderCone,
        }()
        @test MOI.get(model, attr) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(c, x), T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) in
              [config.infeasible_status, MOI.INFEASIBLE_OR_UNBOUNDED]
        has_certificate = MOI.get(model, MOI.DualStatus()) in [
            MOI.INFEASIBILITY_CERTIFICATE,
            MOI.NEARLY_INFEASIBILITY_CERTIFICATE,
        ]
        if _supports(config, MOI.ConstraintDual) && has_certificate
            y1 = MOI.get(model, MOI.ConstraintDual(), vc1)
            @test y1 < -config.atol # Should be strictly negative
            y2 = MOI.get(model, MOI.ConstraintDual(), vc2)
            y3 = MOI.get(model, MOI.ConstraintDual(), vc3)
            @test y3 > config.atol # Should be strictly positive
            y = [y1, y2, y3]
            vardual = MOI.get(model, MOI.ConstraintDual(), rsoc)
            @test ≈(vardual, -y, config)
            @test 2 * vardual[1] * vardual[2] ≥ vardual[3]^2 - config.atol
            @test b' * y > config.atol
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_RotatedSecondOrderCone_INFEASIBLE),
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
            (MOI.VariableIndex, MOI.LessThan{T}) => T[-1],
            (MOI.VariableIndex, MOI.EqualTo{T}) => T[-1],
            (MOI.VariableIndex, MOI.GreaterThan{T}) => T[1],
            (MOI.VectorOfVariables, MOI.RotatedSecondOrderCone) =>
                [T[1, 1, -1]],
        ),
    )
    model.eval_variable_constraint_dual = false
    return () -> model.eval_variable_constraint_dual = true
end

"""
Problem SOCRotated3
max v
s.t.
     x[1:2] ≥ 0
     0 ≤ u ≤ 3
     v
     t1 == 1
     t2 == 1
[t1/√T(2), t2/√T(2), x] in SOC4
[x1/√T(2), u/√T(2),  v] in SOC3
"""
function test_conic_RotatedSecondOrderCone_INFEASIBLE_2(
    model::MOI.ModelLike,
    config::Config{T};
    n = 2,
    ub = T(3),
) where {T}
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.EqualTo{T})
    @requires MOI.supports_add_constrained_variables(model, MOI.Nonnegatives)
    @requires MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{T},
    )
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.LessThan{T})
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.RotatedSecondOrderCone,
    )
    x, cx = MOI.add_constrained_variables(model, MOI.Nonnegatives(n))
    u = MOI.add_variable(model)
    v = MOI.add_variable(model)
    t = MOI.add_variables(model, 2)
    ct1 = MOI.add_constraint(model, t[1], MOI.EqualTo(T(1)))
    @test ct1.value == t[1].value
    ct2 = MOI.add_constraint(model, t[2], MOI.EqualTo(T(1)))
    @test ct2.value == t[2].value
    cu1 = MOI.add_constraint(model, u, MOI.GreaterThan(T(0)))
    @test cu1.value == u.value
    cu2 = MOI.add_constraint(model, u, MOI.LessThan(ub))
    @test cu2.value == u.value
    c1 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                1:(2+n),
                MOI.ScalarAffineTerm.(
                    T[1 / √T(2); 1 / √T(2); ones(T, n)],
                    [t; x],
                ),
            ),
            zeros(T, 2 + n),
        ),
        MOI.RotatedSecondOrderCone(2 + n),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 2, 3],
                MOI.ScalarAffineTerm.(T[1 / √T(2); 1 / √T(2); 1], [x[1], u, v]),
            ),
            zeros(T, 3),
        ),
        MOI.RotatedSecondOrderCone(3),
    )
    if _supports(config, MOI.NumberOfConstraints)
        attr = MOI.NumberOfConstraints{MOI.VariableIndex,MOI.EqualTo{T}}()
        @test MOI.get(model, attr) == 2
        attr = MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonnegatives}()
        @test MOI.get(model, attr) == 1
        attr = MOI.NumberOfConstraints{MOI.VariableIndex,MOI.GreaterThan{T}}()
        @test MOI.get(model, attr) == 1
        attr = MOI.NumberOfConstraints{MOI.VariableIndex,MOI.LessThan{T}}()
        @test MOI.get(model, attr) == 1
        attr = MOI.NumberOfConstraints{MOI.VariableIndex,MOI.GreaterThan{T}}()
        @test MOI.get(model, attr) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.RotatedSecondOrderCone,
            }(),
        ) == 2
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), v)], T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), √ub, config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), √ub, config)
        end
        @test ≈(
            MOI.get(model, MOI.VariablePrimal(), x),
            [1; zeros(T, n - 1)],
            config,
        )
        @test ≈(MOI.get(model, MOI.VariablePrimal(), u), ub, config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), √ub, config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), t), ones(T, 2), config)
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), cx),
            [1; zeros(T, n - 1)],
            config,
        )
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cu1), ub, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cu2), ub, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), ct1), 1, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), ct2), 1, config)
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), c1),
            T[1 / √T(2); 1 / √T(2); 1; zeros(T, n - 1)],
            config,
        )
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), c2),
            T[1/√T(2), ub/√T(2), √ub],
            config,
        )
        if _supports(config, MOI.ConstraintDual)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), cx),
                zeros(T, n),
                config,
            )
            @test ≈(MOI.get(model, MOI.ConstraintDual(), cu1), 0, config)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), cu2),
                -1 / (2 * √ub),
                config,
            )
            @test ≈(MOI.get(model, MOI.ConstraintDual(), ct1), -√ub / 4, config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), ct2), -√ub / 4, config)
            d = [
                √ub / (2 * √T(2))
                √ub / (2 * √T(2))
                -√ub / 2
                zeros(T, n - 1)
            ]
            @test ≈(MOI.get(model, MOI.ConstraintDual(), c1), d, config)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), c2),
                T[√ub/√T(2), 1/√(2 * ub), -1],
                config,
            )
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_RotatedSecondOrderCone_INFEASIBLE_2),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    n = 2
    ub = T(3)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1; zeros(T, n - 1); ub; √ub; ones(T, 2)],
            (MOI.VariableIndex, MOI.EqualTo{T}) => [-√ub / 4, -√ub / 4],
            (MOI.VectorOfVariables, MOI.Nonnegatives) => [zeros(T, n)],
            (MOI.VariableIndex, MOI.GreaterThan{T}) => T[0],
            (MOI.VariableIndex, MOI.LessThan{T}) => [-1 / (2 * √ub)],
            (MOI.VectorAffineFunction{T}, MOI.RotatedSecondOrderCone) => [
                vcat(
                    √ub / (2 * √T(2)),
                    √ub / (2 * √T(2)),
                    -√ub / 2,
                    zeros(T, n - 1),
                ),
                T[√ub/√T(2), 1/√(2 * ub), -1],
            ],
        ),
    )
    model.eval_variable_constraint_dual = false
    return () -> model.eval_variable_constraint_dual = true
end

"""
    test_conic_RotatedSecondOrderCone_out_of_order(
        model::MOI.ModelLike,
        config::Config,
    )

Problem SOCRotated4
max x + y
s.t.
     t + u ≤ 2              (1)
[t, u, x, y] in RSOC(4)     (2)
Solution:
By AM-QM: (x+y)/2 ≤ √((x^2+y^2)/2) with equality iff x = y
That is,
    (x + y)^2/2 ≤ x^2 + y^2 (3)
By AM-GM: √tu ≤ (t+u)/2 with equality iff t = u
That is,
   2tu ≤ (t + u)^2/2        (4)
Combining (2), (3) and (4), we have
   |x + y| ≤ t + u          (5)
with equality iff x = y and t = u.
Combining (1) and (5), we have
   x + y ≤ 2
with equality iff x = y.
We conclude that the optimal solution is x = y = t = u = 1
with objective value 2.
"""
function test_conic_RotatedSecondOrderCone_out_of_order(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.LessThan{T},
    )
    @requires MOI.supports_add_constrained_variables(
        model,
        MOI.RotatedSecondOrderCone,
    )
    v, cv = MOI.add_constrained_variables(model, MOI.RotatedSecondOrderCone(4))
    t, u, x, y = v
    c = MOI.add_constraint(model, T(1)t + T(1)u, MOI.LessThan(T(2)))
    func = T(1)x + T(1)y
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(func)}(), func)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), 2, config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), 2, config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), t), 1, config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), u), 1, config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), 1, config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), y), 1, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cv), ones(T, 4), config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c), 2, config)
        if _supports(config, MOI.ConstraintDual)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), cv),
                [1, 1, -1, -1],
                config,
            )
            @test ≈(MOI.get(model, MOI.ConstraintDual(), c), -1, config)
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_RotatedSecondOrderCone_out_of_order),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(T, 4),
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-1],
            (MOI.VectorOfVariables, MOI.RotatedSecondOrderCone) =>
                [T[1, 1, -1, -1]],
        ),
    )
    return
end

function _test_conic_GeometricMeanCone_helper(
    model::MOI.ModelLike,
    config::Config{T},
    use_VectorOfVariables,
    n = 3,
) where {T}
    # Problem GeoMean1
    # max (xyz)^(1/3)
    # s.t.
    #      x + y + z ≤ 3
    # in conic form:
    # max t
    # s.t.
    #   (t,x,y,z) ∈ GeometricMeanCone(4)
    #     x+y+z-3 ∈ LessThan(0.)
    # By the arithmetic-geometric mean inequality,
    # (xyz)^(1/3) ≤ (x+y+z)/3 = 1
    # Therefore xyz ≤ 1
    # This can be attained using x = y = z = 1 so it is optimal.
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.GeometricMeanCone,
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.GeometricMeanCone,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.LessThan{T},
    )
    t = MOI.add_variable(model)
    x = MOI.add_variables(model, n)
    vov = MOI.VectorOfVariables([t; x])
    if use_VectorOfVariables
        gmc = MOI.add_constraint(model, vov, MOI.GeometricMeanCone(n + 1))
    else
        gmc = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{T}(vov),
            MOI.GeometricMeanCone(n + 1),
        )
    end
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T(1), x), T(0)),
        MOI.LessThan(T(n)),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{T},
                MOI.GeometricMeanCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.LessThan{T},
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), t)], T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), 1, config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), t), 1, config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), ones(T, n), config)
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), gmc),
            ones(T, n + 1),
            config,
        )
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c), n, config)
        if _supports(config, MOI.ConstraintDual)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), gmc),
                vcat(T(-1), fill(inv(T(n)), n)),
                config,
            )
            @test ≈(MOI.get(model, MOI.ConstraintDual(), c), -inv(T(n)), config)
        end
    end
    return
end

function test_conic_GeometricMeanCone_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_GeometricMeanCone_helper(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_GeometricMeanCone_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(T, 4),
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => [-inv(T(3))],
        ),
    )
    return
end

function test_conic_GeometricMeanCone_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_GeometricMeanCone_helper(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_GeometricMeanCone_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(T, 4),
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => [-inv(T(3))],
            (MOI.VectorAffineFunction{T}, MOI.GeometricMeanCone) =>
                [vcat(T(-1), fill(inv(T(3)), 3))],
        ),
    )
    return
end

"""
Problem GeoMean2
max t
st  (t,x_1,x_2,...,x_9) ∈ GeometricMeanCone(10)
    x_1 == x_2, ..., x_9 == 1
the optimal solution is 1 with optimal value 1
addresses bug https://github.com/jump-dev/MathOptInterface.jl/pull/962
"""
function _test_conic_GeometricMeanCone_helper_2(
    model::MOI.ModelLike,
    config::Config{T},
    use_VectorOfVariables,
) where {T}
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.GeometricMeanCone,
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.GeometricMeanCone,
        )
    end
    n = 9
    t = MOI.add_variable(model)
    x = MOI.add_variables(model, n)
    @test MOI.get(model, MOI.NumberOfVariables()) == n + 1
    vov = MOI.VectorOfVariables([t; x])
    if use_VectorOfVariables
        gmc = MOI.add_constraint(model, vov, MOI.GeometricMeanCone(n + 1))
    else
        gmc = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{T}(vov),
            MOI.GeometricMeanCone(n + 1),
        )
    end
    cx =
        Vector{MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}}(
            undef,
            n,
        )
    for i in 1:n
        cx[i] = MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x[i])], T(0)),
            MOI.EqualTo(T(1)),
        )
    end
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{T},
                MOI.GeometricMeanCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.EqualTo{T},
            }(),
        ) == n
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), t)], T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), 1, config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), t), 1, config)
        @test ≈(MOI.get.(model, MOI.VariablePrimal(), x), ones(T, n), config)
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), gmc),
            ones(T, n + 1),
            config,
        )
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cx), ones(T, n), config)
        if _supports(config, MOI.ConstraintDual)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), gmc),
                vcat(-1, fill(inv(T(n)), n)),
                config,
            )
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), cx),
                fill(-inv(T(n)), n),
                config,
            )
        end
    end
    return
end

function test_conic_GeometricMeanCone_VectorOfVariables_2(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_GeometricMeanCone_helper_2(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_GeometricMeanCone_VectorOfVariables_2),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(T, 10),
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) =>
                fill(-inv(T(9)), 9),
        ),
    )
    return
end

function test_conic_GeometricMeanCone_VectorAffineFunction_2(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_GeometricMeanCone_helper_2(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_GeometricMeanCone_VectorAffineFunction_2),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(T, 10),
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) =>
                fill(-inv(T(9)), 9),
            (MOI.VectorAffineFunction{T}, MOI.GeometricMeanCone) =>
                [vcat(T(-1), fill(inv(T(9)), 9))],
        ),
    )
    return
end

"""
Problem GeoMean3
max 2t
st  (t,x) ∈ GeometricMeanCone(2)
    x <= 2
the optimal solution is (t, x) = (2, 2) with objective value 4

Tests case where the dimension of the geometric mean cone is 2
"""
function _test_conic_GeometricMeanCone_helper_3(
    model::MOI.ModelLike,
    config::Config{T},
    use_VectorOfVariables,
) where {T}
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.GeometricMeanCone,
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.GeometricMeanCone,
        )
    end
    t = MOI.add_variable(model)
    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    vov = MOI.VectorOfVariables([t; x])
    if use_VectorOfVariables
        gmc = MOI.add_constraint(model, vov, MOI.GeometricMeanCone(2))
    else
        gmc = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{T}(vov),
            MOI.GeometricMeanCone(2),
        )
    end
    cx = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x)], T(0)),
        MOI.LessThan(T(2)),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{T},
                MOI.GeometricMeanCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.LessThan{T},
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(2), t)], T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), 4, config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), t), 2, config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), 2, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), gmc), [2, 2], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cx), 2, config)
        if _supports(config, MOI.ConstraintDual)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), gmc), [-2, 2], config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), cx), -2, config)
        end
    end
    return
end

function test_conic_GeometricMeanCone_VectorOfVariables_3(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_GeometricMeanCone_helper_3(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_GeometricMeanCone_VectorOfVariables_3),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[2, 2],
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-2],
        ),
    )
    return
end

function test_conic_GeometricMeanCone_VectorAffineFunction_3(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_GeometricMeanCone_helper_3(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_GeometricMeanCone_VectorAffineFunction_3),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[2, 2],
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-2],
            (MOI.VectorAffineFunction{T}, MOI.GeometricMeanCone) =>
                [T[-2, 2]],
        ),
    )
    return
end

function _test_conic_Exponential_helper(
    model::MOI.ModelLike,
    config::Config{T},
    use_VectorOfVariables::Bool,
) where {T}
    F = if use_VectorOfVariables
        MOI.VectorOfVariables
    else
        MOI.VectorAffineFunction{T}
    end
    @requires MOI.supports_constraint(model, F, MOI.ExponentialCone)
    # Problem EXP1 - ExpPrimal
    # min x + y + z
    #  st  y e^(x/y) <= z, y > 0 (i.e (x, y, z) are in the exponential primal cone)
    #      x == 1
    #      y == 2
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.ExponentialCone,
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.ExponentialCone,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.EqualTo{T},
    )

    v = MOI.add_variables(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3
    vov = MOI.VectorOfVariables(v)
    if use_VectorOfVariables
        vc = MOI.add_constraint(model, vov, MOI.ExponentialCone())
    else
        vc = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{T}(vov),
            MOI.ExponentialCone(),
        )
    end
    cx = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), v[1])], T(0)),
        MOI.EqualTo(T(1)),
    )
    cy = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), v[2])], T(0)),
        MOI.EqualTo(T(2)),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T(1), v), T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        eT = exp(T(1 // 2))
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), 3 + 2eT, config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), 3 + 2eT, config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), [1, 2, 2eT], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), vc), [1, 2, 2eT], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cx), 1, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cy), 2, config)
        if _supports(config, MOI.ConstraintDual)
            u, v, w = MOI.get(model, MOI.ConstraintDual(), vc)
            @test ≈(u, -eT, config)
            @test ≈(v, -eT / 2, config)
            @test ≈(w, 1, config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), cx), 1 + eT, config)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), cy),
                1 + eT / 2,
                config,
            )
        end
    end
    return
end

"""
    test_conic_Exponential_VectorOfVariables(
        model::MOI.ModelLike,
        config::Config,
    )

Test an exponential cone in standard conic form.
"""
function test_conic_Exponential_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_Exponential_helper(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_Exponential_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 2, 2exp(T(1 // 2))],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) =>
                [1 + exp(T(1 // 2)), 1 + exp(T(1 // 2)) / 2],
        ),
    )
    return
end

"""
    test_conic_Exponential_VectorAffineFunction(
        model::MOI.ModelLike,
        config::Config,
    )

Test an exponential cone in geometric conic form.
"""
function test_conic_Exponential_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_Exponential_helper(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_Exponential_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1, 2, 2exp(T(1 // 2))],
            (MOI.VectorAffineFunction{T}, MOI.ExponentialCone) =>
                [[-exp(T(1 // 2)), -exp(T(1 // 2)) / 2, 1]],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) =>
                [1 + exp(T(1 // 2)), 1 + exp(T(1 // 2)) / 2],
        ),
    )
    return
end

"""
    test_conic_Exponential_hard_2(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test an exponential cone problem that ECOS failed.
"""
function test_conic_Exponential_hard_2(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.ExponentialCone,
    )
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.ExponentialCone,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.EqualTo{T},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.LessThan{T},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Nonnegatives,
    )
    v = MOI.add_variables(model, 9)
    @test MOI.get(model, MOI.NumberOfVariables()) == 9
    ec1 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 1, 3],
                MOI.ScalarAffineTerm.(T(1), [v[2], v[3], v[4]]),
            ),
            T[0, 1, 0],
        ),
        MOI.ExponentialCone(),
    )
    ec2 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 1, 3],
                MOI.ScalarAffineTerm.(T[1, -1, 1], [v[2], v[3], v[5]]),
            ),
            T[0, 1, 0],
        ),
        MOI.ExponentialCone(),
    )
    c1 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[1//2, 1//2, -1], [v[4], v[5], v[6]]),
            T(0),
        ),
        MOI.EqualTo(T(0)),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 2, 3, 1, 2, 3],
                MOI.ScalarAffineTerm.(
                    T[1, 1, 1, 3//10, 3//10, 3//10],
                    [v[1], v[2], v[3], v[7], v[8], v[9]],
                ),
            ),
            zeros(T, 3),
        ),
        MOI.Nonnegatives(3),
    )
    c3 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 2, 3, 1, 2, 3],
                MOI.ScalarAffineTerm.(
                    T[-1, -1, -1, 3//10, 3//10, 3//10],
                    [v[1], v[2], v[3], v[7], v[8], v[9]],
                ),
            ),
            zeros(T, 3),
        ),
        MOI.Nonnegatives(3),
    )
    c4 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T(1), [v[7], v[8], v[9]]),
            T(0),
        ),
        MOI.LessThan(T(1)),
    )
    c5 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), v[7])], T(0)),
        MOI.EqualTo(T(0)),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), v[6])], T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        eT = exp(T(-3 // 10))
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), eT, config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), eT, config)
        end
        @test ≈(
            MOI.get(model, MOI.VariablePrimal(), v),
            [0, -3 // 10, 0, eT, eT, eT, 0, 1, 0],
            config,
        )
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), ec1),
            [-3 // 10, 1, eT],
            config,
        )
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), ec2),
            [-3 // 10, 1, eT],
            config,
        )
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c1), 0, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c2), zeros(T, 3), config)
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), c3),
            [0, 3 // 5, 0],
            config,
        )
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c4), 1, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c5), 0, config)
        if _supports(config, MOI.ConstraintDual)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), ec1),
                [-eT / 2, -13 * eT / 20, 1 // 2],
                config,
            )
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), ec2),
                [-eT / 2, -13 * eT / 20, 1 // 2],
                config,
            )
            @test ≈(MOI.get(model, MOI.ConstraintDual(), c1), -1, config)
            d5 = MOI.get(model, MOI.ConstraintDual(), c5) # degree of freedom
            d23 = (eT * 3 / 10 - d5) / (3 // 5) # dual constraint corresponding to v[7]
            @test d23 >= -config.atol
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), c2),
                [d23, eT, eT / 2],
                config,
            )
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), c3),
                [d23, 0, eT / 2],
                config,
            )
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), c4),
                -eT * 3 / 10,
                config,
            )
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_Exponential_hard_2),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    eT = exp(T(-3 // 10))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0, -3 // 10, 0, eT, eT, eT, 0, 1, 0],
            (MOI.VectorAffineFunction{T}, MOI.ExponentialCone) => [
                [-eT / 2, -13eT / 20, 1 // 2],
                [-eT / 2, -13eT / 20, 1 // 2],
            ],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) =>
                [-1, eT * 3 / 10],
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) =>
                [-eT * 3 / 10],
            (MOI.VectorAffineFunction{T}, MOI.Nonnegatives) =>
                [[0, eT, eT / 2], [0, 0, eT / 2]],
        ),
    )
    return
end

"""
    test_conic_Exponential_hard(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test an exponential problem that ECOS failed.
"""
function test_conic_Exponential_hard(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.ExponentialCone,
    )
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.LessThan{T},
    )
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.LessThan{T})
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.ExponentialCone,
    )
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    xc = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(2), x)], T(0)),
        MOI.LessThan(T(4)),
    )
    yc = MOI.add_constraint(model, y, MOI.LessThan(T(5)))
    @test yc.value == y.value
    ec = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.([1, 3], MOI.ScalarAffineTerm.(T(1), [x, y])),
            T[0, 1, 0],
        ),
        MOI.ExponentialCone(),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x)], T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end

        @test ≈(MOI.get(model, MOI.ObjectiveValue()), log(T(5)), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), log(T(5)), config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), log(T(5)), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), y), 5, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), xc), 2log(T(5)), config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), yc), 5, config)
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), ec),
            [log(T(5)), 1, 5],
            config,
        )
        if _supports(config, MOI.ConstraintDual)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), xc), 0, config)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), yc),
                T(-1 // 5),
                config,
            )
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), ec),
                T[-1, log(T(5))-1, 1//5],
                config,
            )
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_Exponential_hard),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[log(T(5)), 5],
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[0],
            (MOI.VectorAffineFunction{T}, MOI.ExponentialCone) =>
                [T[-1, log(T(5))-1, 1//5]],
        ),
    )
    return
end

function _test_conic_DualExponentialCone_helper(
    model::MOI.ModelLike,
    config::Config{T},
    use_VectorOfVariables::Bool,
) where {T}
    F = if use_VectorOfVariables
        MOI.VectorOfVariables
    else
        MOI.VectorAffineFunction{T}
    end
    @requires MOI.supports_constraint(model, F, MOI.DualExponentialCone)
    # Problem dual exp
    # max 2x_2 + x_1
    # s.t.
    # x_1 + u == 1
    # x_2 + v == 1
    # w == 1
    # (u, v, w) ∈ DualExponentialCone
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.DualExponentialCone,
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.DualExponentialCone,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.EqualTo{T},
    )
    v = MOI.add_variables(model, 3)
    x = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 5
    vov = MOI.VectorOfVariables(v)
    if use_VectorOfVariables
        vc = MOI.add_constraint(model, vov, MOI.DualExponentialCone())
    else
        vc = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{T}(vov),
            MOI.DualExponentialCone(),
        )
    end
    cu = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[1, 1], [x[1], v[1]]),
            T(0),
        ),
        MOI.EqualTo(T(1)),
    )
    cv = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[1, 1], [x[2], v[2]]),
            T(0),
        ),
        MOI.EqualTo(T(1)),
    )
    cw = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), v[3])], T(0)),
        MOI.EqualTo(T(1)),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[1, 2], x), T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        eT = exp(T(1 // 2))
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), 3 + 2eT, config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), 3 + 2eT, config)
        end
        @test ≈(
            MOI.get(model, MOI.VariablePrimal(), v),
            T[-eT, -eT/2, 1],
            config,
        )
        @test ≈(
            MOI.get(model, MOI.VariablePrimal(), x),
            T[1+eT, 1+eT/2],
            config,
        )
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), vc),
            T[-eT, -eT/2, 1],
            config,
        )
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cu), 1, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cv), 1, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cw), 1, config)
        if _supports(config, MOI.ConstraintDual)
            x, y, z = MOI.get(model, MOI.ConstraintDual(), vc)
            @test ≈(x, 1, config)
            @test ≈(y, 2, config)
            @test ≈(z, 2eT, config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), cu), -1, config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), cv), -2, config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), cw), -2eT, config)
        end
    end
    return
end

function test_conic_DualExponentialCone_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_DualExponentialCone_helper(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_DualExponentialCone_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    eT = exp(T(1 // 2))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[-eT, -eT/2, 1, 1+eT, 1+eT/2],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) =>
                T[-1, -2, -2eT],
        ),
    )
    return
end

function test_conic_DualExponentialCone_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_DualExponentialCone_helper(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_DualExponentialCone_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    eT = exp(T(1 // 2))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[-eT, -eT/2, 1, 1+eT, 1+eT/2],
            (MOI.VectorAffineFunction{T}, MOI.DualExponentialCone) =>
                [T[1, 2, 2eT]],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) =>
                T[-1, -2, -2eT],
        ),
    )
    return
end

"""
```
max z
 st  x^0.9 * y^(0.1) >= |z| (i.e (x, y, z) are in the 3d power cone with a=0.9)
     x == 2
     y == 1
```
Dual
```
min -2α - β
 st (u/0.9)^0.9 (v/0.1)^0.1 >= |w|
    u + α = 0
    v + β = 0
    w = -1
```
"""
function _test_conic_PowerCone_helper(
    model::MOI.ModelLike,
    config::Config{T},
    use_VectorOfVariables::Bool,
) where {T}
    @requires T == Float64
    F = if use_VectorOfVariables
        MOI.VectorOfVariables
    else
        MOI.VectorAffineFunction{T}
    end
    @requires MOI.supports_constraint(model, F, MOI.PowerCone{T})
    a = T(9 // 10)
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.PowerCone{T},
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.PowerCone{T},
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.EqualTo{T},
    )
    v = MOI.add_variables(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3
    vov = MOI.VectorOfVariables(v)
    if use_VectorOfVariables
        vc = MOI.add_constraint(model, vov, MOI.PowerCone(a))
    else
        vc = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{T}(vov),
            MOI.PowerCone(a),
        )
    end
    cx = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), v[1])], T(0)),
        MOI.EqualTo(T(2)),
    )
    cy = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), v[2])], T(0)),
        MOI.EqualTo(T(1)),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), v[3])], T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), 2^a, config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), [2, 1, 2^a], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), vc), [2, 1, 2^a], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cx), 2, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cy), 1, config)
        if _supports(config, MOI.ConstraintDual)
            # Only real solution of u^10 - u^9 / 2^0.1 = -(0.1*0.9^9)/2
            u_value = 0.839729692
            v_value = 2^a - 2u_value
            u, v, w = MOI.get(model, MOI.ConstraintDual(), vc)
            @test ≈(u, u_value, config)
            @test ≈(v, v_value, config)
            @test ≈(w, -1, config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), cx), -u_value, config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), cy), -v_value, config)
        end
    end
    return
end

function test_conic_PowerCone_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_PowerCone_helper(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_PowerCone_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    if T != Float64
        return  # TODO(odow): compute u_value as an analytical expression.
    end
    a = T(9 // 10)
    u_value = 0.839729692
    v_value = 2^a - 2 * u_value
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[2, 1, 2^a],
            (MOI.VectorOfVariables, MOI.PowerCone{T}) =>
                [[u_value, v_value, -1]],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) =>
                [-u_value, -v_value],
        ),
    )
    return
end

function test_conic_PowerCone_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_PowerCone_helper(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_PowerCone_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    if T != Float64
        return  # TODO(odow): compute u_value as an analytical expression.
    end
    a = T(9 // 10)
    u_value = 0.839729692
    v_value = 2^a - 2 * u_value
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [2, 1, 2^a],
            (MOI.VectorAffineFunction{T}, MOI.PowerCone{T}) =>
                [[u_value, v_value, -1]],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) =>
                [-u_value, -v_value],
        ),
    )
    return
end

"""
    _test_conic_DualPowerCone_helper(
        model::MOI.ModelLike,
        config::Config,
        use_VectorOfVariables::Bool;
        exponent::T = 0.9,
    )

Problem dual POW1
```
min -x_1 - x_2
 st  x_1 + u == 0
     x_2 + v == 0
     w == 1
    (u, v, w) ∈ DualPowerCone(exponent)
```
By the Weighted AM–GM inequality, you have
0.9a + 0.1b >= a^0.9 b^0.1
with equality if and only if a == b
here taking a = u/0.9 and b = v/0.1, we have
u + v >= (u/0.9)^0.9 (v/0.1)^0.1
with equality if and only if u/0.9 == v/0.1.
Here the best you can do is u + v == 1 and for that inequality must hold so u = 9v
hence you get v = 0.1 and u = 0.9.
The same works for other values of exponent as key word argument
"""
function _test_conic_DualPowerCone_helper(
    model::MOI.ModelLike,
    config::Config{T},
    use_VectorOfVariables::Bool;
    exponent::T = T(9 // 10),
) where {T}
    F = if use_VectorOfVariables
        MOI.VectorOfVariables
    else
        MOI.VectorAffineFunction{T}
    end
    @requires MOI.supports_constraint(model, F, MOI.DualPowerCone{T})
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.DualPowerCone{T},
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            MOI.DualPowerCone{T},
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.EqualTo{T},
    )
    v = MOI.add_variables(model, 3)
    x = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 5
    vov = MOI.VectorOfVariables(v)
    if use_VectorOfVariables
        vc = MOI.add_constraint(model, vov, MOI.DualPowerCone(exponent))
    else
        vc = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{T}(vov),
            MOI.DualPowerCone(exponent),
        )
    end
    cu = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[1, 1], [x[1], v[1]]),
            T(0),
        ),
        MOI.EqualTo(T(0)),
    )
    cv = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[1, 1], [x[2], v[2]]),
            T(0),
        ),
        MOI.EqualTo(T(0)),
    )
    cw = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), v[3])], T(0)),
        MOI.EqualTo(T(1)),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[-1, -1], [x[1], x[2]]),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), 1, config)
        v_sol = T[exponent, 1-exponent, 1]
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), v_sol, config)
        @test ≈(
            MOI.get(model, MOI.VariablePrimal(), x),
            T[-exponent, -(1 - exponent)],
            config,
        )
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), vc), v_sol, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cu), 0, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cv), 0, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cw), 1, config)
        if _supports(config, MOI.ConstraintDual)
            x, y, z = MOI.get(model, MOI.ConstraintDual(), vc)
            @test ≈(x, 1, config)
            @test ≈(y, 1, config)
            @test ≈(z, -1, config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), cu), -1, config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), cv), -1, config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), cw), 1, config)
        end
    end
    return
end

"""
    test_conic_DualPowerCone_VectorOfVariables(
        model::MOI.ModelLike,
        config::Config,
    )

Test DualPowerCone in the standard conic form.
"""
function test_conic_DualPowerCone_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_DualPowerCone_helper(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_DualPowerCone_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[9//10, 1//10, 1, -9//10, -1//10],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => T[-1, -1, 1],
        ),
    )
    return
end

"""
    test_conic_DualPowerCone_VectorAffineFunction(
        model::MOI.ModelLike,
        config::Config,
    )

Test DualPowerCone in the geometric conic form.
"""
function test_conic_DualPowerCone_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_DualPowerCone_helper(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_DualPowerCone_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[9//10, 1//10, 1, -9//10, -1//10],
            (MOI.VectorAffineFunction{T}, MOI.DualPowerCone{T}) =>
                [T[1, 1, -1]],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => T[-1, -1, 1],
        ),
    )
    return
end

"""
    test_conic_RelativeEntropyCone(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test the problem:
```
min u
 st  u >= 2*log(2/1) + 3*log(3/5)  (i.e. (u, 1, 5, 2, 3) in RelativeEntropyCone(5))
Optimal solution is:
u = 2*log(2/1) + 3*log(3/5) ≈ -0.1461825
```
"""
function test_conic_RelativeEntropyCone(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.RelativeEntropyCone,
    )
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.RelativeEntropyCone,
    )
    u = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    relentr = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), u))],
            T[0, 1, 5, 2, 3],
        ),
        MOI.RelativeEntropyCone(5),
    )
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), u)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        u_opt = 2 * log(T(2)) + 3 * log(T(3 // 5))
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), u_opt, config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), u_opt, config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), u), u_opt, config)
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), relentr),
            T[u_opt, 1, 5, 2, 3],
            config,
        )
        if _supports(config, MOI.ConstraintDual)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), relentr),
                T[1, 2, 3//5, log(T(1 // 2))-1, log(T(5 // 3))-1],
                config,
            )
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_RelativeEntropyCone),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [2 * log(T(2 // 1)) + 3 * log(T(3 // 5))],
            (MOI.VectorAffineFunction{T}, MOI.RelativeEntropyCone) =>
                [T[1, 2, 3//5, log(T(1 // 2))-1, log(T(5 // 3))-1]],
        ),
    )
    return
end

"""
    test_conic_NormSpectralCone(model::MOI.ModelLike, config::Config{T}) where {T}

Test the problem:
```
min t
 st  t >= sigma_1([1 1 0; 1 -1 1]) (i.e (t, 1, 1, 1, -1, 0, 1]) is in NormSpectralCone(2, 3))
Singular values are [sqrt(T(3)), sqrt(T(2))], so optimal solution is:
t = sqrt(T(3))
```
"""
function test_conic_NormSpectralCone(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.NormSpectralCone,
    )
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.NormSpectralCone,
    )

    t = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    data = T[1, 1, 1, -1, 0, 1]
    spec = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), t))],
            vcat(T(0), data),
        ),
        MOI.NormSpectralCone(2, 3),
    )
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        rt3 = sqrt(T(3))
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), rt3, config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), rt3, config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), t), rt3, config)
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), spec),
            vcat(rt3, data),
            config,
        )
        if _supports(config, MOI.ConstraintDual)
            invrt3 = inv(rt3)
            d = T[1, 0, -invrt3, 0, invrt3, 0, -invrt3]
            @test ≈(MOI.get(model, MOI.ConstraintDual(), spec), d, config)
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_NormSpectralCone),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    invrt3 = inv(sqrt(T(3)))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [sqrt(T(3))],
            (MOI.VectorAffineFunction{T}, MOI.NormSpectralCone) =>
                [T[1, 0, -invrt3, 0, invrt3, 0, -invrt3]],
        ),
    )
    return
end

"""
    test_conic_NormSpectralCone_2(model::MOI.ModelLike, config::Config{T}) where {T}

Test the problem:
```
min t
 st  t >= sigma_1([1 1; 1 -1; 0 1]) (i.e (t, 1, 1, 0, 1, -1, 1]) is in NormSpectralCone(3, 2))
Singular values are [sqrt(T(3)), sqrt(T(2))], so optimal solution is:
t = sqrt(T(3))
```
"""
function test_conic_NormSpectralCone_2(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.NormSpectralCone,
    )
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.NormSpectralCone,
    )

    t = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    data = T[1, 1, 0, 1, -1, 1]
    spec = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), t))],
            vcat(T(0), data),
        ),
        MOI.NormSpectralCone(3, 2),
    )
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        rt3 = sqrt(T(3))
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ rt3 atol = atol rtol = rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ rt3 atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ rt3 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), spec) ≈ vcat(rt3, data) atol =
            atol rtol = rtol
        if _supports(config, MOI.ConstraintDual)
            invrt3 = inv(rt3)
            @test MOI.get(model, MOI.ConstraintDual(), spec) ≈
                  T[1, 0, 0, 0, -invrt3, invrt3, -invrt3] atol = atol rtol =
                rtol
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_NormSpectralCone_2),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    invrt3 = inv(sqrt(T(3)))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [sqrt(T(3))],
            (MOI.VectorAffineFunction{T}, MOI.NormSpectralCone) =>
                [T[1, 0, 0, 0, -invrt3, invrt3, -invrt3]],
        ),
    )
    return
end

"""
    test_conic_NormNuclearCone(model::MOI.ModelLike, config::Config{T}) where {T}

Test the problem:
```
min t
 st  t >= sum_i sigma_i([1 1 0; 1 -1 1]) (i.e (t, 1, 1, 1, -1, 0, 1]) is in NormNuclearCone(2, 3))
Singular values are [sqrt(T(3)), sqrt(T(2))], so optimal solution is:
t = sqrt(T(3)) + sqrt(T(2))
```
"""
function test_conic_NormNuclearCone(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.NormNuclearCone,
    )
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.NormNuclearCone,
    )

    t = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    data = T[1, 1, 1, -1, 0, 1]
    nuc = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), t))],
            vcat(T(0), data),
        ),
        MOI.NormNuclearCone(2, 3),
    )
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        rt3 = sqrt(T(3))
        rt2 = sqrt(T(2))
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ rt3 + rt2 atol = atol rtol =
            rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ rt3 + rt2 atol =
                atol rtol = rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ rt3 + rt2 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), nuc) ≈
              vcat(rt3 + rt2, data) atol = atol rtol = rtol
        if _supports(config, MOI.ConstraintDual)
            invrt2 = inv(rt2)
            invrt3 = inv(rt3)
            @test MOI.get(model, MOI.ConstraintDual(), nuc) ≈
                  T[1, -invrt2, -invrt3, -invrt2, invrt3, 0, -invrt3] atol =
                atol rtol = rtol
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_NormNuclearCone),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    invrt2 = inv(sqrt(T(2)))
    invrt3 = inv(sqrt(T(3)))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [sqrt(T(2)) + sqrt(T(3))],
            (MOI.VectorAffineFunction{T}, MOI.NormNuclearCone) =>
                [T[1, -invrt2, -invrt3, -invrt2, invrt3, 0, -invrt3]],
        ),
    )
    return
end

"""
    test_conic_NormNuclearCone_2(model::MOI.ModelLike, config::Config{T}) where {T}

Test the problem:
```
min t
 st  t >= sum_i sigma_i([1 1; 1 -1; 0 1]) (i.e (t, 1, 1, 0, 1, -1, 1]) is in NormNuclearCone(3, 2))
Singular values are [sqrt(T(3)), sqrt(T(2))], so optimal solution is:
t = sqrt(T(3)) + sqrt(T(2))
```
"""
function test_conic_NormNuclearCone_2(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.NormNuclearCone,
    )
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.NormNuclearCone,
    )

    t = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    data = T[1, 1, 0, 1, -1, 1]
    nuc = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), t))],
            vcat(T(0), data),
        ),
        MOI.NormNuclearCone(3, 2),
    )
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        rt3 = sqrt(T(3))
        rt2 = sqrt(T(2))
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ rt3 + rt2 atol = atol rtol =
            rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ rt3 + rt2 atol =
                atol rtol = rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ rt3 + rt2 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), nuc) ≈
              vcat(rt3 + rt2, data) atol = atol rtol = rtol
        if _supports(config, MOI.ConstraintDual)
            invrt2 = inv(rt2)
            invrt3 = inv(rt3)
            @test MOI.get(model, MOI.ConstraintDual(), nuc) ≈
                  T[1, -invrt2, -invrt2, 0, -invrt3, invrt3, -invrt3] atol =
                atol rtol = rtol
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_NormNuclearCone_2),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    invrt2 = inv(sqrt(T(2)))
    invrt3 = inv(sqrt(T(3)))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [sqrt(T(2)) + sqrt(T(3))],
            (MOI.VectorAffineFunction{T}, MOI.NormNuclearCone) =>
                [T[1, -invrt2, -invrt2, 0, -invrt3, invrt3, -invrt3]],
        ),
    )
    return
end

"""
min X[1,1] + X[2,2]    max y
    X[2,1] = 1         [0   y/2     [ 1  0
                        y/2 0    <=   0  1]
    X >= 0              y free
Optimal solution:
    ⎛ 1   1 ⎞
X = ⎜       ⎟           y = 2
    ⎝ 1   1 ⎠
"""
function _test_conic_PositiveSemidefiniteCone_helper(
    model::MOI.ModelLike,
    use_VectorOfVariables::Bool,
    psdcone,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
    square = psdcone == MOI.PositiveSemidefiniteConeSquare
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(model, MOI.VectorOfVariables, psdcone)
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            psdcone,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.EqualTo{T},
    )

    X = MOI.add_variables(model, square ? 4 : 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == (square ? 4 : 3)
    vov = MOI.VectorOfVariables(X)
    if use_VectorOfVariables
        cX = MOI.add_constraint(model, vov, psdcone(2))
    else
        cX = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{T}(vov),
            psdcone(2),
        )
    end
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), X[2])], T(0)),
        MOI.EqualTo(T(1)),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{T},
                psdcone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.EqualTo{T},
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T(1), [X[1], X[end]]),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2 atol = atol rtol = rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ 2 atol = atol rtol =
                rtol
        end
        Xv = square ? ones(T, 4) : ones(T, 3)
        @test MOI.get(model, MOI.VariablePrimal(), X) ≈ Xv atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cX) ≈ Xv atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ 2 atol = atol rtol =
                rtol
            cXv = square ? [1, -2, 0, 1] : [1, -1, 1]
            @test MOI.get(model, MOI.ConstraintDual(), cX) ≈ cXv atol = atol rtol =
                rtol
        end
    end
    return
end

function test_conic_PositiveSemidefiniteConeTriangle_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_PositiveSemidefiniteCone_helper(
        model,
        true,
        MOI.PositiveSemidefiniteConeTriangle,
        config,
    )
    return
end

function setup_test(
    ::typeof(test_conic_PositiveSemidefiniteConeTriangle_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(T, 3),
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => [2],
        ),
    )
    return
end

function test_conic_PositiveSemidefiniteConeTriangle_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_PositiveSemidefiniteCone_helper(
        model,
        false,
        MOI.PositiveSemidefiniteConeTriangle,
        config,
    )
    return
end

function setup_test(
    ::typeof(test_conic_PositiveSemidefiniteConeTriangle_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(T, 3),
            (
                MOI.VectorAffineFunction{T},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [T[1, -1, 1]],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => T[2],
        ),
    )
    return
end

function test_conic_PositiveSemidefiniteConeSquare_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_PositiveSemidefiniteCone_helper(
        model,
        true,
        MOI.PositiveSemidefiniteConeSquare,
        config,
    )
    return
end

function setup_test(
    ::typeof(test_conic_PositiveSemidefiniteConeSquare_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(T, 4),
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => [2],
        ),
    )
    return
end

function test_conic_PositiveSemidefiniteConeSquare_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_PositiveSemidefiniteCone_helper(
        model,
        false,
        MOI.PositiveSemidefiniteConeSquare,
        config,
    )
    return
end

function setup_test(
    ::typeof(test_conic_PositiveSemidefiniteConeSquare_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(T, 4),
            (MOI.VectorAffineFunction{T}, MOI.PositiveSemidefiniteConeSquare) => [T[1, -2, 0, 1]],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => T[2],
        ),
    )
    return
end

"""
Problem SDP1 - sdo1 from MOSEK docs
From Mosek.jl/test/mathprogtestextra.jl, under license:

Copyright (c) 2013 Ulf Worsoe, Mosek ApS

Permission is hereby granted, free of charge, to any person obtaining a copy of this
software and associated documentation files (the "Software"), to deal in the Software
without restriction, including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
to whom the Software is furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

    | 2 1 0 |
min | 1 2 1 | . X + x1
    | 0 1 2 |


s.t. | 1 0 0 |
     | 0 1 0 | . X + x1 = 1
     | 0 0 1 |

     | 1 1 1 |
     | 1 1 1 | . X + x2 + x3 = 1/2
     | 1 1 1 |

     (x1,x2,x3) in C^3_q
     X in C_psd

The dual is
max y1 + y2/2

s.t. | y1+y2    y2    y2 |
     |    y2 y1+y2    y2 | in C_psd
     |    y2    y2 y1+y2 |

     (1-y1, -y2, -y2) in C^3_q
"""
function _test_conic_PositiveSemidefiniteCone_helper_2(
    model::MOI.ModelLike,
    use_VectorOfVariables::Bool,
    psdcone,
    config::Config{T},
) where {T}
    square = psdcone == MOI.PositiveSemidefiniteConeSquare
    # The dual of the SDP constraint is rank two of the form
    # [γ, 0, -γ] * [γ, 0, γ'] + [δ, ε, δ] * [δ, ε, δ]'
    # and the dual of the SOC constraint is of the form (√T(2)*y2, -y2, -y2)
    #
    # The feasible set of the constraint dual contains only four points.
    # Eliminating, y1, y2 and γ from the dual constraints gives
    # -ε^2 + -εδ + 2δ^2 + 1
    # (√T(2)-2)ε^2 + (-2√T(2)+2)δ^2 + 1
    # Eliminating ε from this set of equation give
    # (-6√T(2)+4)δ^4 + (3√T(2)-2)δ^2 + (2√T(2)-3)
    # from which we find the solution
    δ = √(1 + (3 * √T(2) + 2) * √(-116 * √T(2) + 166) / 14) / 2
    # which is optimal
    ε = √((1 - 2 * (√T(2) - 1) * δ^2) / (2 - √T(2)))
    y2 = 1 - ε * δ
    y1 = 1 - √T(2) * y2
    obj = y1 + y2 / 2
    # The primal solution is rank one of the form
    # X = [α, β, α] * [α, β, α]'
    # and by complementary slackness, x is of the form (√T(2)*x2, x2, x2)
    # The primal reduces to
    #      4α^2+4αβ+2β^2+√T(2)*x2= obj
    #      2α^2    + β^2+√T(2)*x2 = 1 (1)
    #      8α^2+8αβ+2β^2+ 4 x2 = 1
    # Eliminating β, we get
    # 4α^2 + 4x2 = 3 - 2obj (2)
    # By complementary slackness, we have β = kα where
    k = -2 * δ / ε
    # Replacing β by kα in (1) allows to eliminate α^2 in (2) to get
    x2 = ((3 - 2obj) * (2 + k^2) - 4) / (4 * (2 + k^2) - 4 * √T(2))
    # With (2) we get
    α = √(3 - 2obj - 4x2) / 2
    β = k * α
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.EqualTo{T},
    )
    if use_VectorOfVariables
        @requires MOI.supports_constraint(model, MOI.VectorOfVariables, psdcone)
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            psdcone,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.SecondOrderCone,
    )

    X = MOI.add_variables(model, square ? 9 : 6)
    @test MOI.get(model, MOI.NumberOfVariables()) == (square ? 9 : 6)
    x = MOI.add_variables(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == (square ? 12 : 9)
    vov = MOI.VectorOfVariables(X)
    if use_VectorOfVariables
        cX = MOI.add_constraint(model, vov, psdcone(3))
    else
        cX = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{T}(vov),
            psdcone(3),
        )
    end
    cx = MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.SecondOrderCone(3),
    )
    c1 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(
                T[1, 1, 1, 1],
                [X[1], X[square ? 5 : 3], X[end], x[1]],
            ),
            T(0),
        ),
        MOI.EqualTo(T(1)),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(
                square ? ones(T, 11) : T[1, 2, 1, 2, 2, 1, 1, 1],
                [X; x[2]; x[3]],
            ),
            T(0),
        ),
        MOI.EqualTo(T(1 / 2)),
    )
    objXidx = square ? [1, 2, 4, 5, 6, 8, 9] : [1, 2, 3, 5, 6]
    objXcoefs = square ? T[2, 1, 1, 2, 1, 1, 2] : 2 * ones(T, 5)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[objXcoefs; 1], [X[objXidx]; x[1]]),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{T},
                psdcone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.EqualTo{T},
            }(),
        ) == 2
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SecondOrderCone}(),
        ) == 1
    end
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), obj, config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), obj, config)
        end
        Xv =
            square ? [α^2, α * β, α^2, α * β, β^2, α * β, α^2, α * β, α^2] :
            [α^2, α * β, β^2, α^2, α * β, α^2]
        xv = [√T(2) * x2, x2, x2]
        @test ≈(MOI.get(model, MOI.VariablePrimal(), X), Xv, config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), xv, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cX), Xv, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cx), xv, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c1), 1, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c2), T(1 // 2), config)
        if _supports(config, MOI.ConstraintDual)
            cX0 = 1 + (√T(2) - 1) * y2
            cX1 = 1 - y2
            cX2 = -y2
            cXv =
                square ? [cX0, cX1, cX2, cX1, cX0, cX1, cX2, cX1, cX0] :
                [cX0, cX1, cX0, cX2, cX1, cX0]
            @test ≈(MOI.get(model, MOI.ConstraintDual(), cX), cXv, config)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), cx),
                [1 - y1, -y2, -y2],
                config,
            )
            @test ≈(MOI.get(model, MOI.ConstraintDual(), c1), y1, config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), c2), y2, config)
        end
    end
    return
end

function test_conic_PositiveSemidefiniteConeTriangle_VectorOfVariables_2(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_PositiveSemidefiniteCone_helper_2(
        model,
        true,
        MOI.PositiveSemidefiniteConeTriangle,
        config,
    )
    return
end

function setup_test(
    ::typeof(test_conic_PositiveSemidefiniteConeTriangle_VectorOfVariables_2),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    δ = √(1 + (3 * √T(2) + 2) * √(-116 * √T(2) + 166) / 14) / 2
    ε = √((1 - 2 * (√T(2) - 1) * δ^2) / (2 - √T(2)))
    y2 = 1 - ε * δ
    y1 = 1 - √T(2) * y2
    obj = y1 + y2 / 2
    k = -2 * δ / ε
    x2 = ((3 - 2obj) * (2 + k^2) - 4) / (4 * (2 + k^2) - 4 * √T(2))
    α = √(3 - 2obj - 4x2) / 2
    β = k * α
    Xv = [α^2, α * β, β^2, α^2, α * β, α^2]
    xv = [√T(2) * x2, x2, x2]
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [Xv; xv],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => [y1, y2],
        ),
    )
    return
end

function test_conic_PositiveSemidefiniteConeTriangle_VectorAffineFunction_2(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_PositiveSemidefiniteCone_helper_2(
        model,
        false,
        MOI.PositiveSemidefiniteConeTriangle,
        config,
    )
    return
end

function setup_test(
    ::typeof(
        test_conic_PositiveSemidefiniteConeTriangle_VectorAffineFunction_2,
    ),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    δ = √(1 + (3 * √T(2) + 2) * √(-116 * √T(2) + 166) / 14) / 2
    ε = √((1 - 2 * (√T(2) - 1) * δ^2) / (2 - √T(2)))
    y2 = 1 - ε * δ
    y1 = 1 - √T(2) * y2
    obj = y1 + y2 / 2
    k = -2 * δ / ε
    x2 = ((3 - 2obj) * (2 + k^2) - 4) / (4 * (2 + k^2) - 4 * √T(2))
    α = √(3 - 2obj - 4x2) / 2
    β = k * α
    Xv = [α^2, α * β, β^2, α^2, α * β, α^2]
    xv = [√T(2) * x2, x2, x2]
    cX0 = 1 + (√T(2) - 1) * y2
    cX1 = 1 - y2
    cX2 = -y2
    cXv = [cX0, cX1, cX0, cX2, cX1, cX0]
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [Xv; xv],
            (
                MOI.VectorAffineFunction{T},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [cXv],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => [y1, y2],
        ),
    )
    return
end

function test_conic_PositiveSemidefiniteConeSquare_VectorOfVariables_2(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_PositiveSemidefiniteCone_helper_2(
        model,
        true,
        MOI.PositiveSemidefiniteConeSquare,
        config,
    )
    return
end

function setup_test(
    ::typeof(test_conic_PositiveSemidefiniteConeSquare_VectorOfVariables_2),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    δ = √(1 + (3 * √T(2) + 2) * √(-116 * √T(2) + 166) / 14) / 2
    ε = √((1 - 2 * (√T(2) - 1) * δ^2) / (2 - √T(2)))
    y2 = 1 - ε * δ
    y1 = 1 - √T(2) * y2
    obj = y1 + y2 / 2
    k = -2 * δ / ε
    x2 = ((3 - 2obj) * (2 + k^2) - 4) / (4 * (2 + k^2) - 4 * √T(2))
    α = √(3 - 2obj - 4x2) / 2
    β = k * α
    Xv = [α^2, α * β, α^2, α * β, β^2, α * β, α^2, α * β, α^2]
    xv = [√T(2) * x2, x2, x2]
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [Xv; xv],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => [y1, y2],
        ),
    )
    return
end

function test_conic_PositiveSemidefiniteConeSquare_VectorAffineFunction_2(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_PositiveSemidefiniteCone_helper_2(
        model,
        false,
        MOI.PositiveSemidefiniteConeSquare,
        config,
    )
    return
end

function setup_test(
    ::typeof(test_conic_PositiveSemidefiniteConeSquare_VectorAffineFunction_2),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    δ = √(1 + (3 * √T(2) + 2) * √(-116 * √T(2) + 166) / 14) / 2
    ε = √((1 - 2 * (√T(2) - 1) * δ^2) / (2 - √T(2)))
    y2 = 1 - ε * δ
    y1 = 1 - √T(2) * y2
    obj = y1 + y2 / 2
    k = -2 * δ / ε
    x2 = ((3 - 2obj) * (2 + k^2) - 4) / (4 * (2 + k^2) - 4 * √T(2))
    α = √(3 - 2obj - 4x2) / 2
    β = k * α
    xv = [√T(2) * x2, x2, x2]
    cX0 = 1 + (√T(2) - 1) * y2
    cX1 = 1 - y2
    cX2 = -y2
    Xv = [α^2, α * β, α^2, α * β, β^2, α * β, α^2, α * β, α^2]
    cXv = [cX0, cX1, cX2, cX1, cX0, cX1, cX2, cX1, cX0]
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [Xv; xv],
            (MOI.VectorAffineFunction{T}, MOI.PositiveSemidefiniteConeSquare) => [cXv],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => [y1, y2],
        ),
    )
    return
end

"""
    test_conic_PositiveSemidefiniteConeTriangle(
        model::MOI.ModelLike,
        config::Config,
    )
"""
function test_conic_PositiveSemidefiniteConeTriangle(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    # Caused getdual to fail on SCS and Mosek
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.GreaterThan{T},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Nonpositives,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.PositiveSemidefiniteConeTriangle,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.EqualTo{T},
    )

    x = MOI.add_variables(model, 7)
    @test MOI.get(model, MOI.NumberOfVariables()) == 7
    η = T(10)
    c1 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T(-1), x[1:6]), T(0)),
        MOI.GreaterThan(-η),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(1:6, MOI.ScalarAffineTerm.(T(-1), x[1:6])),
            zeros(T, 6),
        ),
        MOI.Nonpositives(6),
    )
    α = T(4 // 5)
    δ = T(9 // 10)
    c3 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [fill(1, 7); fill(2, 5); fill(3, 6)],
                MOI.ScalarAffineTerm.(
                    T[
                        δ/2,
                        α,
                        δ,
                        δ/4,
                        δ/8,
                        0,
                        -1,
                        -δ/(2*√T(2)),
                        -δ/4,
                        0,
                        -δ/(8*√T(2)),
                        0,
                        δ/2,
                        δ-α,
                        0,
                        δ/8,
                        δ/4,
                        -1,
                    ],
                    [x[1:7]; x[1:3]; x[5:6]; x[1:3]; x[5:7]],
                ),
            ),
            zeros(T, 3),
        ),
        MOI.PositiveSemidefiniteConeTriangle(2),
    )
    c4 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T(0), [x[1:3]; x[5:6]]),
            T(0),
        ),
        MOI.EqualTo(T(0)),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.GreaterThan{T},
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.Nonpositives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.PositiveSemidefiniteConeTriangle,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.EqualTo{T},
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x[7])], T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test ≈(
            MOI.get(model, MOI.VariablePrimal(), x),
            T[2η/3, 0, η/3, 0, 0, 0, η*δ*(1-1/√T(3))/2],
            config,
        )
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c1), -η, config)
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), c2),
            T[-2η/3, 0, -η/3, 0, 0, 0],
            config,
        )
        @test ≈(
            MOI.get(model, MOI.ConstraintPrimal(), c3),
            T[η*δ*(1/√T(3)+1//3)/2, -η*δ/(3*√T(2)), η*δ*(1/√T(3)-1//3)/2],
            config,
        )
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c4), 0, config)
        if _supports(config, MOI.ConstraintDual)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), c1),
                δ * (1 - 1 / √T(3)) / 2,
                config,
            )
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), c2),
                T[
                    0,
                    -α/√T(3)+δ/(2*√T(6))*(2*√T(2)-1),
                    0,
                    -3δ*(1-1/√T(3))/8,
                    -3δ*(1-1/√T(3))/8,
                    -δ*(3-2*√T(3)+1/√T(3))/8,
                ],
                config,
            )
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), c3),
                T[(1-1/√T(3))/2, 1/√T(6), (1+1/√T(3))/2],
                config,
            )
            # Dual of c4 could be anything
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_PositiveSemidefiniteConeTriangle),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    η = T(10)
    α = T(4 // 5)
    δ = T(9 // 10)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[2η/3, 0, η/3, 0, 0, 0, η*δ*(1-1/√T(3))/2],
            (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}) =>
                T[δ*(1-1/√T(3))/2],
            (MOI.VectorAffineFunction{T}, MOI.Nonpositives) => [
                T[
                    0,
                    -α/√T(3)+δ/(2*√T(6))*(2*√T(2)-1),
                    0,
                    -3δ*(1-1/√T(3))/8,
                    -3δ*(1-1/√T(3))/8,
                    -δ*(3-2*√T(3)+1/√T(3))/8,
                ],
            ],
            (
                MOI.VectorAffineFunction{T},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [T[(1-1/√T(3))/2, 1/√T(6), (1+1/√T(3))/2]],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => T[0],
        ),
    )
    return
end

"""
min x
s.t. [x 1 1]
     [1 x 1] ⪰ 0
     [1 1 x]
"""
function _test_conic_PositiveSemidefiniteCone_helper_3(
    model::MOI.ModelLike,
    psdcone,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        psdcone,
    )

    x = MOI.add_variable(model)
    if psdcone == MOI.PositiveSemidefiniteConeTriangle
        func = MOIU.operate(vcat, T, x, T(1), x, T(1), T(1), x)
    else
        @assert psdcone == MOI.PositiveSemidefiniteConeSquare
        func =
            MOIU.operate(vcat, T, x, T(1), T(1), T(1), x, T(1), T(1), T(1), x)
    end
    c = MOI.add_constraint(model, func, psdcone(3))
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ T(1) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈
              ones(T, MOI.output_dimension(func)) atol = atol rtol = rtol
        if _supports(config, MOI.ConstraintDual)
            if psdcone == MOI.PositiveSemidefiniteConeTriangle
                @test ≈(
                    MOI.get(model, MOI.ConstraintDual(), c),
                    T[2, -1, 2, -1, -1, 2] / T(6),
                    config,
                )
            else
                @assert psdcone == MOI.PositiveSemidefiniteConeSquare
                @test ≈(
                    MOI.get(model, MOI.ConstraintDual(), c),
                    T[1, 0, 0, -1, 1, 0, -1, -1, 1] / T(3),
                    config,
                )
            end
        end
    end
    return
end

"""
    test_conic_PositiveSemidefiniteConeTriangle_3(
        model::MOI.ModelLike,
        config::Config,
    )
"""
function test_conic_PositiveSemidefiniteConeTriangle_3(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_PositiveSemidefiniteCone_helper_3(
        model,
        MOI.PositiveSemidefiniteConeTriangle,
        config,
    )
    return
end

function setup_test(
    ::typeof(test_conic_PositiveSemidefiniteConeTriangle_3),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(T, 1),
            (
                MOI.VectorAffineFunction{T},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [T[2, -1, 2, -1, -1, 2] / T(6)],
        ),
    )
    return
end

"""
    test_conic_PositiveSemidefiniteConeSquare_3(
        model::MOI.ModelLike,
        config::Config,
    )
"""
function test_conic_PositiveSemidefiniteConeSquare_3(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_conic_PositiveSemidefiniteCone_helper_3(
        model,
        MOI.PositiveSemidefiniteConeSquare,
        config,
    )
    return
end

function setup_test(
    ::typeof(test_conic_PositiveSemidefiniteConeSquare_3),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(T, 1),
            (MOI.VectorAffineFunction{T}, MOI.PositiveSemidefiniteConeSquare) => [T[1, 0, 0, -1, 1, 0, -1, -1, 1] / 3],
        ),
    )
    return
end

"""
    _test_det_cone_helper_ellipsoid(
        model::MOI.ModelLike,
        config::Config,
        use_VectorOfVariables::Bool,
        detcone,
    )

We look for an ellipsoid x^T P x ≤ 1 contained in the square.
Let Q = inv(P) (x^T Q x ≤ 1 is its polar ellipsoid), we have
```
max t
    t <= log det Q (or t <= (det Q)^(1/n))
            Q22 ≤ 1
           _________
          |         |
          |         |
-Q11 ≥ -1 |    +    | Q11 ≤ 1
          |         |
          |_________|
           -Q22 ≥ -1
```
"""
function _test_det_cone_helper_ellipsoid(
    model::MOI.ModelLike,
    config::Config{T},
    use_VectorOfVariables::Bool,
    detcone,
) where {T}
    F =
        use_VectorOfVariables ? MOI.VectorOfVariables :
        MOI.VectorAffineFunction{T}
    @requires MOI.supports_constraint(model, F, detcone)
    square = detcone == MOI.LogDetConeSquare || detcone == MOI.RootDetConeSquare
    use_logdet =
        detcone == MOI.LogDetConeTriangle || detcone == MOI.LogDetConeSquare
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_logdet
        @requires MOI.supports_constraint(
            model,
            MOI.VariableIndex,
            MOI.EqualTo{T},
        )
    end
    if use_VectorOfVariables
        @requires MOI.supports_constraint(model, MOI.VectorOfVariables, detcone)
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{T},
            detcone,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Nonnegatives,
    )
    t = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    Q = MOI.add_variables(model, square ? 4 : 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == (square ? 5 : 4)
    if use_logdet
        u = MOI.add_variable(model)
        vc = MOI.add_constraint(model, u, MOI.EqualTo(T(1)))
        @test vc.value == u.value
        vov = MOI.VectorOfVariables([t; u; Q])
    else
        vov = MOI.VectorOfVariables([t; Q])
    end
    if use_VectorOfVariables
        cX = MOI.add_constraint(model, vov, detcone(2))
    else
        cX = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{T}(vov),
            detcone(2),
        )
    end
    c = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                1:2,
                MOI.ScalarAffineTerm.(T[-1, -1], [Q[1], Q[end]]),
            ),
            ones(T, 2),
        ),
        MOI.Nonnegatives(2),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{T},
                detcone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{T},
                MOI.Nonnegatives,
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), t)], T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        expectedobjval = use_logdet ? 0 : 1
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), expectedobjval, config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), t), expectedobjval, config)
        if use_logdet
            @test ≈(MOI.get(model, MOI.VariablePrimal(), u), 1, config)
        end
        Qv = MOI.get(model, MOI.VariablePrimal(), Q)
        @test ≈(Qv[1], 1, config)
        @test ≈(Qv[2], 0, config)
        if square
            @test ≈(Qv[3], 0, config)
        end
        @test ≈(Qv[end], 1, config)
        tQv = MOI.get(model, MOI.ConstraintPrimal(), cX)
        @test ≈(tQv[1], expectedobjval, config)
        @test ≈(tQv[(use_logdet ? 3 : 2):end], Qv, config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c), T[0, 0], config)
        if use_logdet
            @test ≈(MOI.get(model, MOI.ConstraintPrimal(), vc), 1, config)
        end
        if _supports(config, MOI.ConstraintDual)
            if use_logdet
                @test ≈(
                    MOI.get(model, MOI.ConstraintDual(), c),
                    T[1, 1],
                    config,
                )
                @test ≈(MOI.get(model, MOI.ConstraintDual(), vc), 2, config)
                dual = square ? T[-1, -2, 1, 0, 0, 1] : T[-1, -2, 1, 0, 1]
            else
                @test ≈(
                    MOI.get(model, MOI.ConstraintDual(), c),
                    T[1//2, 1//2],
                    config,
                )
                dual = square ? T[-1, 1//2, 0, 0, 1//2] : T[-1, 1//2, 0, 1//2]
            end
            @test ≈(MOI.get(model, MOI.ConstraintDual(), cX), dual, config)
        end
    end
    return
end

"""
    _test_det_cone_helper(model::MOI.ModelLike, config::Config, detcone)

A helper function for testing {Log,Root}DetCone{Square,Triangle}.

We find logdet or rootdet of a symmetric PSD matrix:
```
mat = |3  2  1|
      |2  2  1|
      |1  1  3|
det(mat) = 5, so:
rootdet(mat) ≈ 1.709976
logdet(mat)  ≈ 1.609438
```
"""
function _test_det_cone_helper(
    model::MOI.ModelLike,
    config::Config{T},
    detcone,
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        detcone,
    )
    square = detcone == MOI.LogDetConeSquare || detcone == MOI.RootDetConeSquare
    use_logdet =
        detcone == MOI.LogDetConeTriangle || detcone == MOI.LogDetConeSquare
    mat = T[3 2 1; 2 2 1; 1 1 3]
    matL = T[3, 2, 2, 1, 1, 3]
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        detcone,
    )
    t = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    constant_mat = square ? vec(mat) : matL
    constant_vec = use_logdet ? vcat(0, 1, constant_mat) : vcat(0, constant_mat)
    vaf = MOI.VectorAffineFunction(
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), t))],
        constant_vec,
    )
    det_constraint = MOI.add_constraint(model, vaf, detcone(3))
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorAffineFunction{T},detcone}(),
        ) == 1
    end
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        expected_objval = use_logdet ? log(T(5)) : 5^inv(T(3))
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), expected_objval, config)
        @test ≈(
            MOI.get(model, MOI.VariablePrimal(), t),
            expected_objval,
            config,
        )
        det_value = MOI.get(model, MOI.ConstraintPrimal(), det_constraint)
        @test ≈(det_value[1], expected_objval, config)
        if use_logdet
            @test ≈(det_value[2], 1, config)
        end
        @test ≈(
            det_value[(use_logdet ? 3 : 2):end],
            square ? vec(mat) : matL,
            config,
        )
        if _supports(config, MOI.ConstraintDual)
            psd_dual =
                square ? T[1, -1, 0, -1, 8//5, -1//5, 0, -1//5, 2//5] :
                T[1, -1, 8//5, 0, -1//5, 2//5]
            dual =
                use_logdet ? vcat(-1, log(T(5)) - 3, psd_dual) :
                vcat(-1, psd_dual / 3 * expected_objval)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), det_constraint),
                dual,
                config,
            )
        end
    end
    return
end

"""
    test_conic_LogDetConeTriangle_VectorOfVariables(
        model::MOI.ModelLike,
        config::Config,
    )

Test a problem with LogDetConeTriangle.
"""
function test_conic_LogDetConeTriangle_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_det_cone_helper_ellipsoid(model, config, true, MOI.LogDetConeTriangle)
    return
end

function setup_test(
    ::typeof(test_conic_LogDetConeTriangle_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[0, 1, 0, 1, 1],
            (MOI.VariableIndex, MOI.EqualTo{T}) => T[2],
            (MOI.VectorAffineFunction{T}, MOI.Nonnegatives) => [T[1, 1]],
            (MOI.VectorOfVariables, MOI.LogDetConeTriangle) =>
                [T[-1, -2, 1, 0, 1]],
        ),
    )
    flag = model.eval_variable_constraint_dual
    model.eval_variable_constraint_dual = false
    return () -> model.eval_variable_constraint_dual = flag
end

"""
    test_conic_LogDetConeTriangle_VectorAffineFunction(
        model::MOI.ModelLike,
        config::Config,
    )

Test a problem with LogDetConeTriangle.
"""
function test_conic_LogDetConeTriangle_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_det_cone_helper_ellipsoid(
        model,
        config,
        false,
        MOI.LogDetConeTriangle,
    )
    return
end

function setup_test(
    ::typeof(test_conic_LogDetConeTriangle_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[0, 1, 0, 1, 1],
            (MOI.VariableIndex, MOI.EqualTo{T}) => T[2],
            (MOI.VectorAffineFunction{T}, MOI.Nonnegatives) => [T[1, 1]],
            (MOI.VectorAffineFunction{T}, MOI.LogDetConeTriangle) =>
                [T[-1, -2, 1, 0, 1]],
        ),
    )
    return
end

"""
    test_conic_LogDetConeSquare_VectorOfVariables(
        model::MOI.ModelLike,
        config::Config,
    )

Test a problem with LogDetConeSquare.
"""
function test_conic_LogDetConeSquare_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_det_cone_helper_ellipsoid(model, config, true, MOI.LogDetConeSquare)
    return
end

function setup_test(
    ::typeof(test_conic_LogDetConeSquare_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[0, 1, 0, 0, 1, 1],
            (MOI.VariableIndex, MOI.EqualTo{T}) => T[2],
            (MOI.VectorAffineFunction{T}, MOI.Nonnegatives) => [T[1, 1]],
            (MOI.VectorOfVariables, MOI.LogDetConeSquare) =>
                [T[-1, -2, 1, 0, 0, 1]],
        ),
    )
    flag = model.eval_variable_constraint_dual
    model.eval_variable_constraint_dual = false
    return () -> model.eval_variable_constraint_dual = flag
end

"""
    test_conic_LogDetConeSquare_VectorAffineFunction(
        model::MOI.ModelLike,
        config::Config,
    )

Test a problem with LogDetConeSquare.
"""
function test_conic_LogDetConeSquare_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_det_cone_helper_ellipsoid(model, config, false, MOI.LogDetConeSquare)
    return
end

function setup_test(
    ::typeof(test_conic_LogDetConeSquare_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[0, 1, 0, 0, 1, 1],
            (MOI.VariableIndex, MOI.EqualTo{T}) => T[2],
            (MOI.VectorAffineFunction{T}, MOI.Nonnegatives) => [T[1, 1]],
            (MOI.VectorAffineFunction{T}, MOI.LogDetConeSquare) =>
                [T[-1, -2, 1, 0, 0, 1]],
        ),
    )
    return
end

"""
    test_conic_RootDetConeTriangle_VectorOfVariables(
        model::MOI.ModelLike,
        config::Config,
    )

Test a problem with RootDetConeTriangle.
"""
function test_conic_RootDetConeTriangle_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_det_cone_helper_ellipsoid(
        model,
        config,
        true,
        MOI.RootDetConeTriangle,
    )
    return
end

function setup_test(
    ::typeof(test_conic_RootDetConeTriangle_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 1, 0, 1],
            (MOI.VectorAffineFunction{T}, MOI.Nonnegatives) =>
                [T[1//2, 1//2]],
            (MOI.VectorOfVariables, MOI.RootDetConeTriangle) =>
                [T[-1, 1//2, 0, 1//2]],
        ),
    )
    return
end

"""
    test_conic_RootDetConeTriangle_VectorAffineFunction(
        model::MOI.ModelLike,
        config::Config,
    )

Test a problem with RootDetConeTriangle.
"""
function test_conic_RootDetConeTriangle_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_det_cone_helper_ellipsoid(
        model,
        config,
        false,
        MOI.RootDetConeTriangle,
    )
    return
end

function setup_test(
    ::typeof(test_conic_RootDetConeTriangle_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 1, 0, 1],
            (MOI.VectorAffineFunction{T}, MOI.Nonnegatives) =>
                [T[1//2, 1//2]],
            (MOI.VectorAffineFunction{T}, MOI.RootDetConeTriangle) =>
                [T[-1, 1//2, 0, 1//2]],
        ),
    )
    return
end

"""
    test_conic_RootDetConeSquare_VectorOfVariables(
        model::MOI.ModelLike,
        config::Config,
    )

Test a problem with RootDetConeSquare.
"""
function test_conic_RootDetConeSquare_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_det_cone_helper_ellipsoid(model, config, true, MOI.RootDetConeSquare)
    return
end

function setup_test(
    ::typeof(test_conic_RootDetConeSquare_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 1, 0, 0, 1],
            (MOI.VectorAffineFunction{T}, MOI.Nonnegatives) =>
                [T[1//2, 1//2]],
            (MOI.VectorOfVariables, MOI.RootDetConeSquare) =>
                [T[-1, 1//2, 0, 0, 1//2]],
        ),
    )
    return
end

"""
    test_conic_RootDetConeSquare_VectorAffineFunction(
        model::MOI.ModelLike,
        config::Config,
    )

Test a problem with RootDetConeSquare.
"""
function test_conic_RootDetConeSquare_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_det_cone_helper_ellipsoid(model, config, false, MOI.RootDetConeSquare)
    return
end

function setup_test(
    ::typeof(test_conic_RootDetConeSquare_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 1, 0, 0, 1],
            (MOI.VectorAffineFunction{T}, MOI.Nonnegatives) =>
                [T[1//2, 1//2]],
            (MOI.VectorAffineFunction{T}, MOI.RootDetConeSquare) =>
                [T[-1, 1//2, 0, 0, 1//2]],
        ),
    )
    return
end

"""
    test_conic_LogDetConeTriangle(model::MOI.ModelLike, config::Config{T}) where {T}

Test a problem with LogDetConeTriangle.
"""
function test_conic_LogDetConeTriangle(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_det_cone_helper(model, config, MOI.LogDetConeTriangle)
    return
end

function setup_test(
    ::typeof(test_conic_LogDetConeTriangle),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [log(T(5))],
            (MOI.VectorAffineFunction{T}, MOI.LogDetConeTriangle) =>
                [T[-1, log(T(5))-3, 1, -1, 8//5, 0, -1//5, 2//5]],
        ),
    )
    flag = model.eval_variable_constraint_dual
    model.eval_variable_constraint_dual = false
    return () -> model.eval_variable_constraint_dual = flag
end

"""
    test_conic_LogDetConeSquare(model::MOI.ModelLike, config::Config{T}) where {T}

Test a problem with LogDetConeSquare.
"""
function test_conic_LogDetConeSquare(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_det_cone_helper(model, config, MOI.LogDetConeSquare)
    return
end

function setup_test(
    ::typeof(test_conic_LogDetConeSquare),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [log(T(5))],
            (MOI.VectorAffineFunction{T}, MOI.LogDetConeSquare) => [
                T[-1, log(T(5))-3, 1, -1, 0, -1, 8//5, -1//5, 0, -1//5, 2//5],
            ],
        ),
    )
    return
end

"""
    test_conic_RootDetConeTriangle(model::MOI.ModelLike, config::Config{T}) where {T}

Test a problem with RootDetConeTriangle.
"""
function test_conic_RootDetConeTriangle(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_det_cone_helper(model, config, MOI.RootDetConeTriangle)
    return
end

function setup_test(
    ::typeof(test_conic_RootDetConeTriangle),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [5^inv(T(3))],
            (MOI.VectorAffineFunction{T}, MOI.RootDetConeTriangle) => [
                vcat(-1, T[1, -1, 8//5, 0, -1//5, 2//5] / 3 * (5^inv(T(3)))),
            ],
        ),
    )
    flag = model.eval_variable_constraint_dual
    model.eval_variable_constraint_dual = false
    return () -> model.eval_variable_constraint_dual = flag
end

"""
    test_conic_RootDetConeSquare(model::MOI.ModelLike, config::Config{T}) where {T}

Test a problem with RootDetConeSquare.
"""
function test_conic_RootDetConeSquare(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_det_cone_helper(model, config, MOI.RootDetConeSquare)
    return
end

function setup_test(
    ::typeof(test_conic_RootDetConeSquare),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [5^inv(T(3))],
            (MOI.VectorAffineFunction{T}, MOI.RootDetConeSquare) => [
                vcat(
                    -1,
                    T[1, -1, 0, -1, 8//5, -1//5, 0, -1//5, 2//5] / 3 *
                    (5^inv(T(3))),
                ),
            ],
        ),
    )
    return
end

"""
    test_conic_SecondOrderCone_no_initial_bound(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test a second order cone with no bound on the epigraph variable.
"""
function test_conic_SecondOrderCone_no_initial_bound(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires(
        MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.SecondOrderCone,
        ),
    )
    @requires _supports(config, MOI.optimize!)
    t = MOI.add_variable(model)
    x = MOI.add_variables(model, 2)
    MOI.add_constraints(model, x, MOI.GreaterThan.(T[3, 4]))
    c_soc = MOI.add_constraint(
        model,
        MOI.VectorOfVariables([t; x]),
        MOI.SecondOrderCone(3),
    )
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.optimize!(model)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), t), T(5), config)
    MOI.delete(model, c_soc)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.DUAL_INFEASIBLE
    return
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_no_initial_bound),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[5, 3, 4]),
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.DUAL_INFEASIBLE),
    )
    return
end

"""
    test_conic_SecondOrderCone_nonnegative_initial_bound(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test a second order cone with a non-negative epigraph variable.
"""
function test_conic_SecondOrderCone_nonnegative_initial_bound(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires(
        MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.SecondOrderCone,
        ),
    )
    @requires _supports(config, MOI.optimize!)
    t = MOI.add_variable(model)
    x = MOI.add_variables(model, 2)
    MOI.add_constraint(model, t, MOI.GreaterThan(T(1)))
    MOI.add_constraints(model, x, MOI.GreaterThan.(T[3, 4]))
    c_soc = MOI.add_constraint(
        model,
        MOI.VectorOfVariables([t; x]),
        MOI.SecondOrderCone(3),
    )
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.optimize!(model)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), t), T(5), config)
    MOI.delete(model, c_soc)
    MOI.optimize!(model)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), t), T(1), config)
    return
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_nonnegative_initial_bound),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[5, 3, 4]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[1, 3, 4]),
    )
    return
end

"""
    test_conic_SecondOrderCone_negative_initial_bound(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test a second order cone with an epigraph variable >= -M.
"""
function test_conic_SecondOrderCone_negative_initial_bound(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires(
        MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.SecondOrderCone,
        ),
    )
    @requires _supports(config, MOI.optimize!)
    t = MOI.add_variable(model)
    x = MOI.add_variables(model, 2)
    MOI.add_constraint(model, t, MOI.GreaterThan(T(-1)))
    MOI.add_constraints(model, x, MOI.GreaterThan.(T[3, 4]))
    c_soc = MOI.add_constraint(
        model,
        MOI.VectorOfVariables([t; x]),
        MOI.SecondOrderCone(3),
    )
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.optimize!(model)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), t), T(5), config)
    MOI.delete(model, c_soc)
    MOI.optimize!(model)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), t), T(-1), config)
    return
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_negative_initial_bound),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[5, 3, 4]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[-1, 3, 4]),
    )
    return
end

"""
    test_conic_SecondOrderCone_nonnegative_post_bound(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test a second order cone with a nonnegative epigraph variable.
"""
function test_conic_SecondOrderCone_nonnegative_post_bound(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires(
        MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.SecondOrderCone,
        ),
    )
    @requires _supports(config, MOI.optimize!)
    t = MOI.add_variable(model)
    x = MOI.add_variables(model, 2)
    MOI.add_constraints(model, x, MOI.GreaterThan.(T[3, 4]))
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables([t; x]),
        MOI.SecondOrderCone(3),
    )
    c_lb = MOI.add_constraint(model, t, MOI.GreaterThan(T(6)))
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.optimize!(model)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), t), T(6), config)
    MOI.delete(model, c_lb)
    MOI.optimize!(model)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), t), T(5), config)
    return
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_nonnegative_post_bound),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[6, 3, 4]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[5, 3, 4]),
    )
    return
end

"""
    test_conic_SecondOrderCone_negative_post_bound(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test a second order cone with an epigraph variable >= -M.
"""
function test_conic_SecondOrderCone_negative_post_bound(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires(
        MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.SecondOrderCone,
        ),
    )
    @requires _supports(config, MOI.optimize!)
    t = MOI.add_variable(model)
    x = MOI.add_variables(model, 2)
    MOI.add_constraints(model, x, MOI.GreaterThan.(T[3, 4]))
    c_soc = MOI.add_constraint(
        model,
        MOI.VectorOfVariables([t; x]),
        MOI.SecondOrderCone(3),
    )
    MOI.add_constraint(model, t, MOI.GreaterThan(T(-6)))
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.optimize!(model)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), t), T(5), config)
    MOI.delete(model, c_soc)
    MOI.optimize!(model)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), t), T(-6), config)
    return
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_negative_post_bound),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[5, 3, 4]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[-6, 3, 4]),
    )
    return
end

"""
    test_conic_SecondOrderCone_negative_post_bound_2(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test a second order cosnstraint with an epigraph variable >= -M and the bound
constraints added via `add_constraint`.
"""
function test_conic_SecondOrderCone_negative_post_bound_2(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires(
        MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.SecondOrderCone,
        ),
    )
    @requires _supports(config, MOI.optimize!)
    t = MOI.add_variable(model)
    x = MOI.add_variables(model, 2)
    MOI.add_constraints(model, x, MOI.GreaterThan.(T[3, 4]))
    c_soc = MOI.add_constraint(
        model,
        MOI.VectorOfVariables([t; x]),
        MOI.SecondOrderCone(3),
    )
    c_lb = MOI.add_constraint(model, t, MOI.GreaterThan(T(-6)))
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.optimize!(model)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), t), T(5), config)
    MOI.delete(model, c_lb)
    MOI.optimize!(model)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), t), T(5), config)
    MOI.delete(model, c_soc)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.DUAL_INFEASIBLE
    return
end

function version_added(
    ::typeof(test_conic_SecondOrderCone_negative_post_bound_2),
)
    return v"0.10.6"
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_negative_post_bound_2),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[5, 3, 4]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[5, 3, 4]),
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.DUAL_INFEASIBLE),
    )
    return
end

"""
    test_conic_SecondOrderCone_negative_post_bound_3(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test a second order cosnstraint with an epigraph variable >= -M and the bound
constraints added via `add_constraints`.
"""
function test_conic_SecondOrderCone_negative_post_bound_3(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires(
        MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.SecondOrderCone,
        ),
    )
    @requires _supports(config, MOI.optimize!)
    t = MOI.add_variable(model)
    x = MOI.add_variables(model, 2)
    c_soc = MOI.add_constraint(
        model,
        MOI.VectorOfVariables([t; x]),
        MOI.SecondOrderCone(3),
    )
    c_lbs = MOI.add_constraints(model, [t; x], MOI.GreaterThan.(T[-6, 3, 4]))
    c_lb = first(c_lbs)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.optimize!(model)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), t), T(5), config)
    MOI.delete(model, c_lb)
    MOI.optimize!(model)
    @test isapprox(MOI.get(model, MOI.VariablePrimal(), t), T(5), config)
    MOI.delete(model, c_soc)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.DUAL_INFEASIBLE
    return
end

function version_added(
    ::typeof(test_conic_SecondOrderCone_negative_post_bound_3),
)
    return v"0.10.6"
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_negative_post_bound_3),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[5, 3, 4]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[5, 3, 4]),
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.DUAL_INFEASIBLE),
    )
    return
end
