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
    config::Config,
    use_VectorOfVariables::Bool,
)
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.NonnegativeCone,
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.NonnegativeCone,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.ZeroCone,
    )
    v = MOI.add_variables(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3
    vov = MOI.VectorOfVariables(v)
    if use_VectorOfVariables
        vc = MOI.add_constraint(model, vov, MOI.NonnegativeCone(3))
    else
        vc = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{Float64}(vov),
            MOI.NonnegativeCone(3),
        )
    end
    c = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 1, 1, 2, 2],
                MOI.ScalarAffineTerm.(1.0, [v; v[2]; v[3]]),
            ),
            [-3.0, -2.0],
        ),
        MOI.ZeroCone(2),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                MOI.NonnegativeCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.ZeroCone,
            }(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (
        use_VectorOfVariables ? MOI.VectorOfVariables :
        MOI.VectorAffineFunction{Float64},
        MOI.NonnegativeCone,
    ) in loc
    @test (MOI.VectorAffineFunction{Float64}, MOI.ZeroCone) in loc
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([-3.0, -2.0, -4.0], v),
            0.0,
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
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -11 atol = atol rtol = rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ -11 atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1, 0, 2] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), vc) ≈ [1, 0, 2] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ zeros(2) atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ConstraintDual(), vc) ≈ [0, 2, 0] atol =
                atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ [-3, -1] atol = atol rtol =
                rtol
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
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 0.0, 2.0],
            (MOI.VectorAffineFunction{Float64}, MOI.ZeroCone) => [[-3, -1]],
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
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 0.0, 2.0],
            (MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone) =>
                [[0, 2, 0]],
            (MOI.VectorAffineFunction{Float64}, MOI.ZeroCone) => [[-3, -1]],
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
    config::Config,
    use_VectorOfVariables::Bool,
)
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.NonnegativeCone,
        )
        @requires MOI.supports_add_constrained_variables(
            model,
            MOI.NonpositiveCone,
        )
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.ZeroCone,
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.NonnegativeCone,
        )
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.NonpositiveCone,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.ZeroCone,
    )
    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    if use_VectorOfVariables
        ys, vc = MOI.add_constrained_variables(model, MOI.NonpositiveCone(1))
        y = ys[1]
    else
        y = MOI.add_variable(model)
        func = MOI.VectorAffineFunction{Float64}(MOI.VectorOfVariables([y]))
        vc = MOI.add_constraint(model, func, MOI.NonpositiveCone(1))
    end
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    z, s = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 4
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([3.0, 2.0, -4.0], [x, y, z]),
            0.0,
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    c = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 1, 2, 3, 3],
                MOI.ScalarAffineTerm.(
                    [1.0, -1.0, 1.0, 1.0, 1.0],
                    [x, s, y, x, z],
                ),
            ),
            [4.0, 3.0, -12.0],
        ),
        MOI.ZeroCone(3),
    )
    if use_VectorOfVariables
        # test fallback
        vz = MOI.add_constraint(model, [z], MOI.NonnegativeCone(1))
    else
        vz = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction(
                [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z))],
                [0.0],
            ),
            MOI.NonnegativeCone(1),
        )
    end
    vov = MOI.VectorOfVariables([s])
    if use_VectorOfVariables
        vs = MOI.add_constraint(model, vov, MOI.ZeroCone(1))
    else
        vs = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{Float64}(vov),
            MOI.ZeroCone(1),
        )
    end
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.ZeroCone,
            }(),
        ) == 2 - use_VectorOfVariables
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                MOI.NonpositiveCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                MOI.NonnegativeCone,
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
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -82 atol = atol rtol = rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ -82 atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ -4 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ -3 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), z) ≈ 16 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), s) ≈ 0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ zeros(3) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), vc) ≈ [-3] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), vz) ≈ [16] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), vs) ≈ [0] atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ [7, 2, -4] atol =
                atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc) ≈ [0] atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), vz) ≈ [0] atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), vs) ≈ [7] atol = atol rtol =
                rtol
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
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [-4, -3, 16, 0],
            (MOI.VectorAffineFunction{Float64}, MOI.ZeroCone) => [[7, 2, -4]],
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
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [-4, -3, 16, 0],
            (MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone) => [[0]],
            (MOI.VectorAffineFunction{Float64}, MOI.NonpositiveCone) => [[0]],
            (MOI.VectorAffineFunction{Float64}, MOI.ZeroCone) =>
                [[7, 2, -4], [7]],
        ),
    )
    return
end

"""
    test_conic_linear_INFEASIBLE(model::MOI.ModelLike, config::Config)

Test an infeasible linear program in conic form.

The problem is:
```
min 0
s.t. -1 + x ∈ R₊
      1 + x ∈ R₋
```
"""
function test_conic_linear_INFEASIBLE(model::MOI.ModelLike, config::Config)
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NonpositiveCone,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NonnegativeCone,
    )
    x = MOI.add_variable(model)
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
            [-1.0],
        ),
        MOI.NonnegativeCone(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
            [1.0],
        ),
        MOI.NonpositiveCone(1),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.NonnegativeCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.NonpositiveCone,
            }(),
        ) == 1
    end
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) in
              [MOI.INFEASIBLE, MOI.INFEASIBLE_OR_UNBOUNDED]
        # TODO test dual feasibility and objective sign
    end
    return
end

function setup_test(
    ::typeof(test_conic_linear_INFEASIBLE),
    model::MOIU.MockOptimizer,
    ::Config,
)
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
    test_conic_linear_INFEASIBLE_2(model::MOI.ModelLike, config::Config)

Test an infeasible linear program in conic form.

The problem is:
```
min 0
s.t. -1 + x ∈ R₊
          x ∈ R₋
```
"""
function test_conic_linear_INFEASIBLE_2(model::MOI.ModelLike, config::Config)
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NonnegativeCone,
    )
    @requires MOI.supports_add_constrained_variables(model, MOI.NonpositiveCone)

    xs, cx = MOI.add_constrained_variables(model, MOI.NonpositiveCone(1))
    x = xs[1]
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
            [-1.0],
        ),
        MOI.NonnegativeCone(1),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.NonnegativeCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.NonpositiveCone}(),
        ) == 1
    end
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) in
              [MOI.INFEASIBLE, MOI.INFEASIBLE_OR_UNBOUNDED]
        # TODO test dual feasibility and objective sign
    end
    return
end

function setup_test(
    ::typeof(test_conic_linear_INFEASIBLE_2),
    model::MOIU.MockOptimizer,
    ::Config,
)
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
    config::Config,
    use_VectorOfVariables::Bool,
)
    F = if use_VectorOfVariables
        MOI.VectorOfVariables
    else
        MOI.VectorAffineFunction{Float64}
    end
    @requires MOI.supports_constraint(model, F, MOI.NormInfinityCone)
    atol = config.atol
    rtol = config.rtol
    # Problem NormInf1
    # max 0x + 1y + 1z
    #  st  x == 1
    #      y == 1/2
    #      x >= ||(y,z)||_∞
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.ZeroCone,
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.ZeroCone,
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
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 1.0], [y, z]),
            0.0,
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    ceq1 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
            [-1.0],
        ),
        MOI.ZeroCone(1),
    )
    ceq2 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, y))],
            [-0.5],
        ),
        MOI.ZeroCone(1),
    )
    vov = MOI.VectorOfVariables([x, y, z])
    if use_VectorOfVariables
        ccone = MOI.add_constraint(model, vov, MOI.NormInfinityCone(3))
    else
        ccone = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{Float64}(vov),
            MOI.NormInfinityCone(3),
        )
    end
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.ZeroCone,
            }(),
        ) == 2
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                MOI.NormInfinityCone,
            }(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{Float64}, MOI.ZeroCone) in loc
    @test (
        use_VectorOfVariables ? MOI.VectorOfVariables :
        MOI.VectorAffineFunction{Float64},
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
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1.5 atol = atol rtol = rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ 1.5 atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 1 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 0.5 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), z) ≈ 1 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), ceq1) ≈ [0] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), ceq2) ≈ [0] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), ccone) ≈ [1.0, 0.5, 1.0] atol =
            atol rtol = rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ConstraintDual(), ceq1) ≈ [-1] atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), ceq2) ≈ [-1] atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), ccone) ≈ [1.0, 0.0, -1.0] atol =
                atol rtol = rtol
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
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1, 0.5, 1],
            (MOI.VectorAffineFunction{Float64}, MOI.ZeroCone) => [[-1], [-1]],
            (MOI.VectorOfVariables, MOI.NormInfinityCone) => [[1, 0, -1]],
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
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1, 0.5, 1],
            (MOI.VectorAffineFunction{Float64}, MOI.ZeroCone) => [[-1], [-1]],
            (MOI.VectorAffineFunction{Float64}, MOI.NormInfinityCone) =>
                [[1, 0, -1]],
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
    config::Config,
)
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NonnegativeCone,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NonpositiveCone,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NormInfinityCone,
    )

    x, y = MOI.add_variables(model, 2)
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, y))],
            [-2.0],
        ),
        MOI.NonnegativeCone(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
            [-1.0],
        ),
        MOI.NonpositiveCone(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.(1.0, [x, y])),
            zeros(2),
        ),
        MOI.NormInfinityCone(2),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.NonnegativeCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.NonpositiveCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.NormInfinityCone,
            }(),
        ) == 1
    end
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
        @test MOI.get(model, MOI.PrimalStatus()) in
              (MOI.NO_SOLUTION, MOI.INFEASIBLE_POINT)
        # TODO test dual feasibility and objective sign
    end
    return
end

function setup_test(
    ::typeof(test_conic_NormInfinityCone_INFEASIBLE),
    model::MOIU.MockOptimizer,
    ::Config,
)
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
    test_conic_NormInfinityCone_3(model::MOI.ModelLike, config::Config)

Test the problem:
```
min x
 st  (-1 + x, 2 .+ y) in NormInf(1 + n)
     (1 .+ y) in NonnegativeCone(n)
let n = 3. optimal solution: y .= -1, x = 2
```
"""
function test_conic_NormInfinityCone_3(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.SingleVariable}())
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NormInfinityCone,
    )

    x = MOI.add_variable(model)
    y = MOI.add_variables(model, 3)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    norminf_vaf = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(1:4, MOI.ScalarAffineTerm.(1.0, vcat(x, y))),
        [-1.0, 2, 2, 2],
    )
    norminf = MOI.add_constraint(model, norminf_vaf, MOI.NormInfinityCone(4))
    nonneg_vaf = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(1:3, MOI.ScalarAffineTerm.(1.0, y)),
        ones(3),
    )
    nonneg = MOI.add_constraint(model, nonneg_vaf, MOI.NonnegativeCone(3))
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.NormInfinityCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.NonnegativeCone,
            }(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{Float64}, MOI.NormInfinityCone) in loc
    @test (MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone) in loc
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
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 2 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ fill(-1.0, 3) atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), norminf) ≈ ones(4) atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), nonneg) ≈ zeros(3) atol =
            atol rtol = rtol
        if _supports(config, MOI.ConstraintDual)
            dual_nonneg = MOI.get(model, MOI.ConstraintDual(), nonneg)
            @test minimum(dual_nonneg) >= -atol
            @test sum(dual_nonneg) ≈ 1.0 atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), norminf) ≈
                  vcat(1, -dual_nonneg) atol = atol rtol = rtol
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_NormInfinityCone_3),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [2, -1, -1, -1],
            (MOI.VectorAffineFunction{Float64}, MOI.NormInfinityCone) =>
                [vcat(1, fill(-inv(3), 3))],
            (MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone) =>
                [fill(inv(3), 3)],
        ),
    )
    return
end

function _test_conic_NormOneCone_helper(
    model::MOI.ModelLike,
    config::Config,
    use_VectorOfVariables::Bool,
)
    atol = config.atol
    rtol = config.rtol
    # Problem NormOne1
    # max 0x + 1y + 1z
    #  st  x == 1
    #      y == 1/2
    #      x >= ||(y,z)||_1
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.ZeroCone,
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.ZeroCone,
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
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 1.0], [y, z]),
            0.0,
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    ceq1 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
            [-1.0],
        ),
        MOI.ZeroCone(1),
    )
    ceq2 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, y))],
            [-0.5],
        ),
        MOI.ZeroCone(1),
    )
    vov = MOI.VectorOfVariables([x, y, z])
    if use_VectorOfVariables
        ccone = MOI.add_constraint(model, vov, MOI.NormOneCone(3))
    else
        ccone = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{Float64}(vov),
            MOI.NormOneCone(3),
        )
    end
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.ZeroCone,
            }(),
        ) == 2
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                MOI.NormOneCone,
            }(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{Float64}, MOI.ZeroCone) in loc
    @test (
        use_VectorOfVariables ? MOI.VectorOfVariables :
        MOI.VectorAffineFunction{Float64},
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
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1 atol = atol rtol = rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ 1 atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 1 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 0.5 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), z) ≈ 0.5 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), ceq1) ≈ [0] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), ceq2) ≈ [0] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), ccone) ≈ [1.0, 0.5, 0.5] atol =
            atol rtol = rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ConstraintDual(), ceq1) ≈ [-1] atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), ceq2) ≈ [0] atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), ccone) ≈
                  [1.0, -1.0, -1.0] atol = atol rtol = rtol
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
    config::Config,
)
    return _test_conic_NormOneCone_helper(model, config, true)
end

function setup_test(
    ::typeof(test_conic_NormOneCone_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1, 0.5, 0.5],
            (MOI.VectorAffineFunction{Float64}, MOI.ZeroCone) => [[-1], [0]],
            (MOI.VectorOfVariables, MOI.NormOneCone) => [[1, -1, -1]],
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
    config::Config,
)
    return _test_conic_NormOneCone_helper(model, config, false)
end

function setup_test(
    ::typeof(test_conic_NormOneCone_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1, 0.5, 0.5],
            (MOI.VectorAffineFunction{Float64}, MOI.ZeroCone) => [[-1], [0]],
            (MOI.VectorAffineFunction{Float64}, MOI.NormOneCone) =>
                [[1, -1, -1]],
        ),
    )
    return
end

"""
    test_conic_NormOneCone_INFEASIBLE(model::MOI.ModelLike, config::Config)

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
function test_conic_NormOneCone_INFEASIBLE(model::MOI.ModelLike, config::Config)
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NonnegativeCone,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NonpositiveCone,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NormOneCone,
    )

    x, y = MOI.add_variables(model, 2)
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, y))],
            [-2.0],
        ),
        MOI.NonnegativeCone(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
            [-1.0],
        ),
        MOI.NonpositiveCone(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.(1.0, [x, y])),
            zeros(2),
        ),
        MOI.NormOneCone(2),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.NonnegativeCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.NonpositiveCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.NormOneCone,
            }(),
        ) == 1
    end
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
        @test MOI.get(model, MOI.PrimalStatus()) in
              (MOI.NO_SOLUTION, MOI.INFEASIBLE_POINT)
        # TODO test dual feasibility and objective sign
    end
    return
end

function setup_test(
    ::typeof(test_conic_NormOneCone_INFEASIBLE),
    model::MOIU.MockOptimizer,
    ::Config,
)
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
    test_conic_NormOneCone(model::MOI.ModelLike, config::Config)

Test the following problem:
```
min x
 st  (-1 + x, 2 .+ y) in NormOne(1 + n)
     (1 .+ y) in NonnegativeCone(n)
let n = 3. optimal solution: y .= -1, x = 4
```
"""
function test_conic_NormOneCone(model::MOI.ModelLike, config::Config)
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NormOneCone,
    )
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.SingleVariable}())
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NormOneCone,
    )

    x = MOI.add_variable(model)
    y = MOI.add_variables(model, 3)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    norminf_vaf = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(1:4, MOI.ScalarAffineTerm.(1.0, vcat(x, y))),
        [-1.0, 2, 2, 2],
    )
    norminf = MOI.add_constraint(model, norminf_vaf, MOI.NormOneCone(4))
    nonneg_vaf = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(1:3, MOI.ScalarAffineTerm.(1.0, y)),
        ones(3),
    )
    nonneg = MOI.add_constraint(model, nonneg_vaf, MOI.NonnegativeCone(3))
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.NormOneCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.NonnegativeCone,
            }(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{Float64}, MOI.NormOneCone) in loc
    @test (MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone) in loc
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 4 atol = atol rtol = rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ 4 atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 4 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ fill(-1.0, 3) atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), norminf) ≈ vcat(3, ones(3)) atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), nonneg) ≈ zeros(3) atol =
            atol rtol = rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ConstraintDual(), norminf) ≈
                  vcat(1, fill(-1, 3)) atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), nonneg) ≈ ones(3) atol =
                atol rtol = rtol
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_NormOneCone),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [4, -1, -1, -1],
            (MOI.VectorAffineFunction{Float64}, MOI.NormOneCone) =>
                [vcat(1, fill(-1, 3))],
            (MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone) =>
                [ones(3)],
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
    config::Config,
    use_VectorOfVariables::Bool,
)
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.SecondOrderCone,
    )
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
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
            MOI.VectorAffineFunction{Float64},
            MOI.SecondOrderCone,
        )
    end
    @requires MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.ZeroCone)

    if use_VectorOfVariables
        xyz, csoc = MOI.add_constrained_variables(model, MOI.SecondOrderCone(3))
        x, y, z = xyz
    else
        x, y, z = MOI.add_variables(model, 3)
        vov = MOI.VectorOfVariables([x, y, z])
        csoc = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{Float64}(vov),
            MOI.SecondOrderCone(3),
        )
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 1.0], [y, z]),
            0.0,
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    ceq = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
            [-1.0],
        ),
        MOI.ZeroCone(1),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.ZeroCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                MOI.SecondOrderCone,
            }(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{Float64}, MOI.ZeroCone) in loc
    @test (
        use_VectorOfVariables ? MOI.VectorOfVariables :
        MOI.VectorAffineFunction{Float64},
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
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ √2 atol = atol rtol = rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ √2 atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 1 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 1 / √2 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), z) ≈ 1 / √2 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), ceq) ≈ [0.0] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), csoc) ≈
              [1.0, 1 / √2, 1 / √2] atol = atol rtol = rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ConstraintDual(), ceq) ≈ [-√2] atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), csoc) ≈ [√2, -1.0, -1.0] atol =
                atol rtol = rtol
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
    config::Config,
)
    _test_conic_SecondOrderCone_helper(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 1 / √2, 1 / √2],
            (MOI.VectorAffineFunction{Float64}, MOI.ZeroCone) => [[-√2]],
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
    config::Config,
)
    _test_conic_SecondOrderCone_helper(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 1 / √2, 1 / √2],
            (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone) =>
                [[√2, -1, -1]],
            (MOI.VectorAffineFunction{Float64}, MOI.ZeroCone) => [[-√2]],
        ),
    )
    return
end

function _test_conic_SecondOrderCone_helper_2(
    model::MOI.ModelLike,
    config::Config,
    nonneg::Bool,
)
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.SecondOrderCone,
    )
    atol = config.atol
    rtol = config.rtol
    # Problem SOC2
    # min  x
    # s.t. y ≥ 1/√2
    #      x² + y² ≤ 1
    # in conic form:
    # min  x
    # s.t.  -1/√2 + y ∈ R₊
    #        1 - t ∈ {0}
    #      (t,x,y) ∈ SOC₃
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.ZeroCone,
    )
    if nonneg
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.NonnegativeCone,
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.NonpositiveCone,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.SecondOrderCone,
    )

    x, y, t = MOI.add_variables(model, 3)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if nonneg
        cnon = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction(
                [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, y))],
                [-1 / √2],
            ),
            MOI.NonnegativeCone(1),
        )
    else
        cnon = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction(
                [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(-1.0, y))],
                [1 / √2],
            ),
            MOI.NonpositiveCone(1),
        )
    end
    ceq = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(-1.0, t))],
            [1.0],
        ),
        MOI.ZeroCone(1),
    )
    csoc = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 2, 3],
                MOI.ScalarAffineTerm.(1.0, [t, x, y]),
            ),
            zeros(3),
        ),
        MOI.SecondOrderCone(3),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                nonneg ? MOI.NonnegativeCone : MOI.NonpositiveCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.ZeroCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
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
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -1 / √2 atol = atol rtol =
            rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ -1 / √2 atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ -1 / √2 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 1 / √2 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ 1 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cnon) ≈ [0.0] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), ceq) ≈ [0.0] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), csoc) ≈
              [1.0, -1 / √2, 1 / √2] atol = atol rtol = rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ConstraintDual(), cnon) ≈
                  [nonneg ? 1.0 : -1.0] atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), ceq) ≈ [√2] atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), csoc) ≈ [√2, 1.0, -1.0] atol =
                atol rtol = rtol
        end
    end
    return
end

"""
    test_conic_SecondOrderCone_Nonnegatives(
        model::MOI.ModelLike,
        config::Config,
    )

Test a SecondOrderCone with NonnegativeCone constraints.
"""
function test_conic_SecondOrderCone_Nonnegatives(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_SecondOrderCone_helper_2(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_Nonnegatives),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [-1 / √2, 1 / √2, 1.0],
            (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone) =>
                [[√2, 1, -1]],
            (MOI.VectorAffineFunction{Float64}, MOI.ZeroCone) => [[√2]],
            (MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone) =>
                [[1.0]],
        ),
    )
    return
end

"""
    test_conic_SecondOrderCone_Nonpositives(
        model::MOI.ModelLike,
        config::Config,
    )

Test a SecondOrderCone with NonpositiveCone constraints.
"""
function test_conic_SecondOrderCone_Nonpositives(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_SecondOrderCone_helper_2(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_Nonpositives),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [-1 / √2, 1 / √2, 1.0],
            (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone) =>
                [[√2, 1, -1]],
            (MOI.VectorAffineFunction{Float64}, MOI.ZeroCone) => [[√2]],
            (MOI.VectorAffineFunction{Float64}, MOI.NonpositiveCone) =>
                [[-1.0]],
        ),
    )
    return
end

"""
    test_conic_SecondOrderCone_INFEASIBLE(model::MOI.ModelLike, config::Config)

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
    config::Config,
)
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.SecondOrderCone,
    )
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NonnegativeCone,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NonpositiveCone,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.SecondOrderCone,
    )

    x, y = MOI.add_variables(model, 2)
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, y))],
            [-2.0],
        ),
        MOI.NonnegativeCone(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
            [-1.0],
        ),
        MOI.NonpositiveCone(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.(1.0, [x, y])),
            zeros(2),
        ),
        MOI.SecondOrderCone(2),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.NonnegativeCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.NonpositiveCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.SecondOrderCone,
            }(),
        ) == 1
    end
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
        @test MOI.get(model, MOI.PrimalStatus()) in
              (MOI.NO_SOLUTION, MOI.INFEASIBLE_POINT)
        # TODO test dual feasibility and objective sign
    end
    return
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_INFEASIBLE),
    model::MOIU.MockOptimizer,
    ::Config,
)
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
    config::Config,
)
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.SecondOrderCone,
    )
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.ZeroCone,
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
                MOI.ScalarAffineTerm.([1.0, 1.0, 1.0, -1.0, -1.0], x),
            ),
            [-1.0, 0.0, 0.0],
        ),
        MOI.ZeroCone(3),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.VectorOfVariables([x[1], x[4], x[5]]),
        MOI.SecondOrderCone(3),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.ZeroCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SecondOrderCone}(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([0.0, -2.0, -1.0, 0.0, 0.0], x),
            0.0,
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
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -√5 atol = atol rtol = rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ -√5 atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈
              [1.0, 2 / √5, 1 / √5, 2 / √5, 1 / √5] atol = atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c1) ≈ zeros(3) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ [1.0, 2 / √5, 1 / √5] atol =
            atol rtol = rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ [-√5, -2.0, -1.0] atol =
                atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ [√5, -2.0, -1.0] atol =
                atol rtol = rtol
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_SecondOrderCone_out_of_order),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 2 / √5, 1 / √5, 2 / √5, 1 / √5],
            (MOI.VectorAffineFunction{Float64}, MOI.ZeroCone) =>
                [[-√5, -2.0, -1.0]],
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
 st [0.5] - [      ] SOCRotated
    [1.0] - [      ] SOCRotated
    [0.0] - [-y    ] SOCRotated
    [0.0] - [    -z] SOCRotated
"""
function _test_conic_RotatedSecondOrderCone_helper(
    model::MOI.ModelLike,
    config::Config,
    abvars::Bool,
)
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if abvars
        @requires MOI.supports_constraint(
            model,
            MOI.SingleVariable,
            MOI.EqualTo{Float64},
        )
        @requires MOI.supports_add_constrained_variables(
            model,
            MOI.RotatedSecondOrderCone,
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.RotatedSecondOrderCone,
        )
    end

    if abvars
        abx, rsoc =
            MOI.add_constrained_variables(model, MOI.RotatedSecondOrderCone(4))
        a, b, x1, x2 = abx
        x = [x1, x2]
        vc1 = MOI.add_constraint(model, MOI.SingleVariable(a), MOI.EqualTo(0.5))
        # We test this after the creation of every `SingleVariable` constraint
        # to ensure a good coverage of corner cases.
        @test vc1.value == a.value
        vc2 = MOI.add_constraint(model, MOI.SingleVariable(b), MOI.EqualTo(1.0))
        @test vc2.value == b.value
    else
        x = MOI.add_variables(model, 2)
        a = 0.5
        b = 1.0
        rsoc = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction(
                MOI.VectorAffineTerm.(
                    [3, 4],
                    MOI.ScalarAffineTerm.([1.0, 1.0], x),
                ),
                [a, b, 0.0, 0.0],
            ),
            MOI.RotatedSecondOrderCone(4),
        )
    end
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.SingleVariable,MOI.EqualTo{Float64}}(),
        ) == (abvars ? 2 : 0)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                abvars ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                MOI.RotatedSecondOrderCone,
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x), 0.0),
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
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ √2 atol = atol rtol = rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ √2 atol = atol rtol =
                rtol
        end
        if abvars
            @test MOI.get(model, MOI.VariablePrimal(), a) ≈ 0.5 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.VariablePrimal(), b) ≈ 1.0 atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ [1 / √2, 1 / √2] atol =
            atol rtol = rtol
        if abvars
            @test MOI.get(model, MOI.ConstraintPrimal(), vc1) ≈ 0.5 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintPrimal(), vc2) ≈ 1.0 atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.ConstraintPrimal(), rsoc) ≈
              [0.5, 1.0, 1 / √2, 1 / √2] atol = atol rtol = rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus(1)) == MOI.FEASIBLE_POINT
            if abvars
                @test MOI.get(model, MOI.ConstraintDual(), vc1) ≈ -√2 atol =
                    atol rtol = rtol
                @test MOI.get(model, MOI.ConstraintDual(), vc2) ≈ -1 / √2 atol =
                    atol rtol = rtol
            end
            @test MOI.get(model, MOI.ConstraintDual(), rsoc) ≈
                  [√2, 1 / √2, -1.0, -1.0] atol = atol rtol = rtol
        end
    end
    return
end

function test_conic_RotatedSecondOrderCone_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_RotatedSecondOrderCone_helper(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_RotatedSecondOrderCone_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0.5, 1.0, 1 / √2, 1 / √2],
            (MOI.SingleVariable, MOI.EqualTo{Float64}) => [-√2, -1 / √2],
            (MOI.VectorOfVariables, MOI.RotatedSecondOrderCone) =>
                [[√2, 1 / √2, -1.0, -1.0]],
        ),
    )
    model.eval_variable_constraint_dual = false
    return () -> model.eval_variable_constraint_dual = true
end

function test_conic_RotatedSecondOrderCone_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_RotatedSecondOrderCone_helper(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_RotatedSecondOrderCone_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1 / √2, 1 / √2],
            (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone) => [[√2, 1 / √2, -1.0, -1.0]],
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
    config::Config,
)
    atol = config.atol
    rtol = config.rtol
    b = [-2, -1, 1 / 2]
    c = [0.0, 0.0, 0.0]
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.SingleVariable,
        MOI.EqualTo{Float64},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.SingleVariable,
        MOI.LessThan{Float64},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.SingleVariable,
        MOI.GreaterThan{Float64},
    )
    @requires MOI.supports_add_constrained_variables(
        model,
        MOI.RotatedSecondOrderCone,
    )

    x, rsoc =
        MOI.add_constrained_variables(model, MOI.RotatedSecondOrderCone(3))
    vc1 = MOI.add_constraint(model, MOI.SingleVariable(x[1]), MOI.LessThan(1.0))
    @test vc1.value == x[1].value
    vc2 = MOI.add_constraint(model, MOI.SingleVariable(x[2]), MOI.EqualTo(0.5))
    @test vc2.value == x[2].value
    vc3 = MOI.add_constraint(
        model,
        MOI.SingleVariable(x[3]),
        MOI.GreaterThan(2.0),
    )
    @test vc3.value == x[3].value
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.SingleVariable,MOI.LessThan{Float64}}(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.SingleVariable,MOI.EqualTo{Float64}}(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.SingleVariable,
                MOI.GreaterThan{Float64},
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorOfVariables,
                MOI.RotatedSecondOrderCone,
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(c, x), 0.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) in
              [MOI.INFEASIBLE, MOI.INFEASIBLE_OR_UNBOUNDED]
        has_certificate = MOI.get(model, MOI.DualStatus()) in [
            MOI.INFEASIBILITY_CERTIFICATE,
            MOI.NEARLY_INFEASIBILITY_CERTIFICATE,
        ]
        if _supports(config, MOI.ConstraintDual) && has_certificate
            y1 = MOI.get(model, MOI.ConstraintDual(), vc1)
            @test y1 < -atol # Should be strictly negative
            y2 = MOI.get(model, MOI.ConstraintDual(), vc2)
            y3 = MOI.get(model, MOI.ConstraintDual(), vc3)
            @test y3 > atol # Should be strictly positive
            y = [y1, y2, y3]
            vardual = MOI.get(model, MOI.ConstraintDual(), rsoc)
            @test vardual ≈ -y atol = atol rtol = rtol
            @test 2 * vardual[1] * vardual[2] ≥ vardual[3]^2 - atol
            @test b' * y > atol
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_RotatedSecondOrderCone_INFEASIBLE),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            tuple(),
            (MOI.SingleVariable, MOI.LessThan{Float64}) => [-1],
            (MOI.SingleVariable, MOI.EqualTo{Float64}) => [-1],
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [1],
            (MOI.VectorOfVariables, MOI.RotatedSecondOrderCone) =>
                [[1, 1, -1]],
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
     0 ≤ u ≤ 3.0
     v
     t1 == 1
     t2 == 1
[t1/√2, t2/√2, x] in SOC4
[x1/√2, u/√2,  v] in SOC3
"""
function test_conic_RotatedSecondOrderCone_INFEASIBLE_2(
    model::MOI.ModelLike,
    config::Config;
    n = 2,
    ub = 3.0,
)
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.SingleVariable,
        MOI.EqualTo{Float64},
    )
    @requires MOI.supports_add_constrained_variables(model, MOI.NonnegativeCone)
    @requires MOI.supports_constraint(
        model,
        MOI.SingleVariable,
        MOI.GreaterThan{Float64},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.SingleVariable,
        MOI.LessThan{Float64},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.RotatedSecondOrderCone,
    )

    x, cx = MOI.add_constrained_variables(model, MOI.NonnegativeCone(n))
    u = MOI.add_variable(model)
    v = MOI.add_variable(model)
    t = MOI.add_variables(model, 2)
    ct1 = MOI.add_constraint(model, MOI.SingleVariable(t[1]), MOI.EqualTo(1.0))
    @test ct1.value == t[1].value
    ct2 = MOI.add_constraint(model, MOI.SingleVariable(t[2]), MOI.EqualTo(1.0))
    @test ct2.value == t[2].value
    cu1 = MOI.add_constraint(model, MOI.SingleVariable(u), MOI.GreaterThan(0.0))
    @test cu1.value == u.value
    cu2 = MOI.add_constraint(model, MOI.SingleVariable(u), MOI.LessThan(ub))
    @test cu2.value == u.value
    c1 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                1:(2+n),
                MOI.ScalarAffineTerm.([1 / √2; 1 / √2; ones(n)], [t; x]),
            ),
            zeros(2 + n),
        ),
        MOI.RotatedSecondOrderCone(2 + n),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 2, 3],
                MOI.ScalarAffineTerm.([1 / √2; 1 / √2; 1.0], [x[1], u, v]),
            ),
            zeros(3),
        ),
        MOI.RotatedSecondOrderCone(3),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.SingleVariable,MOI.EqualTo{Float64}}(),
        ) == 2
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.NonnegativeCone}(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.SingleVariable,
                MOI.GreaterThan{Float64},
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.SingleVariable,MOI.LessThan{Float64}}(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.SingleVariable,
                MOI.GreaterThan{Float64},
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.RotatedSecondOrderCone,
            }(),
        ) == 2
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, v)], 0.0),
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
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ √ub atol = atol rtol = rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ √ub atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ [1.0; zeros(n - 1)] atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), u) ≈ ub atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ √ub atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ ones(2) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cx) ≈ [1.0; zeros(n - 1)] atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cu1) ≈ ub atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cu2) ≈ ub atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), ct1) ≈ 1.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), ct2) ≈ 1.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c1) ≈
              [1 / √2; 1 / √2; 1.0; zeros(n - 1)] atol = atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈
              [1 / √2, ub / √2, √ub] atol = atol rtol = rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ConstraintDual(), cx) ≈ zeros(n) atol =
                atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), cu1) ≈ 0.0 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), cu2) ≈ -1 / (2 * √ub) atol =
                atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), ct1) ≈ -√ub / 4 atol =
                atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), ct2) ≈ -√ub / 4 atol =
                atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈
                  [√ub / (2 * √2); √ub / (2 * √2); -√ub / 2; zeros(n - 1)] atol =
                atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈
                  [√ub / √2, 1 / √(2 * ub), -1.0] atol = atol rtol = rtol
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_RotatedSecondOrderCone_INFEASIBLE_2),
    model::MOIU.MockOptimizer,
    ::Config,
)
    n = 2
    ub = 3.0
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0; zeros(n - 1); ub; √ub; ones(2)],
            (MOI.SingleVariable, MOI.EqualTo{Float64}) =>
                [-√ub / 4, -√ub / 4],
            (MOI.VectorOfVariables, MOI.NonnegativeCone) => [zeros(n)],
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [0.0],
            (MOI.SingleVariable, MOI.LessThan{Float64}) => [-1 / (2 * √ub)],
            (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone) => [
                vcat(√ub / (2 * √2), √ub / (2 * √2), -√ub / 2, zeros(n - 1)),
                [√ub / √2, 1 / √(2 * ub), -1.0],
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
    config::Config,
)
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    @requires MOI.supports_add_constrained_variables(
        model,
        MOI.RotatedSecondOrderCone,
    )

    v, cv = MOI.add_constrained_variables(model, MOI.RotatedSecondOrderCone(4))
    t, u, x, y = v
    ft = MOI.SingleVariable(t)
    fu = MOI.SingleVariable(u)
    c = MOI.add_constraint(model, 1.0ft + 1.0fu, MOI.LessThan(2.0))
    fx = MOI.SingleVariable(x)
    fy = MOI.SingleVariable(y)
    func = 1.0fx + 1.0fy
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
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2.0 atol = atol rtol = rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ 2.0 atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ 1.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), u) ≈ 1.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 1.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 1.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cv) ≈ ones(4) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 2.0 atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ConstraintDual(), cv) ≈
                  [1.0, 1.0, -1.0, -1.0] atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1.0 atol = atol rtol =
                rtol
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_RotatedSecondOrderCone_out_of_order),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(4),
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1.0],
            (MOI.VectorOfVariables, MOI.RotatedSecondOrderCone) =>
                [[1.0, 1.0, -1.0, -1.0]],
        ),
    )
    return
end

function _test_conic_GeometricMeanCone_helper(
    model::MOI.ModelLike,
    config::Config,
    use_VectorOfVariables,
    n = 3,
)
    atol = config.atol
    rtol = config.rtol
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
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
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
            MOI.VectorAffineFunction{Float64},
            MOI.GeometricMeanCone,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )

    t = MOI.add_variable(model)
    x = MOI.add_variables(model, n)
    vov = MOI.VectorOfVariables([t; x])
    if use_VectorOfVariables
        gmc = MOI.add_constraint(model, vov, MOI.GeometricMeanCone(n + 1))
    else
        gmc = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{Float64}(vov),
            MOI.GeometricMeanCone(n + 1),
        )
    end
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x), 0.0),
        MOI.LessThan(Float64(n)),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                MOI.GeometricMeanCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{Float64},
                MOI.LessThan{Float64},
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, t)], 0.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1 atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ 1 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ ones(n) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), gmc) ≈ ones(n + 1) atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ n atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ConstraintDual(), gmc) ≈
                  vcat(-1.0, fill(inv(n), n)) atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -inv(n) atol = atol rtol =
                rtol
        end
    end
    return
end

function test_conic_GeometricMeanCone_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_GeometricMeanCone_helper(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_GeometricMeanCone_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(4),
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-inv(3)],
        ),
    )
    return
end

function test_conic_GeometricMeanCone_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_GeometricMeanCone_helper(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_GeometricMeanCone_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(4),
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-inv(3)],
            (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone) =>
                [vcat(-1.0, fill(inv(3), 3))],
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
    config::Config,
    use_VectorOfVariables,
)
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
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
            MOI.VectorAffineFunction{Float64},
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
            MOI.VectorAffineFunction{Float64}(vov),
            MOI.GeometricMeanCone(n + 1),
        )
    end
    cx = Vector{
        MOI.ConstraintIndex{
            MOI.ScalarAffineFunction{Float64},
            MOI.EqualTo{Float64},
        },
    }(
        undef,
        n,
    )
    for i in 1:n
        cx[i] = MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x[i])], 0.0),
            MOI.EqualTo(1.0),
        )
    end
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                MOI.GeometricMeanCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{Float64},
                MOI.EqualTo{Float64},
            }(),
        ) == n
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, t)], 0.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1.0 atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ 1.0 atol = atol rtol =
            rtol
        @test MOI.get.(model, MOI.VariablePrimal(), x) ≈ ones(n) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), gmc) ≈ ones(n + 1) atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cx) ≈ ones(n) atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ConstraintDual(), gmc) ≈
                  vcat(-1, fill(inv(n), n)) atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), cx) ≈ fill(-inv(n), n) atol =
                atol rtol = rtol
        end
    end
    return
end

function test_conic_GeometricMeanCone_VectorOfVariables_2(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_GeometricMeanCone_helper_2(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_GeometricMeanCone_VectorOfVariables_2),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(10),
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                fill(-inv(9), 9),
        ),
    )
    return
end

function test_conic_GeometricMeanCone_VectorAffineFunction_2(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_GeometricMeanCone_helper_2(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_GeometricMeanCone_VectorAffineFunction_2),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(10),
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                fill(-inv(9), 9),
            (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone) =>
                [vcat(-1.0, fill(inv(9), 9))],
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
    config::Config,
    use_VectorOfVariables,
)
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
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
            MOI.VectorAffineFunction{Float64},
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
            MOI.VectorAffineFunction{Float64}(vov),
            MOI.GeometricMeanCone(2),
        )
    end
    cx = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
        MOI.LessThan(2.0),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                MOI.GeometricMeanCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{Float64},
                MOI.LessThan{Float64},
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, t)], 0.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 4.0 atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ 2.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 2.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), gmc) ≈ [2.0; 2.0] atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cx) ≈ 2.0 atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ConstraintDual(), gmc) ≈ [-2.0, 2.0] atol =
                atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), cx) ≈ -2.0 atol = atol rtol =
                rtol
        end
    end
    return
end

function test_conic_GeometricMeanCone_VectorOfVariables_3(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_GeometricMeanCone_helper_3(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_GeometricMeanCone_VectorOfVariables_3),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [2.0, 2.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-2.0],
        ),
    )
    return
end

function test_conic_GeometricMeanCone_VectorAffineFunction_3(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_GeometricMeanCone_helper_3(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_GeometricMeanCone_VectorAffineFunction_3),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [2.0, 2.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-2.0],
            (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone) =>
                [[-2.0, 2.0]],
        ),
    )
    return
end

function _test_conic_Exponential_helper(
    model::MOI.ModelLike,
    config::Config,
    use_VectorOfVariables::Bool,
)
    F = if use_VectorOfVariables
        MOI.VectorOfVariables
    else
        MOI.VectorAffineFunction{Float64}
    end
    @requires MOI.supports_constraint(model, F, MOI.ExponentialCone)
    atol = config.atol
    rtol = config.rtol
    # Problem EXP1 - ExpPrimal
    # min x + y + z
    #  st  y e^(x/y) <= z, y > 0 (i.e (x, y, z) are in the exponential primal cone)
    #      x == 1
    #      y == 2
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
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
            MOI.VectorAffineFunction{Float64},
            MOI.ExponentialCone,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )

    v = MOI.add_variables(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3
    vov = MOI.VectorOfVariables(v)
    if use_VectorOfVariables
        vc = MOI.add_constraint(model, vov, MOI.ExponentialCone())
    else
        vc = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{Float64}(vov),
            MOI.ExponentialCone(),
        )
    end
    cx = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, v[1])], 0.0),
        MOI.EqualTo(1.0),
    )
    cy = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, v[2])], 0.0),
        MOI.EqualTo(2.0),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, v), 0.0),
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
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3 + 2exp(1 / 2) atol = atol rtol =
            rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ 3 + 2exp(1 / 2) atol =
                atol rtol = rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1.0, 2.0, 2exp(1 / 2)] atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), vc) ≈
              [1.0, 2.0, 2exp(1 / 2)] atol = atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cx) ≈ 1 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cy) ≈ 2 atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            u, v, w = MOI.get(model, MOI.ConstraintDual(), vc)
            @test u ≈ -exp(1 / 2) atol = atol rtol = rtol
            @test v ≈ -exp(1 / 2) / 2 atol = atol rtol = rtol
            @test w ≈ 1 atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), cx) ≈ 1 + exp(1 / 2) atol =
                atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), cy) ≈ 1 + exp(1 / 2) / 2 atol =
                atol rtol = rtol
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
    config::Config,
)
    _test_conic_Exponential_helper(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_Exponential_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 2.0, 2exp(1 / 2)],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [1 + exp(1 / 2), 1 + exp(1 / 2) / 2],
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
    config::Config,
)
    _test_conic_Exponential_helper(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_Exponential_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 2.0, 2exp(1 / 2)],
            (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone) =>
                [[-exp(1 / 2), -exp(1 / 2) / 2, 1.0]],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [1 + exp(1 / 2), 1 + exp(1 / 2) / 2],
        ),
    )
    return
end

"""
    test_conic_Exponential_hard_2(model::MOI.ModelLike, config::Config)

Test an exponential cone problem that ECOS failed.
"""
function test_conic_Exponential_hard_2(model::MOI.ModelLike, config::Config)
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.ExponentialCone,
    )
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.ExponentialCone,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NonnegativeCone,
    )

    v = MOI.add_variables(model, 9)
    @test MOI.get(model, MOI.NumberOfVariables()) == 9
    ec1 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 1, 3],
                MOI.ScalarAffineTerm.(1.0, [v[2], v[3], v[4]]),
            ),
            [0.0, 1.0, 0.0],
        ),
        MOI.ExponentialCone(),
    )
    ec2 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 1, 3],
                MOI.ScalarAffineTerm.([1.0, -1.0, 1.0], [v[2], v[3], v[5]]),
            ),
            [0.0, 1.0, 0.0],
        ),
        MOI.ExponentialCone(),
    )
    c1 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([0.5, 0.5, -1.0], [v[4], v[5], v[6]]),
            0.0,
        ),
        MOI.EqualTo(0.0),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 2, 3, 1, 2, 3],
                MOI.ScalarAffineTerm.(
                    [1.0, 1.0, 1.0, 0.3, 0.3, 0.3],
                    [v[1], v[2], v[3], v[7], v[8], v[9]],
                ),
            ),
            zeros(3),
        ),
        MOI.NonnegativeCone(3),
    )
    c3 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 2, 3, 1, 2, 3],
                MOI.ScalarAffineTerm.(
                    [-1.0, -1.0, -1.0, 0.3, 0.3, 0.3],
                    [v[1], v[2], v[3], v[7], v[8], v[9]],
                ),
            ),
            zeros(3),
        ),
        MOI.NonnegativeCone(3),
    )
    c4 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(1.0, [v[7], v[8], v[9]]),
            0.0,
        ),
        MOI.LessThan(1.0),
    )
    c5 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, v[7])], 0.0),
        MOI.EqualTo(0.0),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, v[6])], 0.0),
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
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ exp(-0.3) atol = atol rtol =
            rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ exp(-0.3) atol =
                atol rtol = rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈
              [0.0, -0.3, 0.0, exp(-0.3), exp(-0.3), exp(-0.3), 0.0, 1.0, 0.0] atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), ec1) ≈
              [-0.3, 1.0, exp(-0.3)] atol = atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), ec2) ≈
              [-0.3, 1.0, exp(-0.3)] atol = atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c1) ≈ 0.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ zeros(3) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c3) ≈ [0.0, 0.6, 0.0] atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c4) ≈ 1.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c5) ≈ 0.0 atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ConstraintDual(), ec1) ≈
                  [-exp(-0.3) / 2, -1.3exp(-0.3) / 2, 0.5] atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), ec2) ≈
                  [-exp(-0.3) / 2, -1.3exp(-0.3) / 2, 0.5] atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ -1 atol = atol rtol =
                rtol
            d5 = MOI.get(model, MOI.ConstraintDual(), c5) # degree of freedom
            d23 = (exp(-0.3) * 0.3 - d5) / 0.6 # dual constraint corresponding to v[7]
            @test d23 >= -atol
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈
                  [d23, exp(-0.3), exp(-0.3) / 2] atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), c3) ≈
                  [d23, 0.0, exp(-0.3) / 2] atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), c4) ≈ -exp(-0.3) * 0.3 atol =
                atol rtol = rtol
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_Exponential_hard_2),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0.0, -0.3, 0.0, exp(-0.3), exp(-0.3), exp(-0.3), 0.0, 1.0, 0.0],
            (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone) => [
                [-exp(-0.3) / 2, -1.3exp(-0.3) / 2, 0.5],
                [-exp(-0.3) / 2, -1.3exp(-0.3) / 2, 0.5],
            ],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [-1.0, exp(-0.3) * 0.3],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-exp(-0.3) * 0.3],
            (MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone) => [
                [0.0, exp(-0.3), exp(-0.3) / 2],
                [0.0, 0.0, exp(-0.3) / 2],
            ],
        ),
    )
    return
end

"""
    test_conic_Exponential_hard(model::MOI.ModelLike, config::Config)

Test an exponential problem that ECOS failed.
"""
function test_conic_Exponential_hard(model::MOI.ModelLike, config::Config)
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.ExponentialCone,
    )
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.SingleVariable,
        MOI.LessThan{Float64},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.ExponentialCone,
    )

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    xc = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 0.0),
        MOI.LessThan(4.0),
    )
    yc = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.LessThan(5.0))
    @test yc.value == y.value
    ec = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.([1, 3], MOI.ScalarAffineTerm.(1.0, [x, y])),
            [0.0, 1.0, 0.0],
        ),
        MOI.ExponentialCone(),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
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
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ log(5) atol = atol rtol =
            rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ log(5) atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ log(5) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 5.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), xc) ≈ 2log(5) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), yc) ≈ 5 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), ec) ≈ [log(5), 1.0, 5.0] atol =
            atol rtol = rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ConstraintDual(), xc) ≈ 0.0 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), yc) ≈ -1 / 5 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), ec) ≈
                  [-1.0, log(5) - 1, 1 / 5] atol = atol rtol = rtol
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_Exponential_hard),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [log(5), 5.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [0.0],
            (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone) =>
                [[-1.0, log(5) - 1, 1 / 5]],
        ),
    )
    return
end

function _test_conic_DualExponentialCone_helper(
    model::MOI.ModelLike,
    config::Config,
    use_VectorOfVariables::Bool,
)
    F = if use_VectorOfVariables
        MOI.VectorOfVariables
    else
        MOI.VectorAffineFunction{Float64}
    end
    @requires MOI.supports_constraint(model, F, MOI.DualExponentialCone)
    atol = config.atol
    rtol = config.rtol
    # Problem dual exp
    # max 2x_2 + x_1
    # s.t.
    # x_1 + u == 1
    # x_2 + v == 1
    # w == 1
    # (u, v, w) ∈ DualExponentialCone
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
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
            MOI.VectorAffineFunction{Float64},
            MOI.DualExponentialCone,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
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
            MOI.VectorAffineFunction{Float64}(vov),
            MOI.DualExponentialCone(),
        )
    end
    cu = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 1.0], [x[1], v[1]]),
            0.0,
        ),
        MOI.EqualTo(1.0),
    )
    cv = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 1.0], [x[2], v[2]]),
            0.0,
        ),
        MOI.EqualTo(1.0),
    )
    cw = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, v[3])], 0.0),
        MOI.EqualTo(1.0),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 2.0], x), 0.0),
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
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3 + 2exp(1 / 2) atol = atol rtol =
            rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ 3 + 2exp(1 / 2) atol =
                atol rtol = rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈
              [-exp(1 / 2), -exp(1 / 2) / 2, 1.0] atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈
              [1 + exp(1 / 2), 1 + exp(1 / 2) / 2] atol = atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), vc) ≈
              [-exp(1 / 2), -exp(1 / 2) / 2, 1.0] atol = atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cu) ≈ 1 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cv) ≈ 1 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cw) ≈ 1 atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            x, y, z = MOI.get(model, MOI.ConstraintDual(), vc)
            @test x ≈ 1.0 atol = atol rtol = rtol
            @test y ≈ 2.0 atol = atol rtol = rtol
            @test z ≈ 2exp(1 / 2) atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), cu) ≈ -1 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), cv) ≈ -2 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), cw) ≈ -2exp(1 / 2) atol =
                atol rtol = rtol
        end
    end
    return
end

function test_conic_DualExponentialCone_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_DualExponentialCone_helper(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_DualExponentialCone_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [
                -exp(1 / 2),
                -exp(1 / 2) / 2,
                1.0,
                1 + exp(1 / 2),
                1 + exp(1 / 2) / 2,
            ],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [-1.0, -2.0, -2exp(1 / 2)],
        ),
    )
    return
end

function test_conic_DualExponentialCone_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_DualExponentialCone_helper(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_DualExponentialCone_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [
                -exp(1 / 2),
                -exp(1 / 2) / 2,
                1.0,
                1 + exp(1 / 2),
                1 + exp(1 / 2) / 2,
            ],
            (MOI.VectorAffineFunction{Float64}, MOI.DualExponentialCone) =>
                [[1.0, 2.0, 2exp(1 / 2)]],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [-1.0, -2.0, -2exp(1 / 2)],
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
    config::Config,
    use_VectorOfVariables::Bool,
)
    F = if use_VectorOfVariables
        MOI.VectorOfVariables
    else
        MOI.VectorAffineFunction{Float64}
    end
    @requires MOI.supports_constraint(model, F, MOI.PowerCone{Float64})
    atol = config.atol
    rtol = config.rtol
    a = 0.9
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.PowerCone{Float64},
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.PowerCone{Float64},
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )

    v = MOI.add_variables(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3
    vov = MOI.VectorOfVariables(v)
    if use_VectorOfVariables
        vc = MOI.add_constraint(model, vov, MOI.PowerCone(a))
    else
        vc = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{Float64}(vov),
            MOI.PowerCone(a),
        )
    end
    cx = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, v[1])], 0.0),
        MOI.EqualTo(2.0),
    )
    cy = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, v[2])], 0.0),
        MOI.EqualTo(1.0),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, v[3])], 0.0),
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
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2^0.9 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [2.0, 1.0, 2^0.9] atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), vc) ≈ [2.0, 1.0, 2^0.9] atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cx) ≈ 2.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cy) ≈ 1.0 atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            # Only real solution of u^10 - u^9 / 2^0.1 = -(0.1*0.9^9)/2
            u_value = 0.839729692
            v_value = 2^0.9 - 2u_value
            u, v, w = MOI.get(model, MOI.ConstraintDual(), vc)
            @test u ≈ u_value atol = atol rtol = rtol
            @test v ≈ v_value atol = atol rtol = rtol
            @test w ≈ -1 atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), cx) ≈ -u_value atol =
                atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), cy) ≈ -v_value atol =
                atol rtol = rtol
        end
    end
    return
end

function test_conic_PowerCone_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_PowerCone_helper(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_PowerCone_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config,
)
    u_value = 0.839729692
    v_value = 2^0.9 - 2 * u_value
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [2.0, 1.0, 2^0.9],
            (MOI.VectorOfVariables, MOI.PowerCone{Float64}) =>
                [[u_value, v_value, -1]],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [-u_value, -v_value],
        ),
    )
    return
end

function test_conic_PowerCone_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config,
)
    _test_conic_PowerCone_helper(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_PowerCone_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config,
)
    u_value = 0.839729692
    v_value = 2^0.9 - 2 * u_value
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [2.0, 1.0, 2^0.9],
            (MOI.VectorAffineFunction{Float64}, MOI.PowerCone{Float64}) =>
                [[u_value, v_value, -1]],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
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
        exponent::Float64 = 0.9,
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
    config::Config,
    use_VectorOfVariables::Bool;
    exponent::Float64 = 0.9,
)
    F = if use_VectorOfVariables
        MOI.VectorOfVariables
    else
        MOI.VectorAffineFunction{Float64}
    end
    @requires MOI.supports_constraint(model, F, MOI.DualPowerCone{Float64})
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.DualPowerCone{Float64},
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.DualPowerCone{Float64},
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
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
            MOI.VectorAffineFunction{Float64}(vov),
            MOI.DualPowerCone(exponent),
        )
    end
    cu = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 1.0], [x[1], v[1]]),
            0.0,
        ),
        MOI.EqualTo(0.0),
    )
    cv = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 1.0], [x[2], v[2]]),
            0.0,
        ),
        MOI.EqualTo(0.0),
    )
    cw = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, v[3])], 0.0),
        MOI.EqualTo(1.0),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([-1.0, -1.0], [x[1], x[2]]),
            0.0,
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
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1.0 atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈
              [exponent, (1 - exponent), 1.0] atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈
              [-exponent, -(1 - exponent)] atol = atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), vc) ≈
              [exponent, (1 - exponent), 1.0] atol = atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cu) ≈ 0.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cv) ≈ 0.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cw) ≈ 1.0 atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            x, y, z = MOI.get(model, MOI.ConstraintDual(), vc)
            @test x ≈ 1.0 atol = atol rtol = rtol
            @test y ≈ 1.0 atol = atol rtol = rtol
            @test z ≈ -1 atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), cu) ≈ -1.0 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), cv) ≈ -1.0 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), cw) ≈ 1.0 atol = atol rtol =
                rtol
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
    config::Config,
)
    _test_conic_DualPowerCone_helper(model, config, true)
    return
end

function setup_test(
    ::typeof(test_conic_DualPowerCone_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0.9, 0.1, 1.0, -0.9, -0.1],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [-1.0, -1.0, 1.0],
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
    config::Config,
)
    _test_conic_DualPowerCone_helper(model, config, false)
    return
end

function setup_test(
    ::typeof(test_conic_DualPowerCone_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0.9, 0.1, 1.0, -0.9, -0.1],
            (MOI.VectorAffineFunction{Float64}, MOI.DualPowerCone{Float64}) => [[1.0, 1.0, -1.0]],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [-1.0, -1.0, 1.0],
        ),
    )
    return
end

"""
    test_conic_RelativeEntropyCone(model::MOI.ModelLike, config::Config)

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
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.SingleVariable}())
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.RelativeEntropyCone,
    )

    u = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    relentr = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, u))],
            Float64[0, 1, 5, 2, 3],
        ),
        MOI.RelativeEntropyCone(5),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(u),
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
        u_opt = 2 * log(2) + 3 * log(3 / 5)
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ u_opt atol = atol rtol =
            rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ u_opt atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), u) ≈ u_opt atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), relentr) ≈
              [u_opt, 1, 5, 2, 3] atol = atol rtol = rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ConstraintDual(), relentr) ≈
                  [1, 2, 0.6, log(0.5) - 1, log(5 / 3) - 1] atol = atol rtol =
                rtol
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_RelativeEntropyCone),
    model::MOIU.MockOptimizer,
    ::Config,
)
    u_opt = 2 * log(2 / 1) + 3 * log(3 / 5)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [u_opt],
            (MOI.VectorAffineFunction{Float64}, MOI.RelativeEntropyCone) =>
                [[1, 2, 0.6, log(0.5) - 1, log(5 / 3) - 1]],
        ),
    )
    return
end

"""
    test_conic_NormSpectralCone(model::MOI.ModelLike, config::Config)

Test the problem:
```
min t
 st  t >= sigma_1([1 1 0; 1 -1 1]) (i.e (t, 1, 1, 1, -1, 0, 1]) is in NormSpectralCone(2, 3))
Singular values are [sqrt(3), sqrt(2)], so optimal solution is:
t = sqrt(3)
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
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.SingleVariable}())
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NormSpectralCone,
    )

    t = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    data = Float64[1, 1, 1, -1, 0, 1]
    spec = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, t))],
            vcat(0.0, data),
        ),
        MOI.NormSpectralCone(2, 3),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(t),
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
        rt3 = sqrt(3)
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
                  Float64[1, 0, -invrt3, 0, invrt3, 0, -invrt3] atol = atol rtol =
                rtol
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_NormSpectralCone),
    model::MOIU.MockOptimizer,
    ::Config,
)
    invrt3 = inv(sqrt(3))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [sqrt(3)],
            (MOI.VectorAffineFunction{Float64}, MOI.NormSpectralCone) =>
                [Float64[1, 0, -invrt3, 0, invrt3, 0, -invrt3]],
        ),
    )
    return
end

"""
    test_conic_NormSpectralCone_2(model::MOI.ModelLike, config::Config)

Test the problem:
```
min t
 st  t >= sigma_1([1 1; 1 -1; 0 1]) (i.e (t, 1, 1, 0, 1, -1, 1]) is in NormSpectralCone(3, 2))
Singular values are [sqrt(3), sqrt(2)], so optimal solution is:
t = sqrt(3)
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
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.SingleVariable}())
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NormSpectralCone,
    )

    t = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    data = Float64[1, 1, 0, 1, -1, 1]
    spec = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, t))],
            vcat(0.0, data),
        ),
        MOI.NormSpectralCone(3, 2),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(t),
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
        rt3 = sqrt(3)
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
                  Float64[1, 0, 0, 0, -invrt3, invrt3, -invrt3] atol = atol rtol =
                rtol
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_NormSpectralCone_2),
    model::MOIU.MockOptimizer,
    ::Config,
)
    invrt3 = inv(sqrt(3))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [sqrt(3)],
            (MOI.VectorAffineFunction{Float64}, MOI.NormSpectralCone) =>
                [Float64[1, 0, 0, 0, -invrt3, invrt3, -invrt3]],
        ),
    )
    return
end

"""
    test_conic_NormNuclearCone(model::MOI.ModelLike, config::Config)

Test the problem:
```
min t
 st  t >= sum_i sigma_i([1 1 0; 1 -1 1]) (i.e (t, 1, 1, 1, -1, 0, 1]) is in NormNuclearCone(2, 3))
Singular values are [sqrt(3), sqrt(2)], so optimal solution is:
t = sqrt(3) + sqrt(2)
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
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.SingleVariable}())
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NormNuclearCone,
    )

    t = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    data = Float64[1, 1, 1, -1, 0, 1]
    nuc = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, t))],
            vcat(0.0, data),
        ),
        MOI.NormNuclearCone(2, 3),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(t),
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
        rt3 = sqrt(3)
        rt2 = sqrt(2)
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
                  Float64[1, -invrt2, -invrt3, -invrt2, invrt3, 0, -invrt3] atol =
                atol rtol = rtol
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_NormNuclearCone),
    model::MOIU.MockOptimizer,
    ::Config,
)
    invrt2 = inv(sqrt(2))
    invrt3 = inv(sqrt(3))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [sqrt(2) + sqrt(3)],
            (MOI.VectorAffineFunction{Float64}, MOI.NormNuclearCone) =>
                [Float64[1, -invrt2, -invrt3, -invrt2, invrt3, 0, -invrt3]],
        ),
    )
    return
end

"""
    test_conic_NormNuclearCone_2(model::MOI.ModelLike, config::Config)

Test the problem:
```
min t
 st  t >= sum_i sigma_i([1 1; 1 -1; 0 1]) (i.e (t, 1, 1, 0, 1, -1, 1]) is in NormNuclearCone(3, 2))
Singular values are [sqrt(3), sqrt(2)], so optimal solution is:
t = sqrt(3) + sqrt(2)
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
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.SingleVariable}())
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NormNuclearCone,
    )

    t = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    data = Float64[1, 1, 0, 1, -1, 1]
    nuc = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, t))],
            vcat(0.0, data),
        ),
        MOI.NormNuclearCone(3, 2),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(t),
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
        rt3 = sqrt(3)
        rt2 = sqrt(2)
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
                  Float64[1, -invrt2, -invrt2, 0, -invrt3, invrt3, -invrt3] atol =
                atol rtol = rtol
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_NormNuclearCone_2),
    model::MOIU.MockOptimizer,
    ::Config,
)
    invrt2 = inv(sqrt(2))
    invrt3 = inv(sqrt(3))
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [sqrt(2) + sqrt(3)],
            (MOI.VectorAffineFunction{Float64}, MOI.NormNuclearCone) =>
                [Float64[1, -invrt2, -invrt2, 0, -invrt3, invrt3, -invrt3]],
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
    config::Config,
)
    atol = config.atol
    rtol = config.rtol
    square = psdcone == MOI.PositiveSemidefiniteConeSquare
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_VectorOfVariables
        @requires MOI.supports_constraint(model, MOI.VectorOfVariables, psdcone)
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            psdcone,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )

    X = MOI.add_variables(model, square ? 4 : 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == (square ? 4 : 3)
    vov = MOI.VectorOfVariables(X)
    if use_VectorOfVariables
        cX = MOI.add_constraint(model, vov, psdcone(2))
    else
        cX = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{Float64}(vov),
            psdcone(2),
        )
    end
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, X[2])], 0.0),
        MOI.EqualTo(1.0),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                psdcone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{Float64},
                MOI.EqualTo{Float64},
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(1.0, [X[1], X[end]]),
            0.0,
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
        Xv = square ? ones(4) : ones(3)
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
    config::Config,
)
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
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(3),
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [2],
        ),
    )
    return
end

function test_conic_PositiveSemidefiniteConeTriangle_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config,
)
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
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(3),
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [[1.0, -1.0, 1.0]],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [2],
        ),
    )
    return
end

function test_conic_PositiveSemidefiniteConeSquare_VectorOfVariables(
    model::MOI.ModelLike,
    config::Config,
)
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
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(4),
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [2],
        ),
    )
    return
end

function test_conic_PositiveSemidefiniteConeSquare_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config,
)
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
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(4),
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeSquare,
            ) => [[1.0, -2.0, 0.0, 1.0]],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [2],
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
    config::Config,
)
    atol = config.atol
    rtol = config.rtol
    square = psdcone == MOI.PositiveSemidefiniteConeSquare
    # The dual of the SDP constraint is rank two of the form
    # [γ, 0, -γ] * [γ, 0, γ'] + [δ, ε, δ] * [δ, ε, δ]'
    # and the dual of the SOC constraint is of the form (√2*y2, -y2, -y2)
    #
    # The feasible set of the constraint dual contains only four points.
    # Eliminating, y1, y2 and γ from the dual constraints gives
    # -ε^2 + -εδ + 2δ^2 + 1
    # (√2-2)ε^2 + (-2√2+2)δ^2 + 1
    # Eliminating ε from this set of equation give
    # (-6√2+4)δ^4 + (3√2-2)δ^2 + (2√2-3)
    # from which we find the solution
    δ = √(1 + (3 * √2 + 2) * √(-116 * √2 + 166) / 14) / 2
    # which is optimal
    ε = √((1 - 2 * (√2 - 1) * δ^2) / (2 - √2))
    y2 = 1 - ε * δ
    y1 = 1 - √2 * y2
    obj = y1 + y2 / 2
    # The primal solution is rank one of the form
    # X = [α, β, α] * [α, β, α]'
    # and by complementary slackness, x is of the form (√2*x2, x2, x2)
    # The primal reduces to
    #      4α^2+4αβ+2β^2+√2*x2= obj
    #      2α^2    + β^2+√2*x2 = 1 (1)
    #      8α^2+8αβ+2β^2+ 4 x2 = 1
    # Eliminating β, we get
    # 4α^2 + 4x2 = 3 - 2obj (2)
    # By complementary slackness, we have β = kα where
    k = -2 * δ / ε
    # Replacing β by kα in (1) allows to eliminate α^2 in (2) to get
    x2 = ((3 - 2obj) * (2 + k^2) - 4) / (4 * (2 + k^2) - 4 * √2)
    # With (2) we get
    α = √(3 - 2obj - 4x2) / 2
    β = k * α
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )
    if use_VectorOfVariables
        @requires MOI.supports_constraint(model, MOI.VectorOfVariables, psdcone)
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
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
            MOI.VectorAffineFunction{Float64}(vov),
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
                [1.0, 1, 1, 1],
                [X[1], X[square ? 5 : 3], X[end], x[1]],
            ),
            0.0,
        ),
        MOI.EqualTo(1.0),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(
                square ? ones(11) : [1.0, 2, 1, 2, 2, 1, 1, 1],
                [X; x[2]; x[3]],
            ),
            0.0,
        ),
        MOI.EqualTo(1 / 2),
    )
    objXidx = square ? [1:2; 4:6; 8:9] : [1:3; 5:6]
    objXcoefs = square ? [2.0, 1.0, 1.0, 2.0, 1.0, 1.0, 2.0] : 2 * ones(5)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([objXcoefs; 1.0], [X[objXidx]; x[1]]),
            0.0,
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                psdcone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{Float64},
                MOI.EqualTo{Float64},
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
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ obj atol = atol rtol = rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ obj atol = atol rtol =
                rtol
        end
        Xv =
            square ? [α^2, α * β, α^2, α * β, β^2, α * β, α^2, α * β, α^2] :
            [α^2, α * β, β^2, α^2, α * β, α^2]
        xv = [√2 * x2, x2, x2]
        @test MOI.get(model, MOI.VariablePrimal(), X) ≈ Xv atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ xv atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cX) ≈ Xv atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cx) ≈ xv atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c1) ≈ 1.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ 0.5 atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            cX0 = 1 + (√2 - 1) * y2
            cX1 = 1 - y2
            cX2 = -y2
            cXv =
                square ? [cX0, cX1, cX2, cX1, cX0, cX1, cX2, cX1, cX0] :
                [cX0, cX1, cX0, cX2, cX1, cX0]
            @test MOI.get(model, MOI.ConstraintDual(), cX) ≈ cXv atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), cx) ≈ [1 - y1, -y2, -y2] atol =
                atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ y1 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ y2 atol = atol rtol =
                rtol
        end
    end
    return
end

function test_conic_PositiveSemidefiniteConeTriangle_VectorOfVariables_2(
    model::MOI.ModelLike,
    config::Config,
)
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
    ::Config,
)
    δ = √(1 + (3 * √2 + 2) * √(-116 * √2 + 166) / 14) / 2
    ε = √((1 - 2 * (√2 - 1) * δ^2) / (2 - √2))
    y2 = 1 - ε * δ
    y1 = 1 - √2 * y2
    obj = y1 + y2 / 2
    k = -2 * δ / ε
    x2 = ((3 - 2obj) * (2 + k^2) - 4) / (4 * (2 + k^2) - 4 * √2)
    α = √(3 - 2obj - 4x2) / 2
    β = k * α
    Xv = [α^2, α * β, β^2, α^2, α * β, α^2]
    xv = [√2 * x2, x2, x2]
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [Xv; xv],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [y1, y2],
        ),
    )
    return
end

function test_conic_PositiveSemidefiniteConeTriangle_VectorAffineFunction_2(
    model::MOI.ModelLike,
    config::Config,
)
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
    ::Config,
)
    δ = √(1 + (3 * √2 + 2) * √(-116 * √2 + 166) / 14) / 2
    ε = √((1 - 2 * (√2 - 1) * δ^2) / (2 - √2))
    y2 = 1 - ε * δ
    y1 = 1 - √2 * y2
    obj = y1 + y2 / 2
    k = -2 * δ / ε
    x2 = ((3 - 2obj) * (2 + k^2) - 4) / (4 * (2 + k^2) - 4 * √2)
    α = √(3 - 2obj - 4x2) / 2
    β = k * α
    Xv = [α^2, α * β, β^2, α^2, α * β, α^2]
    xv = [√2 * x2, x2, x2]
    cX0 = 1 + (√2 - 1) * y2
    cX1 = 1 - y2
    cX2 = -y2
    cXv = [cX0, cX1, cX0, cX2, cX1, cX0]
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [Xv; xv],
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [cXv],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [y1, y2],
        ),
    )
    return
end

function test_conic_PositiveSemidefiniteConeSquare_VectorOfVariables_2(
    model::MOI.ModelLike,
    config::Config,
)
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
    ::Config,
)
    δ = √(1 + (3 * √2 + 2) * √(-116 * √2 + 166) / 14) / 2
    ε = √((1 - 2 * (√2 - 1) * δ^2) / (2 - √2))
    y2 = 1 - ε * δ
    y1 = 1 - √2 * y2
    obj = y1 + y2 / 2
    k = -2 * δ / ε
    x2 = ((3 - 2obj) * (2 + k^2) - 4) / (4 * (2 + k^2) - 4 * √2)
    α = √(3 - 2obj - 4x2) / 2
    β = k * α
    Xv = [α^2, α * β, α^2, α * β, β^2, α * β, α^2, α * β, α^2]
    xv = [√2 * x2, x2, x2]
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [Xv; xv],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [y1, y2],
        ),
    )
    return
end

function test_conic_PositiveSemidefiniteConeSquare_VectorAffineFunction_2(
    model::MOI.ModelLike,
    config::Config,
)
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
    ::Config,
)
    δ = √(1 + (3 * √2 + 2) * √(-116 * √2 + 166) / 14) / 2
    ε = √((1 - 2 * (√2 - 1) * δ^2) / (2 - √2))
    y2 = 1 - ε * δ
    y1 = 1 - √2 * y2
    obj = y1 + y2 / 2
    k = -2 * δ / ε
    x2 = ((3 - 2obj) * (2 + k^2) - 4) / (4 * (2 + k^2) - 4 * √2)
    α = √(3 - 2obj - 4x2) / 2
    β = k * α
    xv = [√2 * x2, x2, x2]
    cX0 = 1 + (√2 - 1) * y2
    cX1 = 1 - y2
    cX2 = -y2
    Xv = [α^2, α * β, α^2, α * β, β^2, α * β, α^2, α * β, α^2]
    cXv = [cX0, cX1, cX2, cX1, cX0, cX1, cX2, cX1, cX0]
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [Xv; xv],
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeSquare,
            ) => [cXv],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [y1, y2],
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
    config::Config,
)
    atol = config.atol
    rtol = config.rtol
    # Caused getdual to fail on SCS and Mosek
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.GreaterThan{Float64},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NonpositiveCone,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.PositiveSemidefiniteConeTriangle,
    )
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )

    x = MOI.add_variables(model, 7)
    @test MOI.get(model, MOI.NumberOfVariables()) == 7
    η = 10.0
    c1 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(-1.0, x[1:6]), 0.0),
        MOI.GreaterThan(-η),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(1:6, MOI.ScalarAffineTerm.(-1.0, x[1:6])),
            zeros(6),
        ),
        MOI.NonpositiveCone(6),
    )
    α = 0.8
    δ = 0.9
    c3 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [fill(1, 7); fill(2, 5); fill(3, 6)],
                MOI.ScalarAffineTerm.(
                    [
                        δ / 2,
                        α,
                        δ,
                        δ / 4,
                        δ / 8,
                        0.0,
                        -1.0,
                        -δ / (2 * √2),
                        -δ / 4,
                        0,
                        -δ / (8 * √2),
                        0.0,
                        δ / 2,
                        δ - α,
                        0,
                        δ / 8,
                        δ / 4,
                        -1.0,
                    ],
                    [x[1:7]; x[1:3]; x[5:6]; x[1:3]; x[5:7]],
                ),
            ),
            zeros(3),
        ),
        MOI.PositiveSemidefiniteConeTriangle(2),
    )
    c4 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(0.0, [x[1:3]; x[5:6]]),
            0.0,
        ),
        MOI.EqualTo(0.0),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{Float64},
                MOI.GreaterThan{Float64},
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.NonpositiveCone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{Float64},
                MOI.EqualTo{Float64},
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x[7])], 0.0),
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
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈
              [2η / 3, 0, η / 3, 0, 0, 0, η * δ * (1 - 1 / √3) / 2] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c1) ≈ -η atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈
              [-2η / 3, 0, -η / 3, 0, 0, 0] atol = atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c3) ≈ [
            η * δ * (1 / √3 + 1 / 3) / 2,
            -η * δ / (3 * √2),
            η * δ * (1 / √3 - 1 / 3) / 2,
        ] atol = atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c4) ≈ 0.0 atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈
                  δ * (1 - 1 / √3) / 2 atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ [
                0,
                -α / √3 + δ / (2 * √6) * (2 * √2 - 1),
                0,
                -3δ * (1 - 1 / √3) / 8,
                -3δ * (1 - 1 / √3) / 8,
                -δ * (3 - 2 * √3 + 1 / √3) / 8,
            ] atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), c3) ≈
                  [(1 - 1 / √3) / 2, 1 / √6, (1 + 1 / √3) / 2] atol = atol rtol =
                rtol
            # Dual of c4 could be anything
        end
    end
    return
end

function setup_test(
    ::typeof(test_conic_PositiveSemidefiniteConeTriangle),
    model::MOIU.MockOptimizer,
    ::Config,
)
    η = 10.0
    α = 0.8
    δ = 0.9
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [2η / 3, 0, η / 3, 0, 0, 0, η * δ * (1 - 1 / √3) / 2],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [δ * (1 - 1 / √3) / 2],
            (MOI.VectorAffineFunction{Float64}, MOI.NonpositiveCone) => [[
                0,
                -α / √3 + δ / (2 * √6) * (2 * √2 - 1),
                0,
                -3δ * (1 - 1 / √3) / 8,
                -3δ * (1 - 1 / √3) / 8,
                -δ * (3 - 2 * √3 + 1 / √3) / 8,
            ]],
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [[(1 - 1 / √3) / 2, 1 / √6, (1 + 1 / √3) / 2]],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [0],
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
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.SingleVariable}())
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        psdcone,
    )

    x = MOI.add_variable(model)
    fx = MOI.SingleVariable(x)
    if psdcone == MOI.PositiveSemidefiniteConeTriangle
        func = MOIU.operate(vcat, T, fx, one(T), fx, one(T), one(T), fx)
    else
        @assert psdcone == MOI.PositiveSemidefiniteConeSquare
        func = MOIU.operate(
            vcat,
            T,
            fx,
            one(T),
            one(T),
            one(T),
            fx,
            one(T),
            one(T),
            one(T),
            fx,
        )
    end
    c = MOI.add_constraint(model, func, psdcone(3))
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x),
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
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ one(T) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈
              ones(T, MOI.output_dimension(func)) atol = atol rtol = rtol
        if _supports(config, MOI.ConstraintDual)
            if psdcone == MOI.PositiveSemidefiniteConeTriangle
                @test MOI.get(model, MOI.ConstraintDual(), c) ≈
                      [T(2), -one(T), T(2), -one(T), -one(T), T(2)] / T(6) atol =
                    atol rtol = rtol
            else
                @assert psdcone == MOI.PositiveSemidefiniteConeSquare
                @test MOI.get(model, MOI.ConstraintDual(), c) ≈
                      [
                    one(T),
                    zero(T),
                    zero(T),
                    -one(T),
                    one(T),
                    zero(T),
                    -one(T),
                    -one(T),
                    one(T),
                ] / T(3) atol = atol rtol = rtol
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
    config::Config,
)
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
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(1),
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [[2, -1, 2, -1, -1, 2] / 6],
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
    config::Config,
)
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
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            ones(1),
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeSquare,
            ) => [[1, 0, 0, -1, 1, 0, -1, -1, 1] / 3],
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
    config::Config,
    use_VectorOfVariables::Bool,
    detcone,
)
    F =
        use_VectorOfVariables ? MOI.VectorOfVariables :
        MOI.VectorAffineFunction{Float64}
    @requires MOI.supports_constraint(model, F, detcone)
    atol = config.atol
    rtol = config.rtol
    square = detcone == MOI.LogDetConeSquare || detcone == MOI.RootDetConeSquare
    use_logdet =
        detcone == MOI.LogDetConeTriangle || detcone == MOI.LogDetConeSquare
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_logdet
        @requires MOI.supports_constraint(
            model,
            MOI.SingleVariable,
            MOI.EqualTo{Float64},
        )
    end
    if use_VectorOfVariables
        @requires MOI.supports_constraint(model, MOI.VectorOfVariables, detcone)
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            detcone,
        )
    end
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NonnegativeCone,
    )

    t = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    Q = MOI.add_variables(model, square ? 4 : 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == (square ? 5 : 4)
    if use_logdet
        u = MOI.add_variable(model)
        vc = MOI.add_constraint(model, MOI.SingleVariable(u), MOI.EqualTo(1.0))
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
            MOI.VectorAffineFunction{Float64}(vov),
            detcone(2),
        )
    end
    c = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                1:2,
                MOI.ScalarAffineTerm.([-1.0, -1.0], [Q[1], Q[end]]),
            ),
            ones(2),
        ),
        MOI.NonnegativeCone(2),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                use_VectorOfVariables ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                detcone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.NonnegativeCone,
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, t)], 0.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        expectedobjval = use_logdet ? 0.0 : 1.0
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ expectedobjval atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ expectedobjval atol =
            atol rtol = rtol
        if use_logdet
            @test MOI.get(model, MOI.VariablePrimal(), u) ≈ 1.0 atol = atol rtol =
                rtol
        end
        Qv = MOI.get(model, MOI.VariablePrimal(), Q)
        @test Qv[1] ≈ 1.0 atol = atol rtol = rtol
        @test Qv[2] ≈ 0.0 atol = atol rtol = rtol
        if square
            @test Qv[3] ≈ 0.0 atol = atol rtol = rtol
        end
        @test Qv[end] ≈ 1.0 atol = atol rtol = rtol
        tQv = MOI.get(model, MOI.ConstraintPrimal(), cX)
        @test tQv[1] ≈ expectedobjval atol = atol rtol = rtol
        @test tQv[(use_logdet ? 3 : 2):end] ≈ Qv atol = atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ [0.0, 0.0] atol = atol rtol =
            rtol
        if use_logdet
            @test MOI.get(model, MOI.ConstraintPrimal(), vc) ≈ 1.0 atol = atol rtol =
                rtol
        end
        if _supports(config, MOI.ConstraintDual)
            if use_logdet
                @test MOI.get(model, MOI.ConstraintDual(), c) ≈ [1, 1] atol =
                    atol rtol = rtol
                @test MOI.get(model, MOI.ConstraintDual(), vc) ≈ 2 atol = atol rtol =
                    rtol
                dual = square ? [-1, -2, 1, 0, 0, 1] : [-1, -2, 1, 0, 1]
            else
                @test MOI.get(model, MOI.ConstraintDual(), c) ≈ [0.5, 0.5] atol =
                    atol rtol = rtol
                dual =
                    square ? [-1.0, 0.5, 0.0, 0.0, 0.5] : [-1.0, 0.5, 0.0, 0.5]
            end
            @test MOI.get(model, MOI.ConstraintDual(), cX) ≈ dual atol = atol rtol =
                rtol
        end
    end
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
function _test_det_cone_helper(model::MOI.ModelLike, config::Config, detcone)
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        detcone,
    )
    atol = config.atol
    rtol = config.rtol
    square = detcone == MOI.LogDetConeSquare || detcone == MOI.RootDetConeSquare
    use_logdet =
        detcone == MOI.LogDetConeTriangle || detcone == MOI.LogDetConeSquare
    mat = Float64[3 2 1; 2 2 1; 1 1 3]
    matL = Float64[3, 2, 2, 1, 1, 3]
    @requires MOI.supports_incremental_interface(model, false) #=copy_names=#
    @requires MOI.supports(model, MOI.ObjectiveFunction{MOI.SingleVariable}())
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        detcone,
    )

    t = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(t),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    constant_mat = square ? vec(mat) : matL
    constant_vec = use_logdet ? vcat(0, 1, constant_mat) : vcat(0, constant_mat)
    vaf = MOI.VectorAffineFunction(
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, t))],
        constant_vec,
    )
    det_constraint = MOI.add_constraint(model, vaf, detcone(3))
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},detcone}(),
        ) == 1
    end
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        expected_objval = use_logdet ? log(5) : (5^inv(3))
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ expected_objval atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ expected_objval atol =
            atol rtol = rtol
        det_value = MOI.get(model, MOI.ConstraintPrimal(), det_constraint)
        @test det_value[1] ≈ expected_objval atol = atol rtol = rtol
        if use_logdet
            @test det_value[2] ≈ 1.0 atol = atol rtol = rtol
        end
        @test det_value[(use_logdet ? 3 : 2):end] ≈ (square ? vec(mat) : matL) atol =
            atol rtol = rtol
        if _supports(config, MOI.ConstraintDual)
            psd_dual =
                square ? [1, -1, 0, -1, 1.6, -0.2, 0, -0.2, 0.4] :
                [1, -1, 1.6, 0, -0.2, 0.4]
            dual =
                use_logdet ? vcat(-1, log(5) - 3, psd_dual) :
                vcat(-1, psd_dual / 3 * expected_objval)
            @test MOI.get(model, MOI.ConstraintDual(), det_constraint) ≈ dual atol =
                atol rtol = rtol
        end
    end
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
    config::Config,
)
    _test_det_cone_helper_ellipsoid(model, config, true, MOI.LogDetConeTriangle)
    return
end

function setup_test(
    ::typeof(test_conic_LogDetConeTriangle_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0, 1, 0, 1, 1],
            (MOI.SingleVariable, MOI.EqualTo{Float64}) => [2],
            (MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone) =>
                [[1, 1]],
            (MOI.VectorOfVariables, MOI.LogDetConeTriangle) =>
                [[-1, -2, 1, 0, 1]],
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
    config::Config,
)
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
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0, 1, 0, 1, 1],
            (MOI.SingleVariable, MOI.EqualTo{Float64}) => [2],
            (MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone) =>
                [[1, 1]],
            (MOI.VectorAffineFunction{Float64}, MOI.LogDetConeTriangle) =>
                [[-1, -2, 1, 0, 1]],
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
    config::Config,
)
    _test_det_cone_helper_ellipsoid(model, config, true, MOI.LogDetConeSquare)
    return
end

function setup_test(
    ::typeof(test_conic_LogDetConeSquare_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0, 1, 0, 0, 1, 1],
            (MOI.SingleVariable, MOI.EqualTo{Float64}) => [2],
            (MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone) =>
                [[1, 1]],
            (MOI.VectorOfVariables, MOI.LogDetConeSquare) =>
                [[-1, -2, 1, 0, 0, 1]],
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
    config::Config,
)
    _test_det_cone_helper_ellipsoid(model, config, false, MOI.LogDetConeSquare)
    return
end

function setup_test(
    ::typeof(test_conic_LogDetConeSquare_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0, 1, 0, 0, 1, 1],
            (MOI.SingleVariable, MOI.EqualTo{Float64}) => [2],
            (MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone) =>
                [[1, 1]],
            (MOI.VectorAffineFunction{Float64}, MOI.LogDetConeSquare) =>
                [[-1, -2, 1, 0, 0, 1]],
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
    config::Config,
)
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
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1, 1, 0, 1],
            (MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone) =>
                [[0.5, 0.5]],
            (MOI.VectorOfVariables, MOI.RootDetConeTriangle) =>
                [[-1.0, 0.5, 0.0, 0.5]],
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
    config::Config,
)
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
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1, 1, 0, 1],
            (MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone) =>
                [[0.5, 0.5]],
            (MOI.VectorAffineFunction{Float64}, MOI.RootDetConeTriangle) =>
                [[-1.0, 0.5, 0.0, 0.5]],
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
    config::Config,
)
    _test_det_cone_helper_ellipsoid(model, config, true, MOI.RootDetConeSquare)
    return
end

function setup_test(
    ::typeof(test_conic_RootDetConeSquare_VectorOfVariables),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1, 1, 0, 0, 1],
            (MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone) =>
                [[0.5, 0.5]],
            (MOI.VectorOfVariables, MOI.RootDetConeSquare) =>
                [[-1.0, 0.5, 0.0, 0.0, 0.5]],
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
    config::Config,
)
    _test_det_cone_helper_ellipsoid(model, config, false, MOI.RootDetConeSquare)
    return
end

function setup_test(
    ::typeof(test_conic_RootDetConeSquare_VectorAffineFunction),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1, 1, 0, 0, 1],
            (MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone) =>
                [[0.5, 0.5]],
            (MOI.VectorAffineFunction{Float64}, MOI.RootDetConeSquare) =>
                [[-1.0, 0.5, 0.0, 0.0, 0.5]],
        ),
    )
    return
end

"""
    test_conic_LogDetConeTriangle(model::MOI.ModelLike, config::Config)

Test a problem with LogDetConeTriangle.
"""
function test_conic_LogDetConeTriangle(model::MOI.ModelLike, config::Config)
    _test_det_cone_helper(model, config, MOI.LogDetConeTriangle)
    return
end

function setup_test(
    ::typeof(test_conic_LogDetConeTriangle),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [log(5)],
            (MOI.VectorAffineFunction{Float64}, MOI.LogDetConeTriangle) =>
                [[-1, log(5) - 3, 1, -1, 1.6, 0, -0.2, 0.4]],
        ),
    )
    flag = model.eval_variable_constraint_dual
    model.eval_variable_constraint_dual = false
    return () -> model.eval_variable_constraint_dual = flag
end

"""
    test_conic_LogDetConeSquare(model::MOI.ModelLike, config::Config)

Test a problem with LogDetConeSquare.
"""
function test_conic_LogDetConeSquare(model::MOI.ModelLike, config::Config)
    _test_det_cone_helper(model, config, MOI.LogDetConeSquare)
    return
end

function setup_test(
    ::typeof(test_conic_LogDetConeSquare),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [log(5)],
            (MOI.VectorAffineFunction{Float64}, MOI.LogDetConeSquare) =>
                [[-1, log(5) - 3, 1, -1, 0, -1, 1.6, -0.2, 0, -0.2, 0.4]],
        ),
    )
    return
end

"""
    test_conic_RootDetConeTriangle(model::MOI.ModelLike, config::Config)

Test a problem with RootDetConeTriangle.
"""
function test_conic_RootDetConeTriangle(model::MOI.ModelLike, config::Config)
    _test_det_cone_helper(model, config, MOI.RootDetConeTriangle)
    return
end

function setup_test(
    ::typeof(test_conic_RootDetConeTriangle),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [5^inv(3)],
            (MOI.VectorAffineFunction{Float64}, MOI.RootDetConeTriangle) =>
                [vcat(-1, [1, -1, 1.6, 0, -0.2, 0.4] / 3 * (5^inv(3)))],
        ),
    )
    flag = model.eval_variable_constraint_dual
    model.eval_variable_constraint_dual = false
    return () -> model.eval_variable_constraint_dual = flag
end

"""
    test_conic_RootDetConeSquare(model::MOI.ModelLike, config::Config)

Test a problem with RootDetConeSquare.
"""
function test_conic_RootDetConeSquare(model::MOI.ModelLike, config::Config)
    _test_det_cone_helper(model, config, MOI.RootDetConeSquare)
    return
end

function setup_test(
    ::typeof(test_conic_RootDetConeSquare),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [5^inv(3)],
            (MOI.VectorAffineFunction{Float64}, MOI.RootDetConeSquare) => [
                vcat(
                    -1,
                    [1, -1, 0, -1, 1.6, -0.2, 0, -0.2, 0.4] / 3 * (5^inv(3)),
                ),
            ],
        ),
    )
    return
end
