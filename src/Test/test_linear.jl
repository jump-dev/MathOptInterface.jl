"""
    test_linear_integration(model::MOI.ModelLike, config::Config{T}) where {T}

This test checks the integration of a sequence of common operations for linear
programs, including adding and deleting variables, and modifying variable
bounds and the objective function.
"""
function test_linear_integration(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
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
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.GreaterThan{T},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.LessThan{T},
    )
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.EqualTo{T})
    @requires MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{T},
    )
    v = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    cf = MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.(T(1), v), T(0))
    c = MOI.add_constraint(model, cf, MOI.LessThan(T(1)))
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.LessThan{T},
            }(),
        ) == 1
    end
    vc1 = MOI.add_constraint(model, v[1], MOI.GreaterThan(T(0)))
    # We test this after the creation of every `VariableIndex` constraint
    # to ensure a good coverage of corner cases.
    @test vc1.value == v[1].value
    # test fallback
    vc2 = MOI.add_constraint(model, v[2], MOI.GreaterThan(T(0)))
    @test vc2.value == v[2].value
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.GreaterThan{T}}(),
        ) == 2
    end
    # Note: adding some redundant zero coefficients to catch solvers that don't
    # handle duplicate coefficients correctly:
    objf = MOI.ScalarAffineFunction{T}(
        MOI.ScalarAffineTerm{T}.(T[0, 0, -1, 0, 0, 0], [v; v; v]),
        T(0),
    )
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    if _supports(config, MOI.ConstraintFunction)
        vrs = MOI.get(model, MOI.ListOfVariableIndices())
        @test vrs == v || vrs == reverse(v)
        @test objf ≈ MOI.get(
            model,
            MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        )
        @test cf ≈ MOI.get(model, MOI.ConstraintFunction(), c)
        s = MOI.get(model, MOI.ConstraintSet(), c)
        @test s == MOI.LessThan(T(1))
        s = MOI.get(model, MOI.ConstraintSet(), vc1)
        @test s == MOI.GreaterThan(T(0))
        s = MOI.get(model, MOI.ConstraintSet(), vc2)
        @test s == MOI.GreaterThan(T(0))
    end
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(-1), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), T(-1), config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), T[1, 0], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c), T(1), config)
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test ≈(MOI.get(model, MOI.ConstraintDual(), c), T(-1), config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), vc1), T(0), config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), vc2), T(1), config)
        end
    end
    # change objective to Max +x
    objf =
        MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.(T[1, 0], v), T(0))
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.ObjectiveFunction)
        @test objf ≈ MOI.get(
            model,
            MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        )
    end
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(1), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), T(1), config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), T[1, 0], config)
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test ≈(MOI.get(model, MOI.ConstraintDual(), c), T(-1), config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), vc1), T(0), config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), vc2), T(1), config)
        end
    end
    # add new variable to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x,y,z >= 0
    z = MOI.add_variable(model)
    push!(v, z)
    @test v[3] == z
    if _supports(config, MOI.ObjectiveFunction)
        # Test that the modification of v has not affected the model
        vars = map(
            t -> t.variable,
            MOI.get(model, MOI.ConstraintFunction(), c).terms,
        )
        @test vars == [v[1], v[2]] || vars == [v[2], v[1]]
        @test MOI.ScalarAffineFunction{T}(
            [MOI.ScalarAffineTerm{T}(T(1), v[1])],
            T(0),
        ) ≈ MOI.get(
            model,
            MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        )
    end
    vc3 = MOI.add_constraint(model, v[3], MOI.GreaterThan(T(0)))
    @test vc3.value == v[3].value
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.GreaterThan{T}}(),
        ) == 3
    end
    if _supports(config, MOI.ScalarCoefficientChange)
        MOI.modify(model, c, MOI.ScalarCoefficientChange{T}(z, T(1)))
    else
        MOI.delete(model, c)
        cf =
            MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.(T(1), v), T(0))
        c = MOI.add_constraint(model, cf, MOI.LessThan(T(1)))
    end
    MOI.modify(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarCoefficientChange{T}(z, T(2)),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.LessThan{T},
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.GreaterThan{T}}(),
        ) == 3
    end
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus(1)) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(2), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), T(2), config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), [0, 0, 1], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c), T(1), config)
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test ≈(MOI.get(model, MOI.ConstraintDual(), c), T(-2), config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), vc1), T(1), config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), vc2), T(2), config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), vc3), T(0), config)
        end
    end
    # setting lb of x to -1 to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x >= -1
    # y,z >= 0
    MOI.set(model, MOI.ConstraintSet(), vc1, MOI.GreaterThan(T(-1)))
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(3), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), T(3), config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), T[-1, 0, 2], config)
    end
    # put lb of x back to 0 and fix z to zero to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x, y >= 0, z = 0 (vc3)
    MOI.set(model, MOI.ConstraintSet(), vc1, MOI.GreaterThan(T(0)))
    MOI.delete(model, vc3)
    vc3 = MOI.add_constraint(model, v[3], MOI.EqualTo(T(0)))
    @test vc3.value == v[3].value
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.GreaterThan{T}}(),
        ) == 2
    end
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(1), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), T[1, 0, 0], config)
    end
    # modify affine linear constraint set to be == 2 to get :
    # max x + 2z
    # s.t. x + y + z == 2 (c)
    # x,y >= 0, z = 0
    MOI.delete(model, c)
    # Note: adding some redundant zero coefficients to catch solvers that don't
    # handle duplicate coefficients correctly:
    cf = MOI.ScalarAffineFunction{T}(
        MOI.ScalarAffineTerm{T}.(T[0, 0, 0, 1, 1, 1, 0, 0, 0], [v; v; v]),
        T(0),
    )
    c = MOI.add_constraint(model, cf, MOI.EqualTo(T(2)))
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.LessThan{T},
            }(),
        ) == 0
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.EqualTo{T},
            }(),
        ) == 1
    end
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(2), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), T[2, 0, 0], config)
    end
    # modify objective function to x + 2y to get :
    # max x + 2y
    # s.t. x + y + z == 2 (c)
    # x,y >= 0, z = 0
    objf = MOI.ScalarAffineFunction{T}(
        MOI.ScalarAffineTerm{T}.(T[1, 2, 0], v),
        T(0),
    )
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.ObjectiveFunction)
        @test objf ≈ MOI.get(
            model,
            MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        )
    end
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(4), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), T[0, 2, 0], config)
    end
    # add constraint x - y >= 0 (c2) to get :
    # max x+2y
    # s.t. x + y + z == 2 (c)
    # x - y >= 0 (c2)
    # x,y >= 0 (vc1,vc2), z = 0 (vc3)
    cf2 = MOI.ScalarAffineFunction{T}(
        MOI.ScalarAffineTerm{T}.(T[1, -1, 0], v),
        T(0),
    )
    c2 = MOI.add_constraint(model, cf2, MOI.GreaterThan(T(0)))
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.EqualTo{T},
            }(),
        ) == 1
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
                MOI.ScalarAffineFunction{T},
                MOI.LessThan{T},
            }(),
        ) == 0
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.EqualTo{T}}(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.GreaterThan{T}}(),
        ) == 2
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.LessThan{T}}(),
        ) == 0
    end
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(3), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), T(3), config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), T[1, 1, 0], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c), T(2), config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c2), T(0), config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), vc1), T(1), config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), vc2), T(1), config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), vc3), T(0), config)
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus(1)) == MOI.FEASIBLE_POINT
            @test ≈(MOI.get(model, MOI.ConstraintDual(), c), T(-3 // 2), config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), c2), T(1 // 2), config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), vc1), T(0), config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), vc2), T(0), config)
            @test ≈(
                MOI.get(model, MOI.ConstraintDual(), vc3),
                T(3 // 2),
                config,
            )
        end
    end
    if _supports(config, MOI.ConstraintFunction)
        @test MOI.get(model, MOI.ConstraintFunction(), c2) ≈ cf2
    end
    # delete variable x to get :
    # max 2y
    # s.t. y + z == 2
    # - y >= 0
    # y >= 0, z = 0
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.GreaterThan{T}}(),
        ) == 2
    end
    MOI.delete(model, v[1])
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.GreaterThan{T}}(),
        ) == 1
    end
    if _supports(config, MOI.ConstraintFunction)
        err = MOI.InvalidIndex(vc1)
        # vc1 should have been deleted with `v[1]`.
        @test_throws err MOI.get(model, MOI.ConstraintFunction(), vc1)
        @test_throws err MOI.get(model, MOI.ConstraintSet(), vc1)
        @test MOI.get(model, MOI.ConstraintFunction(), c2) ≈
              MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T[-1, 0], [v[2], z]),
            T(0),
        )
        vrs = MOI.get(model, MOI.ListOfVariableIndices())
        @test vrs == [v[2], z] || vrs == [z, v[2]]
        @test MOI.get(
            model,
            MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        ) ≈ MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T[2, 0], [v[2], z]),
            T(0),
        )
    end
end

function setup_test(
    ::typeof(test_linear_integration),
    mock::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 0],
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-1],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 0],
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-1],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[0, 0, 1],
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-2],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[-1, 0, 2]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[1, 0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[2, 0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[0, 2, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 1, 0],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => T[-3//2],
            (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}) => T[1//2],
        ),
    )
    return
end

"""
    test_linear_program(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test solving a simple linear program.
"""
function test_linear_integration_2(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
    # Min -x
    # s.t. x + y <= 1
    # x, y >= 0
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
    @requires MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{T},
    )
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    cf = MOI.ScalarAffineFunction{T}(
        MOI.ScalarAffineTerm{T}.(T(1), [x, y]),
        T(0),
    )
    c = MOI.add_constraint(model, cf, MOI.LessThan(T(1)))
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.LessThan{T},
            }(),
        ) == 1
    end
    vc1 = MOI.add_constraint(model, x, MOI.GreaterThan(T(0)))
    @test vc1.value == x.value
    vc2 = MOI.add_constraint(model, y, MOI.GreaterThan(T(0)))
    @test vc2.value == y.value
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.GreaterThan{T}}(),
        ) == 2
    end
    objf = MOI.ScalarAffineFunction{T}(
        MOI.ScalarAffineTerm{T}.(T[-1, 0], [x, y]),
        T(0),
    )
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(-1) atol = atol rtol =
            rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ T(-1) atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ T(1) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ T(0) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ T(1) atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ T(-1) atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc1) ≈ T(0) atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc2) ≈ T(1) atol = atol rtol =
                rtol
        end
        if _supports(config, MOI.ConstraintBasisStatus)
            @test MOI.get(model, MOI.VariableBasisStatus(), x) == MOI.BASIC
            @test MOI.get(model, MOI.VariableBasisStatus(), y) ==
                  MOI.NONBASIC_AT_LOWER
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c) == MOI.NONBASIC
            _test_attribute_value_type(model, MOI.ConstraintBasisStatus(), c)
            _test_attribute_value_type(model, MOI.VariableBasisStatus(), x)
        end
    end
end

function setup_test(
    ::typeof(test_linear_integration_2),
    mock::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 0],
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-1],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) =>
                    [MOI.NONBASIC],
            ],
            variable_basis_status = [MOI.BASIC, MOI.NONBASIC_AT_LOWER],
        ),
    )
    return
end

"""
    test_linear_inactive_bounds(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

This test checks solving a linear program with bounds that are not binding.
"""
function test_linear_inactive_bounds(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
    # min  x
    # s.t. x >= 0
    #      x >= 3
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
        MOI.ScalarAffineFunction{T},
        MOI.LessThan{T},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{T},
    )
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.LessThan{T})
    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    vc = MOI.add_constraint(model, x, MOI.GreaterThan(T(0)))
    @test vc.value == x.value
    cf = MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm{T}(T(1), x)], T(0))
    c = MOI.add_constraint(model, cf, MOI.GreaterThan(T(3)))
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.GreaterThan{T}}(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.GreaterThan{T},
            }(),
        ) == 1
    end
    objf = MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm{T}(T(1), x)], T(0))
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3 atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 3 atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintBasisStatus)
            @test MOI.get(model, MOI.VariableBasisStatus(), x) == MOI.BASIC
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c) == MOI.NONBASIC
        end
    end
    # max  x
    # s.t. x <= 0
    #      x <= 3
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    vc = MOI.add_constraint(model, x, MOI.LessThan(T(0)))
    @test vc.value == x.value
    cf = MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm{T}(T(1), x)], T(0))
    c = MOI.add_constraint(model, cf, MOI.LessThan(T(3)))
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.LessThan{T}}(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.LessThan{T},
            }(),
        ) == 1
    end
    objf = MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm{T}(T(1), x)], T(0))
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(0) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ T(0) atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintBasisStatus)
            @test MOI.get(model, MOI.VariableBasisStatus(), x) ==
                  MOI.NONBASIC_AT_UPPER
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c) == MOI.BASIC
        end
    end
end

function setup_test(
    ::typeof(test_linear_inactive_bounds),
    mock::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [3],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}) =>
                    [MOI.NONBASIC],
            ],
            variable_basis_status = [MOI.BASIC],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) =>
                    [MOI.BASIC],
            ],
            variable_basis_status = [MOI.NONBASIC_AT_UPPER],
        ),
    )
    return
end

"""
    test_linear_LessThan_and_GreaterThan(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test solving a linear program with ScalarAffineFunction-in-LessThan and
GreaterThan constraints.
"""
function test_linear_LessThan_and_GreaterThan(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
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
        MOI.ScalarAffineFunction{T},
        MOI.LessThan{T},
    )
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    # Min  x - y
    # s.t. T(0) <= x          (c1)
    #             y <= T(0)   (c2)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T[1, -1], [x, y]),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    fx = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), x)], T(0))
    c1 = MOI.add_constraint(model, fx, MOI.GreaterThan(T(0)))
    fy = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(1), y)], T(0))
    c2 = MOI.add_constraint(model, fy, MOI.LessThan(T(0)))
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(0) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ T(0) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ T(0) atol = atol rtol =
            rtol
    end
    # Min  x - y
    # s.t. T(100) <= x
    #               y <= T(0)
    MOI.set(model, MOI.ConstraintSet(), c1, MOI.GreaterThan(T(100)))
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(100) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ T(100) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ T(0) atol = atol rtol =
            rtol
    end
    # Min  x - y
    # s.t. T(100) <= x
    #               y <= T(-100)
    MOI.set(model, MOI.ConstraintSet(), c2, MOI.LessThan(T(-100)))
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(200) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ T(100) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ T(-100) atol = atol rtol =
            rtol
    end
    return
end

function setup_test(
    ::typeof(test_linear_LessThan_and_GreaterThan),
    mock::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[100, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[100, -100]),
    )
    return
end

"""
    test_linear_integration_modification(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test a range of problem modifications for a linear program.
"""
function test_linear_integration_modification(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
    # Start from simple LP
    # Solve it
    # Copy and solve again
    # Chg coeff, solve, change back solve
    # del constr and solve
    # del var and solve
    #   maximize x + y
    #
    #   s.t. 2 x + 1 y <= 4
    #        1 x + 2 y <= 4
    #        x >= 0, y >= 0
    #
    #   solution: x = 1.3333333, y = 1.3333333, objv = 2.66666666
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
        MOI.ScalarAffineFunction{T},
        MOI.LessThan{T},
    )
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    cf1 = MOI.ScalarAffineFunction{T}(
        MOI.ScalarAffineTerm{T}.(T[2, 1], [x, y]),
        T(0),
    )
    cf2 = MOI.ScalarAffineFunction{T}(
        MOI.ScalarAffineTerm{T}.(T[1, 2], [x, y]),
        T(0),
    )
    c1 = MOI.add_constraint(model, cf1, MOI.LessThan(T(4)))
    c2 = MOI.add_constraint(model, cf2, MOI.LessThan(T(4)))
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.LessThan{T},
            }(),
        ) == 2
    end
    vc1 = MOI.add_constraint(model, x, MOI.GreaterThan(T(0)))
    @test vc1.value == x.value
    vc2 = MOI.add_constraint(model, y, MOI.GreaterThan(T(0)))
    @test vc2.value == y.value
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.GreaterThan{T}}(),
        ) == 2
    end
    objf = MOI.ScalarAffineFunction{T}(
        MOI.ScalarAffineTerm{T}.(T(1), [x, y]),
        T(0),
    )
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(8 // 3) atol = atol rtol =
            rtol
        @test ≈(
            MOI.get(model, MOI.VariablePrimal(), [x, y]),
            T[4//3, 4//3],
            config,
        )
    end
    # copy and solve again
    # missing test
    # change coeff
    #   maximize x + y
    #
    #   s.t. 2 x + 3 y <= 4
    #        1 x + 2 y <= 4
    #        x >= 0, y >= 0
    #
    #   solution: x = 2, y = 0, objv = 2
    if _supports(config, MOI.ScalarCoefficientChange)
        MOI.modify(model, c1, MOI.ScalarCoefficientChange(y, T(3)))
    else
        MOI.delete(model, c1)
        cf1 = MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T[2, 3], [x, y]),
            T(0),
        )
        c1 = MOI.add_constraint(model, cf1, MOI.LessThan(T(4)))
    end
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(2), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), [x, y]), T[2, 0], config)
    end
    # delconstrs and solve
    #   maximize x + y
    #
    #   s.t. 1 x + 2 y <= 4
    #        x >= 0, y >= 0
    #
    #   solution: x = 4, y = 0, objv = 4
    MOI.delete(model, c1)
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), 4, config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), [x, y]), T[4, 0], config)
    end
    # delvars and solve
    #   maximize y
    #
    #   s.t.  2 y <= 4
    #           y >= 0
    #
    #   solution: y = 2, objv = 2
    MOI.delete(model, x)
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(2) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ T(2) atol = atol rtol =
            rtol
    end
end

function setup_test(
    ::typeof(test_linear_integration_modification),
    mock::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[4//3, 4//3]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[2, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[4, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[2]),
    )
    return
end

"""
    test_linear_modify_GreaterThan_and_LessThan_constraints(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test modifying linear constraints.
"""
function test_linear_modify_GreaterThan_and_LessThan_constraints(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
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
        MOI.ScalarAffineFunction{T},
        MOI.LessThan{T},
    )
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    # Min  x - y
    # s.t. T(0) <= x          (c1)
    #             y <= T(0)   (c2)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T[1, -1], [x, y]),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    fx = convert(MOI.ScalarAffineFunction{T}, x)
    c1 = MOI.add_constraint(model, fx, MOI.GreaterThan(T(0)))
    fy = convert(MOI.ScalarAffineFunction{T}, y)
    c2 = MOI.add_constraint(model, fy, MOI.LessThan(T(0)))
    if _supports(config, MOI.ConstraintFunction)
        @test MOI.get(model, MOI.ConstraintFunction(), c1) ≈ fx
        @test MOI.get(model, MOI.ConstraintSet(), c1) == MOI.GreaterThan(T(0))
        @test MOI.get(model, MOI.ConstraintFunction(), c2) ≈ fy
        @test MOI.get(model, MOI.ConstraintSet(), c2) == MOI.LessThan(T(0))
    end
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(0), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(0), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), y), T(0), config)
    end
    # Min  x - y
    # s.t. T(100) <= x
    #               y <= T(0)
    MOI.set(model, MOI.ConstraintSet(), c1, MOI.GreaterThan(T(100)))
    if _supports(config, MOI.ConstraintFunction)
        @test MOI.get(model, MOI.ConstraintFunction(), c1) ≈ fx
        @test MOI.get(model, MOI.ConstraintSet(), c1) == MOI.GreaterThan(T(100))
        @test MOI.get(model, MOI.ConstraintFunction(), c2) ≈ fy
        @test MOI.get(model, MOI.ConstraintSet(), c2) == MOI.LessThan(T(0))
    end
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(100), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(100), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), y), T(0), config)
    end
    # Min  x - y
    # s.t. T(100) <= x
    #               y <= T(-100)
    MOI.set(model, MOI.ConstraintSet(), c2, MOI.LessThan(T(-100)))
    if _supports(config, MOI.ConstraintFunction)
        @test MOI.get(model, MOI.ConstraintFunction(), c1) ≈ fx
        @test MOI.get(model, MOI.ConstraintSet(), c1) == MOI.GreaterThan(T(100))
        @test MOI.get(model, MOI.ConstraintFunction(), c2) ≈ fy
        @test MOI.get(model, MOI.ConstraintSet(), c2) == MOI.LessThan(T(-100))
    end
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(200), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(100), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), y), T(-100), config)
    end
end

function setup_test(
    ::typeof(test_linear_modify_GreaterThan_and_LessThan_constraints),
    mock::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[100, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[100, -100]),
    )
    return
end

"""
    test_linear_VectorAffineFunction(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test solving a linear program provided in vector-form.
"""
function test_linear_VectorAffineFunction(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
    # Min  x - y
    # s.t. bx <= x          (c1)
    #             y <= by   (c2)
    #
    # or, in more detail,
    #
    # Min    1 x - 1 y
    # s.t. - 1 x       <= - bx  (z)   (c1)
    #              1 y <=   by  (w)   (c2)
    #
    # with generic dual
    #
    # Max  - bx z + by w
    # s.t. -    z        == - 1     (c1)
    #                  w ==   1     (c2)
    # i.e. z == w == 1
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
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    # Min  x - y
    # s.t. T(0) <= x          (c1)
    #             y <= T(0)   (c2)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T[1, -1], [x, y]),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    c1 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction{T}(
            [MOI.VectorAffineTerm{T}(1, MOI.ScalarAffineTerm{T}(T(1), x))],
            T[0],
        ),
        MOI.Nonnegatives(1),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction{T}(
            [MOI.VectorAffineTerm{T}(1, MOI.ScalarAffineTerm{T}(T(1), y))],
            T[0],
        ),
        MOI.Nonpositives(1),
    )
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(0), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(0), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), y), T(0), config)
    end
    # Min  x - y
    # s.t. T(100) <= x
    #               y <= T(0)
    if _supports(config, MOI.VectorConstantChange)
        MOI.modify(model, c1, MOI.VectorConstantChange(T[-100]))
    else
        MOI.delete(model, c1)
        c1 = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{T}(
                [MOI.VectorAffineTerm{T}(1, MOI.ScalarAffineTerm{T}(T(1), x))],
                T[-100],
            ),
            MOI.Nonnegatives(1),
        )
    end
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(100), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(100), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), y), T(0), config)
    end
    # Min  x - y
    # s.t. T(100) <= x
    #               y <= T(-100)
    if _supports(config, MOI.VectorConstantChange)
        MOI.modify(model, c2, MOI.VectorConstantChange(T[100]))
    else
        MOI.delete(model, c2)
        c2 = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{T}(
                [MOI.VectorAffineTerm{T}(1, MOI.ScalarAffineTerm{T}(T(1), y))],
                T[100],
            ),
            MOI.Nonpositives(1),
        )
    end
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(200), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(100), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), y), T(-100), config)
    end
end

function setup_test(
    ::typeof(test_linear_VectorAffineFunction),
    mock::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[100, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[100, -100]),
    )
    return
end

"""
    test_linear_INFEASIBLE(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test solving an infeasible linear program.
"""
function test_linear_INFEASIBLE(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
    # min x
    # s.t. 2x+y <= -1
    # x,y >= 0
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
    @requires MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{T},
    )
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T[2, 1], [x, y]),
            T(0),
        ),
        MOI.LessThan(T(-1)),
    )
    bndx = MOI.add_constraint(model, x, MOI.GreaterThan(T(0)))
    @test bndx.value == x.value
    bndy = MOI.add_constraint(model, y, MOI.GreaterThan(T(0)))
    @test bndy.value == y.value
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm{T}(T(1), x)], T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) in
              (config.infeasible_status, MOI.INFEASIBLE_OR_UNBOUNDED)
        has_certificate =
            MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
        if _supports(config, MOI.ConstraintDual) && has_certificate
            # solver returned an infeasibility ray
            @test MOI.get(model, MOI.ResultCount()) >= 1
            cd = MOI.get(model, MOI.ConstraintDual(), c)
            @test cd < -atol
            # TODO: farkas dual on bounds - see #127
            # xd = MOI.get(model, MOI.ConstraintDual(), bndx)
            # yd = MOI.get(model, MOI.ConstraintDual(), bndy)
            # @test xd > atol
            # @test yd > atol
            # @test yd ≈ -cd atol=atol rtol=rtol
            # @test xd ≈ -2cd atol=atol rtol=rtol
        end
    end
end

function setup_test(
    ::typeof(test_linear_INFEASIBLE),
    mock::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            MOI.NO_SOLUTION,
            MOI.INFEASIBILITY_CERTIFICATE,
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-1],
        ),
    )
    return
end

"""
    test_linear_DUAL_INFEASIBLE(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test solving a problem that is dual infeasible (unbounded).
"""
function test_linear_DUAL_INFEASIBLE(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    # min -x-y
    # s.t. -x+2y <= 0
    # x,y >= 0
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
    @requires MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{T},
    )
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T[-1, 2], [x, y]),
            T(0),
        ),
        MOI.LessThan(T(0)),
    )
    vc1 = MOI.add_constraint(model, x, MOI.GreaterThan(T(0)))
    @test vc1.value == x.value
    vc2 = MOI.add_constraint(model, y, MOI.GreaterThan(T(0)))
    @test vc2.value == y.value
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T(-1), [x, y]),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.DUAL_INFEASIBLE ||
              MOI.get(model, MOI.TerminationStatus()) ==
              MOI.INFEASIBLE_OR_UNBOUNDED
        if MOI.get(model, MOI.PrimalStatus()) == MOI.INFEASIBILITY_CERTIFICATE
            # solver returned an unbounded ray
            @test MOI.get(model, MOI.ResultCount()) >= 1
        end
    end
end

function setup_test(
    ::typeof(test_linear_DUAL_INFEASIBLE),
    mock::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.DUAL_INFEASIBLE,
            MOI.INFEASIBILITY_CERTIFICATE,
        ),
    )
    return
end

"""
    test_linear_DUAL_INFEASIBLE_2(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test solving a linear program that is dual infeasible (unbounded).
"""
function test_linear_DUAL_INFEASIBLE_2(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
    # min -x-y
    # s.t. x-y == 0
    # x,y >= 0
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
    @requires MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{T},
    )
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T[1, -1], [x, y]),
            T(0),
        ),
        MOI.EqualTo(T(0)),
    )
    vc1 = MOI.add_constraint(model, x, MOI.GreaterThan(T(0)))
    @test vc1.value == x.value
    vc2 = MOI.add_constraint(model, y, MOI.GreaterThan(T(0)))
    @test vc2.value == y.value
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T(-1), [x, y]),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.DUAL_INFEASIBLE ||
              MOI.get(model, MOI.TerminationStatus()) ==
              MOI.INFEASIBLE_OR_UNBOUNDED
        if MOI.get(model, MOI.PrimalStatus()) == MOI.INFEASIBILITY_CERTIFICATE
            # solver returned an unbounded ray
            @test MOI.get(model, MOI.ResultCount()) >= 1
            ray = MOI.get(model, MOI.VariablePrimal(), [x, y])
            @test ray[1] ≈ ray[2] atol = atol rtol = rtol
        end
    end
end

function setup_test(
    ::typeof(test_linear_DUAL_INFEASIBLE_2),
    mock::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.DUAL_INFEASIBLE,
            (MOI.INFEASIBILITY_CERTIFICATE, T[1, 1]),
        ),
    )
    return
end

"""
    test_linear_add_constraints(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test solving a linear program constructed using the plural `add_constraints`
function.
"""
function test_linear_add_constraints(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    #   maximize 1000 x + 350 y
    #
    #       s.t.                x >= 30
    #                           y >= 0
    #                 x -   1.5 y >= 0
    #            12   x +   8   y <= 1000
    #            1000 x + 300   y <= 70000
    #
    #   solution: (59.0909, 36.3636)
    #   objv: 71818.1818
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
        MOI.ScalarAffineFunction{T},
        MOI.LessThan{T},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{T},
    )
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    vc12 = MOI.add_constraints(
        model,
        [x, y],
        [MOI.GreaterThan(T(30)), MOI.GreaterThan(T(0))],
    )
    @test vc12[1].value == x.value
    @test vc12[2].value == y.value
    c1 = MOI.add_constraints(
        model,
        [
            MOI.ScalarAffineFunction{T}(
                MOI.ScalarAffineTerm{T}.(T[1, -3//2], [x, y]),
                T(0),
            ),
        ],
        [MOI.GreaterThan(T(0))],
    )
    c23 = MOI.add_constraints(
        model,
        [
            MOI.ScalarAffineFunction{T}(
                MOI.ScalarAffineTerm{T}.(T[12, 8], [x, y]),
                T(0),
            ),
            MOI.ScalarAffineFunction{T}(
                MOI.ScalarAffineTerm{T}.(T[1_000, 300], [x, y]),
                T(0),
            ),
        ],
        [MOI.LessThan(T(1_000)), MOI.LessThan(T(70_000))],
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T[1_000, 350], [x, y]),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(790_000 // 11), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(650 // 11), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), y), T(400 // 11), config)
        if _supports(config, MOI.ConstraintBasisStatus)
            @test MOI.get(model, MOI.VariableBasisStatus(), x) == MOI.BASIC
            @test MOI.get(model, MOI.VariableBasisStatus(), y) == MOI.BASIC
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c1[1]) ==
                  MOI.BASIC
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c23[1]) ==
                  MOI.NONBASIC
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c23[2]) ==
                  MOI.NONBASIC
        end
    end
end

function setup_test(
    ::typeof(test_linear_add_constraints),
    mock::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[650//11, 400//11],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) =>
                    [MOI.NONBASIC, MOI.NONBASIC],
                (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}) =>
                    [MOI.BASIC],
            ],
            variable_basis_status = [MOI.BASIC, MOI.BASIC],
        ),
    )
    return
end

"""
    test_linear_integration_Interval(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

This test checks a variety of properties of a model with
ScalarAffineFunction-in-Interval constraints.
"""
function test_linear_integration_Interval(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
    #   maximize x + y
    #
    #       s.t.  5 <= x + y <= 10
    #                  x,  y >= 0
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.Interval{T},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{T},
    )
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    vc = MOI.add_constraints(
        model,
        [x, y],
        [MOI.GreaterThan(T(0)), MOI.GreaterThan(T(0))],
    )
    @test vc[1].value == x.value
    @test vc[2].value == y.value
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T(1), [x, y]),
            T(0),
        ),
        MOI.Interval(T(5), T(10)),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T(1), [x, y]),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(10), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), T(10), config)
        end
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c), T(10), config)
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test ≈(MOI.get(model, MOI.ConstraintDual(), c), T(-1), config)
        end
        if _supports(config, MOI.ConstraintBasisStatus)
            # There are multiple optimal bases. Either x or y can be in the
            # optimal basis.
            @test (
                MOI.get(model, MOI.VariableBasisStatus(), x) == MOI.BASIC ||
                MOI.get(model, MOI.VariableBasisStatus(), y) == MOI.BASIC
            )
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c) ==
                  MOI.NONBASIC_AT_UPPER
        end
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T(1), [x, y]),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(5) atol = atol rtol =
            rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ T(5) atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 5 atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ T(1) atol = atol rtol =
                rtol
        end
        if _supports(config, MOI.ConstraintBasisStatus)
            # There are multiple optimal bases. Either x or y can be in the
            # optimal basis.
            @test (
                MOI.get(model, MOI.VariableBasisStatus(), x) == MOI.BASIC ||
                MOI.get(model, MOI.VariableBasisStatus(), y) == MOI.BASIC
            )
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c) ==
                  MOI.NONBASIC_AT_LOWER
        end
    end
    MOI.set(model, MOI.ConstraintSet(), c, MOI.Interval(T(2), T(12)))
    if _supports(config, MOI.ConstraintSet)
        @test MOI.get(model, MOI.ConstraintSet(), c) ==
              MOI.Interval(T(2), T(12))
    end
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(2) atol = atol rtol =
            rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ T(2) atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ T(2) atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintBasisStatus)
            # There are multiple optimal bases. Either x or y can be in the
            # optimal basis.
            @test (
                MOI.get(model, MOI.VariableBasisStatus(), x) == MOI.BASIC ||
                MOI.get(model, MOI.VariableBasisStatus(), y) == MOI.BASIC
            )
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c) ==
                  MOI.NONBASIC_AT_LOWER
        end
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ T(1) atol = atol rtol =
                rtol
        end
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T(1), [x, y]),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(12) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 12 atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintBasisStatus)
            # There are multiple optimal bases. Either x or y can be in the
            # optimal basis.
            @test (
                MOI.get(model, MOI.VariableBasisStatus(), x) == MOI.BASIC ||
                MOI.get(model, MOI.VariableBasisStatus(), y) == MOI.BASIC
            )
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c) ==
                  MOI.NONBASIC_AT_UPPER
        end
    end
end

function setup_test(
    ::typeof(test_linear_integration_Interval),
    mock::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[5, 5],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{T}, MOI.Interval{T}) =>
                    [MOI.NONBASIC_AT_UPPER],
            ],
            variable_basis_status = [MOI.BASIC, MOI.BASIC],
            (MOI.ScalarAffineFunction{T}, MOI.Interval{T}) => T[-1],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[5//2, 5//2],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{T}, MOI.Interval{T}) =>
                    [MOI.NONBASIC_AT_LOWER],
            ],
            variable_basis_status = [MOI.BASIC, MOI.BASIC],
            (MOI.ScalarAffineFunction{T}, MOI.Interval{T}) => T[1],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1, 1],
            (MOI.ScalarAffineFunction{T}, MOI.Interval{T}) => T[1],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{T}, MOI.Interval{T}) =>
                    [MOI.NONBASIC_AT_LOWER],
            ],
            variable_basis_status = [MOI.BASIC, MOI.BASIC],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[6, 6],
            (MOI.ScalarAffineFunction{T}, MOI.Interval{T}) => T[-1],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{T}, MOI.Interval{T}) =>
                    [MOI.NONBASIC_AT_UPPER],
            ],
            variable_basis_status = [MOI.BASIC, MOI.BASIC],
        ),
    )
    return
end

"""
    test_linear_Interval_inactive(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

This test checks linear programs with a non-binding
ScalarAffineFunction-in-Interval constraint.
"""
function test_linear_Interval_inactive(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
    #   minimize x + y
    #
    #       s.t.  -1 <= x + y <= 10
    #                   x,  y >= 0
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.Interval{T},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{T},
    )
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    vc = MOI.add_constraints(
        model,
        [x, y],
        [MOI.GreaterThan(T(0)), MOI.GreaterThan(T(0))],
    )
    @test vc[1].value == x.value
    @test vc[2].value == y.value
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T(1), [x, y]),
            T(0),
        ),
        MOI.Interval(T(-1), T(10)),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T(1), [x, y]),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(0) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ T(0) atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ T(0) atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc[1]) ≈ T(1) atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc[2]) ≈ T(1) atol = atol rtol =
                rtol
        end
        if _supports(config, MOI.ConstraintBasisStatus)
            @test (
                MOI.get(model, MOI.VariableBasisStatus(), x) ==
                MOI.NONBASIC_AT_LOWER
            )
            @test (
                MOI.get(model, MOI.VariableBasisStatus(), y) ==
                MOI.NONBASIC_AT_LOWER
            )
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c) == MOI.BASIC
        end
    end
end

function setup_test(
    ::typeof(test_linear_Interval_inactive),
    mock::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[0, 0],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{T}, MOI.Interval{T}) =>
                    [MOI.BASIC],
            ],
            variable_basis_status = [
                MOI.NONBASIC_AT_LOWER,
                MOI.NONBASIC_AT_LOWER,
            ],
            (MOI.ScalarAffineFunction{T}, MOI.Interval{T}) => [0],
        ),
    )
    return
end

"""
    test_linear_transform(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

This test checks calling `MOI.transform` to flip the sense of constraints.

It starts with:
```
min x + y
st   x + y >= 1
     x + y >= 2
     sol: x+y = 2 (degenerate)
```
with the dual problem:
```
max  w + 2z
st   w +  z == 1
     w +  z == 1
     w, z >= 0
sol: z = 1, w = 0
```
Then it tranforms problem into:
```
min x + y
st   x + y >= 1
     x + y <= 2
sol: x+y = 1 (degenerate)
```
with the dual problem:
```
max  w + 2z
st   w +  z == 1
     w +  z == 1
     w >= 0, z <= 0
sol: w = 1, z = 0
```
"""
function test_linear_transform(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
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
        MOI.ScalarAffineFunction{T},
        MOI.LessThan{T},
    )
    v = MOI.add_variables(model, 2)
    c1 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.(T(1), v), T(0)),
        MOI.GreaterThan(T(1)),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.(T(1), v), T(0)),
        MOI.GreaterThan(T(2)),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.(T(1), v), T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(2) atol = atol rtol =
            rtol
    end
    c3 = MOI.transform(model, c2, MOI.LessThan(T(2)))
    @test isa(
        c3,
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
    )
    @test MOI.is_valid(model, c2) == false
    @test MOI.is_valid(model, c3) == true
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(1) atol = atol rtol =
            rtol
    end
end

function setup_test(
    ::typeof(test_linear_transform),
    mock::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[1, 1]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[1//2, 1//2]),
    )
    return
end

"""
    test_linear_INFEASIBLE_2(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test solving an infeasible linear program.

The model is
```
 min  x
s.t. 2x - 3y <= -7
           y <=  2
      x,   y >= 0
```
"""
function test_linear_INFEASIBLE_2(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
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
    @requires MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{T},
    )
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    c1 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T[2, -3], [x, y]),
            T(0),
        ),
        MOI.LessThan(T(-7)),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm{T}(T(1), y)], T(0)),
        MOI.LessThan(T(2)),
    )
    bndx = MOI.add_constraint(model, x, MOI.GreaterThan(T(0)))
    @test bndx.value == x.value
    bndy = MOI.add_constraint(model, y, MOI.GreaterThan(T(0)))
    @test bndy.value == y.value
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm{T}(T(1), x)], T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) in
              (config.infeasible_status, MOI.INFEASIBLE_OR_UNBOUNDED)
        has_certificate =
            MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
        if _supports(config, MOI.ConstraintDual) && has_certificate
            @test MOI.get(model, MOI.ResultCount()) >= 1
            cd1 = MOI.get(model, MOI.ConstraintDual(), c1)
            cd2 = MOI.get(model, MOI.ConstraintDual(), c2)
            bndxd = MOI.get(model, MOI.ConstraintDual(), bndx)
            bndyd = MOI.get(model, MOI.ConstraintDual(), bndy)
            @test cd1 < -atol
            @test cd2 < -atol
            @test -3 * cd1 + cd2 ≈ -bndyd atol = atol rtol = rtol
            @test 2 * cd1 ≈ -bndxd atol = atol rtol = rtol
            @test -7 * cd1 + 2 * cd2 > atol
        end
    end
end

function setup_test(
    ::typeof(test_linear_INFEASIBLE_2),
    mock::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.INFEASIBLE,
            MOI.NO_SOLUTION,
            MOI.INFEASIBILITY_CERTIFICATE,
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-1, -1],
        ),
    )
    return
end

"""
    test_linear_FEASIBILITY_SENSE(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test solving a linear program with no objective function.

The problem is
```
Find x, y
s.t. 2x + 3y >= 1
     x - y == 0
```
"""
function test_linear_FEASIBILITY_SENSE(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.EqualTo{T},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.GreaterThan{T},
    )
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    c1 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T[2, 3], [x, y]),
            T(0),
        ),
        MOI.GreaterThan(T(1)),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T[1, -1], [x, y]),
            T(0),
        ),
        MOI.EqualTo(T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.FEASIBILITY_SENSE
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.ResultCount()) > 0
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        xsol = MOI.get(model, MOI.VariablePrimal(), x)
        ysol = MOI.get(model, MOI.VariablePrimal(), y)
        c1sol = 2 * xsol + 3 * ysol
        @test c1sol >= 1 || isapprox(c1sol, T(1), atol = atol, rtol = rtol)
        @test xsol - ysol ≈ T(0) atol = atol rtol = rtol
        c1primval = MOI.get(model, MOI.ConstraintPrimal(), c1)
        @test c1primval >= 1 || isapprox(c1sol, T(1), atol = atol, rtol = rtol)
        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ T(0) atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ T(0) atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ T(0) atol = atol rtol =
                rtol
        end
    end
end

function setup_test(
    ::typeof(test_linear_FEASIBILITY_SENSE),
    mock::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1//5, 1//5],
            (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}) => T[0],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => T[0],
        ),
    )
    return
end

"""
    test_linear_integration_delete_variables(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

This function tests calling `delete(::ModelLike, ::Vector{VariableIndex})`.
"""
function test_linear_integration_delete_variables(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
    # max x + 2y + 3z + 4
    # s.t. 3x + 2y + z <= 2
    #      x, y, z >= 0
    #      z <= 1
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
    @requires MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{T},
    )
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.LessThan{T})
    x, y, z = MOI.add_variables(model, 3)
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T[3, 2, 1], [x, y, z]),
            T(0),
        ),
        MOI.LessThan(T(2)),
    )
    clbx = MOI.add_constraint(model, x, MOI.GreaterThan(T(0)))
    @test clbx.value == x.value
    clby = MOI.add_constraint(model, y, MOI.GreaterThan(T(0)))
    @test clby.value == y.value
    clbz = MOI.add_constraint(model, z, MOI.GreaterThan(T(0)))
    @test clbz.value == z.value
    cubz = MOI.add_constraint(model, z, MOI.LessThan(T(1)))
    @test cubz.value == z.value
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction{T}(
            MOI.ScalarAffineTerm{T}.(T[1, 2, 3], [x, y, z]),
            T(4),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(8), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), T(8), config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(0), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), y), T(1 // 2), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), z), T(1), config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c), T(2), config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), clbx), T(0), config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), clby), T(1 // 2), config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), clbz), T(1), config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), cubz), T(1), config)
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test ≈(MOI.get(model, MOI.ConstraintDual(), c), T(-1), config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), clbx), T(2), config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), clby), T(0), config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), clbz), T(0), config)
            @test ≈(MOI.get(model, MOI.ConstraintDual(), cubz), T(-2), config)
            if _supports(config, MOI.ConstraintBasisStatus)
                @test MOI.get(model, MOI.VariableBasisStatus(), x) ==
                      MOI.NONBASIC_AT_LOWER
                @test MOI.get(model, MOI.VariableBasisStatus(), y) == MOI.BASIC
                @test MOI.get(model, MOI.VariableBasisStatus(), z) ==
                      MOI.NONBASIC_AT_UPPER
                @test MOI.get(model, MOI.ConstraintBasisStatus(), c) ==
                      MOI.NONBASIC
            end
        end
    end
    MOI.delete(model, [x, z])
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 6 atol = atol rtol = rtol
        if _supports(config, MOI.DualObjectiveValue)
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ 6 atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ T(1) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ T(2) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), clby) ≈ T(1) atol = atol rtol =
            rtol
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ T(-1) atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), clby) ≈ T(0) atol = atol rtol =
                rtol
        end
    end
end

function setup_test(
    ::typeof(test_linear_integration_delete_variables),
    mock::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[0, 1//2, 1],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) =>
                    [MOI.NONBASIC],
            ],
            variable_basis_status = [
                MOI.NONBASIC_AT_LOWER,
                MOI.BASIC,
                MOI.NONBASIC_AT_UPPER,
            ],
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-1],
            (MOI.VariableIndex, MOI.GreaterThan{T}) => T[2, 0, 0],
            (MOI.VariableIndex, MOI.LessThan{T}) => T[-2],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[1],
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => T[-1],
            (MOI.VariableIndex, MOI.GreaterThan{T}) => T[0],
        ),
    )
    # test_linear_integration_delete_variables has double variable bounds for
    # the z variable
    mock.eval_variable_constraint_dual = false
    function reset_function()
        mock.eval_variable_constraint_dual = true
        return
    end
    return reset_function
end

"""
    test_linear_VectorAffineFunction_empty_row(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

This test checks solving a problem given a `VectorAffineFunction` with an empty
row equivalent to `0 == 0`.

Models not supporting `VectorAffineFunction`-in-`Zeros` should use a bridge.
"""
function test_linear_VectorAffineFunction_empty_row(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
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
    x = MOI.add_variables(model, 1)
    # Create a VectorAffineFunction with two rows, but only one term, belonging
    # to the second row. The first row, which is empty, is essentially a
    # constraint that 0 == 0.
    c = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction{T}(
            MOI.VectorAffineTerm{T}.(2, MOI.ScalarAffineTerm{T}.(T[1], x)),
            zeros(T, 2),
        ),
        MOI.Zeros(2),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.(T[0], x), T(0)),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(0), config)
        if _supports(config, MOI.DualObjectiveValue)
            @test ≈(MOI.get(model, MOI.DualObjectiveValue()), T(0), config)
        end
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x[1]), T(0), config)
        if _supports(config, MOI.ConstraintDual)
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
    end
end

function setup_test(
    ::typeof(test_linear_VectorAffineFunction_empty_row),
    mock::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            T[0],
            (MOI.VectorAffineFunction{T}, MOI.Zeros) => [T[0, 0]],
        ),
    )
    return
end

"""
    test_linear_VariablePrimalStart_partial(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

This test checks solving a problem with a partial solution provided via
VariablePrimalStart.

This test can be passed by solvers that don't support VariablePrimalStart
because copy_to drops start information with a warning.

The problem is
```
 max 2x + y
s.t. x + y <= 1
     x, y >= 0
```
where `x` starts at `T(1)`. Start point for `y` is unspecified.
"""
function test_linear_VariablePrimalStart_partial(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    atol = config.atol
    rtol = config.rtol
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    MOI.set(model, MOI.VariablePrimalStart(), x, T(1))
    MOI.add_constraint(model, x, MOI.GreaterThan(T(0)))
    MOI.add_constraint(model, y, MOI.GreaterThan(T(0)))
    obj = MOI.ScalarAffineFunction{T}(
        MOI.ScalarAffineTerm{T}.(T[2, 1], [x, y]),
        T(0),
    )
    MOI.set(model, MOI.ObjectiveFunction{typeof(obj)}(), obj)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    x_plus_y = MOI.ScalarAffineFunction{T}(
        MOI.ScalarAffineTerm{T}.(T(1), [x, y]),
        T(0),
    )
    MOI.add_constraint(model, x_plus_y, MOI.LessThan(T(1)))
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(2) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ T(1) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ T(0) atol = atol rtol =
            rtol
    end
end

function setup_test(
    ::typeof(test_linear_VariablePrimalStart_partial),
    mock::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[1, 0]),
    )
    return
end

"""
    test_linear_integer_integration(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Run an integration test on the MILP:
```
maximize 1.1x + 2 y + 5 z
s.t.  x + y + z <= 10
      x + 2 y + z <= 15
      x is continuous: 0 <= x <= 5
      y is integer: 0 <= y <= 10
      z is binary
```
"""
function test_linear_integer_integration(
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
    @requires MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{T},
    )
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.ZeroOne)
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.Integer)
    v = MOI.add_variables(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3
    cf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T(1), v), T(0))
    c = MOI.add_constraint(model, cf, MOI.LessThan(T(10)))
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.LessThan{T},
            }(),
        ) == 1
    end
    cf2 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[1, 2, 1], v), T(0))
    c2 = MOI.add_constraint(model, cf2, MOI.LessThan(T(15)))
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.LessThan{T},
            }(),
        ) == 2
    end
    vc1 = MOI.add_constraint(model, v[1], MOI.Interval(T(0), T(5)))
    # We test this after the creation of every `VariableIndex` constraint
    # to ensure a good coverage of corner cases.
    @test vc1.value == v[1].value
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.Interval{T}}(),
        ) == 1
    end
    vc2 = MOI.add_constraint(model, v[2], MOI.Interval(T(0), T(10)))
    @test vc2.value == v[2].value
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.Interval{T}}(),
        ) == 2
    end
    vc3 = MOI.add_constraint(model, v[2], MOI.Integer())
    @test vc3.value == v[2].value
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.Integer}(),
        ) == 1
    end
    vc4 = MOI.add_constraint(model, v[3], MOI.ZeroOne())
    @test vc4.value == v[3].value
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.ZeroOne}(),
        ) == 1
    end
    objf = MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.(T[11//10, 2, 5], v),
        T(0),
    )
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) in
              [MOI.FEASIBLE_POINT, MOI.NEARLY_FEASIBLE_POINT]
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(97 // 5), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), T[4, 5, 1], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c), T(10), config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c2), T(15), config)
        @test MOI.get(model, MOI.ObjectiveBound()) >= T(97 // 5) - config.atol
        # FIXME the following are currently not implemented in MockOptimizer
        #        @test MOI.get(model, MOI.RelativeGap()) >= T(0)
        #        @test MOI.get(model, MOI.SolveTimeSec()) >= T(0)
        #        @test MOI.get(model, MOI.SimplexIterations()) >= 0
        #        @test MOI.get(model, MOI.BarrierIterations()) >= 0
        #        @test MOI.get(model, MOI.NodeCount()) >= 0
    end
    return
end

function setup_test(
    ::typeof(test_linear_integer_integration),
    model::MOI.Utilities.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), T(20))
            MOIU.mock_optimize!(mock, T[4, 5, 1])
        end,
    )
    return
end

"""
    test_linear_SOS1_integration(model::MOI.ModelLike, config::Config{T}) where {T}

Test Special Ordered Sets of type 1.
"""
function test_linear_SOS1_integration(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.SOS1{T})
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.LessThan{T})
    v = MOI.add_variables(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3
    vc1 = MOI.add_constraint(model, v[1], MOI.LessThan(T(1)))
    @test vc1.value == v[1].value
    vc2 = MOI.add_constraint(model, v[2], MOI.LessThan(T(1)))
    @test vc2.value == v[2].value
    vc3 = MOI.add_constraint(model, v[3], MOI.LessThan(T(2)))
    @test vc3.value == v[3].value
    c1 = MOI.add_constraint(
        model,
        MOI.VectorOfVariables([v[1], v[2]]),
        MOI.SOS1(T[1, 2]),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.VectorOfVariables([v[1], v[3]]),
        MOI.SOS1(T[1, 2]),
    )
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SOS1{T}}(),
        ) == 2
    end
    #=
        To allow for permutations in the sets and variable vectors
        we're going to sort according to the weights
    =#
    cs_sos = MOI.get(model, MOI.ConstraintSet(), c2)
    cf_sos = MOI.get(model, MOI.ConstraintFunction(), c2)
    p = sortperm(cs_sos.weights)
    @test ≈(cs_sos.weights[p], T[1, 2], config)
    @test cf_sos.variables[p] == v[[1, 3]]
    objf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[2, 1, 1], v), T(0))
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(3), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), T[0, 1, 2], config)
    end
    MOI.delete(model, c1)
    MOI.delete(model, c2)
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(5), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), T[1, 1, 2], config)
    end
    return
end

function setup_test(
    ::typeof(test_linear_SOS1_integration),
    model::MOI.Utilities.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[0, 1, 2]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[1, 1, 2]),
    )
    return
end

"""
    test_linear_SOS2_integration(model::MOI.ModelLike, config::Config{T}) where {T}

Test Special Ordered Sets of type 2.
"""
function test_linear_SOS2_integration(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.SOS1{T})
    @requires MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.SOS2{T})
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.ZeroOne)
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.EqualTo{T},
    )
    v = MOI.add_variables(model, 10)
    @test MOI.get(model, MOI.NumberOfVariables()) == 10
    bin_constraints = []
    for i in 1:8
        vc = MOI.add_constraint(model, v[i], MOI.Interval(T(0), T(2)))
        @test vc.value == v[i].value
        push!(bin_constraints, MOI.add_constraint(model, v[i], MOI.ZeroOne()))
        @test bin_constraints[i].value == v[i].value
    end
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[1, 2, 3, -1], v[[1, 2, 3, 9]]),
            T(0),
        ),
        MOI.EqualTo(T(0)),
    )
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[5, 4, 7, 2, 1, -1], v[[4, 5, 6, 7, 8, 10]]),
            T(0),
        ),
        MOI.EqualTo(T(0)),
    )
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(v[[1, 2, 3]]),
        MOI.SOS1(T[1, 2, 3]),
    )
    vv = MOI.VectorOfVariables(v[[4, 5, 6, 7, 8]])
    sos2 = MOI.SOS2(T[5, 4, 7, 2, 1])
    c = MOI.add_constraint(model, vv, sos2)
    #=
        To allow for permutations in the sets and variable vectors
        we're going to sort according to the weights
    =#
    cs_sos = MOI.get(model, MOI.ConstraintSet(), c)
    cf_sos = MOI.get(model, MOI.ConstraintFunction(), c)
    p = sortperm(cs_sos.weights)
    @test ≈(cs_sos.weights[p], T[1, 2, 4, 5, 7], config)
    @test cf_sos.variables[p] == v[[8, 7, 5, 4, 6]]
    objf = MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.(T(1), [v[9], v[10]]),
        T(0),
    )
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(15), config)
        @test ≈(
            MOI.get(model, MOI.VariablePrimal(), v),
            T[0, 0, 1, 1, 0, 1, 0, 0, 3, 12],
            config,
        )
    end
    for cref in bin_constraints
        MOI.delete(model, cref)
    end
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(30), config)
        @test ≈(
            MOI.get(model, MOI.VariablePrimal(), v),
            T[0, 0, 2, 2, 0, 2, 0, 0, 6, 24],
            config,
        )
    end
    return
end

function setup_test(
    ::typeof(test_linear_SOS2_integration),
    model::MOI.Utilities.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, T[0, 0, 1, 1, 0, 1, 0, 0, 3, 12]),
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, T[0, 0, 2, 2, 0, 2, 0, 0, 6, 24]),
    )
    return
end

"""
    test_linear_integer_solve_twice(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test solving a model twice on the integer knapsack problem.
The problem is:
```
max   z - 0.5 ( b1 + b2 + b3) / 40
s.t.  0 <= z - 0.5 eᵀ b / 40 <= 0.999
      b1, b2, ... b10 ∈ {0, 1}
      z in {0, 1, 2, ..., 100}
```
"""
function test_linear_integer_solve_twice(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.ZeroOne)
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.Integer)
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.Interval{T})
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.Interval{T},
    )
    z = MOI.add_variable(model)
    vc1 = MOI.add_constraint(model, z, MOI.Integer())
    @test vc1.value == z.value
    vc2 = MOI.add_constraint(model, z, MOI.Interval(T(0), T(100)))
    @test vc2.value == z.value
    b = MOI.add_variables(model, 10)
    for bi in b
        vc = MOI.add_constraint(model, bi, MOI.ZeroOne())
        @test vc.value == bi.value
    end
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(
                vcat(T(1), fill(T(-1 // 80), 10)),
                vcat(z, b),
            ),
            T(0),
        ),
        MOI.Interval(T(0), T(999 // 1_000)),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(
                vcat(T(1), fill(T(-1 // 80), 3)),
                vcat(z, b[1:3]),
            ),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(1), config)
        # test for CPLEX.jl #76
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(1), config)
    end
    return
end

function setup_test(
    ::typeof(test_linear_integer_solve_twice),
    model::MOI.Utilities.MockOptimizer,
    ::Config{T},
) where {T}
    # FIXME [1, 0...] is not the correct optimal solution but it passes the test
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, [T(1); zeros(T, 10)]),
    )
    return
end

"""
    test_linear_integer_knapsack(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test the integer knapsack problem
```
max 5a + 3b + 2c + 7d + 4e
st  2a + 8b + 4c + 2d + 5e <= 10
     a,   b,   c,   d,   e ∈ binary
```
"""
function test_linear_integer_knapsack(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.ZeroOne)
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.LessThan{T},
    )
    v = MOI.add_variables(model, 5)
    @test MOI.get(model, MOI.NumberOfVariables()) == 5
    for vi in v
        vc = MOI.add_constraint(model, vi, MOI.ZeroOne())
        @test vc.value == vi.value
    end
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.ZeroOne}(),
        ) == 5
    end
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[2, 8, 4, 2, 5], v),
            T(0),
        ),
        MOI.LessThan(T(10)),
    )
    if _supports(config, MOI.NumberOfConstraints)
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
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[5, 3, 2, 7, 4], v),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
        MOI.set(model, MOI.VariablePrimalStart(), v, zeros(T, 5))
    end
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) in
              [MOI.FEASIBLE_POINT, MOI.NEARLY_FEASIBLE_POINT]
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(16), config)
        @test ≈(
            MOI.get(model, MOI.VariablePrimal(), v),
            T[1, 0, 0, 1, 1],
            config,
        )
    end
    return
end

function setup_test(
    ::typeof(test_linear_integer_knapsack),
    model::MOI.Utilities.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, T[1, 0, 0, 1, 1]),
    )
    return
end

"""
    test_linear_Indicator_integration(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test the problem:
```
max  2x1 + 3x2
s.t. x1 + x2 <= 10
     z1 ==> x2 <= 8
     z2 ==> x2 + x1/5 <= 9
     z1 + z2 >= 1
```
"""
function test_linear_Indicator_integration(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.ZeroOne)
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.Interval{T})
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.Interval{T},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.LessThan{T}},
    )
    x1 = MOI.add_variable(model)
    x2 = MOI.add_variable(model)
    z1 = MOI.add_variable(model)
    z2 = MOI.add_variable(model)
    MOI.add_constraint(model, z1, MOI.ZeroOne())
    MOI.add_constraint(model, z2, MOI.ZeroOne())
    f1 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), z1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(1), x2)),
        ],
        T[0, 0],
    )
    iset1 = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(T(8)))
    MOI.add_constraint(model, f1, iset1)
    f2 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), z2)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(1 // 5), x1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(1), x2)),
        ],
        T[0, 0],
    )
    iset2 = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(T(9)))
    MOI.add_constraint(model, f2, iset2)
    # Additional regular constraint.
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(T(1), x1), MOI.ScalarAffineTerm(T(1), x2)],
            T(0),
        ),
        MOI.LessThan(T(10)),
    )
    # Disjunction z1 ⋁ z2
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(T(1), z1), MOI.ScalarAffineTerm(T(1), z2)],
            T(0),
        ),
        MOI.GreaterThan(T(1)),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[2, 3], [x1, x2]),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(115 // 4), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x1), T(5 // 4), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x2), T(35 // 4), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), z1), T(0), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), z2), T(1), config)
    end
    return
end

function setup_test(
    ::typeof(test_linear_Indicator_integration),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, T[5//4, 35//4, 0, 1]),
    )
    return
end

"""
    test_linear_Indicator_ON_ONE(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test the problem:
```
max  2x1 + 3x2 - 30 z2
s.t. x1 + x2 <= 10
     z1 ==> x2 <= 8
     z2 ==> x2 + x1/5 <= 9
     z1 + z2 >= 1
```
"""
function test_linear_Indicator_ON_ONE(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    x1 = MOI.add_variable(model)
    x2 = MOI.add_variable(model)
    z1 = MOI.add_variable(model)
    z2 = MOI.add_variable(model)
    MOI.add_constraint(model, z1, MOI.ZeroOne())
    MOI.add_constraint(model, z2, MOI.ZeroOne())
    f1 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), z1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(1), x2)),
        ],
        T[0, 0],
    )
    iset1 = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(T(8)))
    MOI.add_constraint(model, f1, iset1)
    f2 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), z2)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(1 // 5), x1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(1), x2)),
        ],
        T[0, 0],
    )
    iset2 = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(T(9)))
    MOI.add_constraint(model, f2, iset2)
    # additional regular constraint
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(T(1), x1), MOI.ScalarAffineTerm(T(1), x2)],
            T(0),
        ),
        MOI.LessThan(T(10)),
    )
    # disjunction z1 ⋁ z2
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(T(1), z1), MOI.ScalarAffineTerm(T(1), z2)],
            T(0),
        ),
        MOI.GreaterThan(T(1)),
    )
    # objective penalized on z2
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[2, 3, -30], [x1, x2, z2]),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(28), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x1), T(2), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x2), T(8), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), z1), T(1), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), z2), T(0), config)
    end
    return
end

function setup_test(
    ::typeof(test_linear_Indicator_ON_ONE),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, T[2, 8, 1, 0]),
    )
    return
end

"""
    test_linear_Indicator_ON_ZERO(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test the problem:
```
max  2x1 + 3x2
s.t. x1 + x2 <= 10
     z1 == 0 ==> x2 <= 8
     z2 == 1 ==> x2 + x1/5 <= 9
     (1-z1) + z2 >= 1 <=> z2 - z1 >= 0
```
"""
function test_linear_Indicator_ON_ZERO(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.ZeroOne)
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.Interval{T})
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.Interval{T},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.LessThan{T}},
    )
    x1 = MOI.add_variable(model)
    x2 = MOI.add_variable(model)
    z1 = MOI.add_variable(model)
    z2 = MOI.add_variable(model)
    vc1 = MOI.add_constraint(model, z1, MOI.ZeroOne())
    @test vc1.value == z1.value
    vc2 = MOI.add_constraint(model, z2, MOI.ZeroOne())
    @test vc2.value == z2.value
    f1 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), z1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(1), x2)),
        ],
        T[0, 0],
    )
    iset1 = MOI.Indicator{MOI.ACTIVATE_ON_ZERO}(MOI.LessThan(T(8)))
    MOI.add_constraint(model, f1, iset1)
    f2 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), z2)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(1 // 5), x1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(1), x2)),
        ],
        T[0, 0],
    )
    iset2 = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(T(9)))
    MOI.add_constraint(model, f2, iset2)
    # Additional regular constraint.
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(T(1), x1), MOI.ScalarAffineTerm(T(1), x2)],
            T(0),
        ),
        MOI.LessThan(T(10)),
    )
    # Disjunction (1-z1) ⋁ z2
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(T(-1), z1), MOI.ScalarAffineTerm(T(1), z2)],
            T(0),
        ),
        MOI.GreaterThan(T(0)),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[2, 3], [x1, x2]),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(115 // 4), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x1), T(5 // 4), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x2), T(35 // 4), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), z1), T(1), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), z2), T(1), config)
    end
    return
end

function setup_test(
    ::typeof(test_linear_Indicator_ON_ZERO),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, T[5//4, 35//4, 1, 1]),
    )
    return
end

"""
    test_linear_Indicator_constant_term(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test Indicator constraints with a constant term on the left-hand side. The
problem is:
```
max  2x1 + 3x2
s.t. x1 + x2 <= 10
     z1 ==> x2 - 1 <= 7
     z2 ==> x2 + x1/5 + 1 <= 10
     z1 + z2 >= 1
```
"""
function test_linear_Indicator_constant_term(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.ZeroOne)
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.Interval{T})
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.Interval{T},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.LessThan{T}},
    )
    x1 = MOI.add_variable(model)
    x2 = MOI.add_variable(model)
    z1 = MOI.add_variable(model)
    z2 = MOI.add_variable(model)
    MOI.add_constraint(model, z1, MOI.ZeroOne())
    MOI.add_constraint(model, z2, MOI.ZeroOne())
    f1 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), z1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(1), x2)),
        ],
        T[0, -1],
    )
    iset1 = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(T(7)))
    MOI.add_constraint(model, f1, iset1)
    f2 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(T(1), z2)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(1 // 5), x1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(T(1), x2)),
        ],
        T[0, 1],
    )
    iset2 = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(T(10)))
    MOI.add_constraint(model, f2, iset2)
    # Additional regular constraint.
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(T(1), x1), MOI.ScalarAffineTerm(T(1), x2)],
            T(0),
        ),
        MOI.LessThan(T(10)),
    )
    # Disjunction z1 ⋁ z2
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(T(1), z1), MOI.ScalarAffineTerm(T(1), z2)],
            T(0),
        ),
        MOI.GreaterThan(T(1)),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[2, 3], [x1, x2]),
            T(0),
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(115 // 4), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x1), T(5 // 4), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), x2), T(35 // 4), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), z1), T(0), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), z2), T(1), config)
    end
    return
end

function setup_test(
    ::typeof(test_linear_Indicator_constant_term),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, T[5//4, 35//4, 0, 1]),
    )
    return
end

"""
2 variables

min  x
st   x >= y
     if !int
          x ∈ {0.0} U [2.0,3.0]
     if int
          x ∈ {0.0} U {2.0} U {3.0}
     y = 0.0
"""
function _test_linear_SemiXXX_integration(
    model::MOI.ModelLike,
    config::Config{T},
    use_semiinteger::Bool,
) where {T}
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @requires MOI.supports(model, MOI.ObjectiveSense())
    if use_semiinteger
        @requires MOI.supports_constraint(
            model,
            MOI.VariableIndex,
            MOI.Semiinteger{T},
        )
    else
        @requires MOI.supports_constraint(
            model,
            MOI.VariableIndex,
            MOI.Semicontinuous{T},
        )
    end
    v = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    if !use_semiinteger
        vc1 = MOI.add_constraint(model, v[1], MOI.Semicontinuous(T(2), T(3)))
        if _supports(config, MOI.NumberOfConstraints)
            @test MOI.get(
                model,
                MOI.NumberOfConstraints{
                    MOI.VariableIndex,
                    MOI.Semicontinuous{T},
                }(),
            ) == 1
        end
    else
        vc1 = MOI.add_constraint(model, v[1], MOI.Semiinteger(T(2), T(3)))
        if _supports(config, MOI.NumberOfConstraints)
            @test MOI.get(
                model,
                MOI.NumberOfConstraints{MOI.VariableIndex,MOI.Semiinteger{T}}(),
            ) == 1
        end
    end
    vc2 = MOI.add_constraint(model, v[2], MOI.EqualTo(T(0)))
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.EqualTo{T}}(),
        ) == 1
    end
    cf =
        MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.(T[1, -1], v), T(0))
    c = MOI.add_constraint(model, cf, MOI.GreaterThan(T(0)))
    if _supports(config, MOI.NumberOfConstraints)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.GreaterThan{T},
            }(),
        ) == 1
    end
    objf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(T[1, 0], v), T(0))
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    if _supports(config, MOI.optimize!)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) in
              [MOI.FEASIBLE_POINT, MOI.NEARLY_FEASIBLE_POINT]
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(0), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), T[0, 0], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c), T(0), config)
        @test MOI.get(model, MOI.ObjectiveBound()) <= T(0) + config.atol
    end
    # Change y fixed value
    MOI.set(model, MOI.ConstraintSet(), vc2, MOI.EqualTo(T(1)))
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(2), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), T[2, 1], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c), T(1), config)
        @test MOI.get(model, MOI.ObjectiveBound()) <= T(2) + config.atol
    end
    MOI.set(model, MOI.ConstraintSet(), vc2, MOI.EqualTo(T(2)))
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(2), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), T[2, 2], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c), T(0), config)
        @test MOI.get(model, MOI.ObjectiveBound()) <= T(2) + config.atol
    end
    MOI.set(model, MOI.ConstraintSet(), vc2, MOI.EqualTo(T(5 // 2)))
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if !use_semiinteger
            @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(5 // 2), config)
            @test ≈(
                MOI.get(model, MOI.VariablePrimal(), v),
                T[5//2, 5//2],
                config,
            )
            @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c), T(0), config)
            @test MOI.get(model, MOI.ObjectiveBound()) <=
                  T(5 // 2) + config.atol
        else
            @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(3), config)
            @test ≈(MOI.get(model, MOI.VariablePrimal(), v), T[3, 5//2], config)
            @test ≈(
                MOI.get(model, MOI.ConstraintPrimal(), c),
                T(1 // 2),
                config,
            )
            @test MOI.get(model, MOI.ObjectiveBound()) <= T(3) + config.atol
        end
    end
    MOI.set(model, MOI.ConstraintSet(), vc2, MOI.EqualTo(T(3)))
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(3), config)
        @test ≈(MOI.get(model, MOI.VariablePrimal(), v), T[3, 3], config)
        @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c), T(0), config)
        @test MOI.get(model, MOI.ObjectiveBound()) <= T(3) + config.atol
    end
    MOI.set(model, MOI.ConstraintSet(), vc2, MOI.EqualTo(T(4)))
    if _supports(config, MOI.optimize!)
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) in
              (config.infeasible_status, MOI.INFEASIBLE_OR_UNBOUNDED)
    end
    return
end

"""
    test_linear_Semicontinuous_integration(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Run an integration test on Semicontinuous constraints.
"""
function test_linear_Semicontinuous_integration(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_linear_SemiXXX_integration(model, config, false)
    return
end

function setup_test(
    ::typeof(test_linear_Semicontinuous_integration),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), T(0))
            MOIU.mock_optimize!(mock, T[0, 0])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), T(2))
            MOIU.mock_optimize!(mock, T[2, 1])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), T(2))
            MOIU.mock_optimize!(mock, T[2, 2])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), T(5 // 2))
            MOIU.mock_optimize!(mock, T[5//2, 5//2])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), T(3))
            MOIU.mock_optimize!(mock, T[3, 3])
        end,
        (mock::MOIU.MockOptimizer) ->
            MOI.set(mock, MOI.TerminationStatus(), MOI.INFEASIBLE),
    )
    return
end

"""
    test_linear_Semiinteger_integration(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Run an integration test on Semiinteger constraints.
"""
function test_linear_Semiinteger_integration(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _test_linear_SemiXXX_integration(model, config, true)
    return
end

function setup_test(
    ::typeof(test_linear_Semiinteger_integration),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), T(0))
            MOIU.mock_optimize!(mock, T[0, 0])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), T(2))
            MOIU.mock_optimize!(mock, T[2, 1])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), T(2))
            MOIU.mock_optimize!(mock, T[2, 2])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), T(3))
            MOIU.mock_optimize!(mock, T[3, 5//2])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), T(3))
            MOIU.mock_optimize!(mock, T[3, 3])
        end,
        (mock::MOIU.MockOptimizer) ->
            MOI.set(mock, MOI.TerminationStatus(), MOI.INFEASIBLE),
    )
    return
end
