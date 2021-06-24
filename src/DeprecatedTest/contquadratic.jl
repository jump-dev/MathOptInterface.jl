# Continuous quadratic problems

function qp1test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # homogeneous quadratic objective
    # Min x^2 + xy + y^2 + yz + z^2
    # st  x + 2y + 3z >= 4 (c1)
    #     x +  y      >= 1 (c2)
    #     x, y, z \in R
    @test MOI.supports_incremental_interface(model, false) #=copy_names=#
    MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
    )
    MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.GreaterThan{Float64},
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    v = MOI.add_variables(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3
    cf1 =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 2.0, 3.0], v), 0.0)
    c1 = MOI.add_constraint(model, cf1, MOI.GreaterThan(4.0))
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{Float64},
                MOI.GreaterThan{Float64},
            }(),
        ) == 1
    end
    c2 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 1.0], [v[1], v[2]]),
            0.0,
        ),
        MOI.GreaterThan(1.0),
    )
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{Float64},
                MOI.GreaterThan{Float64},
            }(),
        ) == 2
    end
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    obj = MOI.ScalarQuadraticFunction(
        MOI.ScalarAffineTerm{Float64}[],
        MOI.ScalarQuadraticTerm.(
            [2.0, 1.0, 2.0, 1.0, 2.0],
            v[[1, 1, 2, 2, 3]],
            v[[1, 2, 2, 3, 3]],
        ),
        0.0,
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
        obj,
    )
    if config.query
        @test obj ≈ MOI.get(
            model,
            MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
        )
        @test cf1 ≈ MOI.get(model, MOI.ConstraintFunction(), c1)
        @test MOI.GreaterThan(4.0) == MOI.get(model, MOI.ConstraintSet(), c1)
    end
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 13 / 7 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [4 / 7, 3 / 7, 6 / 7] atol =
            atol rtol = rtol
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            # The dual constraint gives
            # [λ_c1 + λ_c2, 2λ_c1 + λ_c2, 3λ_c1] = [2 1 0; 1 2 1; 0 1 2] * x
            #                                    = [11, 16, 15] / 7
            # hence λ_c1 = 5/7 and λ_c2 = 6/7.
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ 5 / 7 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ 6 / 7 atol = atol rtol =
                rtol
        end
    end
end

function qp2test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Same as `qp1` but with duplicate terms then change the objective and sense
    # simple quadratic objective
    # Min x^2 + xy + y^2 + yz + z^2
    # st  x + 2y + 3z >= 4 (c1)
    #     x +  y      >= 1 (c2)
    #     x, y, z \in R
    @test MOI.supports_incremental_interface(model, false) #=copy_names=#
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
    )
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.GreaterThan{Float64},
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    v = MOI.add_variables(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3
    c1f =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 2.0, 3.0], v), 0.0)
    c1 = MOI.add_constraint(model, c1f, MOI.GreaterThan(4.0))
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{Float64},
                MOI.GreaterThan{Float64},
            }(),
        ) == 1
    end
    c2 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 1.0], [v[1], v[2]]),
            0.0,
        ),
        MOI.GreaterThan(1.0),
    )
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{Float64},
                MOI.GreaterThan{Float64},
            }(),
        ) == 2
    end
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    obj = MOI.ScalarQuadraticFunction(
        MOI.ScalarAffineTerm.(0.0, v),
        MOI.ScalarQuadraticTerm.(
            [2.0, 0.5, 0.5, 2.0, 1.0, 1.0, 1.0],
            [v[1], v[1], v[1], v[2], v[2], v[3], v[3]],
            [v[1], v[2], v[2], v[2], v[3], v[3], v[3]],
        ),
        0.0,
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
        obj,
    )
    if config.query
        @test obj ≈ MOI.get(
            model,
            MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
        )
        @test c1f ≈ MOI.get(model, MOI.ConstraintFunction(), c1)
        @test MOI.GreaterThan(4.0) == MOI.get(model, MOI.ConstraintSet(), c1)
    end
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 13 / 7 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [4 / 7, 3 / 7, 6 / 7] atol =
            atol rtol = rtol
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ 5 / 7 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ 6 / 7 atol = atol rtol =
                rtol
        end
    end
    # change objective to Max -2(x^2 + xy + y^2 + yz + z^2)
    # First clear the objective, this is needed if a `Bridges.Objective.SlackBridge` is used.
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    obj2 = MOI.ScalarQuadraticFunction(
        MOI.ScalarAffineTerm.(0.0, v),
        MOI.ScalarQuadraticTerm.(
            [-4.0, -1.0, -1.0, -4.0, -2.0, -2.0, -2.0],
            [v[1], v[1], v[1], v[2], v[2], v[3], v[3]],
            [v[1], v[2], v[2], v[2], v[3], v[3], v[3]],
        ),
        0.0,
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
        obj2,
    )
    if config.query
        @test obj2 ≈ MOI.get(
            model,
            MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
        )
    end
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -2 * 13 / 7 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [4 / 7, 3 / 7, 6 / 7] atol =
            atol rtol = rtol
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ 10 / 7 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ 12 / 7 atol = atol rtol =
                rtol
        end
    end
end

function qp3test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # non-homogeneous quadratic objective
    #    minimize 2 x^2 + y^2 + xy + x + y + 1
    #       s.t.  x, y >= 0
    #             x + y = 1
    @test MOI.supports_incremental_interface(model, false) #=copy_names=#
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
    )
    MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    c1 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 1.0], [x, y]),
            0.0,
        ),
        MOI.EqualTo(1.0),
    )
    vc1 = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    # We test this after the creation of every `SingleVariable` constraint
    # to ensure a good coverage of corner cases.
    @test vc1.value == x.value
    vc2 = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.GreaterThan(0.0))
    @test vc2.value == y.value
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    obj = MOI.ScalarQuadraticFunction(
        MOI.ScalarAffineTerm.([1.0, 1.0], [x, y]),
        MOI.ScalarQuadraticTerm.([4.0, 2.0, 1.0], [x, y, x], [x, y, y]),
        1.0,
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
        obj,
    )
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2.875 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), [x, y]) ≈ [0.25, 0.75] atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), vc1) ≈ 0.25 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), vc2) ≈ 0.75 atol = atol rtol =
            rtol
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            # The dual constraint gives [λ_vc1 + λ_c1, λ_vc2 + λ_c1] = [4 1; 1 2] * x + [1, 1] = [11, 11] / 4
            # since `vc1` and `vc2` are not active, `λ_vc1` and `λ_vc2` are
            # zero so `λ_c1 = 11/4`.
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ 11 / 4 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc1) ≈ 0 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc2) ≈ 0 atol = atol rtol =
                rtol
        end
    end
    # change back to linear
    #        max 2x + y + 1
    #       s.t.  x, y >= 0
    #             x + y = 1
    # (x,y) = (1,0), obj = 3
    # First clear the objective, this is needed if a `Bridges.Objective.SlackBridge` is used.
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    objf =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0, 1.0], [x, y]), 1.0)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        objf,
    )
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3.0 atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), [x, y]) ≈ [1.0, 0.0] atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), vc1) ≈ 1.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), vc2) ≈ 0.0 atol = atol rtol =
            rtol
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ -2 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc1) ≈ 0 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc2) ≈ 1 atol = atol rtol =
                rtol
        end
    end
end

const qptests = Dict("qp1" => qp1test, "qp2" => qp2test, "qp3" => qp3test)

@moitestset qp

#=
    Quadratically constrained (convex) programs
=#

function qcp1test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # quadratic constraint
    # Max x  + y
    # st -x  + y >= 0 (c1[1])
    #     x  + y >= 0 (c1[2])
    #     x² + y <= 2 (c2)
    # Optimal solution
    # x = 1/2, y = 7/4
    @test MOI.supports_incremental_interface(model, false) #=copy_names=#
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Nonnegatives,
    )
    @test MOI.supports_constraint(
        model,
        MOI.ScalarQuadraticFunction{Float64},
        MOI.LessThan{Float64},
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    c1 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 1, 2, 2],
                MOI.ScalarAffineTerm.([-1.0, 1.0, 1.0, 1.0], [x, y, x, y]),
            ),
            [0.0, 0.0],
        ),
        MOI.Nonnegatives(2),
    )
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Nonnegatives,
            }(),
        ) == 1
    end
    c2f = MOI.ScalarQuadraticFunction(
        [MOI.ScalarAffineTerm(1.0, y)],
        [MOI.ScalarQuadraticTerm(2.0, x, x)],
        0.0,
    )
    c2 = MOI.add_constraint(model, c2f, MOI.LessThan(2.0))
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarQuadraticFunction{Float64},
                MOI.LessThan{Float64},
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 1.0], [x, y]),
            0.0,
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    if config.query
        @test c2f ≈ MOI.get(model, MOI.ConstraintFunction(), c2) atol = atol rtol =
            rtol
    end
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2.25 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), [x, y]) ≈ [0.5, 1.75] atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c1) ≈ [5 / 4, 9 / 4] atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ 2 atol = atol rtol =
            rtol
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            # The dual constraint gives
            # [-1, -1] = [-λ_c1[1] + λ_c1[2], λ_c1[1] + λ_c1[2]] + λ_c2 * ([2 0; 0 0] * x + [0, 1])
            #          = [-λ_c1[1] + λ_c1[2] + λ_c2, λ_c1[1] + λ_c1[2] + λ_c2]
            # By complementary slackness, we have `λ_c1 = [0, 0]` so
            # `λ_c2 = -1`.
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ [0, 0] atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ -1 atol = atol rtol =
                rtol
        end
    end
    # try delete quadratic constraint and go back to linear
    # FIXME why is this commented ?
    # MOI.delete(model, c2)
    #
    # MOI.optimize!(model)
    #
    # @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    #
    # @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
    #
    # @test MOI.get(model, MOI.ObjectiveValue()) ≈ 0.0 atol=atol rtol=rtol
end

function qcp2test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Max x
    # s.t. x^2 <= 2 (c)
    @test MOI.supports_incremental_interface(model, false) #=copy_names=#
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports_constraint(
        model,
        MOI.ScalarQuadraticFunction{Float64},
        MOI.LessThan{Float64},
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    cf = MOI.ScalarQuadraticFunction(
        [MOI.ScalarAffineTerm(0.0, x)],
        [MOI.ScalarQuadraticTerm(2.0, x, x)],
        0.0,
    )
    c = MOI.add_constraint(model, cf, MOI.LessThan(2.0))
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarQuadraticFunction{Float64},
                MOI.LessThan{Float64},
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    if config.query
        @test cf ≈ MOI.get(model, MOI.ConstraintFunction(), c) atol = atol rtol =
            rtol
    end
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ √2 atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ √2 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 2 atol = atol rtol =
            rtol
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            # The dual constraint gives
            # -1 = λ * (2 * x) = λ * 2√2
            # hence λ = -1/(2√2).
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1 / (2 * √2) atol =
                atol rtol = rtol
        end
    end
end

function qcp3test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Min -x
    # s.t. x^2 <= 2
    @test MOI.supports_incremental_interface(model, false) #=copy_names=#
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports_constraint(
        model,
        MOI.ScalarQuadraticFunction{Float64},
        MOI.LessThan{Float64},
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    cf = MOI.ScalarQuadraticFunction(
        MOI.ScalarAffineTerm{Float64}[],
        [MOI.ScalarQuadraticTerm(2.0, x, x)],
        0.0,
    )
    c = MOI.add_constraint(model, cf, MOI.LessThan(2.0))
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarQuadraticFunction{Float64},
                MOI.LessThan{Float64},
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(-1.0, x)], 0.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    if config.query
        @test cf ≈ MOI.get(model, MOI.ConstraintFunction(), c) atol = atol rtol =
            rtol
    end
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -√2 atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ √2 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 2 atol = atol rtol =
            rtol
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            # The dual constraint gives
            # -1 = λ * (2 * x) = λ * 2√2
            # hence λ = -1/(2√2).
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1 / (2 * √2) atol =
                atol rtol = rtol
        end
    end
end

function _qcp4test(model::MOI.ModelLike, config::Config, less_than::Bool)
    atol = config.atol
    rtol = config.rtol
    # Max  x
    # s.t. x^2 + x * y + y^2 <= 3 (c)
    #      y == 1
    @test MOI.supports_incremental_interface(model, false) #=copy_names=#
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    quad_set = less_than ? MOI.LessThan(3.0) : MOI.GreaterThan(-3.0)
    @test MOI.supports_constraint(
        model,
        MOI.ScalarQuadraticFunction{Float64},
        typeof(quad_set),
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    vc = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.EqualTo(1.0))
    @test vc.value == y.value
    cf = MOI.ScalarQuadraticFunction(
        [MOI.ScalarAffineTerm(0.0, x)],
        MOI.ScalarQuadraticTerm.([2.0, 1.0, 2.0], [x, x, y], [x, y, y]),
        0.0,
    )
    if !less_than
        MOIU.operate!(-, Float64, cf)
    end
    c = MOI.add_constraint(model, cf, quad_set)
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarQuadraticFunction{Float64},
                typeof(quad_set),
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    if config.query
        @test cf ≈ MOI.get(model, MOI.ConstraintFunction(), c) atol = atol rtol =
            rtol
        @test quad_set == MOI.get(model, MOI.ConstraintSet(), c)
    end
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1.0 atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 1.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 1.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ MOI.constant(quad_set) atol =
            atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), vc) ≈ 1.0 atol = atol rtol =
            rtol
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            # The dual constraint gives
            # [-1, 0] = λ_c * [2 1; 1 2] * x + [0, λ_vc]
            #         = [3λ_c, 3λ_c + λ_vc]
            # hence `λ_c = -1/3` and `λ_vc = 1`.
            λ_c = less_than ? -1 / 3 : 1 / 3
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ λ_c atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc) ≈ 1 atol = atol rtol =
                rtol
        end
    end
end

function qcp4test(model::MOI.ModelLike, config::Config)
    return _qcp4test(model, config, true)
end
function qcp5test(model::MOI.ModelLike, config::Config)
    return _qcp4test(model, config, false)
end

const qcptests = Dict(
    "qcp1" => qcp1test,
    "qcp2" => qcp2test,
    "qcp3" => qcp3test,
    "qcp4" => qcp4test,
    "qcp5" => qcp5test,
)

@moitestset qcp

#=
    Quadratically constrained (non-convex) programs
=#

function ncqcp1test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Max 2x + y
    # s.t. x * y <= 4 (c)
    #      x, y >= 1
    @test MOI.supports_incremental_interface(model, false) #=copy_names=#
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports_constraint(
        model,
        MOI.ScalarQuadraticFunction{Float64},
        MOI.LessThan{Float64},
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    vc1 = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(1.0))
    @test vc1.value == x.value
    vc2 = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.GreaterThan(1.0))
    @test vc2.value == y.value
    cf = MOI.ScalarQuadraticFunction(
        [MOI.ScalarAffineTerm(0.0, x)],
        [MOI.ScalarQuadraticTerm(1.0, x, y)],
        0.0,
    )
    c = MOI.add_constraint(model, cf, MOI.LessThan(4.0))
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarQuadraticFunction{Float64},
                MOI.LessThan{Float64},
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([2.0, 1.0], [x, y]),
            0.0,
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    if config.query
        @test cf ≈ MOI.get(model, MOI.ConstraintFunction(), c)
    end
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 9.0 atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 4.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 1.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 4.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), vc1) ≈ 4.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), vc2) ≈ 1.0 atol = atol rtol =
            rtol
    end
end

function ncqcp2test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Find x, y
    # s.t. x * y == 4 (c)
    #      x * x == 4 (c2)
    #      x, y >= 0
    @test MOI.supports_incremental_interface(model, false) #=copy_names=#
    @test MOI.supports_constraint(
        model,
        MOI.ScalarQuadraticFunction{Float64},
        MOI.EqualTo{Float64},
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    vc1 = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    @test vc1.value == x.value
    vc2 = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.GreaterThan(0.0))
    @test vc2.value == y.value
    cf = MOI.ScalarQuadraticFunction(
        [MOI.ScalarAffineTerm(0.0, x)],
        [MOI.ScalarQuadraticTerm(1.0, x, y)],
        0.0,
    )
    c = MOI.add_constraint(model, cf, MOI.EqualTo(4.0))
    cf2 = MOI.ScalarQuadraticFunction(
        [MOI.ScalarAffineTerm(0.0, x)],
        [MOI.ScalarQuadraticTerm(2.0, x, x)],
        0.0,
    )
    c2 = MOI.add_constraint(model, cf2, MOI.EqualTo(4.0))
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarQuadraticFunction{Float64},
                MOI.EqualTo{Float64},
            }(),
        ) == 2
    end
    if config.query
        @test cf ≈ MOI.get(model, MOI.ConstraintFunction(), c)
        @test cf2 ≈ MOI.get(model, MOI.ConstraintFunction(), c2)
    end
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 2.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 2.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 4.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ 4.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), vc1) ≈ 2.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), vc2) ≈ 2.0 atol = atol rtol =
            rtol
    end
end

const ncqcptests = Dict("ncqcp1" => ncqcp1test, "ncqcp2" => ncqcp2test)

@moitestset ncqcp

#=
    SOCP
=#

function socp1test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # min t
    # s.t. x + y >= 1 (c1)
    #      x^2 + y^2 <= t^2 (c2)
    #      t >= 0 (bound)
    @test MOI.supports_incremental_interface(model, false) #=copy_names=#
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports_constraint(
        model,
        MOI.ScalarQuadraticFunction{Float64},
        MOI.LessThan{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.GreaterThan{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.SingleVariable,
        MOI.GreaterThan{Float64},
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    t = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3
    c1f =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 1.0], [x, y]), 0.0)
    c1 = MOI.add_constraint(model, c1f, MOI.GreaterThan(1.0))
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{Float64},
                MOI.GreaterThan{Float64},
            }(),
        ) == 1
    end
    c2f = MOI.ScalarQuadraticFunction(
        MOI.ScalarAffineTerm{Float64}[],
        MOI.ScalarQuadraticTerm.([2.0, 2.0, -2.0], [x, y, t], [x, y, t]),
        0.0,
    )
    c2 = MOI.add_constraint(model, c2f, MOI.LessThan(0.0))
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarQuadraticFunction{Float64},
                MOI.LessThan{Float64},
            }(),
        ) == 1
    end
    bound =
        MOI.add_constraint(model, MOI.SingleVariable(t), MOI.GreaterThan(0.0))
    @test bound.value == t.value
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.SingleVariable,
                MOI.GreaterThan{Float64},
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, t)], 0.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    if config.query
        @test c1f ≈ MOI.get(model, MOI.ConstraintFunction(), c1)
        @test c2f ≈ MOI.get(model, MOI.ConstraintFunction(), c2)
    end
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1 / √2 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), [x, y, t]) ≈
              [0.5, 0.5, 1 / √2] atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), [t, x, y, t]) ≈
              [1 / √2, 0.5, 0.5, 1 / √2] atol = atol rtol = rtol
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            # The dual constraint gives
            # [1, 0, 0] = [λ_bound, λ_c1, λ_c1] + λ_c2 * [-2 0 0; 0 2 0; 0 0 2] * x
            #           = [λ_bound - √2*λ_c2, λ_c1 + λ_c2, λ_c1 + λ_c2]
            # and since `bound` is not active, `λ_bound = 0`.
            # This gives `λ_c2 = -1/√2` and `λ_c1 = 1/√2`.
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ 1 / √2 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ -1 / √2 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), bound) ≈ 0 atol = atol rtol =
                rtol
        end
    end
end

const socptests = Dict("socp1" => socp1test)

@moitestset socp

const contquadratictests = Dict(
    "qp" => qptest,
    "qcp" => qcptest,
    "ncqcp" => ncqcptest,
    "socp" => socptest,
)

@moitestset contquadratic true
