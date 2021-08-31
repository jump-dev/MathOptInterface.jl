# Continuous conic problems
using LinearAlgebra # for dot

function _lin1test(model::MOI.ModelLike, config::Config, vecofvars::Bool)
    atol = config.atol
    rtol = config.rtol
    # linear conic problem
    # min -3x - 2y - 4z
    # st    x +  y +  z == 3
    #            y +  z == 2
    #       x>=0 y>=0 z>=0
    # Opt obj = -11, soln x = 1, y = 0, z = 2
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    if vecofvars
        @test MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.Nonnegatives,
        )
    else
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.Nonnegatives,
        )
    end
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Zeros,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    v = MOI.add_variables(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3
    vov = MOI.VectorOfVariables(v)
    if vecofvars
        vc = MOI.add_constraint(model, vov, MOI.Nonnegatives(3))
    else
        vc = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{Float64}(vov),
            MOI.Nonnegatives(3),
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
        MOI.Zeros(2),
    )
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                vecofvars ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                MOI.Nonnegatives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Zeros,
            }(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (
        vecofvars ? MOI.VectorOfVariables : MOI.VectorAffineFunction{Float64},
        MOI.Nonnegatives,
    ) in loc
    @test (MOI.VectorAffineFunction{Float64}, MOI.Zeros) in loc
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([-3.0, -2.0, -4.0], v),
            0.0,
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -11 atol = atol rtol = rtol
        if config.dual_objective_value
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ -11 atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1, 0, 2] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), vc) ≈ [1, 0, 2] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ zeros(2) atol = atol rtol =
            rtol
        if config.duals
            @test MOI.get(model, MOI.ConstraintDual(), vc) ≈ [0, 2, 0] atol =
                atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ [-3, -1] atol = atol rtol =
                rtol
        end
    end
end

function lin1vtest(model::MOI.ModelLike, config::Config)
    return _lin1test(model, config, true)
end
function lin1ftest(model::MOI.ModelLike, config::Config)
    return _lin1test(model, config, false)
end

function _lin2test(model::MOI.ModelLike, config::Config, vecofvars::Bool)
    atol = config.atol
    rtol = config.rtol
    #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64},
    #[
    #    (MOI.VectorAffineFunction{Float64},MOI.Zeros),
    #    (MOI.VectorOfVariables,MOI.Nonnegatives),
    #    (MOI.VectorOfVariables,MOI.Nonpositives)
    #])
    # mixed cones
    # min  3x + 2y - 4z + 0s
    # st    x           -  s  == -4    (i.e. x >= -4)
    #            y            == -3
    #       x      +  z       == 12
    #       x free
    #       y <= 0
    #       z >= 0
    #       s zero
    # Opt solution = -82
    # x = -4, y = -3, z = 16, s == 0
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    if vecofvars
        @test MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.Nonnegatives,
        )
        @test MOI.supports_add_constrained_variables(model, MOI.Nonpositives)
        @test MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.Zeros)
    else
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.Nonnegatives,
        )
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.Nonpositives,
        )
    end
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Zeros,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    if vecofvars
        ys, vc = MOI.add_constrained_variables(model, MOI.Nonpositives(1))
        y = ys[1]
    else
        y = MOI.add_variable(model)
        func = MOI.VectorAffineFunction{Float64}(MOI.VectorOfVariables([y]))
        vc = MOI.add_constraint(model, func, MOI.Nonpositives(1))
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
        MOI.Zeros(3),
    )
    if vecofvars
        # test fallback
        vz = MOI.add_constraint(model, [z], MOI.Nonnegatives(1))
    else
        vz = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction(
                [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z))],
                [0.0],
            ),
            MOI.Nonnegatives(1),
        )
    end
    vov = MOI.VectorOfVariables([s])
    if vecofvars
        vs = MOI.add_constraint(model, vov, MOI.Zeros(1))
    else
        vs = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{Float64}(vov),
            MOI.Zeros(1),
        )
    end
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Zeros,
            }(),
        ) == 2 - vecofvars
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                vecofvars ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                MOI.Nonpositives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                vecofvars ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                MOI.Nonnegatives,
            }(),
        ) == 1
    end
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -82 atol = atol rtol = rtol
        if config.dual_objective_value
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
        if config.duals
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
end

function lin2vtest(model::MOI.ModelLike, config::Config)
    return _lin2test(model, config, true)
end
function lin2ftest(model::MOI.ModelLike, config::Config)
    return _lin2test(model, config, false)
end

function lin3test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Problem LIN3 - Infeasible LP
    # min  0
    # s.t. x ≥ 1
    #      x ≤ -1
    # in conic form:
    # min 0
    # s.t. -1 + x ∈ R₊
    #       1 + x ∈ R₋
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Nonpositives,
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Nonnegatives,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
            [-1.0],
        ),
        MOI.Nonnegatives(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
            [1.0],
        ),
        MOI.Nonpositives(1),
    )
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Nonnegatives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Nonpositives,
            }(),
        ) == 1
    end
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        if config.infeas_certificates
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
            @test MOI.get(model, MOI.ResultCount()) > 0
        else
            @test MOI.get(model, MOI.TerminationStatus()) in
                  [MOI.INFEASIBLE, MOI.INFEASIBLE_OR_UNBOUNDED]
        end
        if MOI.get(model, MOI.ResultCount()) > 0
            @test MOI.get(model, MOI.PrimalStatus()) in
                  (MOI.NO_SOLUTION, MOI.INFEASIBLE_POINT)
            if config.duals && config.infeas_certificates
                @test MOI.get(model, MOI.DualStatus()) ==
                      MOI.INFEASIBILITY_CERTIFICATE
            end
        end
        # TODO test dual feasibility and objective sign
    end
end

function lin4test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Problem LIN4 - Infeasible LP
    # min  0
    # s.t. x ≥ 1
    #      x ≤ 0
    # in conic form:
    # min 0
    # s.t. -1 + x ∈ R₊
    #           x ∈ R₋
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Nonnegatives,
    )
    @test MOI.supports_add_constrained_variables(model, MOI.Nonpositives)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    xs, cx = MOI.add_constrained_variables(model, MOI.Nonpositives(1))
    x = xs[1]
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
            [-1.0],
        ),
        MOI.Nonnegatives(1),
    )
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Nonnegatives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonpositives}(),
        ) == 1
    end
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        if config.infeas_certificates
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
            @test MOI.get(model, MOI.ResultCount()) > 0
        else
            @test MOI.get(model, MOI.TerminationStatus()) in
                  [MOI.INFEASIBLE, MOI.INFEASIBLE_OR_UNBOUNDED]
        end
        if MOI.get(model, MOI.ResultCount()) > 0
            @test MOI.get(model, MOI.PrimalStatus()) in
                  (MOI.NO_SOLUTION, MOI.INFEASIBLE_POINT)
            if config.duals && config.infeas_certificates
                @test MOI.get(model, MOI.DualStatus()) ==
                      MOI.INFEASIBILITY_CERTIFICATE
            end
        end
        # TODO test dual feasibility and objective sign
    end
end

const lintests = Dict(
    "lin1v" => lin1vtest,
    "lin1f" => lin1ftest,
    "lin2v" => lin2vtest,
    "lin2f" => lin2ftest,
    "lin3" => lin3test,
    "lin4" => lin4test,
)

@moitestset lin

function _norminf1test(model::MOI.ModelLike, config::Config, vecofvars::Bool)
    atol = config.atol
    rtol = config.rtol
    # Problem NormInf1
    # max 0x + 1y + 1z
    #  st  x == 1
    #      y == 1/2
    #      x >= ||(y,z)||_∞
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    if vecofvars
        @test MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.Zeros)
    else
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.Zeros,
        )
    end
    @test MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.NormInfinityCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
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
        MOI.Zeros(1),
    )
    ceq2 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, y))],
            [-0.5],
        ),
        MOI.Zeros(1),
    )
    vov = MOI.VectorOfVariables([x, y, z])
    if vecofvars
        ccone = MOI.add_constraint(model, vov, MOI.NormInfinityCone(3))
    else
        ccone = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{Float64}(vov),
            MOI.NormInfinityCone(3),
        )
    end
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Zeros,
            }(),
        ) == 2
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                vecofvars ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                MOI.NormInfinityCone,
            }(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{Float64}, MOI.Zeros) in loc
    @test (
        vecofvars ? MOI.VectorOfVariables : MOI.VectorAffineFunction{Float64},
        MOI.NormInfinityCone,
    ) in loc
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1.5 atol = atol rtol = rtol
        if config.dual_objective_value
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
        if config.duals
            @test MOI.get(model, MOI.ConstraintDual(), ceq1) ≈ [-1] atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), ceq2) ≈ [-1] atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), ccone) ≈ [1.0, 0.0, -1.0] atol =
                atol rtol = rtol
        end
    end
end

function norminf1vtest(model::MOI.ModelLike, config::Config)
    return _norminf1test(model, config, true)
end
function norminf1ftest(model::MOI.ModelLike, config::Config)
    return _norminf1test(model, config, false)
end

function norminf2test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Problem NormInf2 - Infeasible
    # min 0
    # s.t. y ≥ 2
    #      x ≤ 1
    #      |y| ≤ x
    # in conic form:
    # min 0
    # s.t. -2 + y ∈ R₊
    #      -1 + x ∈ R₋
    #       (x,y) ∈ NormInf₂
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Nonnegatives,
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Nonpositives,
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NormInfinityCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x, y = MOI.add_variables(model, 2)
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, y))],
            [-2.0],
        ),
        MOI.Nonnegatives(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
            [-1.0],
        ),
        MOI.Nonpositives(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.(1.0, [x, y])),
            zeros(2),
        ),
        MOI.NormInfinityCone(2),
    )
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Nonnegatives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Nonpositives,
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
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
        @test MOI.get(model, MOI.PrimalStatus()) in
              (MOI.NO_SOLUTION, MOI.INFEASIBLE_POINT)
        if config.duals && config.infeas_certificates
            @test MOI.get(model, MOI.DualStatus()) ==
                  MOI.INFEASIBILITY_CERTIFICATE
        end
        # TODO test dual feasibility and objective sign
    end
end

function norminf3test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Problem NormInf3
    # min x
    #  st  (-1 + x, 2 .+ y) in NormInf(1 + n)
    #      (1 .+ y) in Nonnegatives(n)
    # let n = 3. optimal solution: y .= -1, x = 2
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NormInfinityCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    y = MOI.add_variables(model, 3)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
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
    nonneg = MOI.add_constraint(model, nonneg_vaf, MOI.Nonnegatives(3))
    if config.query_number_of_constraints
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
                MOI.Nonnegatives,
            }(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{Float64}, MOI.NormInfinityCone) in loc
    @test (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) in loc
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2 atol = atol rtol = rtol
        if config.dual_objective_value
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
        if config.duals
            dual_nonneg = MOI.get(model, MOI.ConstraintDual(), nonneg)
            @test minimum(dual_nonneg) >= -atol
            @test sum(dual_nonneg) ≈ 1.0 atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), norminf) ≈
                  vcat(1, -dual_nonneg) atol = atol rtol = rtol
        end
    end
end

const norminftests = Dict(
    "norminf1v" => norminf1vtest,
    "norminf1f" => norminf1ftest,
    "norminf2" => norminf2test,
    "norminf3" => norminf3test,
)

@moitestset norminf

function _normone1test(model::MOI.ModelLike, config::Config, vecofvars::Bool)
    atol = config.atol
    rtol = config.rtol
    # Problem NormOne1
    # max 0x + 1y + 1z
    #  st  x == 1
    #      y == 1/2
    #      x >= ||(y,z)||_1
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    if vecofvars
        @test MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.Zeros)
    else
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.Zeros,
        )
    end
    @test MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.NormOneCone)
    MOI.empty!(model)
    @test MOI.is_empty(model)
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
        MOI.Zeros(1),
    )
    ceq2 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, y))],
            [-0.5],
        ),
        MOI.Zeros(1),
    )
    vov = MOI.VectorOfVariables([x, y, z])
    if vecofvars
        ccone = MOI.add_constraint(model, vov, MOI.NormOneCone(3))
    else
        ccone = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction{Float64}(vov),
            MOI.NormOneCone(3),
        )
    end
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Zeros,
            }(),
        ) == 2
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                vecofvars ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                MOI.NormOneCone,
            }(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{Float64}, MOI.Zeros) in loc
    @test (
        vecofvars ? MOI.VectorOfVariables : MOI.VectorAffineFunction{Float64},
        MOI.NormOneCone,
    ) in loc
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1 atol = atol rtol = rtol
        if config.dual_objective_value
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
        if config.duals
            @test MOI.get(model, MOI.ConstraintDual(), ceq1) ≈ [-1] atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), ceq2) ≈ [0] atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), ccone) ≈
                  [1.0, -1.0, -1.0] atol = atol rtol = rtol
        end
    end
end

function normone1vtest(model::MOI.ModelLike, config::Config)
    return _normone1test(model, config, true)
end
function normone1ftest(model::MOI.ModelLike, config::Config)
    return _normone1test(model, config, false)
end

function normone2test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Problem NormOne2 - Infeasible
    # min 0
    # s.t. y ≥ 2
    #      x ≤ 1
    #      |y| ≤ x
    # in conic form:
    # min 0
    # s.t. -2 + y ∈ R₊
    #      -1 + x ∈ R₋
    #       (x,y) ∈ NormOne₂
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Nonnegatives,
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Nonpositives,
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NormOneCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x, y = MOI.add_variables(model, 2)
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, y))],
            [-2.0],
        ),
        MOI.Nonnegatives(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
            [-1.0],
        ),
        MOI.Nonpositives(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.(1.0, [x, y])),
            zeros(2),
        ),
        MOI.NormOneCone(2),
    )
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Nonnegatives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Nonpositives,
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
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
        @test MOI.get(model, MOI.PrimalStatus()) in
              (MOI.NO_SOLUTION, MOI.INFEASIBLE_POINT)
        if config.duals && config.infeas_certificates
            @test MOI.get(model, MOI.DualStatus()) ==
                  MOI.INFEASIBILITY_CERTIFICATE
        end
        # TODO test dual feasibility and objective sign
    end
end

function normone3test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Problem NormOne3
    # min x
    #  st  (-1 + x, 2 .+ y) in NormOne(1 + n)
    #      (1 .+ y) in Nonnegatives(n)
    # let n = 3. optimal solution: y .= -1, x = 4
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NormOneCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    y = MOI.add_variables(model, 3)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
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
    nonneg = MOI.add_constraint(model, nonneg_vaf, MOI.Nonnegatives(3))
    if config.query_number_of_constraints
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
                MOI.Nonnegatives,
            }(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{Float64}, MOI.NormOneCone) in loc
    @test (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) in loc
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 4 atol = atol rtol = rtol
        if config.dual_objective_value
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
        if config.duals
            @test MOI.get(model, MOI.ConstraintDual(), norminf) ≈
                  vcat(1, fill(-1, 3)) atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), nonneg) ≈ ones(3) atol =
                atol rtol = rtol
        end
    end
end

const normonetests = Dict(
    "normone1v" => normone1vtest,
    "normone1f" => normone1ftest,
    "normone2" => normone2test,
    "normone3" => normone3test,
)

@moitestset normone

function _soc1test(model::MOI.ModelLike, config::Config, vecofvars::Bool)
    atol = config.atol
    rtol = config.rtol
    # Problem SOC1
    # max 0x + 1y + 1z
    #  st  x == 1
    #      x >= ||(y,z)||
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    if vecofvars
        @test MOI.supports_add_constrained_variables(model, MOI.SecondOrderCone)
    else
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.SecondOrderCone,
        )
    end
    @test MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.Zeros)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    if vecofvars
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
        MOI.Zeros(1),
    )
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Zeros,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                vecofvars ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                MOI.SecondOrderCone,
            }(),
        ) == 1
    end
    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{Float64}, MOI.Zeros) in loc
    @test (
        vecofvars ? MOI.VectorOfVariables : MOI.VectorAffineFunction{Float64},
        MOI.SecondOrderCone,
    ) in loc
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ √2 atol = atol rtol = rtol
        if config.dual_objective_value
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
        if config.duals
            @test MOI.get(model, MOI.ConstraintDual(), ceq) ≈ [-√2] atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), csoc) ≈ [√2, -1.0, -1.0] atol =
                atol rtol = rtol
        end
    end
end

function soc1vtest(model::MOI.ModelLike, config::Config)
    return _soc1test(model, config, true)
end
function soc1ftest(model::MOI.ModelLike, config::Config)
    return _soc1test(model, config, false)
end

function _soc2test(model::MOI.ModelLike, config::Config, nonneg::Bool)
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
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Zeros,
    )
    if nonneg
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.Nonnegatives,
        )
    else
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.Nonpositives,
        )
    end
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.SecondOrderCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
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
            MOI.Nonnegatives(1),
        )
    else
        cnon = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction(
                [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(-1.0, y))],
                [1 / √2],
            ),
            MOI.Nonpositives(1),
        )
    end
    ceq = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(-1.0, t))],
            [1.0],
        ),
        MOI.Zeros(1),
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
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                nonneg ? MOI.Nonnegatives : MOI.Nonpositives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Zeros,
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
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -1 / √2 atol = atol rtol =
            rtol
        if config.dual_objective_value
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
        if config.duals
            @test MOI.get(model, MOI.ConstraintDual(), cnon) ≈
                  [nonneg ? 1.0 : -1.0] atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), ceq) ≈ [√2] atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), csoc) ≈ [√2, 1.0, -1.0] atol =
                atol rtol = rtol
        end
    end
end

function soc2ntest(model::MOI.ModelLike, config::Config)
    return _soc2test(model, config, true)
end
function soc2ptest(model::MOI.ModelLike, config::Config)
    return _soc2test(model, config, false)
end

function soc3test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Problem SOC3 - Infeasible
    # min 0
    # s.t. y ≥ 2
    #      x ≤ 1
    #      |y| ≤ x
    # in conic form:
    # min 0
    # s.t. -2 + y ∈ R₊
    #      -1 + x ∈ R₋
    #       (x,y) ∈ SOC₂
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Nonnegatives,
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Nonpositives,
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.SecondOrderCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x, y = MOI.add_variables(model, 2)
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, y))],
            [-2.0],
        ),
        MOI.Nonnegatives(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
            [-1.0],
        ),
        MOI.Nonpositives(1),
    )
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.(1.0, [x, y])),
            zeros(2),
        ),
        MOI.SecondOrderCone(2),
    )
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Nonnegatives,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Nonpositives,
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
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
        @test MOI.get(model, MOI.PrimalStatus()) in
              (MOI.NO_SOLUTION, MOI.INFEASIBLE_POINT)
        if config.duals && config.infeas_certificates
            @test MOI.get(model, MOI.DualStatus()) ==
                  MOI.INFEASIBILITY_CERTIFICATE
        end
        # TODO test dual feasibility and objective sign
    end
end

function soc4test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Problem SOC4
    # min 0x[1] - 2x[2] - 1x[3]
    #  st  x[1]                                == 1 (c1a)
    #              x[2]         - x[4]         == 0 (c1b)
    #                      x[3]         - x[5] == 0 (c1c)
    #      x[1] >= ||(x[4],x[5])||                  (c2)
    # in conic form:
    # min  c^Tx
    # s.t. Ax + b ∈ {0}₃
    #      (x[1],x[4],x[5]) ∈ SOC₃
    # Like SOCINT1 but with copies of variables and integrality relaxed
    # Tests out-of-order indices in cones
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Zeros,
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.SecondOrderCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
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
        MOI.Zeros(3),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.VectorOfVariables([x[1], x[4], x[5]]),
        MOI.SecondOrderCone(3),
    )
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Zeros,
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
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -√5 atol = atol rtol = rtol
        if config.dual_objective_value
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ -√5 atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈
              [1.0, 2 / √5, 1 / √5, 2 / √5, 1 / √5] atol = atol rtol = rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c1) ≈ zeros(3) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ [1.0, 2 / √5, 1 / √5] atol =
            atol rtol = rtol
        if config.duals
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ [-√5, -2.0, -1.0] atol =
                atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ [√5, -2.0, -1.0] atol =
                atol rtol = rtol
        end
    end
end

const soctests = Dict(
    "soc1v" => soc1vtest,
    "soc1f" => soc1ftest,
    "soc2n" => soc2ntest,
    "soc2p" => soc2ptest,
    "soc3" => soc3test,
    "soc4" => soc4test,
)

@moitestset soc

function _rotatedsoc1test(model::MOI.ModelLike, config::Config, abvars::Bool)
    atol = config.atol
    rtol = config.rtol
    # Problem SOCRotated1v
    # min 0a + 0b - 1x - 1y
    #  st  a            == 1/2
    #  st  b            == 1
    #      2a*b >= x^2+y^2
    # Problem SOCRotated1f - Problem SOCRotated1v with a and b substituted
    # min          -y - z
    #  st [0.5] - [      ] SOCRotated
    #     [1.0] - [      ] SOCRotated
    #     [0.0] - [-y    ] SOCRotated
    #     [0.0] - [    -z] SOCRotated
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    if abvars
        @test MOI.supports_constraint(
            model,
            MOI.VariableIndex,
            MOI.EqualTo{Float64},
        )
        @test MOI.supports_add_constrained_variables(
            model,
            MOI.RotatedSecondOrderCone,
        )
    else
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.RotatedSecondOrderCone,
        )
    end
    MOI.empty!(model)
    @test MOI.is_empty(model)
    if abvars
        abx, rsoc =
            MOI.add_constrained_variables(model, MOI.RotatedSecondOrderCone(4))
        a, b, x1, x2 = abx
        x = [x1, x2]
        vc1 = MOI.add_constraint(model, a, MOI.EqualTo(0.5))
        # We test this after the creation of every `VariableIndex` constraint
        # to ensure a good coverage of corner cases.
        @test vc1.value == a.value
        vc2 = MOI.add_constraint(model, b, MOI.EqualTo(1.0))
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
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.EqualTo{Float64}}(),
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
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ √2 atol = atol rtol = rtol
        if config.dual_objective_value
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
        if config.duals
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
end

function rotatedsoc1vtest(model::MOI.ModelLike, config::Config)
    return _rotatedsoc1test(model, config, true)
end
function rotatedsoc1ftest(model::MOI.ModelLike, config::Config)
    return _rotatedsoc1test(model, config, false)
end

function rotatedsoc2test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Problem SOCRotated2 - Infeasible
    # min 0
    # s.t.
    #      x ≤ 1
    #      y = 1/2
    #      z ≥ 2
    #      z^2 ≤ 2x*y
    # in conic form:
    # min 0
    # s.t.
    #      -1 + x ∈ R₋
    #     1/2 - y ∈ {0}
    #      -2 + z ∈ R₊
    #       (x,y,z) ∈ SOCRotated
    b = [-2, -1, 1 / 2]
    c = [0.0, 0.0, 0.0]
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.EqualTo{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.LessThan{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{Float64},
    )
    @test MOI.supports_add_constrained_variables(
        model,
        MOI.RotatedSecondOrderCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x, rsoc =
        MOI.add_constrained_variables(model, MOI.RotatedSecondOrderCone(3))
    vc1 = MOI.add_constraint(model, x[1], MOI.LessThan(1.0))
    @test vc1.value == x[1].value
    vc2 = MOI.add_constraint(model, x[2], MOI.EqualTo(0.5))
    @test vc2.value == x[2].value
    vc3 = MOI.add_constraint(model, x[3], MOI.GreaterThan(2.0))
    @test vc3.value == x[3].value
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.LessThan{Float64}}(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.EqualTo{Float64}}(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VariableIndex,
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
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) in
              [MOI.INFEASIBLE, MOI.INFEASIBLE_OR_UNBOUNDED]
        if config.duals
            if config.infeas_certificates
                @test MOI.get(model, MOI.DualStatus()) in [
                    MOI.INFEASIBILITY_CERTIFICATE,
                    MOI.NEARLY_INFEASIBILITY_CERTIFICATE,
                ]
            end
            y1 = MOI.get(model, MOI.ConstraintDual(), vc1)
            @test y1 < -atol # Should be strictly negative
            y2 = MOI.get(model, MOI.ConstraintDual(), vc2)
            y3 = MOI.get(model, MOI.ConstraintDual(), vc3)
            @test y3 > atol # Should be strictly positive
            y = [y1, y2, y3]
            vardual = MOI.get(model, MOI.ConstraintDual(), rsoc)
            @test vardual ≈ -y atol = atol rtol = rtol
            @test 2 * vardual[1] * vardual[2] ≥ vardual[3]^2 - atol
            @test dot(b, y) > atol
        end
    end
end

function rotatedsoc3test(model::MOI.ModelLike, config::Config; n = 2, ub = 3.0)
    atol = config.atol
    rtol = config.rtol
    # Problem SOCRotated3
    # max v
    # s.t.
    #      x[1:2] ≥ 0
    #      0 ≤ u ≤ 3.0
    #      v
    #      t1 == 1
    #      t2 == 1
    # [t1/√2, t2/√2, x] in SOC4
    # [x1/√2, u/√2,  v] in SOC3
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.EqualTo{Float64},
    )
    @test MOI.supports_add_constrained_variables(model, MOI.Nonnegatives)
    @test MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.LessThan{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.RotatedSecondOrderCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x, cx = MOI.add_constrained_variables(model, MOI.Nonnegatives(n))
    u = MOI.add_variable(model)
    v = MOI.add_variable(model)
    t = MOI.add_variables(model, 2)
    ct1 = MOI.add_constraint(model, t[1], MOI.EqualTo(1.0))
    @test ct1.value == t[1].value
    ct2 = MOI.add_constraint(model, t[2], MOI.EqualTo(1.0))
    @test ct2.value == t[2].value
    cu1 = MOI.add_constraint(model, u, MOI.GreaterThan(0.0))
    @test cu1.value == u.value
    cu2 = MOI.add_constraint(model, u, MOI.LessThan(ub))
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
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.EqualTo{Float64}}(),
        ) == 2
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonnegatives}(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VariableIndex,
                MOI.GreaterThan{Float64},
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VariableIndex,MOI.LessThan{Float64}}(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VariableIndex,
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
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ √ub atol = atol rtol = rtol
        if config.dual_objective_value
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
        if config.duals
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
end

function rotatedsoc4test(model::MOI.ModelLike, config::Config; n = 2, ub = 3.0)
    atol = config.atol
    rtol = config.rtol
    # Problem SOCRotated4
    # max x + y
    # s.t.
    #      t + u ≤ 2              (1)
    # [t, u, x, y] in RSOC(4)     (2)
    # Solution:
    # By AM-QM: (x+y)/2 ≤ √((x^2+y^2)/2) with equality iff x = y
    # That is,
    #     (x + y)^2/2 ≤ x^2 + y^2 (3)
    # By AM-GM: √tu ≤ (t+u)/2 with equality iff t = u
    # That is,
    #    2tu ≤ (t + u)^2/2        (4)
    # Combining (2), (3) and (4), we have
    #    |x + y| ≤ t + u          (5)
    # with equality iff x = y and t = u.
    # Combining (1) and (5), we have
    #    x + y ≤ 2
    # with equality iff x = y.
    # We conclude that the optimal solution is x = y = t = u = 1
    # with objective value 2.
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    @test MOI.supports_add_constrained_variables(
        model,
        MOI.RotatedSecondOrderCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    v, cv = MOI.add_constrained_variables(model, MOI.RotatedSecondOrderCone(4))
    t, u, x, y = v
    ft = t
    fu = u
    c = MOI.add_constraint(model, 1.0ft + 1.0fu, MOI.LessThan(2.0))
    fx = x
    fy = y
    func = 1.0fx + 1.0fy
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(func)}(), func)
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2.0 atol = atol rtol = rtol
        if config.dual_objective_value
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
        if config.duals
            @test MOI.get(model, MOI.ConstraintDual(), cv) ≈
                  [1.0, 1.0, -1.0, -1.0] atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1.0 atol = atol rtol =
                rtol
        end
    end
end

const rsoctests = Dict(
    "rotatedsoc1v" => rotatedsoc1vtest,
    "rotatedsoc1f" => rotatedsoc1ftest,
    "rotatedsoc2" => rotatedsoc2test,
    "rotatedsoc3" => rotatedsoc3test,
    "rotatedsoc4" => rotatedsoc4test,
)

@moitestset rsoc

function _geomean1test(model::MOI.ModelLike, config::Config, vecofvars, n = 3)
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
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    if vecofvars
        @test MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.GeometricMeanCone,
        )
    else
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.GeometricMeanCone,
        )
    end
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    t = MOI.add_variable(model)
    x = MOI.add_variables(model, n)
    vov = MOI.VectorOfVariables([t; x])
    if vecofvars
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
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                vecofvars ? MOI.VectorOfVariables :
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
    if config.solve
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
        if config.duals
            @test MOI.get(model, MOI.ConstraintDual(), gmc) ≈
                  vcat(-1.0, fill(inv(n), n)) atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -inv(n) atol = atol rtol =
                rtol
        end
    end
end

function geomean1vtest(model::MOI.ModelLike, config::Config)
    return _geomean1test(model, config, true)
end
function geomean1ftest(model::MOI.ModelLike, config::Config)
    return _geomean1test(model, config, false)
end

# addresses bug https://github.com/jump-dev/MathOptInterface.jl/pull/962
function _geomean2test(model::MOI.ModelLike, config::Config, vecofvars)
    atol = config.atol
    rtol = config.rtol
    # Problem GeoMean2
    # max t
    # st  (t,x_1,x_2,...,x_9) ∈ GeometricMeanCone(10)
    #     x_1 == x_2, ..., x_9 == 1
    # the optimal solution is 1 with optimal value 1
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    if vecofvars
        @test MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.GeometricMeanCone,
        )
    else
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.GeometricMeanCone,
        )
    end
    MOI.empty!(model)
    @test MOI.is_empty(model)
    n = 9
    t = MOI.add_variable(model)
    x = MOI.add_variables(model, n)
    @test MOI.get(model, MOI.NumberOfVariables()) == n + 1
    vov = MOI.VectorOfVariables([t; x])
    if vecofvars
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
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                vecofvars ? MOI.VectorOfVariables :
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
    if config.solve
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
        if config.duals
            @test MOI.get(model, MOI.ConstraintDual(), gmc) ≈
                  vcat(-1, fill(inv(n), n)) atol = atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), cx) ≈ fill(-inv(n), n) atol =
                atol rtol = rtol
        end
    end
end

function geomean2vtest(model::MOI.ModelLike, config::Config)
    return _geomean2test(model, config, true)
end
function geomean2ftest(model::MOI.ModelLike, config::Config)
    return _geomean2test(model, config, false)
end

# Tests case where the dimension of the geometric mean cone is 2
function _geomean3test(model::MOI.ModelLike, config::Config, vecofvars)
    atol = config.atol
    rtol = config.rtol
    # Problem GeoMean3
    # max 2t
    # st  (t,x) ∈ GeometricMeanCone(2)
    #     x <= 2
    # the optimal solution is (t, x) = (2, 2) with objective value 4
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    if vecofvars
        @test MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.GeometricMeanCone,
        )
    else
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.GeometricMeanCone,
        )
    end
    MOI.empty!(model)
    @test MOI.is_empty(model)
    t = MOI.add_variable(model)
    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    vov = MOI.VectorOfVariables([t; x])
    if vecofvars
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
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                vecofvars ? MOI.VectorOfVariables :
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
    if config.solve
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
        if config.duals
            @test MOI.get(model, MOI.ConstraintDual(), gmc) ≈ [-2.0, 2.0] atol =
                atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintDual(), cx) ≈ -2.0 atol = atol rtol =
                rtol
        end
    end
end

function geomean3vtest(model::MOI.ModelLike, config::Config)
    return _geomean3test(model, config, true)
end
function geomean3ftest(model::MOI.ModelLike, config::Config)
    return _geomean3test(model, config, false)
end

geomeantests = Dict(
    "geomean1v" => geomean1vtest,
    "geomean1f" => geomean1ftest,
    "geomean2f" => geomean2ftest,
    "geomean2v" => geomean2vtest,
    "geomean3f" => geomean3ftest,
    "geomean3v" => geomean3vtest,
)

@moitestset geomean

function _exp1test(model::MOI.ModelLike, config::Config, vecofvars::Bool)
    atol = config.atol
    rtol = config.rtol
    # Problem EXP1 - ExpPrimal
    # min x + y + z
    #  st  y e^(x/y) <= z, y > 0 (i.e (x, y, z) are in the exponential primal cone)
    #      x == 1
    #      y == 2
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    if vecofvars
        @test MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.ExponentialCone,
        )
    else
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.ExponentialCone,
        )
    end
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    v = MOI.add_variables(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3
    vov = MOI.VectorOfVariables(v)
    if vecofvars
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
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3 + 2exp(1 / 2) atol = atol rtol =
            rtol
        if config.dual_objective_value
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
        if config.duals
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
end

function exp1vtest(model::MOI.ModelLike, config::Config)
    return _exp1test(model, config, true)
end
function exp1ftest(model::MOI.ModelLike, config::Config)
    return _exp1test(model, config, false)
end

function exp2test(model::MOI.ModelLike, config::Config)
    # Problem EXP2
    # A problem where ECOS was failing
    atol = config.atol
    rtol = config.rtol
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.ExponentialCone,
    )
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Nonnegatives,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
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
        MOI.Nonnegatives(3),
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
        MOI.Nonnegatives(3),
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
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ exp(-0.3) atol = atol rtol =
            rtol
        if config.dual_objective_value
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
        if config.duals
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
end

function exp3test(model::MOI.ModelLike, config::Config)
    # Problem EXP3
    # A problem where ECOS was failing
    atol = config.atol
    rtol = config.rtol
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.LessThan{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.ExponentialCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    xc = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 0.0),
        MOI.LessThan(4.0),
    )
    yc = MOI.add_constraint(model, y, MOI.LessThan(5.0))
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
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ log(5) atol = atol rtol =
            rtol
        if config.dual_objective_value
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
        if config.duals
            @test MOI.get(model, MOI.ConstraintDual(), xc) ≈ 0.0 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), yc) ≈ -1 / 5 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ConstraintDual(), ec) ≈
                  [-1.0, log(5) - 1, 1 / 5] atol = atol rtol = rtol
        end
    end
end

exptests = Dict(
    "exp1v" => exp1vtest,
    "exp1f" => exp1ftest,
    "exp2" => exp2test,
    "exp3" => exp3test,
)

@moitestset exp

function _dualexp1test(model::MOI.ModelLike, config::Config, vecofvars::Bool)
    atol = config.atol
    rtol = config.rtol
    # Problem dual exp
    # max 2x_2 + x_1
    # s.t.
    # x_1 + u == 1
    # x_2 + v == 1
    # w == 1
    # (u, v, w) ∈ DualExponentialCone
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    if vecofvars
        @test MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.DualExponentialCone,
        )
    else
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.DualExponentialCone,
        )
    end
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    v = MOI.add_variables(model, 3)
    x = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 5
    vov = MOI.VectorOfVariables(v)
    if vecofvars
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
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3 + 2exp(1 / 2) atol = atol rtol =
            rtol
        if config.dual_objective_value
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
        if config.duals
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
end

function dualexp1vtest(model::MOI.ModelLike, config::Config)
    return _dualexp1test(model, config, true)
end
function dualexp1ftest(model::MOI.ModelLike, config::Config)
    return _dualexp1test(model, config, false)
end

dualexptests = Dict("dualexp1v" => dualexp1vtest, "dualexp1f" => dualexp1ftest)

@moitestset dualexp

function _pow1test(model::MOI.ModelLike, config::Config, vecofvars::Bool)
    atol = config.atol
    rtol = config.rtol
    # Problem POW1
    # max z
    #  st  x^0.9 * y^(0.1) >= |z| (i.e (x, y, z) are in the 3d power cone with a=0.9)
    #      x == 2
    #      y == 1
    # Dual
    # min -2α - β
    #  st (u/0.9)^0.9 (v/0.1)^0.1 >= |w|
    #     u + α = 0
    #     v + β = 0
    #     w = -1
    a = 0.9
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    if vecofvars
        @test MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.PowerCone{Float64},
        )
    else
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.PowerCone{Float64},
        )
    end
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    v = MOI.add_variables(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3
    vov = MOI.VectorOfVariables(v)
    if vecofvars
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
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
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
        if config.duals
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
end

function pow1vtest(model::MOI.ModelLike, config::Config)
    return _pow1test(model, config, true)
end
function pow1ftest(model::MOI.ModelLike, config::Config)
    return _pow1test(model, config, false)
end

powtests = Dict("pow1v" => pow1vtest, "pow1f" => pow1ftest)

@moitestset pow

function _dualpow1test(
    model::MOI.ModelLike,
    config::Config,
    vecofvars::Bool;
    exponent::Float64 = 0.9,
)
    atol = config.atol
    rtol = config.rtol
    # Problem dual POW1
    # min -x_1 - x_2
    #  st  x_1 + u == 0
    #      x_2 + v == 0
    #      w == 1
    #     (u, v, w) ∈ DualPowerCone(exponent)
    # By the Weighted AM–GM inequality, you have
    # 0.9a + 0.1b >= a^0.9 b^0.1
    # with equality if and only if a == b
    # here taking a = u/0.9 and b = v/0.1, we have
    # u + v >= (u/0.9)^0.9 (v/0.1)^0.1
    # with equality if and only if u/0.9 == v/0.1.
    # Here the best you can do is u + v == 1 and for that inequality must hold so u = 9v
    # hence you get v = 0.1 and u = 0.9.
    # The same works for other values of exponent as key word argument
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    if vecofvars
        @test MOI.supports_constraint(
            model,
            MOI.VectorOfVariables,
            MOI.DualPowerCone{Float64},
        )
    else
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            MOI.DualPowerCone{Float64},
        )
    end
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    v = MOI.add_variables(model, 3)
    x = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 5
    vov = MOI.VectorOfVariables(v)
    if vecofvars
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
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
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
        if config.duals
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
end

function dualpow1vtest(model::MOI.ModelLike, config::Config)
    return _dualpow1test(model, config, true)
end
function dualpow1ftest(model::MOI.ModelLike, config::Config)
    return _dualpow1test(model, config, false)
end

dualpowtests = Dict("dualpow1v" => dualpow1vtest, "dualpow1f" => dualpow1ftest)

@moitestset dualpow

function relentr1test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Problem RelEntr1
    # min u
    #  st  u >= 2*log(2/1) + 3*log(3/5)  (i.e. (u, 1, 5, 2, 3) in RelativeEntropyCone(5))
    # Optimal solution is:
    # u = 2*log(2/1) + 3*log(3/5) ≈ -0.1461825
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.RelativeEntropyCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
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
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), u)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        u_opt = 2 * log(2) + 3 * log(3 / 5)
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ u_opt atol = atol rtol =
            rtol
        if config.dual_objective_value
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ u_opt atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), u) ≈ u_opt atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), relentr) ≈
              [u_opt, 1, 5, 2, 3] atol = atol rtol = rtol
        if config.duals
            @test MOI.get(model, MOI.ConstraintDual(), relentr) ≈
                  [1, 2, 0.6, log(0.5) - 1, log(5 / 3) - 1] atol = atol rtol =
                rtol
        end
    end
end

relentrtests = Dict("relentr1" => relentr1test)

@moitestset relentr

function normspec1test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Problem NormSpec1
    # min t
    #  st  t >= sigma_1([1 1 0; 1 -1 1]) (i.e (t, 1, 1, 1, -1, 0, 1]) is in NormSpectralCone(2, 3))
    # Singular values are [sqrt(3), sqrt(2)], so optimal solution is:
    # t = sqrt(3)
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NormSpectralCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
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
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        rt3 = sqrt(3)
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ rt3 atol = atol rtol = rtol
        if config.dual_objective_value
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ rt3 atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ rt3 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), spec) ≈ vcat(rt3, data) atol =
            atol rtol = rtol
        if config.duals
            invrt3 = inv(rt3)
            @test MOI.get(model, MOI.ConstraintDual(), spec) ≈
                  Float64[1, 0, -invrt3, 0, invrt3, 0, -invrt3] atol = atol rtol =
                rtol
        end
    end
end

function normspec2test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Problem NormSpec2
    # min t
    #  st  t >= sigma_1([1 1; 1 -1; 0 1]) (i.e (t, 1, 1, 0, 1, -1, 1]) is in NormSpectralCone(3, 2))
    # Singular values are [sqrt(3), sqrt(2)], so optimal solution is:
    # t = sqrt(3)
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NormSpectralCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
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
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        rt3 = sqrt(3)
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ rt3 atol = atol rtol = rtol
        if config.dual_objective_value
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ rt3 atol = atol rtol =
                rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ rt3 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), spec) ≈ vcat(rt3, data) atol =
            atol rtol = rtol
        if config.duals
            invrt3 = inv(rt3)
            @test MOI.get(model, MOI.ConstraintDual(), spec) ≈
                  Float64[1, 0, 0, 0, -invrt3, invrt3, -invrt3] atol = atol rtol =
                rtol
        end
    end
end

normspectests = Dict("normspec1" => normspec1test, "normspec2" => normspec2test)

@moitestset normspec

function normnuc1test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Problem NormNuc1
    # min t
    #  st  t >= sum_i sigma_i([1 1 0; 1 -1 1]) (i.e (t, 1, 1, 1, -1, 0, 1]) is in NormNuclearCone(2, 3))
    # Singular values are [sqrt(3), sqrt(2)], so optimal solution is:
    # t = sqrt(3) + sqrt(2)
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NormNuclearCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
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
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        rt3 = sqrt(3)
        rt2 = sqrt(2)
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ rt3 + rt2 atol = atol rtol =
            rtol
        if config.dual_objective_value
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ rt3 + rt2 atol =
                atol rtol = rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ rt3 + rt2 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), nuc) ≈
              vcat(rt3 + rt2, data) atol = atol rtol = rtol
        if config.duals
            invrt2 = inv(rt2)
            invrt3 = inv(rt3)
            @test MOI.get(model, MOI.ConstraintDual(), nuc) ≈
                  Float64[1, -invrt2, -invrt3, -invrt2, invrt3, 0, -invrt3] atol =
                atol rtol = rtol
        end
    end
end

function normnuc2test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Problem NormNuc2
    # min t
    #  st  t >= sum_i sigma_i([1 1; 1 -1; 0 1]) (i.e (t, 1, 1, 0, 1, -1, 1]) is in NormNuclearCone(3, 2))
    # Singular values are [sqrt(3), sqrt(2)], so optimal solution is:
    # t = sqrt(3) + sqrt(2)
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.NormNuclearCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
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
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        rt3 = sqrt(3)
        rt2 = sqrt(2)
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ rt3 + rt2 atol = atol rtol =
            rtol
        if config.dual_objective_value
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ rt3 + rt2 atol =
                atol rtol = rtol
        end
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ rt3 + rt2 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), nuc) ≈
              vcat(rt3 + rt2, data) atol = atol rtol = rtol
        if config.duals
            invrt2 = inv(rt2)
            invrt3 = inv(rt3)
            @test MOI.get(model, MOI.ConstraintDual(), nuc) ≈
                  Float64[1, -invrt2, -invrt2, 0, -invrt3, invrt3, -invrt3] atol =
                atol rtol = rtol
        end
    end
end

normnuctests = Dict("normnuc1" => normnuc1test, "normnuc2" => normnuc2test)

@moitestset normnuc

function _psd0test(
    model::MOI.ModelLike,
    vecofvars::Bool,
    psdcone,
    config::Config,
)
    atol = config.atol
    rtol = config.rtol
    square = psdcone == MOI.PositiveSemidefiniteConeSquare
    # min X[1,1] + X[2,2]    max y
    #     X[2,1] = 1         [0   y/2     [ 1  0
    #                         y/2 0    <=   0  1]
    #     X >= 0              y free
    # Optimal solution:
    #     ⎛ 1   1 ⎞
    # X = ⎜       ⎟           y = 2
    #     ⎝ 1   1 ⎠
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    if vecofvars
        @test MOI.supports_constraint(model, MOI.VectorOfVariables, psdcone)
    else
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            psdcone,
        )
    end
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    X = MOI.add_variables(model, square ? 4 : 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == (square ? 4 : 3)
    vov = MOI.VectorOfVariables(X)
    if vecofvars
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
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                vecofvars ? MOI.VectorOfVariables :
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
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2 atol = atol rtol = rtol
        if config.dual_objective_value
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ 2 atol = atol rtol =
                rtol
        end
        Xv = square ? ones(4) : ones(3)
        @test MOI.get(model, MOI.VariablePrimal(), X) ≈ Xv atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cX) ≈ Xv atol = atol rtol =
            rtol
        if config.duals
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ 2 atol = atol rtol =
                rtol
            cXv = square ? [1, -2, 0, 1] : [1, -1, 1]
            @test MOI.get(model, MOI.ConstraintDual(), cX) ≈ cXv atol = atol rtol =
                rtol
        end
    end
end

function psdt0vtest(model::MOI.ModelLike, config::Config)
    return _psd0test(model, true, MOI.PositiveSemidefiniteConeTriangle, config)
end
function psdt0ftest(model::MOI.ModelLike, config::Config)
    return _psd0test(model, false, MOI.PositiveSemidefiniteConeTriangle, config)
end
function psds0vtest(model::MOI.ModelLike, config::Config)
    return _psd0test(model, true, MOI.PositiveSemidefiniteConeSquare, config)
end
function psds0ftest(model::MOI.ModelLike, config::Config)
    return _psd0test(model, false, MOI.PositiveSemidefiniteConeSquare, config)
end

function _psd1test(
    model::MOI.ModelLike,
    vecofvars::Bool,
    psdcone,
    config::Config,
)
    atol = config.atol
    rtol = config.rtol
    square = psdcone == MOI.PositiveSemidefiniteConeSquare
    # Problem SDP1 - sdo1 from MOSEK docs
    # From Mosek.jl/test/mathprogtestextra.jl, under license:
    #   Copyright (c) 2013 Ulf Worsoe, Mosek ApS
    #   Permission is hereby granted, free of charge, to any person obtaining a copy of this
    #   software and associated documentation files (the "Software"), to deal in the Software
    #   without restriction, including without limitation the rights to use, copy, modify, merge,
    #   publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
    #   to whom the Software is furnished to do so, subject to the following conditions:
    #   The above copyright notice and this permission notice shall be included in all copies or
    #   substantial portions of the Software.
    #
    #     | 2 1 0 |
    # min | 1 2 1 | . X + x1
    #     | 0 1 2 |
    #
    #
    # s.t. | 1 0 0 |
    #      | 0 1 0 | . X + x1 = 1
    #      | 0 0 1 |
    #
    #      | 1 1 1 |
    #      | 1 1 1 | . X + x2 + x3 = 1/2
    #      | 1 1 1 |
    #
    #      (x1,x2,x3) in C^3_q
    #      X in C_psd
    #
    # The dual is
    # max y1 + y2/2
    #
    # s.t. | y1+y2    y2    y2 |
    #      |    y2 y1+y2    y2 | in C_psd
    #      |    y2    y2 y1+y2 |
    #
    #      (1-y1, -y2, -y2) in C^3_q
    #
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
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )
    if vecofvars
        @test MOI.supports_constraint(model, MOI.VectorOfVariables, psdcone)
    else
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            psdcone,
        )
    end
    @test MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.SecondOrderCone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    X = MOI.add_variables(model, square ? 9 : 6)
    @test MOI.get(model, MOI.NumberOfVariables()) == (square ? 9 : 6)
    x = MOI.add_variables(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == (square ? 12 : 9)
    vov = MOI.VectorOfVariables(X)
    if vecofvars
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
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                vecofvars ? MOI.VectorOfVariables :
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
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ obj atol = atol rtol = rtol
        if config.dual_objective_value
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
        if config.duals
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
end

function psdt1vtest(model::MOI.ModelLike, config::Config)
    return _psd1test(model, true, MOI.PositiveSemidefiniteConeTriangle, config)
end
function psdt1ftest(model::MOI.ModelLike, config::Config)
    return _psd1test(model, false, MOI.PositiveSemidefiniteConeTriangle, config)
end
function psds1vtest(model::MOI.ModelLike, config::Config)
    return _psd1test(model, true, MOI.PositiveSemidefiniteConeSquare, config)
end
function psds1ftest(model::MOI.ModelLike, config::Config)
    return _psd1test(model, false, MOI.PositiveSemidefiniteConeSquare, config)
end

function psdt2test(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    # Caused getdual to fail on SCS and Mosek
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.GreaterThan{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Nonpositives,
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.PositiveSemidefiniteConeTriangle,
    )
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
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
        MOI.Nonpositives(6),
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
    if config.query_number_of_constraints
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
                MOI.Nonpositives,
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
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
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
        if config.duals
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
end

function _psd3test(model::MOI.ModelLike, psdcone, config::Config{T}) where {T}
    # min x
    # s.t. [x 1 1]
    #      [1 x 1] ⪰ 0
    #      [1 1 x]
    atol = config.atol
    rtol = config.rtol
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{T}, psdcone)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    fx = x
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
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        end
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ one(T) atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈
              ones(T, MOI.output_dimension(func)) atol = atol rtol = rtol
        if config.duals
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
end
function psdt3test(model, config)
    return _psd3test(model, MOI.PositiveSemidefiniteConeTriangle, config)
end
function psds3test(model, config)
    return _psd3test(model, MOI.PositiveSemidefiniteConeSquare, config)
end

# PSDConeTriangle
const psdttests = Dict(
    "psdt0v" => psdt0vtest,
    "psdt0f" => psdt0ftest,
    "psdt1v" => psdt1vtest,
    "psdt1f" => psdt1ftest,
    "psdt2" => psdt2test,
    "psdt3" => psdt3test,
)

@moitestset psdt

# PSDConeSquare
const psdstests = Dict(
    "psds0v" => psds0vtest,
    "psds0f" => psds0ftest,
    "psds1v" => psds1vtest,
    "psds1f" => psds1ftest,
    "psds3" => psds3test,
)

@moitestset psds

const sdptests = Dict("psdt" => psdttest, "psds" => psdstest)

@moitestset sdp true

function _det1test(
    model::MOI.ModelLike,
    config::Config,
    vecofvars::Bool,
    detcone,
)
    atol = config.atol
    rtol = config.rtol
    square = detcone == MOI.LogDetConeSquare || detcone == MOI.RootDetConeSquare
    use_logdet =
        detcone == MOI.LogDetConeTriangle || detcone == MOI.LogDetConeSquare
    # We look for an ellipsoid x^T P x ≤ 1 contained in the square.
    # Let Q = inv(P) (x^T Q x ≤ 1 is its polar ellipsoid), we have
    # max t
    #     t <= log det Q (or t <= (det Q)^(1/n))
    #             Q22 ≤ 1
    #            _________
    #           |         |
    #           |         |
    # -Q11 ≥ -1 |    +    | Q11 ≤ 1
    #           |         |
    #           |_________|
    #            -Q22 ≥ -1
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    if use_logdet
        @test MOI.supports_constraint(
            model,
            MOI.VariableIndex,
            MOI.EqualTo{Float64},
        )
    end
    if vecofvars
        @test MOI.supports_constraint(model, MOI.VectorOfVariables, detcone)
    else
        @test MOI.supports_constraint(
            model,
            MOI.VectorAffineFunction{Float64},
            detcone,
        )
    end
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Nonnegatives,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    t = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    Q = MOI.add_variables(model, square ? 4 : 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == (square ? 5 : 4)
    if use_logdet
        u = MOI.add_variable(model)
        vc = MOI.add_constraint(model, u, MOI.EqualTo(1.0))
        @test vc.value == u.value
        vov = MOI.VectorOfVariables([t; u; Q])
    else
        vov = MOI.VectorOfVariables([t; Q])
    end
    if vecofvars
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
        MOI.Nonnegatives(2),
    )
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                vecofvars ? MOI.VectorOfVariables :
                MOI.VectorAffineFunction{Float64},
                detcone,
            }(),
        ) == 1
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.VectorAffineFunction{Float64},
                MOI.Nonnegatives,
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, t)], 0.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if config.solve
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
        if config.duals
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

function logdett1vtest(model::MOI.ModelLike, config::Config)
    return _det1test(model, config, true, MOI.LogDetConeTriangle)
end
function logdett1ftest(model::MOI.ModelLike, config::Config)
    return _det1test(model, config, false, MOI.LogDetConeTriangle)
end
function logdets1vtest(model::MOI.ModelLike, config::Config)
    return _det1test(model, config, true, MOI.LogDetConeSquare)
end
function logdets1ftest(model::MOI.ModelLike, config::Config)
    return _det1test(model, config, false, MOI.LogDetConeSquare)
end

function _det2test(model::MOI.ModelLike, config::Config, detcone)
    atol = config.atol
    rtol = config.rtol
    square = detcone == MOI.LogDetConeSquare || detcone == MOI.RootDetConeSquare
    use_logdet =
        detcone == MOI.LogDetConeTriangle || detcone == MOI.LogDetConeSquare
    # We find logdet or rootdet of a symmetric PSD matrix:
    # mat = |3  2  1|
    #       |2  2  1|
    #       |1  1  3|
    # det(mat) = 5, so:
    # rootdet(mat) ≈ 1.709976
    # logdet(mat)  ≈ 1.609438
    mat = Float64[3 2 1; 2 2 1; 1 1 3]
    matL = Float64[3, 2, 2, 1, 1, 3]
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        detcone,
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    t = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), t)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    constant_mat = square ? vec(mat) : matL
    constant_vec = use_logdet ? vcat(0, 1, constant_mat) : vcat(0, constant_mat)
    vaf = MOI.VectorAffineFunction(
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, t))],
        constant_vec,
    )
    det_constraint = MOI.add_constraint(model, vaf, detcone(3))
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},detcone}(),
        ) == 1
    end
    if config.solve
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
        if config.duals
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

function logdett2test(model::MOI.ModelLike, config::Config)
    return _det2test(model, config, MOI.LogDetConeTriangle)
end
function logdets2test(model::MOI.ModelLike, config::Config)
    return _det2test(model, config, MOI.LogDetConeSquare)
end

const logdetttests = Dict(
    "logdett1v" => logdett1vtest,
    "logdett1f" => logdett1ftest,
    "logdett2" => logdett2test,
)

@moitestset logdett

const logdetstests = Dict(
    "logdets1v" => logdets1vtest,
    "logdets1f" => logdets1ftest,
    "logdets2" => logdets2test,
)

@moitestset logdets

const logdettests = Dict("logdett" => logdetttest, "logdets" => logdetstest)

@moitestset logdet true

function rootdett1vtest(model::MOI.ModelLike, config::Config)
    return _det1test(model, config, true, MOI.RootDetConeTriangle)
end
function rootdett1ftest(model::MOI.ModelLike, config::Config)
    return _det1test(model, config, false, MOI.RootDetConeTriangle)
end
function rootdets1vtest(model::MOI.ModelLike, config::Config)
    return _det1test(model, config, true, MOI.RootDetConeSquare)
end
function rootdets1ftest(model::MOI.ModelLike, config::Config)
    return _det1test(model, config, false, MOI.RootDetConeSquare)
end
function rootdett2test(model::MOI.ModelLike, config::Config)
    return _det2test(model, config, MOI.RootDetConeTriangle)
end
function rootdets2test(model::MOI.ModelLike, config::Config)
    return _det2test(model, config, MOI.RootDetConeSquare)
end

const rootdetttests = Dict(
    "rootdett1v" => rootdett1vtest,
    "rootdett1f" => rootdett1ftest,
    "rootdett2" => rootdett2test,
)

@moitestset rootdett

const rootdetstests = Dict(
    "rootdets1v" => rootdets1vtest,
    "rootdets1f" => rootdets1ftest,
    "rootdets2" => rootdets2test,
)

@moitestset rootdets

const rootdettests =
    Dict("rootdett" => rootdetttest, "rootdets" => rootdetstest)

@moitestset rootdet true

const contconictests = Dict(
    "lin" => lintest,
    "norminf" => norminftest,
    "normone" => normonetest,
    "soc" => soctest,
    "rsoc" => rsoctest,
    "geomean" => geomeantest,
    "exp" => exptest,
    "dualexp" => dualexptest,
    "pow" => powtest,
    "dualpow" => dualpowtest,
    "relentr" => relentrtest,
    "normspec" => normspectest,
    "normnuc" => normnuctest,
    "sdp" => sdptest,
    "logdet" => logdettest,
    "rootdet" => rootdettest,
)

@moitestset contconic true
