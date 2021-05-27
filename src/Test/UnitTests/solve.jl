"""
    solve_objbound_edge_cases(model::MOI.ModelLike, config::Config)

Test a variety of edge cases related to the ObjectiveBound attribute.
"""
function solve_objbound_edge_cases(model::MOI.ModelLike, config::Config)
    @testset "Min IP with constant" begin
        MOI.empty!(model)
        @test MOI.is_empty(model)
        MOIU.loadfromstring!(
            model,
            """
    variables: x
    minobjective: 2.0x + -1.0
    x >= 1.5
    x in Integer()
""",
        )
        x = MOI.get(model, MOI.VariableIndex, "x")
        test_model_solution(
            model,
            config;
            objective_value = 3.0,
            variable_primal = [(x, 2.0)],
        )
        if config.solve
            @test MOI.get(model, MOI.ObjectiveBound()) <= 3.0
        end
    end

    @testset "Max IP with constant" begin
        MOI.empty!(model)
        @test MOI.is_empty(model)
        MOIU.loadfromstring!(
            model,
            """
    variables: x
    maxobjective: 2.0x + 1.0
    x <= 1.5
    x in Integer()
""",
        )
        x = MOI.get(model, MOI.VariableIndex, "x")
        test_model_solution(
            model,
            config;
            objective_value = 3.0,
            variable_primal = [(x, 1.0)],
        )
        if config.solve
            @test MOI.get(model, MOI.ObjectiveBound()) >= 3.0
        end
    end

    @testset "Min LP with constant" begin
        MOI.empty!(model)
        @test MOI.is_empty(model)
        MOIU.loadfromstring!(
            model,
            """
    variables: x
    minobjective: 2.0x + -1.0
    x >= 1.5
""",
        )
        x = MOI.get(model, MOI.VariableIndex, "x")
        test_model_solution(
            model,
            config;
            objective_value = 2.0,
            variable_primal = [(x, 1.5)],
        )
        if config.solve
            @test MOI.get(model, MOI.ObjectiveBound()) <= 2.0
        end
    end

    @testset "Max LP with constant" begin
        MOI.empty!(model)
        @test MOI.is_empty(model)
        MOIU.loadfromstring!(
            model,
            """
    variables: x
    maxobjective: 2.0x + 1.0
    x <= 1.5
""",
        )
        x = MOI.get(model, MOI.VariableIndex, "x")
        test_model_solution(
            model,
            config;
            objective_value = 4.0,
            variable_primal = [(x, 1.5)],
        )
        if config.solve
            @test MOI.get(model, MOI.ObjectiveBound()) >= 4.0
        end
    end
end
unittests["solve_objbound_edge_cases"] = solve_objbound_edge_cases

function solve_unbounded_model(model::MOI.ModelLike, config::Config)
    MOI.empty!(model)
    x = MOI.add_variables(model, 5)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x), 0.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.DUAL_INFEASIBLE
    end
end
unittests["solve_unbounded_model"] = solve_unbounded_model

function solve_single_variable_dual_min(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    x = MOI.add_variable(model)
    xl = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(1.0))
    xu = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.LessThan(1.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x),
    )
    if config.solve && config.duals
        MOI.optimize!(model)
        @test isapprox(
            MOI.get(model, MOI.VariablePrimal(), x),
            1.0,
            atol = config.atol,
        )
        sl = MOI.get(model, MOI.ConstraintDual(), xl)
        su = MOI.get(model, MOI.ConstraintDual(), xu)
        @test isapprox(sl + su, 1.0, atol = config.atol)
        @test sl >= -config.atol
        @test su <= config.atol
    end
end
unittests["solve_single_variable_dual_min"] = solve_single_variable_dual_min

function solve_single_variable_dual_max(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    x = MOI.add_variable(model)
    xl = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(1.0))
    xu = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.LessThan(1.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x),
    )
    if config.solve && config.duals
        MOI.optimize!(model)
        @test isapprox(
            MOI.get(model, MOI.VariablePrimal(), x),
            1.0,
            atol = config.atol,
        )
        sl = MOI.get(model, MOI.ConstraintDual(), xl)
        su = MOI.get(model, MOI.ConstraintDual(), xu)
        @test isapprox(sl + su, -1.0, atol = config.atol)
        @test sl >= -config.atol
        @test su <= config.atol
    end
end
unittests["solve_single_variable_dual_max"] = solve_single_variable_dual_max

function solve_result_index(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    MOI.empty!(model)
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(1.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x),
    )
    if config.solve
        MOI.optimize!(model)
        result_count = MOI.get(model, MOI.ResultCount())
        function result_err(attr)
            return MOI.ResultIndexBoundsError{typeof(attr)}(attr, result_count)
        end
        result_index = result_count + 1
        @test MOI.get(model, MOI.ObjectiveValue(1)) ≈ 1.0 atol = atol rtol =
            rtol
        @test_throws result_err(MOI.ObjectiveValue(result_index)) MOI.get(
            model,
            MOI.ObjectiveValue(result_index),
        )
        if config.dual_objective_value
            @test MOI.get(model, MOI.DualObjectiveValue(1)) ≈ 1.0 atol = atol rtol =
                rtol
            @test_throws result_err(MOI.DualObjectiveValue(result_index)) MOI.get(
                model,
                MOI.DualObjectiveValue(result_index),
            )
        end
        @test MOI.get(model, MOI.PrimalStatus(1)) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.PrimalStatus(result_index)) == MOI.NO_SOLUTION
        @test MOI.get(model, MOI.VariablePrimal(1), x) ≈ 1.0 atol = atol rtol =
            rtol
        @test_throws result_err(MOI.VariablePrimal(result_index)) MOI.get(
            model,
            MOI.VariablePrimal(result_index),
            x,
        )
        @test MOI.get(model, MOI.ConstraintPrimal(1), c) ≈ 1.0 atol = atol rtol =
            rtol
        @test_throws result_err(MOI.ConstraintPrimal(result_index)) MOI.get(
            model,
            MOI.ConstraintPrimal(result_index),
            c,
        )
        if config.duals
            @test MOI.get(model, MOI.DualStatus(1)) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.DualStatus(result_index)) ==
                  MOI.NO_SOLUTION
            @test MOI.get(model, MOI.ConstraintDual(1), c) ≈ 1.0 atol = atol rtol =
                rtol
            @test_throws result_err(MOI.ConstraintDual(result_index)) MOI.get(
                model,
                MOI.ConstraintDual(result_index),
                c,
            )
        end
    end
end
unittests["solve_result_index"] = solve_result_index

function solve_farkas_equalto_upper(model::MOI.ModelLike, config::Config)
    MOI.empty!(model)
    x = MOI.add_variables(model, 2)
    clb =
        MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.GreaterThan(0.0))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0, 1.0], x), 0.0),
        MOI.EqualTo(-1.0),
    )
    if config.solve && config.infeas_certificates
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
        @test MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
        clb_dual = MOI.get.(model, MOI.ConstraintDual(), clb)
        c_dual = MOI.get(model, MOI.ConstraintDual(), c)
        @test clb_dual[1] > config.atol
        @test clb_dual[2] > config.atol
        @test c_dual[1] < -config.atol
        @test clb_dual ≈ [2, 1] .* -c_dual atol = config.atol rtol = config.rtol
    end
end
unittests["solve_farkas_equalto_upper"] = solve_farkas_equalto_upper

function solve_farkas_equalto_lower(model::MOI.ModelLike, config::Config)
    MOI.empty!(model)
    x = MOI.add_variables(model, 2)
    clb =
        MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.GreaterThan(0.0))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-2.0, -1.0], x), 0.0),
        MOI.EqualTo(1.0),
    )
    if config.solve && config.infeas_certificates
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
        @test MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
        clb_dual = MOI.get.(model, MOI.ConstraintDual(), clb)
        c_dual = MOI.get(model, MOI.ConstraintDual(), c)
        @test clb_dual[1] > config.atol
        @test clb_dual[2] > config.atol
        @test c_dual[1] > config.atol
        @test clb_dual ≈ [2, 1] .* c_dual atol = config.atol rtol = config.rtol
    end
end
unittests["solve_farkas_equalto_lower"] = solve_farkas_equalto_lower

function solve_farkas_lessthan(model::MOI.ModelLike, config::Config)
    MOI.empty!(model)
    x = MOI.add_variables(model, 2)
    clb =
        MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.GreaterThan(0.0))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0, 1.0], x), 0.0),
        MOI.LessThan(-1.0),
    )
    if config.solve && config.infeas_certificates
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
        @test MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
        clb_dual = MOI.get.(model, MOI.ConstraintDual(), clb)
        c_dual = MOI.get(model, MOI.ConstraintDual(), c)
        @test clb_dual[1] > config.atol
        @test clb_dual[2] > config.atol
        @test c_dual[1] < -config.atol
        @test clb_dual ≈ [2, 1] .* -c_dual atol = config.atol rtol = config.rtol
    end
end
unittests["solve_farkas_lessthan"] = solve_farkas_lessthan

function solve_farkas_greaterthan(model::MOI.ModelLike, config::Config)
    MOI.empty!(model)
    x = MOI.add_variables(model, 2)
    clb =
        MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.GreaterThan(0.0))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-2.0, -1.0], x), 0.0),
        MOI.GreaterThan(1.0),
    )
    if config.solve && config.infeas_certificates
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
        @test MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
        clb_dual = MOI.get.(model, MOI.ConstraintDual(), clb)
        c_dual = MOI.get(model, MOI.ConstraintDual(), c)
        @test clb_dual[1] > config.atol
        @test clb_dual[2] > config.atol
        @test c_dual[1] > config.atol
        @test clb_dual ≈ [2, 1] .* c_dual atol = config.atol rtol = config.rtol
    end
end
unittests["solve_farkas_greaterthan"] = solve_farkas_greaterthan

function solve_farkas_interval_upper(model::MOI.ModelLike, config::Config)
    MOI.empty!(model)
    x = MOI.add_variables(model, 2)
    clb =
        MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.GreaterThan(0.0))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0, 1.0], x), 0.0),
        MOI.Interval(-2.0, -1.0),
    )
    if config.solve && config.infeas_certificates
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
        @test MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
        clb_dual = MOI.get.(model, MOI.ConstraintDual(), clb)
        c_dual = MOI.get(model, MOI.ConstraintDual(), c)
        @test clb_dual[1] > config.atol
        @test clb_dual[2] > config.atol
        @test c_dual[1] < -config.atol
        @test clb_dual ≈ [2, 1] .* -c_dual atol = config.atol rtol = config.rtol
    end
end
unittests["solve_farkas_interval_upper"] = solve_farkas_interval_upper

function solve_farkas_interval_lower(model::MOI.ModelLike, config::Config)
    MOI.empty!(model)
    x = MOI.add_variables(model, 2)
    clb =
        MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.GreaterThan(0.0))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-2.0, -1.0], x), 0.0),
        MOI.Interval(1.0, 2.0),
    )
    if config.solve && config.infeas_certificates
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
        @test MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
        clb_dual = MOI.get.(model, MOI.ConstraintDual(), clb)
        c_dual = MOI.get(model, MOI.ConstraintDual(), c)
        @test clb_dual[1] > config.atol
        @test clb_dual[2] > config.atol
        @test c_dual[1] > config.atol
        @test clb_dual ≈ [2, 1] .* c_dual atol = config.atol rtol = config.rtol
    end
end
unittests["solve_farkas_interval_lower"] = solve_farkas_interval_lower

function solve_farkas_variable_lessthan(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    x = MOI.add_variables(model, 2)
    clb = MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.LessThan(0.0))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0, 1.0], x), 0.0),
        MOI.GreaterThan(1.0),
    )
    if config.solve && config.infeas_certificates
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
        @test MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
        clb_dual = MOI.get.(model, MOI.ConstraintDual(), clb)
        c_dual = MOI.get(model, MOI.ConstraintDual(), c)
        @test clb_dual[1] < -config.atol
        @test clb_dual[2] < -config.atol
        @test c_dual[1] > config.atol
        @test clb_dual ≈ [2, 1] .* -c_dual atol = config.atol rtol = config.rtol
    end
end
unittests["solve_farkas_variable_lessthan"] = solve_farkas_variable_lessthan

function solve_farkas_variable_lessthan_max(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    x = MOI.add_variables(model, 2)
    clb = MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.LessThan(0.0))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0, 1.0], x), 0.0),
        MOI.GreaterThan(1.0),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x[1]),
    )
    if config.solve && config.infeas_certificates
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE
        @test MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
        clb_dual = MOI.get.(model, MOI.ConstraintDual(), clb)
        c_dual = MOI.get(model, MOI.ConstraintDual(), c)
        @test clb_dual[1] < -config.atol
        @test clb_dual[2] < -config.atol
        @test c_dual[1] > config.atol
        @test clb_dual ≈ [2, 1] .* -c_dual atol = config.atol rtol = config.rtol
    end
end
unittests["solve_farkas_variable_lessthan_max"] =
    solve_farkas_variable_lessthan_max

function solve_twice(model::MOI.ModelLike, config::Config)
    MOI.empty!(model)
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(1.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x),
    )
    if config.solve
        MOI.optimize!(model)
        MOI.optimize!(model)
        MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
        MOI.get(model, MOI.VariablePrimal(), x) == 1.0
    end
end
unittests["solve_twice"] = solve_twice
