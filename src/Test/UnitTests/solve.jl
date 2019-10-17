"""
    solve_objbound_edge_cases(model::MOI.ModelLike, config::TestConfig)

Test a variety of edge cases related to the ObjectiveBound attribute.
"""
function solve_objbound_edge_cases(model::MOI.ModelLike, config::TestConfig)
    @testset "Min IP with constant" begin
        MOI.empty!(model)
        @test MOI.is_empty(model)
        MOIU.loadfromstring!(model, """
            variables: x
            minobjective: 2.0x + -1.0
            c1: x >= 1.5
            c2: x in Integer()
        """)
        x = MOI.get(model, MOI.VariableIndex, "x")
        test_model_solution(model, config;
            objective_value = 3.0,
            variable_primal = [(x, 2.0)]
        )
        if config.solve
            @test MOI.get(model, MOI.ObjectiveBound()) <= 3.0
        end
    end

    @testset "Max IP with constant" begin
        MOI.empty!(model)
        @test MOI.is_empty(model)
        MOIU.loadfromstring!(model, """
            variables: x
            maxobjective: 2.0x + 1.0
            c1: x <= 1.5
            c2: x in Integer()
        """)
        x = MOI.get(model, MOI.VariableIndex, "x")
        test_model_solution(model, config;
            objective_value = 3.0,
            variable_primal = [(x, 1.0)]
        )
        if config.solve
            @test MOI.get(model, MOI.ObjectiveBound()) >= 3.0
        end
    end

    @testset "Min LP with constant" begin
        MOI.empty!(model)
        @test MOI.is_empty(model)
        MOIU.loadfromstring!(model, """
            variables: x
            minobjective: 2.0x + -1.0
            c1: x >= 1.5
        """)
        x = MOI.get(model, MOI.VariableIndex, "x")
        test_model_solution(model, config;
            objective_value = 2.0,
            variable_primal = [(x, 1.5)]
        )
        if config.solve
            @test MOI.get(model, MOI.ObjectiveBound()) <= 2.0
        end
    end

    @testset "Max LP with constant" begin
        MOI.empty!(model)
        @test MOI.is_empty(model)
        MOIU.loadfromstring!(model, """
            variables: x
            maxobjective: 2.0x + 1.0
            c1: x <= 1.5
        """)
        x = MOI.get(model, MOI.VariableIndex, "x")
        test_model_solution(model, config;
            objective_value = 4.0,
            variable_primal = [(x, 1.5)]
        )
        if config.solve
            @test MOI.get(model, MOI.ObjectiveBound()) >= 4.0
        end
    end
end
unittests["solve_objbound_edge_cases"] = solve_objbound_edge_cases

function solve_unbounded_model(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    x = MOI.add_variables(model, 5)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x), 0.0)
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.DUAL_INFEASIBLE
    end
end
unittests["solve_unbounded_model"] = solve_unbounded_model

function solve_single_variable_dual_min(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    x = MOI.add_variable(model)
    xl = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(1.0))
    xu = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.LessThan(2.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.SingleVariable}(), MOI.SingleVariable(x))
    if config.solve && config.duals
        MOI.optimize!(model)
        @test isapprox(
            MOI.get(model, MOI.VariablePrimal(), x), 1.0, atol = config.atol
        )
        sl = MOI.get(model, MOI.ConstraintDual(), xl)
        su = MOI.get(model, MOI.ConstraintDual(), xu)
        @test isapprox(sl + su, 1.0, atol = config.atol)
        @test sl >= -config.atol
        @test su <= config.atol
    end
end
unittests["solve_single_variable_dual_min"] = solve_single_variable_dual_min

function solve_single_variable_dual_max(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    x = MOI.add_variable(model)
    xl = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    xu = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.LessThan(1.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.SingleVariable}(), MOI.SingleVariable(x))
    if config.solve && config.duals
        MOI.optimize!(model)
        @test isapprox(
            MOI.get(model, MOI.VariablePrimal(), x), 1.0, atol = config.atol
        )
        sl = MOI.get(model, MOI.ConstraintDual(), xl)
        su = MOI.get(model, MOI.ConstraintDual(), xu)
        @test isapprox(sl + su, -1.0, atol = config.atol)
        @test sl >= -config.atol
        @test su <= config.atol
    end
end
unittests["solve_single_variable_dual_max"] = solve_single_variable_dual_max

function solve_result_index(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    MOI.empty!(model)
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(1.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.SingleVariable}(), MOI.SingleVariable(x))
    if config.solve
        MOI.optimize!(model)
        result_count = MOI.get(model, MOI.ResultCount())
        result_err(attr) = MOI.ResultIndexBoundsError{typeof(attr)}(attr, result_count)
        result_index = result_count + 1
        @test MOI.get(model, MOI.ObjectiveValue(1)) ≈ 1.0 atol=atol rtol=rtol
        @test_throws result_err(MOI.ObjectiveValue(result_index)) MOI.get(model, MOI.ObjectiveValue(result_index))
        @test MOI.get(model, MOI.PrimalStatus(1)) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.PrimalStatus(result_index)) == MOI.NO_SOLUTION
        @test MOI.get(model, MOI.VariablePrimal(1), x) ≈ 1.0 atol=atol rtol=rtol
        @test_throws result_err(MOI.VariablePrimal(result_index)) MOI.get(model, MOI.VariablePrimal(result_index), x)
        @test MOI.get(model, MOI.ConstraintPrimal(1), c) ≈ 1.0 atol=atol rtol=rtol
        @test_throws result_err(MOI.ConstraintPrimal(result_index)) MOI.get(model, MOI.ConstraintPrimal(result_index), c)
        if config.duals
            @test MOI.get(model, MOI.DualStatus(1)) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.DualStatus(result_index)) == MOI.NO_SOLUTION
            @test MOI.get(model, MOI.ConstraintDual(1), c) ≈ 1.0 atol=atol rtol=rtol
            @test_throws result_err(MOI.ConstraintDual(result_index)) MOI.get(model, MOI.ConstraintDual(result_index), c)
            @test MOI.get(model, MOI.DualObjectiveValue(1)) ≈ 1.0 atol=atol rtol=rtol
            @test_throws result_err(MOI.DualObjectiveValue(result_index)) MOI.get(model, MOI.DualObjectiveValue(result_index))
        end
    end
end
unittests["solve_result_index"] = solve_result_index
