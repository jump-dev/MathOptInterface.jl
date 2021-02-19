#=
    Functions in this file test functionality relating to objectives in MOI.

### Requires
    - optimize!

### Functionality currently tested
    - get/set ObjectiveSense
    - a constant in a affine objective
    - a blank objective

### Functionality not yet tested
    - Quadratic Objectives
    - Modifications
=#

"""
    max_sense(model::MOI.ModelLike, config::TestConfig)

Test setting objective sense to MAX_SENSE.
"""
function max_sense(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test MOI.supports(model, MOI.ObjectiveSense())
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
end
unittests["max_sense"] = max_sense

"""
    min_sense(model::MOI.ModelLike, config::TestConfig)

Test setting objective sense to MIN_SENSE.
"""
function min_sense(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test MOI.supports(model, MOI.ObjectiveSense())
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
end
unittests["min_sense"] = min_sense

"""
    feasibility_sense(model::MOI.ModelLike, config::TestConfig)

Test setting objective sense to FEASIBILITY_SENSE.
"""
function feasibility_sense(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test MOI.supports(model, MOI.ObjectiveSense())
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.FEASIBILITY_SENSE
end
unittests["feasibility_sense"] = feasibility_sense

"""
    get_objective_function(model::MOI.ModelLike, config::TestConfig)

Test get objective function.
"""
function get_objective_function(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    obj_attr = MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()
    @test MOI.supports(model, obj_attr)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    minobjective: 2.0x + 1.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    expected_obj_fun =
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 1.0)
    @test_throws InexactError begin
        MOI.get(model, MOI.ObjectiveFunction{MOI.SingleVariable}())
    end
    obj_fun = MOI.get(model, obj_attr)
    @test obj_fun ≈ expected_obj_fun
    quad_obj_attr =
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}()
    quad_obj_fun = MOI.get(model, quad_obj_attr)
    @test convert(MOI.ScalarAffineFunction{Float64}, quad_obj_fun) ≈
          expected_obj_fun
end
unittests["get_objective_function"] = get_objective_function

"""
    solve_constant_obj(model::MOI.ModelLike, config::TestConfig)

Test constant in linear objective,  if `config.solve=true` confirm that it
solves correctly, and if `config.duals=true`, check that the duals are computed
correctly.
"""
function solve_constant_obj(model::MOI.ModelLike, config::TestConfig)
    atol, rtol = config.atol, config.rtol
    MOI.empty!(model)
    @test MOI.is_empty(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    minobjective: 2.0x + 1.0
    c: x >= 1.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(
        model,
        MOI.ConstraintIndex{MOI.SingleVariable,MOI.GreaterThan{Float64}},
        "c",
    )
    # We test this after the creation of every `SingleVariable` constraint
    # to ensure a good coverage of corner cases.
    @test c.value == x.value
    return test_model_solution(
        model,
        config;
        objective_value = 3.0,
        variable_primal = [(x, 1.0)],
        constraint_primal = [(c, 1.0)],
        constraint_dual = [(c, 2.0)],
    )
end
unittests["solve_constant_obj"] = solve_constant_obj

"""
    solve_blank_obj(model::MOI.ModelLike, config::TestConfig)

Test blank linear objective,  if `config.solve=true` confirm that it
solves correctly, and if `config.duals=true`, check that the duals are computed
correctly.
"""
function solve_blank_obj(model::MOI.ModelLike, config::TestConfig)
    atol, rtol = config.atol, config.rtol
    MOI.empty!(model)
    @test MOI.is_empty(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    minobjective: 0.0x + 0.0
    c: x >= 1.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(
        model,
        MOI.ConstraintIndex{MOI.SingleVariable,MOI.GreaterThan{Float64}},
        "c",
    )
    @test c.value == x.value
    test_model_solution(
        model,
        config;
        objective_value = 0.0,
        constraint_dual = [(c, 0.0)],
    )
    if config.solve
        # The objective is blank so any primal value ≥ 1 is correct
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.VariablePrimal(), x) + atol + rtol ≥ 1.0
        @test MOI.get(model, MOI.ConstraintPrimal(), c) + atol + rtol ≥ 1.0
    end
end
unittests["solve_blank_obj"] = solve_blank_obj

"""
    solve_singlevariable_obj(model::MOI.ModelLike, config::TestConfig)

Test SingleVariable objective,  if `config.solve=true` confirm that it
solves correctly, and if `config.duals=true`, check that the duals are computed
correctly.
"""
function solve_singlevariable_obj(model::MOI.ModelLike, config::TestConfig)
    atol, rtol = config.atol, config.rtol
    MOI.empty!(model)
    @test MOI.is_empty(model)
    MOIU.loadfromstring!(
        model,
        """
    variables: x
    minobjective: x
    c: x >= 1.0
""",
    )
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(
        model,
        MOI.ConstraintIndex{MOI.SingleVariable,MOI.GreaterThan{Float64}},
        "c",
    )
    @test c.value == x.value
    return test_model_solution(
        model,
        config;
        objective_value = 1.0,
        variable_primal = [(x, 1.0)],
        constraint_primal = [(c, 1.0)],
        constraint_dual = [(c, 1.0)],
    )
end
unittests["solve_singlevariable_obj"] = solve_singlevariable_obj

"""
    solve_qp_edge_cases(model::MOI.ModelLike, config::TestConfig)

Test various edge cases relating to quadratic programs (i.e., with a quadratic
objective function).

If `config.solve=true` confirm that it solves correctly.
"""
function solve_qp_edge_cases(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    vc1 = MOI.add_constraint(
        model,
        MOI.SingleVariable(x[1]),
        MOI.GreaterThan(1.0),
    )
    @test vc1.value == x[1].value
    vc2 = MOI.add_constraint(
        model,
        MOI.SingleVariable(x[2]),
        MOI.GreaterThan(2.0),
    )
    @test vc2.value == x[2].value

    @testset "Basic model" begin
        # min x^2 + y^2 | x>=1, y>=2
        MOI.set(
            model,
            MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
            MOI.ScalarQuadraticFunction(
                MOI.ScalarAffineTerm{Float64}[],  # affine terms
                MOI.ScalarQuadraticTerm.([2.0, 2.0], x, x),  # quad
                0.0,  # constant
            ),
        )
        test_model_solution(
            model,
            config;
            objective_value = 5.0,
            variable_primal = [(x[1], 1.0), (x[2], 2.0)],
        )
    end
    @testset "Duplicate linear terms" begin
        # min x + x + x^2 + y^2 | x>=1, y>=2
        MOI.set(
            model,
            MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
            MOI.ScalarQuadraticFunction(
                MOI.ScalarAffineTerm.([1.0, 1.0], [x[1], x[1]]),  # affine terms
                MOI.ScalarQuadraticTerm.([2.0, 2.0], x, x),  # quad
                0.0,  # constant
            ),
        )
        test_model_solution(
            model,
            config;
            objective_value = 7.0,
            variable_primal = [(x[1], 1.0), (x[2], 2.0)],
        )
    end
    @testset "Duplicate diagonal terms" begin
        # min x^2 + x^2 | x>=1, y>=2
        MOI.set(
            model,
            MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
            MOI.ScalarQuadraticFunction(
                MOI.ScalarAffineTerm{Float64}[],  # affine terms
                MOI.ScalarQuadraticTerm.(
                    [2.0, 2.0],
                    [x[1], x[1]],
                    [x[1], x[1]],
                ),  # quad
                0.0,  # constant
            ),
        )
        test_model_solution(
            model,
            config;
            objective_value = 2.0,
            variable_primal = [(x[1], 1.0)],
        )
    end
    @testset "Duplicate off-diagonal terms" begin
        # min x^2 + 0.25x*y + 0.25y*x + 0.5x*y + y^2 | x>=1, y>=2
        MOI.set(
            model,
            MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
            MOI.ScalarQuadraticFunction(
                MOI.ScalarAffineTerm{Float64}[],  # affine terms
                MOI.ScalarQuadraticTerm.(
                    [2.0, 0.25, 0.25, 0.5, 2.0],
                    [x[1], x[1], x[2], x[1], x[2]],
                    [x[1], x[2], x[1], x[2], x[2]],
                ),  # quad
                0.0,  # constant
            ),
        )
        test_model_solution(
            model,
            config;
            objective_value = 7.0,
            variable_primal = [(x[1], 1.0), (x[2], 2.0)],
        )
    end
end
unittests["solve_qp_edge_cases"] = solve_qp_edge_cases

"""
    solve_qp_zero_offdiag(model::MOI.ModelLike, config::TestConfig)

Test quadratic program with a zero off-diagonal term.

If `config.solve=true` confirm that it solves correctly.
"""
function solve_qp_zero_offdiag(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    vc1 = MOI.add_constraint(
        model,
        MOI.SingleVariable(x[1]),
        MOI.GreaterThan(1.0),
    )
    @test vc1.value == x[1].value
    vc2 = MOI.add_constraint(
        model,
        MOI.SingleVariable(x[2]),
        MOI.GreaterThan(2.0),
    )
    @test vc2.value == x[2].value
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
        MOI.ScalarQuadraticFunction(
            MOI.ScalarAffineTerm{Float64}[],  # affine terms
            MOI.ScalarQuadraticTerm.(
                [2.0, 0.0, 2.0],
                [x[1], x[1], x[2]],
                [x[1], x[2], x[2]],
            ),  # quad
            0.0,  # constant
        ),
    )
    test_model_solution(
        model,
        config;
        objective_value = 5.0,
        variable_primal = [(x[1], 1.0), (x[2], 2.0)],
    )
end
unittests["solve_qp_zero_offdiag"] = solve_qp_zero_offdiag

"""
    solve_duplicate_terms_obj(model::MOI.ModelLike, config::TestConfig)

Test duplicate terms in linear objective, if `config.solve=true` confirm that it
solves correctly.
"""
function solve_duplicate_terms_obj(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(1.0))
    @test c.value == x.value
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(2.0, x), MOI.ScalarAffineTerm(1.0, x)],
            0.0,
        ),
    )
    return test_model_solution(
        model,
        config;
        objective_value = 3.0,
        variable_primal = [(x, 1.0)],
        constraint_primal = [(c, 1.0)],
        constraint_dual = [(c, 3.0)],
    )
end
unittests["solve_duplicate_terms_obj"] = solve_duplicate_terms_obj
