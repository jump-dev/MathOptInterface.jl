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

Test setting objective sense to MaxSense.
"""
function max_sense(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.isempty(model)
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)
    @test MOI.canget(model, MOI.ObjectiveSense())
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MaxSense
end
unittests["max_sense"] = max_sense

"""
    min_sense(model::MOI.ModelLike, config::TestConfig)

Test setting objective sense to MinSense.
"""
function min_sense(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.isempty(model)
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)
    @test MOI.canget(model, MOI.ObjectiveSense())
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MinSense
end
unittests["min_sense"] = min_sense

"""
    feasibility_sense(model::MOI.ModelLike, config::TestConfig)

Test setting objective sense to FeasibilitySense.
"""
function feasibility_sense(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.isempty(model)
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.FeasibilitySense)
    @test MOI.canget(model, MOI.ObjectiveSense())
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.FeasibilitySense
end
unittests["feasibility_sense"] = feasibility_sense

"""
    solve_constant_obj(model::MOI.ModelLike, config::TestConfig)

Test constant in linear objective,  if `config.solve=true` confirm that it
solves correctly, and if `config.duals=true`, check that the duals are computed
correctly.
"""
function solve_constant_obj(model::MOI.ModelLike, config::TestConfig)
    atol, rtol = config.atol, config.rtol
    MOI.empty!(model)
    @test MOI.isempty(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        minobjective: 2.0x + 1.0
        c: x >= 1.0
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{Float64}}, "c")
    if config.solve
        test_model_solution(model, config;
            objective_value   = 3.0,
            variable_primal   = [(x, 1.0)],
            constraint_primal = [(c, 1.0)],
            constraint_dual   = [(c, 2.0)]
        )
    end
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
    @test MOI.isempty(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        minobjective: 0.0x + 0.0
        c: x >= 1.0
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{Float64}}, "c")
    if config.solve
        test_model_solution(model, config;
            objective_value   = 0.0,
            variable_primal   = [(x, 1.0)],
            constraint_primal = [(c, 1.0)],
            constraint_dual   = [(c, 0.0)]
        )
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
    @test MOI.isempty(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        minobjective: x
        c: x >= 1.0
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{Float64}}, "c")
    if config.solve
        test_model_solution(model, config;
            objective_value   = 1.0,
            variable_primal   = [(x, 1.0)],
            constraint_primal = [(c, 1.0)],
            constraint_dual   = [(c, 1.0)]
        )
    end
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
    x = MOI.addvariables!(model, 2)
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)
    MOI.addconstraint!(model, MOI.SingleVariable(x[1]), MOI.GreaterThan(1.0))
    MOI.addconstraint!(model, MOI.SingleVariable(x[2]), MOI.GreaterThan(2.0))

    # min x^2 + y^2 | x>=1, y>=2
    MOI.set!(model,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
        MOI.ScalarQuadraticFunction(
            MOI.ScalarAffineTerm{Float64}[],  # affine terms
            MOI.ScalarQuadraticTerm.([2.0, 2.0], x, x),  # quad
            0.0  # constant
        )
    )
    test_model_solution(model, config;
        objective_value   = 5.0,
        variable_primal   = [(x[1], 1.0), (x[2], 2.0)]
    )

    # duplicate terms on diagonal
    # min x^2 + x^2 | x>=1, y>=2
    MOI.set!(model,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
        MOI.ScalarQuadraticFunction(
            MOI.ScalarAffineTerm{Float64}[],  # affine terms
            MOI.ScalarQuadraticTerm.([2.0, 2.0], [x[1], x[1]], [x[1], x[1]]),  # quad
            0.0  # constant
        )
    )
    test_model_solution(model, config;
        objective_value   = 2.0,
        variable_primal   = [(x[1], 1.0)]
    )

    # duplicate terms on off-diagonal
    # min x^2 + 0.25x*y + 0.25y*x + 0.5x*y + y^2 | x>=1, y>=2
    MOI.set!(model,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
        MOI.ScalarQuadraticFunction(
            MOI.ScalarAffineTerm{Float64}[],  # affine terms
            MOI.ScalarQuadraticTerm.(
                [ 2.0, 0.25, 0.25,  0.5,  2.0],
                [x[1], x[1], x[2], x[1], x[2]],
                [x[1], x[2], x[1], x[2], x[2]]),  # quad
            0.0  # constant
        )
    )
    test_model_solution(model, config;
        objective_value   = 7.0,
        variable_primal   = [(x[1], 1.0), (x[2], 2.0)]
    )
end
unittests["solve_qp_edge_cases"] = solve_qp_edge_cases
