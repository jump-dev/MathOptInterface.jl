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
