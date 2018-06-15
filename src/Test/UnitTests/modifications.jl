const modificationtests = Dict{String, Function}()

"""
    solve_set_singlevariable_lessthan(model::MOI.ModelLike, config::TestConfig)

Test set modification SingleVariable-in-LessThan constraint. If
`config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function solve_set_singlevariable_lessthan(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        maxobjective: 1.0x
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.addconstraint!(model, MOI.SingleVariable(x), MOI.LessThan(1.0))
    if config.solve
        test_model_solution(model, config;
            objective_value   = 1.0,
            variable_primal   = [(x, 1.0)],
            constraint_primal = [(c, 1.0)],
            constraint_dual   = [(c, -1.0)]
        )
    end
    @test MOI.canset(model, MOI.ConstraintSet(), typeof(c))
    MOI.set!(model, MOI.ConstraintSet(), c, MOI.LessThan(2.0))
    @test MOI.canget(model, MOI.ConstraintSet(), typeof(c))
    @test MOI.get(model, MOI.ConstraintSet(), c) == MOI.LessThan(2.0)
    if config.solve
        test_model_solution(model, config;
            objective_value   = 2.0,
            variable_primal   = [(x, 2.0)],
            constraint_primal = [(c, 2.0)],
            constraint_dual   = [(c, -1.0)]
        )
    end
end
modificationtests["solve_set_singlevariable_lessthan"] = solve_set_singlevariable_lessthan

"""
    solve_set_scalaraffine_lessthan(model::MOI.ModelLike, config::TestConfig)

Test modifying set of ScalarAffineFunction-in-LessThan constraint. If
`config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function solve_set_scalaraffine_lessthan(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        maxobjective: 1.0x
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.addconstraint!(model,
            MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
            MOI.LessThan(1.0)
        )
    if config.solve
        test_model_solution(model, config;
            objective_value   = 1.0,
            variable_primal   = [(x, 1.0)],
            constraint_primal = [(c, 1.0)],
            constraint_dual   = [(c, -1.0)]
        )
    end
    @test MOI.canset(model, MOI.ConstraintSet(), typeof(c))
    MOI.set!(model, MOI.ConstraintSet(), c, MOI.LessThan(2.0))
    @test MOI.canget(model, MOI.ConstraintSet(), typeof(c))
    @test MOI.get(model, MOI.ConstraintSet(), c) == MOI.LessThan(2.0)
    if config.solve
        test_model_solution(model, config;
            objective_value   = 2.0,
            variable_primal   = [(x, 2.0)],
            constraint_primal = [(c, 2.0)],
            constraint_dual   = [(c, -1.0)]
        )
    end
end
modificationtests["solve_set_scalaraffine_lessthan"] = solve_set_scalaraffine_lessthan

"""
    solve_coef_scalaraffine_lessthan(model::MOI.ModelLike, config::TestConfig)

Test modifying set of ScalarAffineFunction-in-LessThan constraint. If
`config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function solve_coef_scalaraffine_lessthan(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        maxobjective: 1.0x
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.addconstraint!(model,
            MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
            MOI.LessThan(1.0)
        )
    if config.solve
        test_model_solution(model, config;
            objective_value   = 1.0,
            variable_primal   = [(x, 1.0)],
            constraint_primal = [(c, 1.0)],
            constraint_dual   = [(c, -1.0)]
        )
    end
    @test MOI.canmodify(model, typeof(c), MOI.ScalarCoefficientChange{Float64})
    MOI.modify!(model, c, MOI.ScalarCoefficientChange(x, 2.0))
    if config.solve
        test_model_solution(model, config;
            objective_value   = 0.5,
            variable_primal   = [(x, 0.5)],
            constraint_primal = [(c, 1.0)],
            constraint_dual   = [(c, -0.5)]
        )
    end
end
modificationtests["solve_coef_scalaraffine_lessthan"] = solve_coef_scalaraffine_lessthan

"""
    solve_coef_scalar_objective(model::MOI.ModelLike, config::TestConfig)

Test modifying set of ScalarAffineFunction-in-LessThan constraint. If
`config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function solve_coef_scalar_objective(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        maxobjective: 1.0x + 2.0
        c1: 1.0x <= 1.0
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    if config.solve
        test_model_solution(model, config;
            objective_value   = 3.0,
            variable_primal   = [(x, 1.0)]
        )
    end
    @test MOI.canmodify(model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarConstantChange{Float64}
    )
    MOI.modify!(model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarConstantChange(3.0)
    )
    if config.solve
        test_model_solution(model, config;
            objective_value   = 4.0,
            variable_primal   = [(x, 1.0)]
        )
    end
end
modificationtests["solve_coef_scalar_objective"] = solve_coef_scalar_objective

@moitestset modification
