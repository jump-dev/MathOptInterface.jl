const modificationtests = Dict{String, Function}()

"""
    solve_modify_variable_bound(model::MOI.ModelLike, config::TestConfig)

Test set modification SingleVariable-in-LessThan constraint. If
`config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function solve_modify_variable_bound(model::MOI.ModelLike, config::TestConfig)
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
modificationtests["solve_modify_variable_bound"] = solve_modify_variable_bound

"""
    solve_modify_rhs(model::MOI.ModelLike, config::TestConfig)

Test modifying set of ScalarAffineFunction-in-LessThan constraint. If
`config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function solve_modify_rhs(model::MOI.ModelLike, config::TestConfig)
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
modificationtests["solve_modify_rhs"] = solve_modify_rhs

@moitestset modification
