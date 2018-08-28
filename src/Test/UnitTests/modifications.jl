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
        c: x <= 1.0
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(model, MOI.ConstraintIndex, "c")
    test_model_solution(model, config;
        objective_value   = 1.0,
        variable_primal   = [(x, 1.0)],
        constraint_primal = [(c, 1.0)],
        constraint_dual   = [(c, -1.0)]
    )
    MOI.set(model, MOI.ConstraintSet(), c, MOI.LessThan(2.0))
    @test MOI.get(model, MOI.ConstraintSet(), c) == MOI.LessThan(2.0)
    test_model_solution(model, config;
        objective_value   = 2.0,
        variable_primal   = [(x, 2.0)],
        constraint_primal = [(c, 2.0)],
        constraint_dual   = [(c, -1.0)]
    )
end
modificationtests["solve_set_singlevariable_lessthan"] = solve_set_singlevariable_lessthan

"""
    solve_transform_singlevariable_lessthan(model::MOI.ModelLike, config::TestConfig)

Test set transformation of a SingleVariable-in-LessThan constraint. If
`config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function solve_transform_singlevariable_lessthan(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        maxobjective: 1.0x
        c: x <= 1.0
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(model, MOI.ConstraintIndex, "c")
    test_model_solution(model, config;
        objective_value   = 1.0,
        variable_primal   = [(x, 1.0)],
        constraint_primal = [(c, 1.0)],
        constraint_dual   = [(c, -1.0)]
    )
    c2 = MOI.transform(model, c, MOI.GreaterThan(2.0))
    @test !MOI.is_valid(model, c)
    @test MOI.is_valid(model, c2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MinSense)
    test_model_solution(model, config;
        objective_value   = 2.0,
        variable_primal   = [(x, 2.0)],
        constraint_primal = [(c2, 2.0)],
        constraint_dual   = [(c2, 1.0)]
    )
end
modificationtests["solve_transform_singlevariable_lessthan"] = solve_transform_singlevariable_lessthan

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
        c: 1.0x <= 1.0
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(model, MOI.ConstraintIndex, "c")
    test_model_solution(model, config;
        objective_value   = 1.0,
        variable_primal   = [(x, 1.0)],
        constraint_primal = [(c, 1.0)],
        constraint_dual   = [(c, -1.0)]
    )
    MOI.set(model, MOI.ConstraintSet(), c, MOI.LessThan(2.0))
    @test MOI.get(model, MOI.ConstraintSet(), c) == MOI.LessThan(2.0)
    test_model_solution(model, config;
        objective_value   = 2.0,
        variable_primal   = [(x, 2.0)],
        constraint_primal = [(c, 2.0)],
        constraint_dual   = [(c, -1.0)]
    )
end
modificationtests["solve_set_scalaraffine_lessthan"] = solve_set_scalaraffine_lessthan

"""
    solve_coef_scalaraffine_lessthan(model::MOI.ModelLike, config::TestConfig)

Test modifying a variable coefficient in a ScalarAffineFunction-in-LessThan
constraint. If `config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function solve_coef_scalaraffine_lessthan(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        maxobjective: 1.0x
        c: 1.0x <= 1.0
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(model, MOI.ConstraintIndex, "c")
    test_model_solution(model, config;
        objective_value   = 1.0,
        variable_primal   = [(x, 1.0)],
        constraint_primal = [(c, 1.0)],
        constraint_dual   = [(c, -1.0)]
    )
    MOI.modify(model, c, MOI.ScalarCoefficientChange(x, 2.0))
    test_model_solution(model, config;
        objective_value   = 0.5,
        variable_primal   = [(x, 0.5)],
        constraint_primal = [(c, 1.0)],
        constraint_dual   = [(c, -0.5)]
    )
end
modificationtests["solve_coef_scalaraffine_lessthan"] = solve_coef_scalaraffine_lessthan

"""
    solve_func_scalaraffine_lessthan(model::MOI.ModelLike, config::TestConfig)

Test setting the function in a ScalarAffineFunction-in-LessThan
constraint. If `config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function solve_func_scalaraffine_lessthan(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        maxobjective: 1.0x
        c: 1.0x <= 1.0
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(model, MOI.ConstraintIndex, "c")
    test_model_solution(model, config;
        objective_value   = 1.0,
        variable_primal   = [(x, 1.0)],
        constraint_primal = [(c, 1.0)],
        constraint_dual   = [(c, -1.0)]
    )
    MOI.set(model, MOI.ConstraintFunction(), c,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 0.0)
    )
    foo = MOI.get(model, MOI.ConstraintFunction(), c)
    @test foo ≈ MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 0.0)
    test_model_solution(model, config;
        objective_value   = 0.5,
        variable_primal   = [(x, 0.5)],
        # constraint_primal = [(c, 1.0)],
        constraint_dual   = [(c, -0.5)]
    )
end
modificationtests["solve_func_scalaraffine_lessthan"] = solve_func_scalaraffine_lessthan

"""
    solve_const_vectoraffine_nonpos(model::MOI.ModelLike, config::TestConfig)

Test modifying the constant term in a VectorAffineFunction-in-Nonpositives
constraint. If `config.solve=true` confirm that it solves correctly, and if
`config.duals=true`, check that the duals are computed correctly.
"""
function solve_const_vectoraffine_nonpos(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    MOIU.loadfromstring!(model,"""
        variables: x, y
        maxobjective: 1.0x + 2.0y
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    y = MOI.get(model, MOI.VariableIndex, "y")
    c = MOI.add_constraint(model,
            MOI.VectorAffineFunction([
                    MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x)),
                    MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(2.0, y))
                ],
                [0.0, 0.0]
            ),
            MOI.Nonpositives(2)
        )
    test_model_solution(model, config;
        objective_value   = 0.0,
        variable_primal   = [(x, 0.0), (y, 0.0)],
        constraint_primal = [(c, [0.0, 0.0])]
    )
    MOI.modify(model, c, MOI.VectorConstantChange([-1.0, -1.5]))
    test_model_solution(model, config;
        objective_value   = 2.5,
        variable_primal   = [(x, 1.0), (y, 0.75)],
        constraint_primal = [(c, [0.0, 0.0])]
    )
end
modificationtests["solve_const_vectoraffine_nonpos"] = solve_const_vectoraffine_nonpos

"""
    solve_multirow_vectoraffine_nonpos(model::MOI.ModelLike, config::TestConfig)

Test modifying the variable coefficients in a
VectorAffineFunction-in-Nonpositives constraint. If `config.solve=true` confirm
that it solves correctly.
"""
function solve_multirow_vectoraffine_nonpos(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        maxobjective: 1.0x
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.add_constraint(model,
            MOI.VectorAffineFunction([
                    MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x)),
                    MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(2.0, x))
                ],
                [-1.0, -1.0]
            ),
            MOI.Nonpositives(2)
        )
    test_model_solution(model, config;
        objective_value   = 0.5,
        variable_primal   = [(x, 0.5)],
        constraint_primal = [(c, [-0.5, 0.0])]
    )
    MOI.modify(model, c, MOI.MultirowChange(x, [(1,4.0), (2,3.0)]))
    test_model_solution(model, config;
        objective_value   = 0.25,
        variable_primal   = [(x, 0.25)],
        constraint_primal = [(c, [0.0, -0.25])]
    )
end
modificationtests["solve_multirow_vectoraffine_nonpos"] = solve_multirow_vectoraffine_nonpos

"""
    solve_const_scalar_objective(model::MOI.ModelLike, config::TestConfig)

Test the constant of a scalaraffine objective. If `config.solve=true` confirm
that it solves correctly.
"""
function solve_const_scalar_objective(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        maxobjective: 1.0x + 2.0
        c1: 1.0x <= 1.0
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    test_model_solution(model, config;
        objective_value   = 3.0,
        variable_primal   = [(x, 1.0)]
    )
    MOI.modify(model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarConstantChange(3.0)
    )
    test_model_solution(model, config;
        objective_value   = 4.0,
        variable_primal   = [(x, 1.0)]
    )
end
modificationtests["solve_const_scalar_objective"] = solve_const_scalar_objective

"""
    solve_coef_scalar_objective(model::MOI.ModelLike, config::TestConfig)

Test modifying a variable coefficient in a scalaraffine objective. If
`config.solve=true` confirm that it solves correctly.
"""
function solve_coef_scalar_objective(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        maxobjective: 1.0x
        c1: 1.0x <= 1.0
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    test_model_solution(model, config;
        objective_value   = 1.0,
        variable_primal   = [(x, 1.0)]
    )
    MOI.modify(model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarCoefficientChange(x, 3.0)
    )
    test_model_solution(model, config;
        objective_value   = 3.0,
        variable_primal   = [(x, 1.0)]
    )
end
modificationtests["solve_coef_scalar_objective"] = solve_coef_scalar_objective

@moitestset modification
