"""
    getconstraint(model::MOI.ModelLike, config::TestConfig)

Test getting constraints by name.
"""
function getconstraint(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        minobjective: 2.0x
        c1: x >= 1.0
        c2: x <= 2.0
    """)
    @test !MOI.canget(model, MOI.ConstraintIndex, "c3")
    @test MOI.canget(model, MOI.ConstraintIndex, "c1")
    @test MOI.canget(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{Float64}}, "c1")
    @test !MOI.canget(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{Float64}}, "c1")
    @test MOI.canget(model, MOI.ConstraintIndex, "c2")
    @test !MOI.canget(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{Float64}}, "c2")
    @test MOI.canget(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{Float64}}, "c2")
    c1 = MOI.get(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{Float64}}, "c1")
    @test MOI.isvalid(model, c1)
    c2 = MOI.get(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{Float64}}, "c2")
    @test MOI.isvalid(model, c2)
end
unittests["getconstraint"]    = getconstraint

"""
    solve_affine_lessthan(model::MOI.ModelLike, config::TestConfig)

Add an ScalarAffineFunction-in-LessThan constraint. If `config.solve=true`
confirm that it solves correctly, and if `config.duals=true`, check that the
duals are computed correctly.
"""
function solve_affine_lessthan(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        maxobjective: 1.0x
        c: 2.0x <= 1.0
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}}, "c")
    if config.solve
        test_model_solution(model, config;
            objective_value   = 0.5,
            variable_primal   = [(x, 0.5)],
            constraint_primal = [(c, 1.0)],
            constraint_dual   = [(c, -0.5)]
        )
    end
end
unittests["solve_affine_lessthan"] = solve_affine_lessthan

"""
    solve_affine_greaterthan(model::MOI.ModelLike, config::TestConfig)

Add an ScalarAffineFunction-in-GreaterThan constraint. If `config.solve=true`
confirm that it solves correctly, and if `config.duals=true`, check that the
duals are computed correctly.
"""
function solve_affine_greaterthan(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        minobjective: 1.0x
        c: 2.0x >= 1.0
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}, "c")
    if config.solve
        test_model_solution(model, config;
            objective_value   = 0.5,
            variable_primal   = [(x, 0.5)],
            constraint_primal = [(c, 1.0)],
            constraint_dual   = [(c, 0.5)]
        )
    end
end
unittests["solve_affine_greaterthan"] = solve_affine_greaterthan

"""
    solve_affine_equalto(model::MOI.ModelLike, config::TestConfig)

Add an ScalarAffineFunction-in-EqualTo constraint. If `config.solve=true`
confirm that it solves correctly, and if `config.duals=true`, check that the
duals are computed correctly.
"""
function solve_affine_equalto(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        minobjective: 1.0x
        c: 2.0x == 1.0
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}}, "c")
    if config.solve
        test_model_solution(model, config;
            objective_value   = 0.5,
            variable_primal   = [(x, 0.5)],
            constraint_primal = [(c, 1.0)],
            constraint_dual   = [(c, 0.5)]
        )
    end
end
unittests["solve_affine_equalto"] = solve_affine_equalto

"""
    solve_affine_interval(model::MOI.ModelLike, config::TestConfig)

Add an ScalarAffineFunction-in-Interval constraint. If `config.solve=true`
confirm that it solves correctly, and if `config.duals=true`, check that the
duals are computed correctly.
"""
function solve_affine_interval(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        maxobjective: 3.0x
        c: 2.0x in Interval(1.0, 4.0)
    """)
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}}, "c")
    if config.solve
        test_model_solution(model, config;
            objective_value   = 6.0,
            variable_primal   = [(x, 2.0)],
            constraint_primal = [(c, 4.0)],
            constraint_dual   = [(c, -1.5)]
        )
    end
end
unittests["solve_affine_interval"] = solve_affine_interval
