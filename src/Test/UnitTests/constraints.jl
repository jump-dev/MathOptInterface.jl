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
    @test MOI.get(model, MOI.ConstraintIndex, "c3") === nothing
    @test MOI.get(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{Float64}}, "c1") === nothing
    @test MOI.get(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{Float64}}, "c2") === nothing
    c1 = MOI.get(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{Float64}}, "c1")
    @test MOI.get(model, MOI.ConstraintIndex, "c1") == c1
    @test MOI.isvalid(model, c1)
    c2 = MOI.get(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{Float64}}, "c2")
    @test MOI.get(model, MOI.ConstraintIndex, "c2") == c2
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
    test_model_solution(model, config;
        objective_value   = 0.5,
        variable_primal   = [(x, 0.5)],
        constraint_primal = [(c, 1.0)],
        constraint_dual   = [(c, -0.5)]
    )
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
    test_model_solution(model, config;
        objective_value   = 0.5,
        variable_primal   = [(x, 0.5)],
        constraint_primal = [(c, 1.0)],
        constraint_dual   = [(c, 0.5)]
    )
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
    test_model_solution(model, config;
        objective_value   = 0.5,
        variable_primal   = [(x, 0.5)],
        constraint_primal = [(c, 1.0)],
        constraint_dual   = [(c, 0.5)]
    )
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
    test_model_solution(model, config;
        objective_value   = 6.0,
        variable_primal   = [(x, 2.0)],
        constraint_primal = [(c, 4.0)],
        constraint_dual   = [(c, -1.5)]
    )
end
unittests["solve_affine_interval"] = solve_affine_interval

"""
    solve_qcp_edge_cases(model::MOI.ModelLike, config::TestConfig)

Test various edge cases relating to quadratically constrainted programs (i.e.,
with a ScalarQuadraticFunction-in-Set constraint.

If `config.solve=true` confirm that it solves correctly.
"""
function solve_qcp_edge_cases(model::MOI.ModelLike, config::TestConfig)
    @testset "Duplicate on-diagonal" begin
        # max x + 2y | y + x^2 + x^2 <= 1, x >= 0.5, y >= 0.5
        MOI.empty!(model)
        x = MOI.addvariables!(model, 2)
        MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)
        MOI.set!(model,
            MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
            MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 2.0], x), 0.0)
        )
        MOI.addconstraint!(model, MOI.SingleVariable(x[1]), MOI.GreaterThan(0.5))
        MOI.addconstraint!(model, MOI.SingleVariable(x[2]), MOI.GreaterThan(0.5))
        MOI.addconstraint!(model,
            MOI.ScalarQuadraticFunction(
                MOI.ScalarAffineTerm.([1.0], [x[2]]),  # affine terms
                MOI.ScalarQuadraticTerm.([2.0, 2.0], [x[1], x[1]], [x[1], x[1]]),  # quad
                0.0  # constant
            ),
            MOI.LessThan(1.0)
        )
        test_model_solution(model, config;
            objective_value   = 1.5,
            variable_primal   = [(x[1], 0.5), (x[2], 0.5)]
        )
    end
    @testset "Duplicate off-diagonal" begin
        # max x + 2y | x^2 + 0.25y*x + 0.25x*y + 0.5x*y + y^2 <= 1, x >= 0.5, y >= 0.5
        MOI.empty!(model)
        x = MOI.addvariables!(model, 2)
        MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)
        MOI.set!(model,
            MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
            MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 2.0], x), 0.0)
        )
        MOI.addconstraint!(model, MOI.SingleVariable(x[1]), MOI.GreaterThan{Float64}(0.5))
        MOI.addconstraint!(model, MOI.SingleVariable(x[2]), MOI.GreaterThan{Float64}(0.5))
        MOI.addconstraint!(model,
            MOI.ScalarQuadraticFunction(
                MOI.ScalarAffineTerm{Float64}[],  # affine terms
                MOI.ScalarQuadraticTerm.(
                    [ 2.0, 0.25, 0.25,  0.5,  2.0],
                    [x[1], x[1], x[2], x[1], x[2]],
                    [x[1], x[2], x[1], x[2], x[2]]),  # quad
                0.0  # constant
            ),
            MOI.LessThan(1.0)
        )
        test_model_solution(model, config;
            objective_value   = 0.5 + (√13-1)/2,
            variable_primal   = [(x[1], 0.5), (x[2], (√13-1)/4)]
        )
    end
end
unittests["solve_qcp_edge_cases"] = solve_qcp_edge_cases

"""
    solve_affine_deletion_edge_cases(model::MOI.ModelLike, config::TestConfig)

Test various edge cases relating to deleting affine constraints. This requires
    + ScalarAffineFunction-in-LessThan; and
    + VectorAffineFunction-in-Nonpositives.

If `config.solve=true` confirm that it solves correctly.
"""
function solve_affine_deletion_edge_cases(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    x = MOI.addvariable!(model)
    # helpers. The function 1.0x + 0.0
    saf  = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0)
    vaf  = MOI.VectorAffineFunction([MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))], [0.0])
    vaf2 = MOI.VectorAffineFunction([MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))], [-2.0])
    # max x
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), saf)
    # test adding a VectorAffineFunction -in- LessThan
    c1 = MOI.addconstraint!(model, vaf, MOI.Nonpositives(1))
    test_model_solution(model, config; objective_value = 0.0,
                        constraint_primal = [(c1, [0.0])]
    )
    # test adding a ScalarAffineFunction -in- LessThan
    c2 = MOI.addconstraint!(model, saf, MOI.LessThan(1.0))
    test_model_solution(model, config; objective_value = 0.0,
        constraint_primal = [(c1, [0.0]), (c2, 0.0)]
    )
    # now delete the VectorAffineFunction
    MOI.delete!(model, c1)
    @test_throws MOI.InvalidIndex{typeof(c1)} MOI.delete!(model, c1)
    try
        MOI.delete!(model, c1)
    catch err
        @test err.index == c1
    end
    test_model_solution(model, config; objective_value = 1.0,
        constraint_primal = [(c2, 1.0)]
    )
    # add a different VectorAffineFunction constraint
    c3 = MOI.addconstraint!(model, vaf2, MOI.Nonpositives(1))
    test_model_solution(model, config; objective_value = 1.0,
        constraint_primal = [(c2, 1.0), (c3, [-1.0])]
    )
    # delete the ScalarAffineFunction
    MOI.delete!(model, c2)
    test_model_solution(model, config; objective_value = 2.0,
        constraint_primal = [(c3, [0.0])]
    )
end
unittests["solve_affine_deletion_edge_cases"] = solve_affine_deletion_edge_cases
