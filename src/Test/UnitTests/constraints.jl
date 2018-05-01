"""
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

function testsolution(model, config;
        objective_value   = nothing,
        variable_primal   = nothing,
        constraint_primal = nothing,
        constraint_dual   = nothing
    )
    atol, rtol = config.atol, config.rtol
    MOI.optimize!(model)
    @test MOI.canget(model, MOI.TerminationStatus())
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
    @test MOI.canget(model, MOI.PrimalStatus())
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
    if objective_value != nothing
        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ objective_value atol=atol rtol=rtol
    end
    if variable_primal != nothing
        for (index, solution_value) in variable_primal
            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            @test MOI.get(model, MOI.VariablePrimal(), index) ≈ solution_value atol=atol rtol=rtol
        end
    end
    if constraint_primal != nothing
        for (index, solution_value) in constraint_primal
            @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(index))
            @test MOI.get(model, MOI.ConstraintPrimal(), index) ≈ solution_value atol=atol rtol=rtol
        end
    end
    if constraint_dual != nothing
        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
            for (index, solution_value) in constraint_dual
                @test MOI.canget(model, MOI.ConstraintDual(), typeof(index))
                @test MOI.get(model, MOI.ConstraintDual(), index) ≈ solution_value atol=atol rtol=rtol
            end
        end
    end
end

"""
    Add an ScalarAffineFunction-in-LessThan constraint.
    If `config.solve`, solve and test solution.
    In addition, if `config.duals`, test dual solution.
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
        testsolution(model, config;
            objective_value   = 0.5,
            variable_primal   = [(x, 0.5)],
            constraint_primal = [(c, 1.0)],
            constraint_dual   = [(c, -0.5)]
        )
    end
end
unittests["solve_affine_lessthan"] = solve_affine_lessthan

"""
    Add an ScalarAffineFunction-in-GreaterThan constraint.
    If `config.solve`, solve and test solution.
    In addition, if `config.duals`, test dual solution.
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
        testsolution(model, config;
            objective_value   = 0.5,
            variable_primal   = [(x, 0.5)],
            constraint_primal = [(c, 1.0)],
            constraint_dual   = [(c, 0.5)]
        )
    end
end
unittests["solve_affine_greaterthan"] = solve_affine_greaterthan

"""
    Add an ScalarAffineFunction-in-EqualTo constraint.
    If `config.solve`, solve and test solution.
    In addition, if `config.duals`, test dual solution.
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
        testsolution(model, config;
            objective_value   = 0.5,
            variable_primal   = [(x, 0.5)],
            constraint_primal = [(c, 1.0)],
            constraint_dual   = [(c, 0.5)]
        )
    end
end
unittests["solve_affine_equalto"] = solve_affine_equalto

"""
    Add an ScalarAffineFunction-in-Interval constraint.
    If `config.solve`, solve and test solution.
    In addition, if `config.duals`, test dual solution.
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
        testsolution(model, config;
            objective_value   = 6.0,
            variable_primal   = [(x, 2.0)],
            constraint_primal = [(c, 4.0)],
            constraint_dual   = [(c, -1.5)]
        )
    end
end
unittests["solve_affine_interval"] = solve_affine_interval
