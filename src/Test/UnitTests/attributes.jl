"""
    solver_name(model::MOI.ModelLike, config::TestConfig)

Test that the [`MOI.SolverName`](@ref) attributes is implemented for `model`.
"""
function solver_name(model::MOI.ModelLike, config::TestConfig)
    if config.solve
        @test MOI.get(model, MOI.SolverName()) isa AbstractString
    end
end
unittests["solver_name"] = solver_name

"""
    raw_status_string(model::MOI.ModelLike, config::TestConfig)

Test that the [`MOI.RawStatusString`](@ref) attributes is implemented for
`model`.
"""
function raw_status_string(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test MOI.get(model, MOI.SolverName()) isa AbstractString
    x = MOI.add_variable(model)
    MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.SingleVariable}(),
            MOI.SingleVariable(x))
    test_model_solution(model, config, objective_value = 0.0,
                        variable_primal = [(x, 0.0)])
    if config.solve
        @test MOI.get(model, MOI.RawStatusString()) isa AbstractString
    end
end
unittests["raw_status_string"] = raw_status_string

"""
    solve_time(model::MOI.ModelLike, config::TestConfig)

Test that the [`MOI.SolveTime`](@ref) attributes is implemented for `model`.
"""
function solve_time(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test MOI.get(model, MOI.SolverName()) isa AbstractString
    x = MOI.add_variable(model)
    MOI.add_constraint(model, MOI.SingleVariable(x), MOI.LessThan(0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.SingleVariable}(),
            MOI.SingleVariable(x))
    test_model_solution(model, config, objective_value = 0.0,
                        variable_primal = [(x, 0.0)])
    if config.solve
        @test MOI.get(model, MOI.SolveTime()) isa AbstractFloat
    end
end
unittests["solve_time"] = solve_time
