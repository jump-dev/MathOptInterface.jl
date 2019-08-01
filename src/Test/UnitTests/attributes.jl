"""
    solver_name(model::MOI.ModelLike, config::TestConfig)

Test that the [`MOI.SolverName`](@ref) attribute is implemented for `model`.
"""
function solver_name(model::MOI.ModelLike, config::TestConfig)
    if config.solve
        @test MOI.get(model, MOI.SolverName()) isa AbstractString
    end
end
unittests["solver_name"] = solver_name

"""
    silent(model::MOI.ModelLike, config::TestConfig)

Test that the [`MOI.Silent`](@ref) attribute is implemented for `model`.
"""
function silent(model::MOI.ModelLike, config::TestConfig)
    if config.solve
        @test MOI.supports(model, MOI.Silent())
        # Get the current value to restore it at the end of the test
        value = MOI.get(model, MOI.Silent())
        MOI.set(model, MOI.Silent(), !value)
        @test !value == MOI.get(model, MOI.Silent())
        # Check that `set` does not just take `!` of the current value
        MOI.set(model, MOI.Silent(), !value)
        @test !value == MOI.get(model, MOI.Silent())
        MOI.set(model, MOI.Silent(), value)
        @test value == MOI.get(model, MOI.Silent())
    end
end
unittests["silent"] = silent

"""
    timelimit(model::MOI.ModelLike, config::TestConfig)

Test that the [`MOI.TimeLimit`](@ref) attribute is implemented for `model`.
"""
function timelimit(model::MOI.ModelLike, config::TestConfig)
    if config.solve
        @test MOI.supports(model, MOI.TimeLimit())
        # Get the current value to restore it at the end of the test
        value = MOI.get(model, MOI.TimeLimit())
        MOI.set(model, MOI.TimeLimit(), value + 1)
        @test value + 1 == MOI.get(model, MOI.TimeLimit())
        # Check that `set` does not just increment the current value
        MOI.set(model, MOI.TimeLimit(), value + 1)
        @test value + 1 == MOI.get(model, MOI.TimeLimit())
        MOI.set(model, MOI.TimeLimit(), value)
        @test value == MOI.get(model, MOI.TimeLimit())
    end
end
unittests["timelimit"] = timelimit

"""
    raw_status_string(model::MOI.ModelLike, config::TestConfig)

Test that the [`MOI.RawStatusString`](@ref) attribute is implemented for
`model`.
"""
function raw_status_string(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.is_empty(model)
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

Test that the [`MOI.SolveTime`](@ref) attribute is implemented for `model`.
"""
function solve_time(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, MOI.SingleVariable(x), MOI.LessThan(0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.SingleVariable}(),
            MOI.SingleVariable(x))
    test_model_solution(model, config, objective_value = 0.0,
                        variable_primal = [(x, 0.0)])
    if config.solve
        time = MOI.get(model, MOI.SolveTime())
        @test time ≥ 0.0
    end
end
unittests["solve_time"] = solve_time
