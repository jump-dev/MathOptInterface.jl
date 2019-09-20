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
    time_limit_sec(model::MOI.ModelLike, config::TestConfig)

Test that the [`MOI.TimeLimitSec`](@ref) attribute is implemented for `model`.
"""
function time_limit_sec(model::MOI.ModelLike, config::TestConfig)
    if config.solve
        @test MOI.supports(model, MOI.TimeLimitSec())
        # Get the current value to restore it at the end of the test
        value = MOI.get(model, MOI.TimeLimitSec())
        MOI.set(model, MOI.TimeLimitSec(), 0.0)
        @test MOI.get(model, MOI.TimeLimitSec()) == 0.0
        MOI.set(model, MOI.TimeLimitSec(), 1.0)
        @test MOI.get(model, MOI.TimeLimitSec()) == 1.0
        MOI.set(model, MOI.TimeLimitSec(), value)
        @test value == MOI.get(model, MOI.TimeLimitSec()) # Equality should hold
    end
end
unittests["time_limit_sec"] = time_limit_sec

"""
    number_threads(model::MOI.ModelLike, config::TestConfig)

Test that the [`MOI.NumberThreads`](@ref) attribute is implemented for `model`.
"""
function number_threads(model::MOI.ModelLike, config::TestConfig)
    if config.solve
        @test MOI.supports(model, MOI.NumberThreads())
        # Get the current value to restore it at the end of the test
        value = MOI.get(model, MOI.NumberThreads())
        MOI.set(model, MOI.NumberThreads(), 2)
        @test MOI.get(model, MOI.NumberThreads()) == 2
        MOI.set(model, MOI.NumberThreads(), 3)
        @test MOI.get(model, MOI.NumberThreads()) == 3
        MOI.set(model, MOI.NumberThreads(), value)
        @test value == MOI.get(model, MOI.NumberThreads())
    end
end
unittests["time_limit_sec"] = time_limit_sec

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
