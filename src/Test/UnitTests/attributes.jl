"""
    test_solver_name(model::MOI.ModelLike, config::Config)

Test that the [`MOI.SolverName`](@ref) attribute is implemented for `model`.
"""
function test_SolverName(model::MOI.ModelLike, config::Config)
    if config.solve
        @test MOI.get(model, MOI.SolverName()) isa AbstractString
    end
    return
end

"""
    test_Silent(model::MOI.ModelLike, config::Config)

Test that the [`MOI.Silent`](@ref) attribute is implemented for `model`.
"""
function test_Silent(model::MOI.ModelLike, config::Config)
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
    return
end

function setup_test(
    ::typeof(test_Silent),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOI.set(model, MOI.Silent(), true)
    return
end


"""
    test_TimeLimitSec(model::MOI.ModelLike, config::Config)

Test that the [`MOI.TimeLimitSec`](@ref) attribute is implemented for `model`.
"""
function test_TimeLimitSec(model::MOI.ModelLike, config::Config)
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
    return
end

function setup_test(
    ::typeof(test_TimeLimitSec),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOI.set(model, MOI.TimeLimitSec(), nothing)
    return
end

"""
    test_NumberThreads(model::MOI.ModelLike, config::Config)

Test that the [`MOI.NumberOfThreads`](@ref) attribute is implemented for `model`.
"""
function test_NumberThreads(model::MOI.ModelLike, config::Config)
    if config.solve
        @test MOI.supports(model, MOI.NumberOfThreads())
        # Get the current value to restore it at the end of the test
        value = MOI.get(model, MOI.NumberOfThreads())
        MOI.set(model, MOI.NumberOfThreads(), 1)
        @test MOI.get(model, MOI.NumberOfThreads()) == 1
        MOI.set(model, MOI.NumberOfThreads(), 3)
        @test MOI.get(model, MOI.NumberOfThreads()) == 3
        MOI.set(model, MOI.NumberOfThreads(), value)
        @test value == MOI.get(model, MOI.NumberOfThreads())
    end
    return
end

function setup_test(
    ::typeof(test_NumberThreads),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOI.set(model, MOI.NumberOfThreads(), nothing)
    return
end

"""
    test_RawStatusString(model::MOI.ModelLike, config::Config)

Test that the [`MOI.RawStatusString`](@ref) attribute is implemented for
`model`.
"""
function test_RawStatusString(model::MOI.ModelLike, config::Config)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x),
    )
    test_model_solution(
        model,
        config,
        objective_value = 0.0,
        variable_primal = [(x, 0.0)],
    )
    if config.solve
        @test MOI.get(model, MOI.RawStatusString()) isa AbstractString
    end
    return
end

function setup_test(
    ::typeof(test_RawStatusString),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(
                mock,
                MOI.RawStatusString(),
                "Mock solution set by `mock_optimize!`.",
            )
            MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [0.0]),
            )
        end,
    )
    return
end

"""
    test_SolveTimeSec(model::MOI.ModelLike, config::Config)

Test that the [`MOI.SolveTimeSec`](@ref) attribute is implemented for `model`.
"""
function test_SolveTimeSec(model::MOI.ModelLike, config::Config)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, MOI.SingleVariable(x), MOI.LessThan(0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x),
    )
    test_model_solution(
        model,
        config,
        objective_value = 0.0,
        variable_primal = [(x, 0.0)],
    )
    if config.solve
        time = MOI.get(model, MOI.SolveTimeSec())
        @test time ≥ 0.0
    end
    return
end

function setup_test(
    ::typeof(test_SolveTimeSec),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.SolveTimeSec(), 0.0)
            MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [0.0]),
            )
        end,
    )
    return
end
