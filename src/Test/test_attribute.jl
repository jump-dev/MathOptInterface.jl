"""
    test_attribute_NumberThreads(model::MOI.AbstractOptimizer, config::Config)

Test that the [`MOI.NumberOfThreads`](@ref) attribute is implemented for
`model`.
"""
function test_attribute_NumberThreads(model::MOI.AbstractOptimizer, ::Config)
    @requires MOI.supports(model, MOI.NumberOfThreads())
    # Get the current value to restore it at the end of the test
    value = MOI.get(model, MOI.NumberOfThreads())
    MOI.set(model, MOI.NumberOfThreads(), 1)
    @test MOI.get(model, MOI.NumberOfThreads()) == 1
    MOI.set(model, MOI.NumberOfThreads(), 3)
    @test MOI.get(model, MOI.NumberOfThreads()) == 3
    MOI.set(model, MOI.NumberOfThreads(), value)
    @test value == MOI.get(model, MOI.NumberOfThreads())
    _test_attribute_value_type(model, MOI.NumberOfThreads())
    return
end
test_attribute_NumberThreads(::MOI.ModelLike, ::Config) = nothing

function setup_test(
    ::typeof(test_attribute_NumberThreads),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOI.set(model, MOI.NumberOfThreads(), nothing)
    return
end

"""
    test_attribute_RawStatusString(model::MOI.AbstractOptimizer, config::Config)

Test that the [`MOI.RawStatusString`](@ref) attribute is implemented for
`model`.
"""
function test_attribute_RawStatusString(
    model::MOI.AbstractOptimizer,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.RawStatusString)
    MOI.add_variable(model)
    MOI.optimize!(model)
    _test_attribute_value_type(model, MOI.RawStatusString())
    return
end
test_attribute_RawStatusString(::MOI.ModelLike, ::Config) = nothing

function setup_test(
    ::typeof(test_attribute_RawStatusString),
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
        end,
    )
    return
end

"""
    test_attribute_Silent(model::MOI.AbstractOptimizer, config::Config)

Test that the [`MOI.Silent`](@ref) attribute is implemented for `model`.
"""
function test_attribute_Silent(model::MOI.AbstractOptimizer, ::Config)
    @requires MOI.supports(model, MOI.Silent())
    # Get the current value to restore it at the end of the test
    value = MOI.get(model, MOI.Silent())
    MOI.set(model, MOI.Silent(), !value)
    @test !value == MOI.get(model, MOI.Silent())
    # Check that `set` does not just take `!` of the current value
    MOI.set(model, MOI.Silent(), !value)
    @test !value == MOI.get(model, MOI.Silent())
    MOI.set(model, MOI.Silent(), value)
    @test value == MOI.get(model, MOI.Silent())
    _test_attribute_value_type(model, MOI.Silent())
    return
end
test_attribute_Silent(::MOI.ModelLike, ::Config) = nothing

function setup_test(
    ::typeof(test_attribute_Silent),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOI.set(model, MOI.Silent(), true)
    return
end

"""
    test_attribute_SolverName(model::MOI.AbstractOptimizer, config::Config)

Test that the [`MOI.SolverName`](@ref) attribute is implemented for `model`.
"""
function test_attribute_SolverName(model::MOI.AbstractOptimizer, config::Config)
    if _supports(config, MOI.SolverName)
        _test_attribute_value_type(model, MOI.SolverName())
    end
    return
end
test_attribute_SolverName(::MOI.ModelLike, ::Config) = nothing

"""
    test_attribute_SolveTimeSec(model::MOI.AbstractOptimizer, config::Config)

Test that the [`MOI.SolveTimeSec`](@ref) attribute is implemented for `model`.
"""
function test_attribute_SolveTimeSec(
    model::MOI.AbstractOptimizer,
    config::Config,
)
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.SolveTimeSec)
    MOI.add_variable(model)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.SolveTimeSec()) >= 0.0
    _test_attribute_value_type(model, MOI.SolveTimeSec())
    return
end
test_attribute_SolveTimeSec(::MOI.ModelLike, ::Config) = nothing

function setup_test(
    ::typeof(test_attribute_SolveTimeSec),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOI.set(mock, MOI.SolveTimeSec(), 0.0),
    )
    return
end

"""
    test_attribute_TimeLimitSec(model::MOI.AbstractOptimizer, config::Config)

Test that the [`MOI.TimeLimitSec`](@ref) attribute is implemented for `model`.
"""
function test_attribute_TimeLimitSec(model::MOI.AbstractOptimizer, ::Config)
    @requires MOI.supports(model, MOI.TimeLimitSec())
    # Get the current value to restore it at the end of the test
    value = MOI.get(model, MOI.TimeLimitSec())
    MOI.set(model, MOI.TimeLimitSec(), 0.0)
    @test MOI.get(model, MOI.TimeLimitSec()) == 0.0
    MOI.set(model, MOI.TimeLimitSec(), 1.0)
    @test MOI.get(model, MOI.TimeLimitSec()) == 1.0
    MOI.set(model, MOI.TimeLimitSec(), value)
    @test value == MOI.get(model, MOI.TimeLimitSec()) # Equality should hold
    _test_attribute_value_type(model, MOI.TimeLimitSec())
    return
end
test_attribute_TimeLimitSec(::MOI.ModelLike, ::Config) = nothing

function setup_test(
    ::typeof(test_attribute_TimeLimitSec),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOI.set(model, MOI.TimeLimitSec(), nothing)
    return
end

"""
    test_attribute_after_empty(model::MOI.AbstractOptimizer, config::Config)

Test that optimizer attributes such as `Silent` are not cleared by `MOI.empty!`.
"""
function test_attribute_after_empty(model::MOI.AbstractOptimizer, ::Config)
    @requires MOI.supports(model, MOI.Silent())
    @test MOI.get(model, MOI.Silent()) == false
    MOI.set(model, MOI.Silent(), true)
    @test MOI.get(model, MOI.Silent()) == true
    MOI.empty!(model)
    @test MOI.get(model, MOI.Silent()) == true
    return
end
