# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

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
    test_attribute_SolverVersion(model::MOI.AbstractOptimizer, config::Config)

Test that the [`MOI.SolverVersion`](@ref) attribute is implemented for `model`.
"""
function test_attribute_SolverVersion(
    model::MOI.AbstractOptimizer,
    config::Config,
)
    if _supports(config, MOI.SolverVersion)
        _test_attribute_value_type(model, MOI.SolverVersion())
    end
    return
end
test_attribute_SolverVersion(::MOI.ModelLike, ::Config) = nothing

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
    function _get_default(model)
        try
            return MOI.get(model, MOI.TimeLimitSec())
        catch err
            @assert err isa MOI.GetAttributeNotAllowed{MOI.TimeLimitSec}
        end
        return
    end
    # Get the current value to restore it at the end of the test
    value = _get_default(model)
    MOI.set(model, MOI.TimeLimitSec(), 0.0)
    @test MOI.get(model, MOI.TimeLimitSec()) == 0.0
    _test_attribute_value_type(model, MOI.TimeLimitSec())
    MOI.set(model, MOI.TimeLimitSec(), 1.0)
    @test MOI.get(model, MOI.TimeLimitSec()) == 1.0
    MOI.set(model, MOI.TimeLimitSec(), nothing)
    reset_value = _get_default(model)
    @test reset_value === nothing || reset_value == value
    MOI.set(model, MOI.TimeLimitSec(), value)
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

function test_attribute_ObjectiveLimit(model::MOI.AbstractOptimizer, ::Config)
    @requires MOI.supports(model, MOI.ObjectiveLimit())
    # Get the current value to restore it at the end of the test
    value = MOI.get(model, MOI.ObjectiveLimit())
    MOI.set(model, MOI.ObjectiveLimit(), 0.0)
    @test MOI.get(model, MOI.ObjectiveLimit()) == 0.0
    MOI.set(model, MOI.ObjectiveLimit(), nothing)
    @test MOI.get(model, MOI.ObjectiveLimit()) === nothing
    MOI.set(model, MOI.ObjectiveLimit(), 1.0)
    @test MOI.get(model, MOI.ObjectiveLimit()) == 1.0
    MOI.set(model, MOI.ObjectiveLimit(), value)
    @test value == MOI.get(model, MOI.ObjectiveLimit()) # Equality should hold
    _test_attribute_value_type(model, MOI.ObjectiveLimit())
    return
end
test_attribute_ObjectiveLimit(::MOI.ModelLike, ::Config) = nothing

function setup_test(
    ::typeof(test_attribute_ObjectiveLimit),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOI.set(model, MOI.ObjectiveLimit(), nothing)
    return
end

version_added(::typeof(test_attribute_ObjectiveLimit)) = v"1.20.0"

function test_attribute_SolutionLimit(model::MOI.AbstractOptimizer, ::Config)
    @requires MOI.supports(model, MOI.SolutionLimit())
    # Get the current value to restore it at the end of the test
    value = MOI.get(model, MOI.SolutionLimit())
    MOI.set(model, MOI.SolutionLimit(), 3)
    @test MOI.get(model, MOI.SolutionLimit()) == 3
    MOI.set(model, MOI.SolutionLimit(), nothing)
    @test MOI.get(model, MOI.SolutionLimit()) === nothing
    MOI.set(model, MOI.SolutionLimit(), 1)
    @test MOI.get(model, MOI.SolutionLimit()) == 1
    MOI.set(model, MOI.SolutionLimit(), value)
    _test_attribute_value_type(model, MOI.SolutionLimit())
    return
end

test_attribute_SolutionLimit(::MOI.ModelLike, ::Config) = nothing

function setup_test(
    ::typeof(test_attribute_SolutionLimit),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOI.set(model, MOI.SolutionLimit(), nothing)
    return
end

version_added(::typeof(test_attribute_SolutionLimit)) = v"1.21.0"

function test_attribute_NodeLimit(model::MOI.AbstractOptimizer, ::Config)
    @requires MOI.supports(model, MOI.NodeLimit())
    # Get the current value to restore it at the end of the test
    value = MOI.get(model, MOI.NodeLimit())
    MOI.set(model, MOI.NodeLimit(), 3)
    @test MOI.get(model, MOI.NodeLimit()) == 3
    MOI.set(model, MOI.NodeLimit(), nothing)
    @test MOI.get(model, MOI.NodeLimit()) === nothing
    MOI.set(model, MOI.NodeLimit(), 1)
    @test MOI.get(model, MOI.NodeLimit()) == 1
    MOI.set(model, MOI.NodeLimit(), value)
    _test_attribute_value_type(model, MOI.NodeLimit())
    return
end

test_attribute_NodeLimit(::MOI.ModelLike, ::Config) = nothing

function setup_test(
    ::typeof(test_attribute_NodeLimit),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOI.set(model, MOI.NodeLimit(), nothing)
    return
end

version_added(::typeof(test_attribute_NodeLimit)) = v"1.32.0"

"""
    test_attribute_AbsoluteGapTolerance(model::MOI.AbstractOptimizer, config::Config)

Test that the [`MOI.AbsoluteGapTolerance`](@ref) attribute is implemented for
`model`.
"""
function test_attribute_AbsoluteGapTolerance(
    model::MOI.AbstractOptimizer,
    ::Config,
)
    @requires MOI.supports(model, MOI.AbsoluteGapTolerance())
    # Get the current value to restore it at the end of the test
    value = MOI.get(model, MOI.AbsoluteGapTolerance())
    MOI.set(model, MOI.AbsoluteGapTolerance(), 1e-2)
    @test MOI.get(model, MOI.AbsoluteGapTolerance()) == 1e-2
    MOI.set(model, MOI.AbsoluteGapTolerance(), 100.0)
    @test MOI.get(model, MOI.AbsoluteGapTolerance()) == 100.0
    MOI.set(model, MOI.AbsoluteGapTolerance(), value)
    @test value == MOI.get(model, MOI.AbsoluteGapTolerance())
    _test_attribute_value_type(model, MOI.AbsoluteGapTolerance())
    return
end
test_attribute_AbsoluteGapTolerance(::MOI.ModelLike, ::Config) = nothing

function setup_test(
    ::typeof(test_attribute_AbsoluteGapTolerance),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOI.set(model, MOI.AbsoluteGapTolerance(), nothing)
    return
end

version_added(::typeof(test_attribute_AbsoluteGapTolerance)) = v"1.7.0"

"""
    test_attribute_RelativeGapTolerance(model::MOI.AbstractOptimizer, config::Config)

Test that the [`MOI.RelativeGapTolerance`](@ref) attribute is implemented for
`model`.
"""
function test_attribute_RelativeGapTolerance(
    model::MOI.AbstractOptimizer,
    ::Config,
)
    @requires MOI.supports(model, MOI.RelativeGapTolerance())
    # Get the current value to restore it at the end of the test
    value = MOI.get(model, MOI.RelativeGapTolerance())
    MOI.set(model, MOI.RelativeGapTolerance(), 1e-2)
    @test MOI.get(model, MOI.RelativeGapTolerance()) == 1e-2
    MOI.set(model, MOI.RelativeGapTolerance(), 5e-5)
    @test MOI.get(model, MOI.RelativeGapTolerance()) == 5e-5
    MOI.set(model, MOI.RelativeGapTolerance(), value)
    @test value == MOI.get(model, MOI.RelativeGapTolerance())
    _test_attribute_value_type(model, MOI.RelativeGapTolerance())
    return
end
test_attribute_RelativeGapTolerance(::MOI.ModelLike, ::Config) = nothing

function setup_test(
    ::typeof(test_attribute_RelativeGapTolerance),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOI.set(model, MOI.RelativeGapTolerance(), nothing)
    return
end

version_added(::typeof(test_attribute_RelativeGapTolerance)) = v"1.7.0"

"""
    test_attribute_after_empty(model::MOI.AbstractOptimizer, config::Config)

Test that optimizer attributes such as `Silent` are not cleared by `MOI.empty!`.
"""
function test_attribute_after_empty(model::MOI.AbstractOptimizer, ::Config)
    @requires MOI.supports(model, MOI.Silent())
    current = MOI.get(model, MOI.Silent())
    for value in (true, false)
        MOI.set(model, MOI.Silent(), value)
        @test MOI.get(model, MOI.Silent()) == value
        MOI.empty!(model)
        @test MOI.get(model, MOI.Silent()) == value
    end
    # Make sure to reset the value before leaving this function
    MOI.set(model, MOI.Silent(), current)
    return
end

test_attribute_after_empty(::MOI.ModelLike, ::Config) = nothing

struct _UnsupportedVectorSet2010 <: MOI.AbstractVectorSet end

"""
    test_attribute_unsupported_constraint(model::MOI.ModelLike, config::Config)

Test that `ListOfConstraintIndices` and `NumberOfConstraints` return the correct
values if the constraint type is unsupported.
"""
function test_attribute_unsupported_constraint(model::MOI.ModelLike, ::Config)
    F, S = MOI.VectorOfVariables, _UnsupportedVectorSet2010
    @requires !MOI.supports_constraint(model, F, S)
    @test MOI.get(model, MOI.ListOfConstraintIndices{F,S}()) ==
          MOI.ConstraintIndex{F,S}[]
    @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == Int64(0)
    return
end

version_added(::typeof(test_attribute_unsupported_constraint)) = v"1.9.0"
