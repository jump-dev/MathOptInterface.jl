# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintIntervalToHyperRectangle

using Test

import MathOptInterface as MOI

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name) $T" for T in [Float32, Float64]
                getfield(@__MODULE__, name)(T)
            end
        end
    end
    return
end

include("../utilities.jl")

function test_ScalarFunctionConstantNotZero(T)
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}()),
    )
    bridged_mock = MOI.Bridges.Constraint.IntervalToHyperRectangle{T}(mock)
    config = MOI.Test.Config(T)
    MOI.Test.test_model_ScalarFunctionConstantNotZero(bridged_mock, config)
    return
end

function test_basic(T)
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}()),
    )
    bridged_mock = MOI.Bridges.Constraint.IntervalToHyperRectangle{T}(mock)
    config = MOI.Test.Config()
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = ["test_basic_ScalarAffineFunction_Interval"],
    )
    return
end

function test_linear_integration(T)
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}()),
    )
    bridged_mock = MOI.Bridges.Constraint.IntervalToHyperRectangle{T}(mock)
    config = MOI.Test.Config(T, exclude = Any[MOI.ConstraintBasisStatus])
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            T[5, 5],
            (MOI.VectorAffineFunction{T}, MOI.HyperRectangle{T}) => [T[-1]],
        ),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            T[5//2, 5//2],
            (MOI.VectorAffineFunction{T}, MOI.HyperRectangle{T}) => [T[1]],
        ),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            T[1, 1],
            (MOI.VectorAffineFunction{T}, MOI.HyperRectangle{T}) => [T[1]],
        ),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            T[6, 6],
            (MOI.VectorAffineFunction{T}, MOI.HyperRectangle{T}) => [T[-1]],
        ),
    )
    MOI.Test.test_linear_integration_Interval(bridged_mock, config)
    return
end

function test_runtests(T)
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.IntervalToHyperRectangleBridge,
        model -> begin
            x = MOI.add_variable(model)
            MOI.add_constraint(model, x, MOI.Interval{T}(3, 5))
        end,
        model -> begin
            x = MOI.add_variable(model)
            MOI.add_constraint(
                model,
                MOI.VectorOfVariables([x]),
                MOI.HyperRectangle(T[3], T[5]),
            )
        end,
        eltype = T,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.IntervalToHyperRectangleBridge,
        model -> begin
            x = MOI.add_variable(model)
            MOI.add_constraint(model, T(2) * x, MOI.Interval{T}(3, 5))
        end,
        model -> begin
            x = MOI.add_variable(model)
            MOI.add_constraint(
                model,
                MOI.Utilities.vectorize([T(2) * x]),
                MOI.HyperRectangle(T[3], T[5]),
            )
        end,
        eltype = T,
    )
    return
end

function test_modify_ScalarCoefficientChange(T)
    inner = MOI.Utilities.Model{T}()
    model = MOI.Bridges.Constraint.IntervalToHyperRectangle{T}(inner)
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, T(1) * x, MOI.Interval(T(0), T(1)))
    @test ≈(MOI.get(model, MOI.ConstraintFunction(), c), T(1) * x)
    MOI.modify(model, c, MOI.ScalarCoefficientChange(x, T(2)))
    @test ≈(MOI.get(model, MOI.ConstraintFunction(), c), T(2) * x)
    return
end

end  # module

TestConstraintIntervalToHyperRectangle.runtests()
