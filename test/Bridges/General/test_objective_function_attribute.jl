# Copyright (c) 2026: jump-dev contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestObjectiveFunctionAttribute

using Test
import MathOptInterface as MOI

struct ObjectiveData <: MOI.AbstractModelAttribute end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.Bridges.ObjectiveFunctionAttribute{ObjectiveData},
    bridge::MOI.Bridges.Objective.SlackBridge,
)
    return MOI.get(model, MOI.ConstraintPrimalStart(), bridge.constraint)
end

function MOI.set(
    model::MOI.ModelLike,
    ::MOI.Bridges.ObjectiveFunctionAttribute{ObjectiveData},
    bridge::MOI.Bridges.Objective.SlackBridge,
    value,
)
    MOI.set(model, MOI.ConstraintPrimalStart(), bridge.constraint, value)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.Bridges.ObjectiveFunctionAttribute{ObjectiveData,G},
    ::MOI.Bridges.Objective.FunctionConversionBridge{T,F,G},
) where {T,F,G}
    return MOI.get(
        model,
        MOI.Bridges.ObjectiveFunctionAttribute{ObjectiveData,F}(attr.attr),
    )
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.Bridges.ObjectiveFunctionAttribute{ObjectiveData,G},
    ::MOI.Bridges.Objective.FunctionConversionBridge{T,F,G},
    value,
) where {T,F,G}
    MOI.set(
        model,
        MOI.Bridges.ObjectiveFunctionAttribute{ObjectiveData,F}(attr.attr),
        value,
    )
    return
end

function test_unbridged()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    attr =
        MOI.Bridges.ObjectiveFunctionAttribute{ObjectiveData,MOI.VariableIndex}(
            ObjectiveData(),
        )
    for model in (inner, MOI.Bridges.Objective.Slack{Float64}(inner))
        MOI.set(model, attr, 1.23)
        @test MOI.get(inner, ObjectiveData()) == 1.23
        @test MOI.get(model, attr) == 1.23
    end
    return
end

function test_bridged()
    for stacked in (false, true)
        inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
        slack = MOI.Bridges.Objective.Slack{Float64}(inner)
        model = if stacked
            MOI.Bridges.Objective.SingleBridgeOptimizer{
                MOI.Bridges.Objective.FunctionConversionBridge{
                    Float64,
                    MOI.ScalarQuadraticFunction{Float64},
                },
            }(
                slack,
            )
        else
            slack
        end
        x = MOI.add_variable(model)
        f = 2.0 * x + 3.0
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
        MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
        attr = MOI.Bridges.ObjectiveFunctionAttribute{ObjectiveData,typeof(f)}(
            ObjectiveData(),
        )
        MOI.set(model, attr, 4.56)
        @test MOI.get(model, attr) == 4.56
        F = MOI.get(slack, MOI.ObjectiveFunctionType())
        bridge = MOI.Bridges.bridge(slack, MOI.ObjectiveFunction{F}())
        @test MOI.get(inner, MOI.ConstraintPrimalStart(), bridge.constraint) ==
              4.56
        MOI.set(inner, MOI.ConstraintPrimalStart(), bridge.constraint, 7.89)
        @test MOI.get(model, attr) == 7.89
    end
    return
end

@testset "ObjectiveFunctionAttribute" begin
    test_unbridged()
    test_bridged()
end

end
