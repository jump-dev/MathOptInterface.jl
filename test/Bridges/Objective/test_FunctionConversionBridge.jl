# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestObjectiveConversion

using Test

import MathOptInterface as MOI

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

include("../utilities.jl")

struct VariableDifference <: MOI.AbstractScalarFunction
    x::MOI.VariableIndex
    y::MOI.VariableIndex
end

function MOI.Utilities.is_coefficient_type(
    ::Type{VariableDifference},
    ::Type{T},
) where {T}
    return true
end

function MOI.Bridges.Constraint.conversion_cost(
    ::Type{<:MOI.ScalarAffineFunction},
    ::Type{VariableDifference},
)
    return 1.0
end

function MOI.convert(
    ::Type{MOI.ScalarAffineFunction{T}},
    f::VariableDifference,
) where {T}
    return one(T) * f.x - one(T) * f.y
end

function test_variable_difference(T = Float64)
    F = MOI.ScalarAffineFunction{T}
    B = MOI.Bridges.Objective.FunctionConversionBridge{T,F}
    inner = MOI.Utilities.Model{T}()
    model = MOI.Bridges.Objective.SingleBridgeOptimizer{B}(inner)
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    g = VariableDifference(x, y)
    G = typeof(g)
    MOI.set(model, MOI.ObjectiveFunction{G}(), g)
    f = one(T) * x - one(T) * y
    @test MOI.get(inner, MOI.ObjectiveFunction{F}()) ≈ f
    return
end

end  # module

TestObjectiveConversion.runtests()
