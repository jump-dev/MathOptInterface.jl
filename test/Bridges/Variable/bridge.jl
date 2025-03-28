# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestVariableBridge

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

struct DummyVariableBridge <: MOI.Bridges.Variable.AbstractBridge end

function test_AbstractBridge()
    model = MOI.Utilities.Model{Float64}()
    bridge = DummyVariableBridge()
    attr = MOI.VariablePrimalStart()
    @test !MOI.supports(model, attr, typeof(bridge))
    i = MOI.Bridges.IndexInVector(1)
    @test_throws MOI.UnsupportedAttribute(attr) MOI.set(
        model,
        attr,
        bridge,
        1.0,
    )
    @test_throws MOI.UnsupportedAttribute(attr) MOI.set(
        model,
        attr,
        bridge,
        1.0,
        i,
    )
    attr = MOI.VariablePrimal()
    err = MOI.SetAttributeNotAllowed(attr)
    @test_throws err MOI.set(model, attr, bridge, 1.0)
    @test_throws err MOI.set(model, attr, bridge, 1.0, i)
    return
end

function test_bridging_cost()
    model = MOI.Bridges.Variable.SingleBridgeOptimizer{DummyVariableBridge}(
        MOI.Utilities.Model{Float64}(),
    )
    @test MOI.Bridges.bridging_cost(model) == 1.0
    @test MOI.Bridges.bridging_cost(model, MOI.ZeroOne) == 1.0
    return
end

end  # module

TestVariableBridge.runtests()
