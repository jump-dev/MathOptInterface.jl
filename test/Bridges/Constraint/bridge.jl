# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintBridge

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

struct DummyBridge <: MOI.Bridges.Constraint.AbstractBridge end

function test_AbstractBridge()
    model = MOI.Utilities.Model{Float64}()
    bridge = DummyBridge()
    attr = MOI.ConstraintPrimalStart()
    @test !MOI.supports(model, attr, typeof(bridge))
    @test_throws MOI.UnsupportedAttribute(attr) MOI.set(
        model,
        attr,
        bridge,
        1.0,
    )
    attr = MOI.ConstraintFunction()
    err = MOI.SetAttributeNotAllowed(attr)
    @test_throws err MOI.set(model, attr, bridge, MOI.EqualTo(1.0))
end

end  # module

TestConstraintBridge.runtests()
