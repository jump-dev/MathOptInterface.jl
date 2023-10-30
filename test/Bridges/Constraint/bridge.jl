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
    @test !MOI.supports(model, MOI.ConstraintPrimalStart(), typeof(bridge))
    message = MOI.Bridges._attribute_error_message(
        MOI.ConstraintPrimalStart(),
        DummyBridge,
        "setting a value for",
    )
    @test_throws(
        MOI.UnsupportedAttribute(MOI.ConstraintPrimalStart(), message),
        MOI.set(model, MOI.ConstraintPrimalStart(), bridge, 1.0),
    )
    message = MOI.Bridges._attribute_error_message(
        MOI.ConstraintSet(),
        DummyBridge,
        "setting a value for",
    )
    @test_throws(
        MOI.SetAttributeNotAllowed(MOI.ConstraintSet(), message),
        MOI.set(model, MOI.ConstraintSet(), bridge, MOI.EqualTo(1.0)),
    )
    return
end

end  # module

TestConstraintBridge.runtests()
