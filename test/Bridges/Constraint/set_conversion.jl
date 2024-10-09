# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintSetConversion

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

struct Zero <: MOI.AbstractScalarSet
end

MOI.Bridges.Constraint.conversion_cost(::Type{MOI.EqualTo{Float64}}, ::Type{Zero}) = 1.0

Base.convert(::Type{MOI.EqualTo{Float64}}, ::Zero) = MOI.EqualTo(0.0)

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ScalarFunctionizeBridge,
        model -> begin
            x = MOI.add_variable(model)
            MOI.add_constraint(model, Zero())
        end,
        model -> begin
            x = MOI.add_variable(model)
            MOI.add_constraint(model, MOI.EqualTo(0.0))
        end,
    )
    return
end

end  # module

TestConstraintSetConversion.runtests()
