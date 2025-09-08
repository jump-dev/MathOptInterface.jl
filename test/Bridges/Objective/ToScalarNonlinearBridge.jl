# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestObjectiveToScalarNonlinear

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

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.ToScalarNonlinearBridge,
        """
        variables: x
        minobjective: 2.0 * x + 1.0
        """,
        """
        variables: x
        minobjective: ScalarNonlinearFunction(2.0 * x + 1.0)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.ToScalarNonlinearBridge,
        """
        variables: x
        maxobjective: 2.0 * x + 1.0
        """,
        """
        variables: x
        maxobjective: ScalarNonlinearFunction(2.0 * x + 1.0)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.ToScalarNonlinearBridge,
        """
        variables: x, y
        minobjective: 1.0 * x * x + 2.0 * x * y + 3.0 * y + 4.0
        """,
        """
        variables: x, y
        minobjective: ScalarNonlinearFunction(1.0 * x * x + 2.0 * x * y + 3.0 * y + 4.0)
        """,
    )
    return
end

function test_supports()
    for T in (Int, Float64)
        model = MOI.instantiate(MOI.Utilities.Model{T}; with_bridge_type = T)
        for (F, flag) in [
            MOI.ScalarNonlinearFunction => true,
            MOI.ScalarAffineFunction{Float64} => (T == Float64),
            MOI.ScalarAffineFunction{Int} => (T == Int),
        ]
            @test MOI.supports(model, MOI.ObjectiveFunction{F}()) == flag
        end
    end
    return
end

end  # module

TestObjectiveToScalarNonlinear.runtests()
