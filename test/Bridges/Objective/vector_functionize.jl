# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestObjectiveVectorFunctionize

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
        MOI.Bridges.Objective.VectorFunctionizeBridge,
        """
        variables: x, y
        minobjective: [x, y]
        """,
        """
        variables: x, y
        minobjective: [1.0 * x + 0.0, 1.0 * y + 0.0]
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.VectorFunctionizeBridge,
        """
        variables: x, y
        maxobjective: [x, y]
        """,
        """
        variables: x, y
        maxobjective: [1.0 * x + 0.0, 1.0 * y + 0.0]
        """,
    )
    return
end

function test_objective_function_value()
    inner = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    model = MOI.Bridges.Objective.VectorFunctionize{Float64}(inner)
    MOI.Utilities.loadfromstring!(
        model,
        """
        variables: x, y
        minobjective: [x, y]
        """,
    )
    MOI.Utilities.set_mock_optimize!(
        inner,
        mock -> MOI.Utilities.mock_optimize!(mock, [3.0, 5.6]),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.ObjectiveValue()) ≈ [3.0, 5.6]
    return
end

function test_set_objective_sense()
    inner = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    model = MOI.Bridges.Objective.VectorFunctionize{Float64}(inner)
    MOI.Utilities.loadfromstring!(
        model,
        """
        variables: x, y
        minobjective: [x, y]
        """,
    )
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    return
end

end  # module

TestObjectiveVectorFunctionize.runtests()
