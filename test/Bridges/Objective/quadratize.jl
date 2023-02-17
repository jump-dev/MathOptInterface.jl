# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestObjectiveQuadratize

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

function test_solve_singlevariable_obj()
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Quadratize{Float64}(mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [1.0], MOI.FEASIBLE_POINT),
    )
    MOI.Test.test_objective_ObjectiveFunction_duplicate_terms(
        model,
        MOI.Test.Config(),
    )
    @test MOI.get(mock, MOI.ObjectiveFunctionType()) ==
          MOI.ScalarQuadraticFunction{Float64}
    @test MOI.get(model, MOI.ObjectiveFunctionType()) ==
          MOI.ScalarAffineFunction{Float64}
    @test MOI.get(mock, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    vis = MOI.get(model, MOI.ListOfVariableIndices())
    func = 3.0 * vis[1] + 0.0
    @test MOI.get(
        mock,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
    ) ≈ convert(MOI.ScalarQuadraticFunction{Float64}, func)
    @test MOI.get(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    ) ≈ func
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(mock, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    _test_delete_objective(model, 1, tuple())
    return
end

function test_solve_result_index()
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Quadratize{Float64}(mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0]),
            MOI.FEASIBLE_POINT,
            (MOI.VariableIndex, MOI.GreaterThan{Float64}) => [1.0],
        ),
    )
    MOI.Test.test_solve_result_index(model, MOI.Test.Config())
    return
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.QuadratizeBridge,
        """
        variables: x
        minobjective: 1.1 * x + 2.2
        """,
        """
        variables: x
        minobjective: 0.0 * x * x + 1.1 * x + 2.2
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.QuadratizeBridge,
        """
        variables: x
        maxobjective: 1.1 * x + 2.2
        """,
        """
        variables: x
        maxobjective: 0.0 * x * x + 1.1 * x + 2.2
        """,
    )
    return
end

end  # module

TestObjectiveQuadratize.runtests()
