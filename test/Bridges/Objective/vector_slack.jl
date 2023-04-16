# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestObjectiveVectorSlack

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
        MOI.Bridges.Objective.VectorSlackBridge,
        """
        variables: x
        minobjective: [1.1 * x + 2.2]
        """,
        """
        variables: x, y
        minobjective: [y]
        [1.0 * y + -1.1 * x + -2.2] in Nonnegatives(1)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.VectorSlackBridge,
        """
        variables: x
        maxobjective: [1.1 * x + 2.2]
        """,
        """
        variables: x, y
        maxobjective: [y]
        [-1.0 * y + 1.1 * x + 2.2] in Nonnegatives(1)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.VectorSlackBridge,
        """
        variables: x
        minobjective: [1.1 * x + 2.2, -1.0 * x]
        """,
        """
        variables: x, y, z
        minobjective: [y, z]
        [1.0 * y + -1.1 * x + -2.2, 1.0 * z + 1.0 * x] in Nonnegatives(2)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.VectorSlackBridge,
        """
        variables: x
        maxobjective: [1.1 * x + 2.2, -1.0 * x]
        """,
        """
        variables: x, y, z
        maxobjective: [y, z]
        [-1.0 * y + 1.1 * x + 2.2, -1.0 * z + -1.0 * x] in Nonnegatives(2)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.VectorSlackBridge,
        """
        variables: x
        maxobjective: [1.0 * x, -1.1 * x]
        """,
        """
        variables: x, y
        maxobjective: [x, y]
        [-1.0 * y + -1.1 * x] in Nonnegatives(1)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.VectorSlackBridge,
        """
        variables: x
        minobjective: [-1.1 * x, 1.0 * x]
        """,
        """
        variables: x, y
        minobjective: [y, x]
        [1.0 * y + 1.1 * x] in Nonnegatives(1)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Objective.VectorSlackBridge,
        """
        variables: x
        maxobjective: [x]
        """,
        """
        variables: x
        maxobjective: [x]
        """,
    )
    return
end

function test_objective_sense_before_function()
    inner = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    model = MOI.Bridges.Objective.VectorSlack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.Utilities.operate(vcat, Float64, 1.0 * x, 1.0 * x)
    err = ErrorException(
        "Set `MOI.ObjectiveSense` before `MOI.ObjectiveFunction` when using " *
        "`MOI.Bridges.Objective.VectorSlackBridge`.",
    )
    @test_throws err MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    return
end

function test_objective_function_value()
    for sense in ("min", "max")
        inner = MOI.Utilities.MockOptimizer(
            MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
        )
        model = MOI.Bridges.Objective.VectorSlack{Float64}(inner)
        MOI.Utilities.loadfromstring!(
            model,
            """
            variables: x
            $(sense)objective: [1.1 * x + 2.2, -1.0 * x]
            """,
        )
        MOI.Utilities.set_mock_optimize!(
            inner,
            mock -> MOI.Utilities.mock_optimize!(mock, [3.0, 5.6, -3.0]),
        )
        MOI.optimize!(model)
        # Test that we get 5.5 here, not 5.6 as set for the slack variable. This
        # ensures we return the value of the f(x) objective, not the `y` slack.
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ [5.5, -3.0]
    end
    return
end

function test_modify_vector_constant_change()
    inner = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    model = MOI.Bridges.Objective.VectorSlack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.Utilities.operate(vcat, Float64, -1.1 * x, 1.0 * x)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    attr = MOI.ObjectiveFunction{typeof(f)}()
    MOI.set(model, attr, f)
    @test MOI.get(model, attr) ≈ f
    @test_throws(
        MOI.ModifyObjectiveNotAllowed,
        MOI.modify(model, attr, MOI.VectorConstantChange([1.0, 2.0])),
    )
    return
end

end  # module

TestObjectiveVectorSlack.runtests()
