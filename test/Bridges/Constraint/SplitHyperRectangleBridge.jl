# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintHyperRectangle

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

function test_runtests_VectorOfVariables()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitHyperRectangleBridge,
        """
        variables: x, y, z
        [x, y, z] in HyperRectangle([-1.0, 2.0, 3.0], [1.1, 2.2, 3.3])
        """,
        """
        variables: x, y, z
        [1.0 * x + 1.0, 1.0 * y + -2.0, 1.0 * z + -3.0, 1.1 + -1.0 * x, 2.2 + -1.0 * y, 3.3 + -1.0 * z] in Nonnegatives(6)
        """,
    )
    return
end

function test_runtests_VectorOfVariables_zeros()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitHyperRectangleBridge,
        """
        variables: x, y
        [x, y] in HyperRectangle([0.0, -1.0], [1.0, 0.0])
        """,
        """
        variables: x, y
        [1.0 * x, 1.0 * y + 1.0, 1.0 + -1.0 * x, -1.0 * y] in Nonnegatives(4)
        """,
    )
    return
end

function test_runtests_infinity_lower()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitHyperRectangleBridge,
        """
        variables: x, z
        [x, z] in HyperRectangle([-Inf, 3.0], [1.1, 3.3])
        """,
        """
        variables: x, z
        [1.0 * z + -3.0, 1.1 + -1.0 * x, 3.3 + -1.0 * z] in Nonnegatives(3)
        """;
        constraint_start = -1.2,
    )
    return
end

function test_runtests_infinity_upper()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitHyperRectangleBridge,
        """
        variables: y, z
        [y, z] in HyperRectangle([2.0, 3.0], [Inf, 3.3])
        """,
        """
        variables: y, z
        [1.0 * y + -2.0, 1.0 * z + -3.0, 3.3 + -1.0 * z] in Nonnegatives(3)
        """;
        constraint_start = 1.2,
    )
    return
end

function test_runtests_VectorAffineFunction()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitHyperRectangleBridge,
        """
        variables: x, y, z
        [1.0 * x, -2.0 * y, 3.0 * z] in HyperRectangle([-1.0, 2.0, 3.0], [1.1, 2.2, 3.3])
        """,
        """
        variables: x, y, z
        [1.0 * x + 1.0, -2.0 * y + -2.0, 3.0 * z + -3.0, 1.1 + -1.0 * x, 2.2 + 2.0 * y, 3.3 + -3.0 * z] in Nonnegatives(6)
        """,
    )
    return
end

function test_runtests_free_row()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitHyperRectangleBridge,
        """
        variables: x, z
        [x, z] in HyperRectangle([-Inf, 3.0], [Inf, 3.3])
        """,
        """
        variables: x, z
        [1.0 * z + -3.0, 3.3 + -1.0 * z] in Nonnegatives(2)
        """;
        constraint_start = 0.0,
    )
    return
end

function test_basic_HyperRectangle()
    model = MOI.Bridges.Constraint.SplitHyperRectangle{Float64}(
        MOI.Utilities.Model{Float64}(),
    )
    config = MOI.Test.Config()
    MOI.empty!(model)
    MOI.Test.test_basic_VectorOfVariables_HyperRectangle(model, config)
    MOI.empty!(model)
    MOI.Test.test_basic_VectorAffineFunction_HyperRectangle(model, config)
    MOI.empty!(model)
    MOI.Test.test_basic_VectorQuadraticFunction_HyperRectangle(model, config)
    MOI.empty!(model)
    MOI.Test.test_basic_VectorNonlinearFunction_HyperRectangle(model, config)
    return
end

end  # module

TestConstraintHyperRectangle.runtests()
