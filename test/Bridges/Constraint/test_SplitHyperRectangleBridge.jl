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

function test_runtests_all_free_rows()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitHyperRectangleBridge,
        """
        variables: x
        [x] in HyperRectangle([-Inf], [Inf])
        """,
        """
        variables: x
        """,
    )
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Constraint.SplitHyperRectangle{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.Utilities.operate(vcat, Float64, 1.0 * x)
    c = MOI.add_constraint(model, f, MOI.HyperRectangle([-Inf], [Inf]))
    @test MOI.get(model, MOI.ConstraintDual(), c) == [0.0]
    @test_throws(
        MOI.GetAttributeNotAllowed{MOI.ConstraintPrimal},
        MOI.get(model, MOI.ConstraintPrimal(), c)
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

function test_runtests_VectorOfVariables_mix_of_signs()
    #     0 <= a <= 1       |        a >= 0
    #  -Inf <= b <= Inf     |        c >= 0
    #     0 <= c <= Inf     |   -1 + d >= 0
    #     1 <= d <= Inf     |    1 - a >= 0
    #  -Inf <= e <= 0       |      - e >= 0
    #  -Inf <= f <= Inf     |    1 - g >= 0
    #  -Inf <= g <= 1       |
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitHyperRectangleBridge,
        """
        variables: a, b, c, d, e, f, g
        [a, b, c, d, e, f, g] in HyperRectangle([0.0, -Inf, 0.0, 1.0, -Inf, -Inf, -Inf], [1.0, Inf, Inf, Inf, 0.0, Inf, 1.0])
        """,
        """
        variables: a, b, c, d, e, f, g
        [1.0 * a, 1.0 * c, 1.0 * d + -1.0, 1.0 + -1.0 * a, -1.0 * e, 1.0 + -1.0 * g] in Nonnegatives(6)
        """;
        constraint_start = [1.1, 0.0, 1.2, 1.3, -1.1, 0.0, -1.2],
    )
    return
end

end  # module

TestConstraintHyperRectangle.runtests()
