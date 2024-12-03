# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintInequalityToComplements

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

function test_runtests_GreaterThan()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.InequalityToComplementsBridge,
        """
        variables: x
        1.0 * x >= 0.0
        """,
        """
        variables: x, y
        [1.0 * x, y] in Complements(2)
        y >= 0.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.InequalityToComplementsBridge,
        """
        variables: x
        2.0 * x >= 3.0
        """,
        """
        variables: x, y
        [2.0 * x + -3.0, y] in Complements(2)
        y >= 0.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.InequalityToComplementsBridge,
        """
        variables: x
        2.0 * x * x >= 3.0
        """,
        """
        variables: x, y
        [2.0 * x * x + -3.0, y] in Complements(2)
        y >= 0.0
        """,
    )
    return
end

function test_runtests_LessThan()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.InequalityToComplementsBridge,
        """
        variables: x
        1.0 * x <= 0.0
        """,
        """
        variables: x, y
        [1.0 * x, y] in Complements(2)
        y <= 0.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.InequalityToComplementsBridge,
        """
        variables: x
        2.0 * x <= 3.0
        """,
        """
        variables: x, y
        [2.0 * x + -3.0, y] in Complements(2)
        y <= 0.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.InequalityToComplementsBridge,
        """
        variables: x
        2.0 * x * x <= 3.0
        """,
        """
        variables: x, y
        [2.0 * x * x + -3.0, y] in Complements(2)
        y <= 0.0
        """,
    )
    return
end

function test_runtests_EqualTo()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.InequalityToComplementsBridge,
        """
        variables: x
        1.0 * x == 0.0
        """,
        """
        variables: x, y
        [1.0 * x, y] in Complements(2)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.InequalityToComplementsBridge,
        """
        variables: x
        2.0 * x == 3.0
        """,
        """
        variables: x, y
        [2.0 * x + -3.0, y] in Complements(2)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.InequalityToComplementsBridge,
        """
        variables: x
        2.0 * x * x == 3.0
        """,
        """
        variables: x, y
        [2.0 * x * x + -3.0, y] in Complements(2)
        """,
    )
    return
end

function test_ScalarNonlinearFunction()
    # We can't use the standard runtests because ScalarNonlinearFunction does
    # not preserve f(x) ≈ (f(x) - g(x)) + g(x)
    for set in (MOI.EqualTo(1.0), MOI.LessThan(1.0), MOI.GreaterThan(1.0))
        inner = MOI.Utilities.Model{Float64}()
        model = MOI.Bridges.Constraint.InequalityToComplements{Float64}(inner)
        x = MOI.add_variable(model)
        f = MOI.ScalarNonlinearFunction(:sin, Any[x])
        c = MOI.add_constraint(model, f, set)
        F, S = MOI.VectorNonlinearFunction, MOI.Complements
        indices = MOI.get(inner, MOI.ListOfConstraintIndices{F,S}())
        @test length(indices) == 1
        inner_variables = MOI.get(inner, MOI.ListOfVariableIndices())
        @test length(inner_variables) == 2
        u, v = inner_variables
        u_sin = MOI.ScalarNonlinearFunction(:sin, Any[u])
        g = MOI.VectorNonlinearFunction([
            MOI.ScalarNonlinearFunction(:-, Any[u_sin, 1.0]),
            MOI.ScalarNonlinearFunction(:+, Any[v]),
        ])
        @test ≈(MOI.get(inner, MOI.ConstraintFunction(), indices[1]), g)
        h = MOI.ScalarNonlinearFunction(
            :+,
            Any[MOI.ScalarNonlinearFunction(:-, Any[f, 1.0]), 1.0],
        )
        @test ≈(MOI.get(model, MOI.ConstraintFunction(), c), h)
    end
    return
end

end  # module

TestConstraintInequalityToComplements.runtests()
