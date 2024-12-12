# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintComplementsToScalarNonlinearFunctionBridge

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
        MOI.Bridges.Constraint.ComplementsToScalarNonlinearFunctionBridge,
        """
        variables: f, x
        [f, x] in Complements(2)
        x >= 0.0
        """,
        """
        variables: f, x
        ScalarNonlinearFunction(x * f) <= 0.0
        ScalarNonlinearFunction(*(-1.0, f)) <= 0.0
        x >= 0.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ComplementsToScalarNonlinearFunctionBridge,
        """
        variables: f, x
        [f, x] in Complements(2)
        x >= 1.0
        """,
        """
        variables: f, x
        ScalarNonlinearFunction((x - 1.0) * f) <= 0.0
        ScalarNonlinearFunction(-1.0 * f) <= 0.0
        x >= 1.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ComplementsToScalarNonlinearFunctionBridge,
        """
        variables: f, x
        [f, x] in Complements(2)
        x <= 1.0
        """,
        """
        variables: f, x
        ScalarNonlinearFunction((x - 1.0) * f) <= 0.0
        ScalarNonlinearFunction(+(f)) <= 0.0
        x <= 1.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ComplementsToScalarNonlinearFunctionBridge,
        """
        variables: f, x
        [f, x] in Complements(2)
        x >= 1.0
        x <= 2.0
        """,
        """
        variables: f, x
        ScalarNonlinearFunction((x - 2.0) * f) <= 0.0
        ScalarNonlinearFunction((x - 1.0) * f) <= 0.0
        x >= 1.0
        x <= 2.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ComplementsToScalarNonlinearFunctionBridge,
        """
        variables: f, x
        [f, x] in Complements(2)
        """,
        """
        variables: f, x
        ScalarNonlinearFunction(+(f)) <= 0.0
        ScalarNonlinearFunction(-1.0 * f) <= 0.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ComplementsToScalarNonlinearFunctionBridge,
        """
        variables: f1, f2, x1, x2
        [f1, f2, x1, x2] in Complements(4)
        f1 >= 1.0
        f2 >= 2.0
        x1 >= 3.0
        x2 >= 4.0
        """,
        """
        variables: f1, f2, x1, x2
        f1 >= 1.0
        f2 >= 2.0
        x1 >= 3.0
        x2 >= 4.0
        ScalarNonlinearFunction((x1 - 3.0) * f1) <= 0.0
        ScalarNonlinearFunction(-1.0 * f1) <= 0.0
        ScalarNonlinearFunction((x2 - 4.0) * f2) <= 0.0
        ScalarNonlinearFunction(-1.0 * f2) <= 0.0
        """,
    )
    return
end

function test_runtests_ScalarAffineFunction()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ComplementsToScalarNonlinearFunctionBridge,
        """
        variables: f, x
        [2.0 * f + 1.0, x] in Complements(2)
        x >= 0.0
        """,
        """
        variables: f, x
        ScalarNonlinearFunction(x * esc(2.0 * f + 1.0)) <= 0.0
        ScalarNonlinearFunction(*(-1.0, esc(2.0 * f + 1.0))) <= 0.0
        x >= 0.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ComplementsToScalarNonlinearFunctionBridge,
        """
        variables: f, x
        [2.0 * f + 1.0, x] in Complements(2)
        """,
        """
        variables: f, x
        ScalarNonlinearFunction(+(esc(2.0 * f + 1.0))) <= 0.0
        ScalarNonlinearFunction(-1.0 * esc(2.0 * f + 1.0)) <= 0.0
        """,
    )
    return
end

function test_runtests_ScalarQuadraticFunction()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ComplementsToScalarNonlinearFunctionBridge,
        """
        variables: f, x
        [2.0 * f * f + 1.0, x] in Complements(2)
        x >= 0.0
        """,
        """
        variables: f, x
        ScalarNonlinearFunction(x * esc(2.0 * f * f + 1.0)) <= 0.0
        ScalarNonlinearFunction(*(-1.0, esc(2.0 * f * f + 1.0))) <= 0.0
        x >= 0.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ComplementsToScalarNonlinearFunctionBridge,
        """
        variables: f, x
        [2.0 * f * f + 1.0, x] in Complements(2)
        """,
        """
        variables: f, x
        ScalarNonlinearFunction(+(esc(2.0 * f * f + 1.0))) <= 0.0
        ScalarNonlinearFunction(-1.0 * esc(2.0 * f * f + 1.0)) <= 0.0
        """,
    )
    return
end

function test_runtests_ScalarNonlinearFunction()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ComplementsToScalarNonlinearFunctionBridge,
        """
        variables: f, x
        VectorNonlinearFunction([sin(f), x]) in Complements(2)
        x >= 0.0
        """,
        """
        variables: f, x
        ScalarNonlinearFunction(x * sin(f)) <= 0.0
        ScalarNonlinearFunction(*(-1.0, sin(f))) <= 0.0
        x >= 0.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ComplementsToScalarNonlinearFunctionBridge,
        """
        variables: f, x
        VectorNonlinearFunction([sin(f), x]) in Complements(2)
        """,
        """
        variables: f, x
        ScalarNonlinearFunction(sin(f)) <= 0.0
        ScalarNonlinearFunction(-1.0 * sin(f)) <= 0.0
        """,
    )
    return
end

end  # module

TestConstraintComplementsToScalarNonlinearFunctionBridge.runtests()
