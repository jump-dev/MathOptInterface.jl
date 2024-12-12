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
        variables: f, x, y
        1.0 * f + -1.0 * y == 0.0
        1.0 * x * y <= 0.0
        y in Interval(0.0, Inf)
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
        variables: f, x, y
        1.0 * f + -1.0 * y == 0.0
        1.0 * x * y + -1.0 * y <= 0.0
        y in Interval(0.0, Inf)
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
        variables: f, x, y
        1.0 * f + -1.0 * y == 0.0
        1.0 * x * y + -1.0 * y <= 0.0
        y in Interval(-Inf, 0.0)
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
        variables: f, x, y
        1.0 * f + -1.0 * y == 0.0
        1.0 * x * y + -2.0 * y <= 0.0
        1.0 * x * y + -1.0 * y <= 0.0
        y in Interval(-Inf, Inf)
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
        variables: f, x, y
        1.0 * f + -1.0 * y == 0.0
        y in Interval(0.0, 0.0)
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
        variables: f1, f2, x1, x2, y1, y2
        f1 >= 1.0
        f2 >= 2.0
        x1 >= 3.0
        x2 >= 4.0
        1.0 * f1 + -1.0 * y1 == 0.0
        1.0 * f2 + -1.0 * y2 == 0.0
        1.0 * x1 * y1 + -3.0 * y1 <= 0.0
        y1 in Interval(0.0, Inf)
        1.0 * x2 * y2 + -4.0 * y2 <= 0.0
        y2 in Interval(0.0, Inf)
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
        variables: f, x, y
        2.0 * f + 1.0 + -1.0 * y == 0.0
        1.0 * x * y <= 0.0
        y in Interval(0.0, Inf)
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
        variables: f, x, y
        2.0 * f + 1.0 + -1.0 * y == 0.0
        y in Interval(0.0, 0.0)
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
        variables: f, x, y
        2.0 * f * f + 1.0 + -1.0 * y == 0.0
        1.0 * x * y <= 0.0
        y in Interval(0.0, Inf)
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
        variables: f, x, y
        2.0 * f * f + 1.0 + -1.0 * y == 0.0
        y in Interval(0.0, 0.0)
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
        variables: f, x, y
        ScalarNonlinearFunction(sin(f) - y) == 0.0
        1.0 * x * y <= 0.0
        y in Interval(0.0, Inf)
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
        variables: f, x, y
        ScalarNonlinearFunction(sin(f) - y) == 0.0
        y in Interval(0.0, 0.0)
        """,
    )
    return
end

end  # module

TestConstraintComplementsToScalarNonlinearFunctionBridge.runtests()
