# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintExponentialConeToScalarNonlinearFunctionBridge

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
        MOI.Bridges.Constraint.ExponentialConeToScalarNonlinearFunctionBridge,
        """
        variables: x, y, z
        [x, y, z] in ExponentialCone()
        """,
        """
        variables: x, y, z
        ScalarNonlinearFunction(y * exp(x / y) - z) <= 0.0
        1.0 * y >= 0.0
        """,
    )
    return
end

function test_runtests_VectorAffineFunction()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ExponentialConeToScalarNonlinearFunctionBridge,
        """
        variables: x, y, z
        [1.0 * x, 2.0 * y, 3.0 * z + 1.0] in ExponentialCone()
        """,
        """
        variables: x, y, z
        ScalarNonlinearFunction(esc(2.0 * y) * exp(esc(1.0 * x) / esc(2.0 * y)) - esc(3.0 * z + 1.0)) <= 0.0
        2.0 * y >= 0.0
        """,
    )
    return
end

end  # module

TestConstraintExponentialConeToScalarNonlinearFunctionBridge.runtests()
