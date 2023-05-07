# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintSymmetricMatrixScaling

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

function test_scaling()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SymmetricMatrixScalingBridge,
        """
        variables: x, y, z
        [x, 1.0 * y, z] in PositiveSemidefiniteConeTriangle(2)
        """,
        """
        variables: x, y, z
        [x, √2 * y, z] in ScaledPositiveSemidefiniteConeTriangle(2)
        """,
    )
    return
end

function test_scaling_vector_of_variables()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SymmetricMatrixScalingBridge,
        """
        variables: x, y, z
        [x, y, z] in PositiveSemidefiniteConeTriangle(2)
        """,
        """
        variables: x, y, z
        [x, √2 * y, z] in ScaledPositiveSemidefiniteConeTriangle(2)
        """,
    )
    return
end

function test_scaling_quadratic()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SymmetricMatrixScalingBridge,
        """
        variables: x, y, z
        [x, 1.0 * y * y + 1.0 * y + 3.0, z] in PositiveSemidefiniteConeTriangle(2)
        """,
        """
        variables: x, y, z
        [x, √2 * y * y + √2 * y + 4.242640687119286, z] in ScaledPositiveSemidefiniteConeTriangle(2)
        """,
    )
    return
end

function test_inverse_scaling()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SymmetricMatrixInverseScalingBridge,
        """
        variables: x, y, z
        [x, √2 * y, z] in ScaledPositiveSemidefiniteConeTriangle(2)
        """,
        """
        variables: x, y, z
        [x, 1.0 * y, z] in PositiveSemidefiniteConeTriangle(2)
        """,
    )
    return
end

function test_inverse_scaling_vector_of_variables()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SymmetricMatrixInverseScalingBridge,
        """
        variables: x, y, z
        [x, y, z] in ScaledPositiveSemidefiniteConeTriangle(2)
        """,
        """
        variables: x, y, z
        [x, 0.7071067811865475 * y, z] in PositiveSemidefiniteConeTriangle(2)
        """,
    )
    return
end

function test_inverse_scaling_quadratic()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SymmetricMatrixInverseScalingBridge,
        """
        variables: x, y, z
        [x, √2 * y * y, z] in ScaledPositiveSemidefiniteConeTriangle(2)
        """,
        """
        variables: x, y, z
        [x, 1.0 * y * y, z] in PositiveSemidefiniteConeTriangle(2)
        """,
    )
    return
end

end  # module

TestConstraintSymmetricMatrixScaling.runtests()
