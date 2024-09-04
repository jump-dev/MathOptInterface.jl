# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintTable

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
        MOI.Bridges.Constraint.TableToMILPBridge,
        """
        variables: x, y, z
        [x, y, z] in Table(Float64[1 1 0; 0 1 1; 1 0 1; 1 1 1])
        """,
        """
        variables: x, y, z, a1, a2, a3, a4
        a1 + a2 + a3 + a4 == 1.0
        a1 +      a3 + a4 + -1.0 * x == 0.0
        a1 + a2 +      a4 + -1.0 * y == 0.0
             a2 + a3 + a4 + -1.0 * z == 0.0
        a1 in ZeroOne()
        a2 in ZeroOne()
        a3 in ZeroOne()
        a4 in ZeroOne()
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.TableToMILPBridge,
        """
        variables: x, y
        [x, y, 1] in Table(Float64[1 1 0; 0 1 1; 1 0 1; 1 1 1])
        """,
        """
        variables: x, y, a1, a2, a3, a4
        a1 + a2 + a3 + a4 == 1.0
        a1 +      a3 + a4 + -1.0 * x == 0.0
        a1 + a2 +      a4 + -1.0 * y == 0.0
             a2 + a3 + a4            == 1.0
        a1 in ZeroOne()
        a2 in ZeroOne()
        a3 in ZeroOne()
        a4 in ZeroOne()
        """,
    )
    return
end

end  # module

TestConstraintTable.runtests()
