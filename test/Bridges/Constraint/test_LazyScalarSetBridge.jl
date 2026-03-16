# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintLazyScalarSetBridge

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

function test_runtests_scalar_affine_function()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.LazyScalarSetBridge,
        """
        variables: x
        1.0 * x in LazyScalarSet(GreaterThan(0.0))
        """,
        """
        variables: x
        1.0 * x in GreaterThan(0.0)
        """,
    )
    return
end

end  # module

TestConstraintLazyScalarSetBridge.runtests()
