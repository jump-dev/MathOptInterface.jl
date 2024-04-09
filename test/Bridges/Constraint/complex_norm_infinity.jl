# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintComplexNormInfinityToSecondOrderCone

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
        MOI.Bridges.Constraint.ComplexNormInfinityToSecondOrderConeBridge,
        """
        variables: t, x
        ::Complex{Float64}: [t, (1 + 2im) * x + (3 + 4im)] in NormInfinityCone(2)
        """,
        """
        variables: t, x
        ::Float64: [t, 1 * x + 3, 2 * x + 4] in SecondOrderCone(3)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ComplexNormInfinityToSecondOrderConeBridge,
        """
        variables: t, x, y
        ::Complex{Float64}: [2.0 * t + 3.0, x + im * y] in NormInfinityCone(2)
        """,
        """
        variables: t, x, y
        ::Float64: [2.0 * t + 3.0, 1.0 * x, 1.0 * y] in SecondOrderCone(3)
        """,
    )
    return
end

end  # module

TestConstraintComplexNormInfinityToSecondOrderCone.runtests()
