# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintSplitComplexIndicatorEqualTo

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
        MOI.Bridges.Constraint.SplitComplexIndicatorEqualToBridge,
        """
        variables: x, z
        ::Complex{Float64}: [z, (1.0 + 2.0im) * x] in Indicator{ACTIVATE_ON_ONE}(EqualTo(3.0 + 4.0im))
        z in ZeroOne()
        """,
        """
        variables: x, z
        ::Float64: [z, 1.0 * x] in Indicator{ACTIVATE_ON_ONE}(EqualTo(3.0))
        ::Float64: [z, 2.0 * x] in Indicator{ACTIVATE_ON_ONE}(EqualTo(4.0))
        z in ZeroOne()
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitComplexIndicatorEqualToBridge,
        """
        variables: x, z
        ::Complex{Float64}: [z, (1.0 + 2.0im) * x] in Indicator{ACTIVATE_ON_ZERO}(EqualTo(3.0 + 4.0im))
        z in ZeroOne()
        """,
        """
        variables: x, z
        ::Float64: [z, 1.0 * x] in Indicator{ACTIVATE_ON_ZERO}(EqualTo(3.0))
        ::Float64: [z, 2.0 * x] in Indicator{ACTIVATE_ON_ZERO}(EqualTo(4.0))
        z in ZeroOne()
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitComplexIndicatorEqualToBridge,
        """
        variables: x, z
        ::Complex{Float64}: [z, (0.0 + 2.0im) * x] in Indicator{ACTIVATE_ON_ZERO}(EqualTo(0.0 + 4.0im))
        z in ZeroOne()
        """,
        """
        variables: x, z
        ::Float64: [z, 2.0 * x] in Indicator{ACTIVATE_ON_ZERO}(EqualTo(4.0))
        z in ZeroOne()
        """;
        constraint_start = [1.0, 0.0 + 1.2im],
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitComplexIndicatorEqualToBridge,
        """
        variables: x, z
        ::Complex{Float64}: [z, (1.0 + 0.0im) * x] in Indicator{ACTIVATE_ON_ZERO}(EqualTo(3.0 + 0.0im))
        z in ZeroOne()
        """,
        """
        variables: x, z
        ::Float64: [z, 1.0 * x] in Indicator{ACTIVATE_ON_ZERO}(EqualTo(3.0))
        z in ZeroOne()
        """;
        constraint_start = [1.0, 1.2 + 0.0im],
    )
    return
end

end  # module

TestConstraintSplitComplexIndicatorEqualTo.runtests()
