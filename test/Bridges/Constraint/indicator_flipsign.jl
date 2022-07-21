# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintIndicatorFlipSign

using Test

using MathOptInterface
const MOI = MathOptInterface

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
        MOI.Bridges.Constraint.IndicatorFlipSignBridge,
        """
        variables: x, z
        [z, 2.0 * x] in Indicator{ACTIVATE_ON_ONE}(LessThan(2.0))
        z in ZeroOne()
        """,
        """
        variables: x, z
        [z, -2.0 * x] in Indicator{ACTIVATE_ON_ONE}(GreaterThan(-2.0))
        z in ZeroOne()
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.IndicatorFlipSignBridge,
        """
        variables: x, z
        [z, 2.0 * x] in Indicator{ACTIVATE_ON_ONE}(GreaterThan(2.0))
        z in ZeroOne()
        """,
        """
        variables: x, z
        [z, -2.0 * x] in Indicator{ACTIVATE_ON_ONE}(LessThan(-2.0))
        z in ZeroOne()
        """,
    )
    return
end

end  # module

TestConstraintIndicatorFlipSign.runtests()
