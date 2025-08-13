# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintNumberConversion

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
        MOI.Bridges.Constraint.NumberConversionBridge,
        """
        variables: x
        ::Int: 1 * x + 2 in GreaterThan(2)
        """,
        """
        variables: x
        ::Float64: 1.0 * x + 2.0 in GreaterThan(2.0)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.NumberConversionBridge,
        """
        variables: x
        ::Int: 1 * x in EqualTo(2)
        """,
        """
        variables: x
        ::Float64: 1.0 * x in EqualTo(2.0)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.NumberConversionBridge,
        """
        variables: x
        ::Int: 1 * x in Interval(2, 3)
        """,
        """
        variables: x
        ::Float64: 1.0 * x in Interval(2.0, 3.0)
        """,
    )
    # VariableIndex
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.NumberConversionBridge,
        # We add ::String here to double check that we're making VariableIndex
        # Anything else would error
        """
        variables: x
        ::String: x in LessThan(2)
        """,
        """
        variables: x
        ::String: x in LessThan(2.0)
        """,
    )
    # ScalarAffineFunction
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.NumberConversionBridge,
        """
        variables: x
        ::Int: 1 * x in ZeroOne()
        """,
        """
        variables: x
        ::Float64: 1.0 * x in ZeroOne()
        """,
    )
    # ScalarQuadraticFunction
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.NumberConversionBridge,
        """
        variables: x
        ::Int: 1 * x * x + 2 * x in ZeroOne()
        """,
        """
        variables: x
        ::Float64: 1.0 * x * x + 2.0 * x in ZeroOne()
        """,
    )
    # VectorOfVariables
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.NumberConversionBridge,
        """
        variables: x
        ::Int: [x, x] in Zeros(2)
        """,
        """
        variables: x
        ::Float64: [x, x] in Zeros(2)
        """,
        no_bridge_used = true,
    )
    # VectorAffineFunction
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.NumberConversionBridge,
        """
        variables: x
        ::Int: [1 * x, 2 * x] in Nonnegatives(2)
        """,
        """
        variables: x
        ::Float64: [1.0 * x, 2.0 * x] in Nonnegatives(2)
        """,
    )
    # VectorQuadraticFunction
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.NumberConversionBridge,
        """
        variables: x
        ::Int: [1 * x * x, 2 * x] in Nonnegatives(2)
        """,
        """
        variables: x
        ::Float64: [1.0 * x * x, 2.0 * x] in Nonnegatives(2)
        """,
    )
    return
end

end  # module

TestConstraintNumberConversion.runtests()
