# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintGeomeanToPower

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

function test_runtests_dimension_5()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.GeoMeanToPowerBridge,
        """
        variables: t, x1, x2, x3, x4
        [t, x1, x2, x3, x4] in GeometricMeanCone(5)
        """,
        """
        variables: t, x1, x2, x3, x4
        constrainedvariable: [y1, y2] in Nonnegatives(2)
        [x1, y1, t] in PowerCone(0.25)
        [x2, y2, y1] in PowerCone(0.3333333333333333)
        [x3, x4, y2] in PowerCone(0.5)
        """,
    )
    return
end

function test_runtests_dimension_4()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.GeoMeanToPowerBridge,
        """
        variables: a, t, x1, x2, x3
        [t, x1, x2, x3] in GeometricMeanCone(4)
        [a] in Nonnegatives(1)
        """,
        """
        variables: a, t, x1, x2, x3
        constrainedvariable: [y] in Nonnegatives(1)
        [a] in Nonnegatives(1)
        [x1, y, t] in PowerCone(0.3333333333333333)
        [x2, x3, y] in PowerCone(0.5)
        """,
    )
    return
end

function test_runtests_dimension_3()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.GeoMeanToPowerBridge,
        """
        variables: a, t, x1, x2
        [t, x1, x2] in GeometricMeanCone(3)
        [a] in Nonnegatives(1)
        """,
        """
        variables: a, t, x1, x2
        [x1, x2, t] in PowerCone(0.5)
        [a] in Nonnegatives(1)
        """,
    )
    return
end

function test_runtests_dimension_2()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.GeoMeanToPowerBridge,
        """
        variables: a, t, x1
        [t, x1] in GeometricMeanCone(2)
        [a] in Nonnegatives(1)
        """,
        """
        variables: a, t, x1
        [x1, x1, t] in PowerCone(0.5)
        [a] in Nonnegatives(1)
        """,
    )
    return
end

end  # module

TestConstraintGeomeanToPower.runtests()
