# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintSquare

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

include("../utilities.jl")

function test_Square()
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.Square{Float64}(mock)
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_VectorOfVariables_PositiveSemidefiniteConeSquare",
            "test_basic_VectorAffineFunction_PositiveSemidefiniteConeSquare",
            "test_basic_VectorQuadraticFunction_PositiveSemidefiniteConeSquare",
        ],
    )
    MOI.empty!(bridged_mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            ones(4),
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [2, 2],
        )
    MOI.Test.test_conic_PositiveSemidefiniteConeSquare_VectorOfVariables(
        bridged_mock,
        config,
    )
    MOI.empty!(bridged_mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            ones(4),
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [[1, -1, 1]],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [2, 2],
        )
    MOI.Test.test_conic_PositiveSemidefiniteConeSquare_VectorAffineFunction(
        bridged_mock,
        config,
    )
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeSquare,
            }(),
        ),
    )
    _test_delete_bridge(
        bridged_mock,
        ci,
        4,
        (
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
                0,
            ),
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}, 1),
        ),
    )
    return
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SquareBridge,
        """
        variables: x11, x21, x12, x22
        [x11, x21, x12, x22] in PositiveSemidefiniteConeSquare(2)
        """,
        """
        variables: x11, x21, x12, x22
        [x11, x12, x22] in PositiveSemidefiniteConeTriangle(2)
        x12 + -1.0 * x21 == 0.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SquareBridge,
        """
        variables: x11, x12, x22
        [x11, x12, x12, x22] in PositiveSemidefiniteConeSquare(2)
        """,
        """
        variables: x11, x12, x22
        [x11, x12, x22] in PositiveSemidefiniteConeTriangle(2)
        """,
    )
    return
end

end  # module

TestConstraintSquare.runtests()
