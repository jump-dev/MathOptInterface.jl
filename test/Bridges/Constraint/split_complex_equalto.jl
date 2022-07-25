# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintSplitComplexEqualTo

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

function test_complex_zeros()
    T = Float64
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}()),
    )
    _bridged_mock = MOI.Bridges.Constraint.SplitComplexEqualTo{T}(mock)
    bridged_mock = MOI.Bridges.Constraint.Scalarize{Complex{T}}(_bridged_mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [2 / 3, 1 / 3],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => zeros(T, 2),
        ),
    )
    config = MOI.Test.Config()
    MOI.empty!(bridged_mock)
    MOI.Test.test_linear_complex_Zeros(bridged_mock, config)
    cis = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Complex{T}},
            MOI.EqualTo,
        }(),
    )
    @test length(cis) == 0
    cis = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{T},
            MOI.EqualTo{T},
        }(),
    )
    @test length(cis) == 2

    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [2],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => zeros(T, 1),
        ),
    )
    MOI.empty!(bridged_mock)
    MOI.Test.test_linear_complex_Zeros_duplicate(bridged_mock, config)
    cis = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{T},
            MOI.EqualTo{T},
        }(),
    )
    @test length(cis) == 1
    return
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitComplexEqualToBridge,
        """
        variables: x
        ::Complex{Float64}: (1.0 + 2.0im) * x == (3.0 + 4.0im)
        """,
        """
        variables: x
        ::Float64: 1.0 * x == 3.0
        ::Float64: 2.0 * x == 4.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitComplexEqualToBridge,
        """
        variables: x
        ::Complex{Float64}: (0.0 + 2.0im) * x == (0.0 + 4.0im)
        """,
        """
        variables: x
        ::Float64: 2.0 * x == 4.0
        """;
        constraint_start = 0.0 + 1.2im,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitComplexEqualToBridge,
        """
        variables: x
        ::Complex{Float64}: (2.0 + 0.0im) * x == (4.0 + 0.0im)
        """,
        """
        variables: x
        ::Float64: 2.0 * x == 4.0
        """;
        constraint_start = 1.2 + 0.0im,
    )
    return
end

end  # module

TestConstraintSplitComplexEqualTo.runtests()
