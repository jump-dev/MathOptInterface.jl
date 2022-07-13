# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintSplitComplexZeros

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
    bridged_mock = MOI.Bridges.Constraint.SplitComplexZeros{T}(mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [2 / 3, 1 / 3],
            (MOI.VectorAffineFunction{T}, MOI.Zeros) => [zeros(T, 2)],
        ),
    )
    config = MOI.Test.Config()
    MOI.Test.test_linear_complex_Zeros(bridged_mock, config)
    cis = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Complex{T}},
            MOI.Zeros,
        }(),
    )
    @test length(cis) == 0
    cis = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{T},MOI.Zeros}(),
    )
    @test length(cis) == 1

    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [2],
            (MOI.VectorAffineFunction{T}, MOI.Zeros) => [zeros(T, 1)],
        ),
    )
    MOI.Test.test_linear_complex_Zeros_duplicate(bridged_mock, config)
    cis = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{T},MOI.Zeros}(),
    )
    @test length(cis) == 1
    return
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitComplexZerosBridge,
        """
        variables: x
        ::Complex{Float64}: [(1 + 2im) * x + (3 + 4im)] in Zeros(1)
        """,
        """
        variables: x
        ::Float64: [1 * x + 3, 2 * x + 4] in Zeros(2)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitComplexZerosBridge,
        """
        variables: x
        ::Complex{Float64}: [(0 + 2im) * x + (0 + 4im)] in Zeros(1)
        """,
        """
        variables: x
        ::Float64: [2 * x + 4] in Zeros(1)
        """;
        # The default start vectors that we test aren't feasible, because the
        # 0 in Zeros(1) part of the real constraint should have a dual of 0.
        exclude = [MOI.ConstraintDualStart(), MOI.ConstraintPrimalStart()],
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitComplexZerosBridge,
        """
        variables: x
        ::Complex{Float64}: [(2 + 0im) * x + (4 + 0im)] in Zeros(1)
        """,
        """
        variables: x
        ::Float64: [2 * x + 4] in Zeros(1)
        """;
        # The default start vectors that we test aren't feasible, because the
        # 0 in Zeros(1) part of the real constraint should have a dual of 0.
        exclude = [MOI.ConstraintDualStart(), MOI.ConstraintPrimalStart()],
    )
    return
end

end  # module

TestConstraintSplitComplexZeros.runtests()
