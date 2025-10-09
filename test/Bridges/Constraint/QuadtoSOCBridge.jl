# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintQuadToSOC

import LinearAlgebra
import SparseArrays
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

include("../utilities.jl")

function test_error_for_nonconvex_quadratic_constraints()
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    bridged_mock = MOI.Bridges.Constraint.QuadtoSOC{Float64}(mock)
    x = MOI.add_variable(bridged_mock)
    @test_throws(
        MOI.UnsupportedConstraint,
        MOI.add_constraint(
            bridged_mock,
            MOI.ScalarQuadraticFunction(
                [MOI.ScalarQuadraticTerm(1.0, x, x)],
                MOI.ScalarAffineTerm{Float64}[],
                0.0,
            ),
            MOI.GreaterThan(0.0),
        )
    )
    @test_throws(
        MOI.UnsupportedConstraint,
        MOI.add_constraint(
            bridged_mock,
            MOI.ScalarQuadraticFunction(
                [MOI.ScalarQuadraticTerm(-1.0, x, x)],
                MOI.ScalarAffineTerm{Float64}[],
                0.0,
            ),
            MOI.LessThan(0.0),
        )
    )
    return
end

function test_quadratic_constraints_with_2_variables()
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.QuadtoSOC{Float64}(mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.5, 0.5]),
        ),
    )
    MOI.Test.test_constraint_qcp_duplicate_diagonal(bridged_mock, config)
    MOI.empty!(bridged_mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.5, (√13 - 1) / 4]),
        ),
    )
    MOI.Test.test_constraint_qcp_duplicate_off_diagonal(bridged_mock, config)
    ci = first(
        MOI.get(
            mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.RotatedSecondOrderCone,
            }(),
        ),
    )
    x, y = MOI.get(mock, MOI.ListOfVariableIndices())
    # The matrix is
    # 2 1
    # 1 2
    # for which the Cholesky factorization is U' * U with U =
    # √2 √2/2
    #  . √3/√2
    expected = MOI.VectorAffineFunction{Float64}(
        MOI.VectorAffineTerm.(
            [3, 3, 4],
            MOI.ScalarAffineTerm.([√2, √2 / 2, √3 / √2], [x, y, y]),
        ),
        [1.0, 1.0, 0.0, 0.0],
    )
    @test MOI.get(mock, MOI.ConstraintFunction(), ci) ≈ expected
end

function test_qcp()
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    mock = MOI.Utilities.MockOptimizer(model)
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.QuadtoSOC{Float64}(mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1 / 2, 7 / 4],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                [zeros(2)],
            (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone) => [[0.25, 1.0, -1 / √2]],
        ),
    )
    MOI.Test.test_quadratic_constraint_integration(bridged_mock, config)
    MOI.empty!(bridged_mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [√2],
            (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone) => [[1 / √2, 1 / (2 * √2), -1 / √2]],
        ),
    )
    MOI.Test.test_quadratic_constraint_basic(bridged_mock, config)
    MOI.empty!(bridged_mock)
    MOI.Test.test_quadratic_constraint_minimize(bridged_mock, config)
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarQuadraticFunction{Float64},
                MOI.LessThan{Float64},
            }(),
        ),
    )
    _test_delete_bridge(
        bridged_mock,
        ci,
        1,
        ((MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0),),
    )
    MOI.empty!(bridged_mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1.0, 1.0],
            (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone) => [[1.0, 1 / 3, -1 / √2, -1 / √6]],
        ),
    )
    MOI.Test.test_quadratic_constraint_LessThan(bridged_mock, config)
    MOI.empty!(bridged_mock)
    MOI.Test.test_quadratic_constraint_GreaterThan(bridged_mock, config)
    F = MOI.ScalarQuadraticFunction{Float64}
    S = MOI.GreaterThan{Float64}
    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{F,S}()))
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        msg = "In order to set the `$attr`, the `MOI.Bridges.Constraint.QuadtoSOCBridge` needs to get the `MathOptInterface.VariablePrimalStart()` but it is not set. Set the `MathOptInterface.VariablePrimalStart()` first before setting the `$attr` in order to fix this."
        err = MOI.SetAttributeNotAllowed(attr, msg)
        @test_throws err MOI.set(bridged_mock, attr, ci, 0.0)
    end
    return
end

function test_fill_reducing_permutation()
    model = MOI.Utilities.Model{Float64}()
    bridge = MOI.Bridges.Constraint.QuadtoSOC{Float64}(model)
    x = MOI.add_variables(bridge, 3)
    Q = Float64[2 1 1; 1 2 0; 1 0 2]
    f = 0.5 * x' * Q * x
    MOI.add_constraint(bridge, f, MOI.LessThan(2.0))
    indices = MOI.get(
        model,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.RotatedSecondOrderCone,
        }(),
    )
    F = LinearAlgebra.cholesky(Q)
    # Test that the sparse Cholesky pivot is permuted
    SF = LinearAlgebra.cholesky(SparseArrays.sparse(Q))
    @test SF.p == [3, 2, 1]
    U = Matrix(F.U)
    @test MOI.get(model, MOI.ConstraintFunction(), indices[1]) ≈
          MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(U[1, 1], x[1])),
            MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(U[1, 2], x[2])),
            MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(U[1, 3], x[3])),
            MOI.VectorAffineTerm(4, MOI.ScalarAffineTerm(U[2, 2], x[2])),
            MOI.VectorAffineTerm(4, MOI.ScalarAffineTerm(U[2, 3], x[3])),
            MOI.VectorAffineTerm(5, MOI.ScalarAffineTerm(U[3, 3], x[3])),
        ],
        [1.0, 2.0, 0.0, 0.0, 0.0],
    )
    return
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.QuadtoSOCBridge,
        """
        variables: x, y
        2.0 * x * x + 2.0 * y * y <= 0.0
        """,
        """
        variables: x, y
        [1.0, 0.0, 2.0 * x, 2.0 * y] in RotatedSecondOrderCone(4)
        """,
        variable_start = 0.0,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.QuadtoSOCBridge,
        """
        variables: x, y
        -2.0 * x * x + -2.0 * y * y >= 0.0
        """,
        """
        variables: x, y
        [1.0, 0.0, 2.0 * x, 2.0 * y] in RotatedSecondOrderCone(4)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.QuadtoSOCBridge,
        """
        variables: x, y
        2.0 * x * x + 0.5 * y * y + 2.0 * x <= 3.0
        """,
        """
        variables: x, y
        [1.0, -2.0 * x + 3.0, 2.0 * x, 1.0 * y] in RotatedSecondOrderCone(4)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.QuadtoSOCBridge,
        """
        variables: x, y, z
        -2.0 * x * x + -1.0 * y * y + -2.0 * x * y + 3.0 * z >= 4.0
        """,
        """
        variables: x, y, z
        [1.0, 3.0 * z + -4.0, 2.0 * x + 1.0 * y, 1.0 * y] in RotatedSecondOrderCone(4)
        """,
    )
    return
end

"""
    test_copy_to_start()

Tests that `copy_to` copies the variable starting values first and the
constraint primal and dual starting values second as is needed by the
[`MOI.Bridges.Constraint.QuadtoSOCBridge`](@ref).
"""
function test_copy_to_start()
    src = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x = MOI.add_variable(src)
    c = MOI.add_constraint(src, 1.0x * x + 2.0x, MOI.LessThan(-1.0))
    MOI.set(src, MOI.ConstraintPrimalStart(), c, 1.0)
    MOI.set(src, MOI.ConstraintDualStart(), c, -1.0)
    MOI.set(src, MOI.VariablePrimalStart(), x, -1.0)

    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    dest = MOI.Bridges.Constraint.QuadtoSOC{Float64}(model)
    index_map = MOI.copy_to(dest, src)
    @test MOI.get(dest, MOI.ConstraintPrimalStart(), index_map[c]) == 1.0
    @test MOI.get(dest, MOI.ConstraintDualStart(), index_map[c]) == -1.0
    @test MOI.get(dest, MOI.VariablePrimalStart(), index_map[x]) == -1.0
end

function test_deletion_of_variable_in_bridged_constraint()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Constraint.QuadtoSOC{Float64}(inner)
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.VariableName(), x, ["x", "y"])
    f = 1.0 * x[1] * x[1] + 1.0 * x[2] * x[2]
    c = MOI.add_constraint(model, f, MOI.LessThan(1.0))
    MOI.delete(model, x[1])
    @test MOI.get(model, MOI.ConstraintFunction(), c) ≈ 1.0 * x[2] * x[2]
    return
end

MOI.Utilities.@model(
    Model2153,
    (),
    (MOI.EqualTo,),
    (MOI.RotatedSecondOrderCone,),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (MOI.VectorOfVariables,),
    (),
)

function MOI.supports(
    ::Model2153{T},
    ::MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}},
) where {T}
    return false
end

function test_deletion_of_variable_in_bridged_slacked_objective()
    model = MOI.Bridges.full_bridge_optimizer(Model2153{Float64}(), Float64)
    MOI.Bridges.add_bridge(
        model,
        MOI.Bridges.Constraint.QuadtoSOCBridge{Float64},
    )
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.VariableName(), x, ["x", "y"])
    f = 1.0 * x[1] * x[1] + 1.0 * x[2] * x[2]
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.delete(model, x[1])
    @test MOI.get(model, MOI.ObjectiveFunction{typeof(f)}()) ≈ 1.0 * x[2] * x[2]
    return
end

function test_constraint_primal_no_quad_terms()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Constraint.QuadtoSOC{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarQuadraticFunction(
        MOI.ScalarQuadraticTerm{Float64}[],
        [MOI.ScalarAffineTerm(1.0, x)],
        0.0,
    )
    c = MOI.add_constraint(model, f, MOI.LessThan(1.0))
    MOI.set(model, MOI.ConstraintPrimalStart(), c, 1.0)
    @test MOI.get(model, MOI.ConstraintPrimalStart(), c) == 1.0
    return
end

end  # module

TestConstraintQuadToSOC.runtests()
