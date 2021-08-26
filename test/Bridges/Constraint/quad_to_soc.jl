module TestConstraintQuadToSOC

import LinearAlgebra
import SparseArrays
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

function test_error_for_nonconvex_quadratic_constraints()
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    bridged_mock = MOI.Bridges.Constraint.QuadtoSOC{Float64}(mock)
    x = MOI.add_variable(bridged_mock)
    @test_throws ErrorException begin
        MOI.add_constraint(
            bridged_mock,
            MOI.ScalarQuadraticFunction(
                [MOI.ScalarQuadraticTerm(1.0, x, x)],
                MOI.ScalarAffineTerm{Float64}[],
                0.0,
            ),
            MOI.GreaterThan(0.0),
        )
    end
    @test_throws ErrorException begin
        MOI.add_constraint(
            bridged_mock,
            MOI.ScalarQuadraticFunction(
                [MOI.ScalarQuadraticTerm(-1.0, x, x)],
                MOI.ScalarAffineTerm{Float64}[],
                0.0,
            ),
            MOI.LessThan(0.0),
        )
    end
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
    # for which the cholesky factorization is U' * U with U =
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
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
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
    # Test that the sparse cholesky pivot is permuted
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

end  # module

TestConstraintQuadToSOC.runtests()
