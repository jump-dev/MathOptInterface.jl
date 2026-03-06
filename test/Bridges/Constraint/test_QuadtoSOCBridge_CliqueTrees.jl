# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintQuadToSOCCliqueTrees

import SparseArrays
using Test

import CliqueTrees
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

function test_compute_sparse_sqrt_edge_cases()
    for A in Any[
        # Trivial Cholesky
        [1.0 0.0; 0.0 2.0],
        # Cholesky works, with pivoting
        [1.0 0.0 1.0; 0.0 1.0 1.0; 1.0 1.0 3.0],
        # Cholesky fails due to 0 eigenvalue. Pivoted Cholesky works.
        [1.0 1.0; 1.0 1.0],
        # Cholesky succeeds, even though 0 eigenvalue
        [2.0 2.0; 2.0 2.0],
        # Cholesky fails because of 0 column/row. Pivoted Cholesky works.
        [2.0 0.0; 0.0 0.0],
        # Early zero pivot - this case breaks LDLFactorizations but works
        # with CliqueTrees' pivoted Cholesky.
        [1.0 1.0 0.0; 1.0 1.0 0.0; 0.0 0.0 1.0],
    ]
        B = SparseArrays.sparse(A)
        f = zero(MOI.ScalarQuadraticFunction{eltype(A)})
        s = MOI.GreaterThan(zero(eltype(A)))
        I, J, V = MOI.Bridges.Constraint.compute_sparse_sqrt(B, f, s)
        U = zeros(eltype(A), size(A))
        for (i, j, v) in zip(I, J, V)
            U[i, j] += v
        end
        @test isapprox(A, U' * U; atol = 1e-10)
    end
    # Test failures
    for A in Any[
        [-1.0 0.0; 0.0 1.0],
        # Indefinite matrix
        [0.0 -1.0; -1.0 0.0],
        # BigFloat not supported
        BigFloat[-1.0 0.0; 0.0 1.0],
        BigFloat[1.0 0.0; 0.0 2.0],
        BigFloat[1.0 1.0; 1.0 1.0],
    ]
        B = SparseArrays.sparse(A)
        f = zero(MOI.ScalarQuadraticFunction{eltype(A)})
        s = MOI.GreaterThan(zero(eltype(A)))
        @test_throws(
            MOI.UnsupportedConstraint{typeof(f),typeof(s)},
            MOI.Bridges.Constraint.compute_sparse_sqrt(B, f, s),
        )
    end
    return
end

function test_semidefinite_cholesky_fail()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Constraint.QuadtoSOC{Float64}(inner)
    x = MOI.add_variables(model, 2)
    f = 0.5 * x[1] * x[1] + 1.0 * x[1] * x[2] + 0.5 * x[2] * x[2]
    c = MOI.add_constraint(model, f, MOI.LessThan(1.0))
    F, S = MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone
    ci = only(MOI.get(inner, MOI.ListOfConstraintIndices{F,S}()))
    g = MOI.get(inner, MOI.ConstraintFunction(), ci)
    y = MOI.get(inner, MOI.ListOfVariableIndices())
    sum_y = 1.0 * y[1] + 1.0 * y[2]
    @test isapprox(g, MOI.Utilities.vectorize([1.0, 1.0, sum_y, 0.0]))
    return
end

function test_early_zero_pivot()
    # This matrix has an early zero pivot that causes LDLFactorizations to
    # halt early, but CliqueTrees' pivoted Cholesky handles it correctly.
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Constraint.QuadtoSOC{Float64}(inner)
    x = MOI.add_variables(model, 3)
    # (x[1] + x[2])^2 + x[3]^2 = x[1]^2 + 2*x[1]*x[2] + x[2]^2 + x[3]^2
    # Q = [1 1 0; 1 1 0; 0 0 1]
    f = 0.5 * x[1] * x[1] + 1.0 * x[1] * x[2] + 0.5 * x[2] * x[2] + 0.5 * x[3] * x[3]
    c = MOI.add_constraint(model, f, MOI.LessThan(1.0))
    F, S = MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone
    ci = only(MOI.get(inner, MOI.ListOfConstraintIndices{F,S}()))
    g = MOI.get(inner, MOI.ConstraintFunction(), ci)
    # Verify the constraint was created successfully
    @test MOI.output_dimension(g) == 5  # [1, rhs, Ux...]
    return
end

end  # module

TestConstraintQuadToSOCCliqueTrees.runtests()
