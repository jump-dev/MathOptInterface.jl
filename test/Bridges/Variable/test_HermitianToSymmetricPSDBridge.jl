# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestVariableHermitianToSymmetricPSD

using Test

import MathOptInterface as MOI

import LinearAlgebra

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

function test_conic_HermitianPositiveSemidefiniteConeTriangle_1()
    T = Float64
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{T}())
    bridged_mock = MOI.Bridges.Variable.HermitianToSymmetricPSD{T}(mock)
    primal = [
        (T(1) + √T(3)) / T(2),
        -T(1) / T(2),
        (-T(1) + √T(3)) / T(2),
        T(1) / T(2),
    ]
    soc_primal = [
        primal[1] - one(T),
        √T(2) * (primal[2] + one(T)),
        primal[3] + one(T),
        √T(2) * (primal[4] - one(T)),
    ]
    t_value = LinearAlgebra.norm(soc_primal)
    real_primal = [
        primal[1:3] / 2
        zero(T)
        -primal[4] / 2
        primal[1] / 2
        primal[4] / 2
        zero(T)
        primal[2:3] / 2
        t_value
    ]
    dual = [
        (T(3) - √T(3)) / T(6),
        √T(3) / T(6),
        (T(3) + √T(3)) / T(6),
        -√T(3) / T(6),
    ]
    soc_dual = [one(T), -dual[1], -dual[2] * √T(2), -dual[3], -dual[4] * √T(2)]
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            real_primal,
            (MOI.VectorAffineFunction{T}, MOI.SecondOrderCone) =>
                [soc_dual],
        )
    MOI.empty!(bridged_mock)
    MOI.Test.test_conic_HermitianPositiveSemidefiniteConeTriangle_1(
        bridged_mock,
        MOI.Test.Config(),
    )
    return
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Variable.HermitianToSymmetricPSDBridge,
        """
        variables: a, b, c
        constrainedvariable: [r11, r12, r22, c12] in HermitianPositiveSemidefiniteConeTriangle(2)
        1.0 * r11 >= 1.0
        1.0 * r12 >= 2.0
        1.0 * r22 >= 3.0
        1.0 * c12 >= 4.0
        [a, b, c] in PositiveSemidefiniteConeTriangle(2)
        """,
        """
        variables: a, b, c
        constrainedvariable: [v11, v12, v22, v13, v23, v33, v14, v24, v34, v44] in PositiveSemidefiniteConeTriangle(4)
        1.0 * v11 + 1.0 * v33 >= 1.0
        1.0 * v12 + 1.0 * v34 >= 2.0
        1.0 * v22 + 1.0 * v44 >= 3.0
        1.0 * v14 + -1.0 * v23 >= 4.0
        [a, b, c] in PositiveSemidefiniteConeTriangle(2)
        """,
    )
    return
end

function test_delete()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Variable.HermitianToSymmetricPSD{Float64}(inner)
    set = MOI.HermitianPositiveSemidefiniteConeTriangle(2)
    x, _ = MOI.add_constrained_variables(model, set)
    MOI.delete(model, x)
    @test MOI.is_empty(inner)
    return
end

function test_set_variable_primal_start()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Variable.HermitianToSymmetricPSD{Float64}(inner)
    for n in 1:4
        MOI.empty!(model)
        set = MOI.HermitianPositiveSemidefiniteConeTriangle(n)
        x, _ = MOI.add_constrained_variables(model, set)
        attr = MOI.VariablePrimalStart()
        @test MOI.get(model, attr, x[1]) === nothing
        MOI.set(model, attr, x[1], 1.0)
        @test MOI.get(model, attr, x[1]) == 1.0
        MOI.set(model, attr, x[1], nothing)
        @test MOI.get(model, attr, x[1]) === nothing
        v = ComplexF64[complex(i, n - i) for i in 1:n]
        H = v * v'
        h = vcat(
            [real(H[i, j]) for j in 1:n for i in 1:j],
            [imag(H[i, j]) for j in 1:n for i in 1:(j-1)],
        )
        MOI.set(model, attr, x, h)
        @test MOI.get(model, attr, x) ≈ h
        y = MOI.get(inner, MOI.ListOfVariableIndices())
        r = MOI.get(inner, attr, y)
        R = [r[MOI.Utilities.trimap(i, j)] for i in 1:2n, j in 1:2n]
        @test R ≈ [real(H) imag(H); -imag(H) real(H)] / 2
        @test minimum(LinearAlgebra.eigvals(LinearAlgebra.Symmetric(R))) >=
              -1e-12
        MOI.set(model, attr, x, fill(nothing, length(x)))
        @test all(isnothing, MOI.get(model, attr, x))
        @test all(isnothing, MOI.get(inner, attr, y))
    end
    return
end

function test_unstructured_primal()
    for n in 1:4
        mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
        model = MOI.Bridges.Variable.HermitianToSymmetricPSD{Float64}(mock)
        x, ci = MOI.add_constrained_variables(
            model,
            MOI.HermitianPositiveSemidefiniteConeTriangle(n),
        )
        v = collect(1.0:2n)
        R = v * v' + LinearAlgebra.Diagonal(v)
        H =
            R[1:n, 1:n] +
            R[(n+1):2n, (n+1):2n] +
            im * (R[1:n, (n+1):2n] - R[(n+1):2n, 1:n])
        h = vcat(
            [real(H[i, j]) for j in 1:n for i in 1:j],
            [imag(H[i, j]) for j in 1:n for i in 1:(j-1)],
        )
        r = [R[i, j] for j in 1:2n for i in 1:j]
        MOI.Utilities.set_mock_optimize!(
            mock,
            mock -> MOI.Utilities.mock_optimize!(mock, r),
        )
        MOI.optimize!(model)
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ h
        @test MOI.get(model, MOI.ConstraintPrimal(), ci) ≈ h
        @test MOI.get(mock, MOI.NumberOfVariables()) == n * (2n + 1)
        @test MOI.get(mock, MOI.ListOfConstraintTypesPresent()) ==
              [(MOI.VectorOfVariables, MOI.PositiveSemidefiniteConeTriangle)]
    end
    return
end

function test_dimension_3()
    MOI.Bridges.runtests(
        MOI.Bridges.Variable.HermitianToSymmetricPSDBridge,
        """
        constrainedvariable: [x11, x12, x22, x13, x23, x33, y12, y13, y23] in HermitianPositiveSemidefiniteConeTriangle(3)
        minobjective: 1.0 * x11 + 2.0 * x12 + 3.0 * x22 + 4.0 * x13 + 5.0 * x23 + 6.0 * x33 + 7.0 * y12 + 8.0 * y13 + 9.0 * y23
        """,
        """
        constrainedvariable: [r11, r12, r22, r13, r23, r33, r14, r24, r34, r44, r15, r25, r35, r45, r55, r16, r26, r36, r46, r56, r66] in PositiveSemidefiniteConeTriangle(6)
        minobjective: 1.0 * r11 + 2.0 * r12 + 3.0 * r22 + 4.0 * r13 + 5.0 * r23 + 6.0 * r33 + 1.0 * r44 + 2.0 * r45 + 3.0 * r55 + 4.0 * r46 + 5.0 * r56 + 6.0 * r66 + 7.0 * r15 + -7.0 * r24 + 8.0 * r16 + -8.0 * r34 + 9.0 * r26 + -9.0 * r35
        """,
    )
    return
end

end  # module

TestVariableHermitianToSymmetricPSD.runtests()
