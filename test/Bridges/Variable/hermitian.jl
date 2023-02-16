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
        primal[1:3]
        zero(T)
        -primal[4]
        primal[1]
        primal[4]
        zero(T)
        primal[2:3]
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
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => [
                dual[1] / T(2)
                zero(T)
                dual[2]
                -dual[2]
                dual[3] / T(2)
                zero(T)
            ],
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
        1.0 * v11 >= 1.0
        1.0 * v12 >= 2.0
        1.0 * v22 >= 3.0
        1.0 * v14 >= 4.0
        1.0 * v11 + -1.0 * v33 == 0.0
        1.0 * v13 == 0.0
        1.0 * v12 + -1.0 * v34 == 0.0
        1.0 * v23 + 1.0 * v14 == 0.0
        1.0 * v22 + -1.0 * v44 == 0.0
        1.0 * v24 == 0.0
        [a, b, c] in PositiveSemidefiniteConeTriangle(2)
        """,
    )
    return
end

end  # module

TestVariableHermitianToSymmetricPSD.runtests()
