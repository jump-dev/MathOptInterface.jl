# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestVariableHermitianToSymmetricPSD

using LinearAlgebra, Test

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

function test_conic_HermitianPositiveSemidefiniteConeTriangle_1()
    T = Float64
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{T}())
    bridged_mock = MOI.Bridges.Variable.HermitianToSymmetricPSD{T}(mock)
    primal = [(T(1) + √T(3)) / T(2), -T(1) / T(2), (-T(1) + √T(3)) / T(2), T(1) / T(2)]
    soc_primal = [
        primal[1] - one(T),
        √T(2) * (primal[2] + one(T)),
        primal[3] + one(T),
        √T(2) * (primal[4] - one(T)),
    ]
    t_value = norm(soc_primal)
    real_primal = [primal[1:3]; zero(T); -primal[4]; primal[1]; primal[4]; zero(T); primal[2:3]; t_value]
    dual = [(T(3) - √T(3)) / T(6), √T(3)/T(6), (T(3) + √T(3)) / T(6), -√T(3)/T(6)]
    soc_dual = [one(T), -dual[1], -dual[2]*√T(2), -dual[3], -dual[4]*√T(2)]
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            real_primal,
            (MOI.VectorAffineFunction{T}, MOI.SecondOrderCone) => [soc_dual],
            (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}) => [dual[1]/T(2); zero(T); dual[2]; -dual[2]; dual[3]/T(2); zero(T)],
        )
    MOI.Test.test_conic_HermitianPositiveSemidefiniteConeTriangle_1(
        bridged_mock,
        MOI.Test.Config(),
    )
    return
end

end  # module

TestVariableHermitianToSymmetricPSD.runtests()
