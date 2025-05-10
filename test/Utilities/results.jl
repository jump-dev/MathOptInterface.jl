# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestResults

using Test

import MathOptInterface as MOI

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name) $T" for T in [Int, Float64]
                getfield(@__MODULE__, name)(T)
            end
        end
    end
    return
end

function test_hyperrectangle(T)
    model = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}()),
        T,
    )
    x = MOI.add_variables(model, 2)
    c1 = MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.HyperRectangle(T[3, -7], T[5, -2]),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.Utilities.vectorize(x .+ T[11, 13]),
        MOI.HyperRectangle(T[-T(6), -T(4)], [T(3), T(2)]),
    )
    MOI.set(model, MOI.ConstraintDual(), c1, T[4, -3])
    MOI.set(model, MOI.ConstraintDual(), c2, T[-2, 5])
    @test -53 == @inferred MOI.Utilities.get_fallback(
        model,
        MOI.DualObjectiveValue(),
        T,
    )
end

end

TestResults.runtests()
