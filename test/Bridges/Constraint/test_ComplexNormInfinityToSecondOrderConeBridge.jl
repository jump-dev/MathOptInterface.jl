# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintComplexNormInfinityToSecondOrderCone

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

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ComplexNormInfinityToSecondOrderConeBridge,
        """
        variables: t, x
        ::Complex{Float64}: [t, (1 + 2im) * x + (3 + 4im)] in NormInfinityCone(2)
        """,
        """
        variables: t, x
        ::Float64: [t, 1 * x + 3, 2 * x + 4] in SecondOrderCone(3)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.ComplexNormInfinityToSecondOrderConeBridge,
        """
        variables: t, x, y
        ::Complex{Float64}: [2.0 * t + 3.0, x + im * y] in NormInfinityCone(2)
        """,
        """
        variables: t, x, y
        ::Float64: [2.0 * t + 3.0, 1.0 * x, 1.0 * y] in SecondOrderCone(3)
        """,
    )
    return
end

function test_imag_t()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model =
        MOI.Bridges.Constraint.ComplexNormInfinityToSecondOrderCone{Float64}(
            inner,
        )
    x = MOI.add_variables(model, 2)
    f_t = (1.0 + 2.0im) * x[1]
    f_x = (1.0 + 2.0im) * x[2] + (3.0 + 4.0im)
    f = MOI.Utilities.operate(vcat, Complex{Float64}, f_t, f_x)
    @test_throws(
        MOI.AddConstraintNotAllowed{typeof(f),MOI.NormInfinityCone}(
            "The epigraph variable `t` in `[t; x] in NormInfinityCone()` " *
            "must be real. It is: $f_t",
        ),
        MOI.add_constraint(model, f, MOI.NormInfinityCone(2))
    )
    return
end

end  # module

TestConstraintComplexNormInfinityToSecondOrderCone.runtests()
