# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintHermitianToComplexSymmetric

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

function test_dimension_2()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.HermitianToComplexSymmetricBridge,
        model -> begin
            a, b, c = MOI.add_variables(model, 3)
            MOI.add_constraint(
                model,
                MOI.Utilities.vectorize([1.0 * a + 2.0 * b, 3.0 * c, 4.0 * b, 5.0 * a]),
                MOI.HermitianPositiveSemidefiniteConeTriangle(2),
            )
        end,
        model -> begin
            a, b, c = MOI.add_variables(model, 3)
            MOI.add_constraint(
                model,
                MOI.Utilities.vectorize([Complex(1.0) * a + Complex(2.0) * b, Complex(3.0) * c + 5.0 * im * a, Complex(4.0) * b]),
                MOI.PositiveSemidefiniteConeTriangle(2),
            )
        end,
    )
    return
end

end  # module

TestConstraintHermitianToComplexSymmetric.runtests()
