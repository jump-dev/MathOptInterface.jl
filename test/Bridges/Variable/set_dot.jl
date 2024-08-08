# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestVariableDotProducts

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

function test_psd()
    MOI.Bridges.runtests(
        MOI.Bridges.Variable.DotProductsBridge,
        model -> begin
            x, _ = MOI.add_constrained_variables(
                model,
                MOI.SetDotProducts(
                    MOI.PositiveSemidefiniteConeTriangle(2),
                    MOI.TriangleVectorization.([
                        [
                            1 2.0
                            2 3
                        ],
                        [
                            4 5.0
                            5 6
                        ],
                    ]),
                ),
            )
            MOI.add_constraint(model, 1.0x[1], MOI.EqualTo(0.0))
            MOI.add_constraint(model, 1.0x[2], MOI.LessThan(0.0))
        end,
        model -> begin
            Q, _ = MOI.add_constrained_variables(
                model,
                MOI.PositiveSemidefiniteConeTriangle(2),
            )
            MOI.add_constraint(
                model,
                1.0 * Q[1] + 4.0 * Q[2] + 3.0 * Q[3],
                MOI.EqualTo(0.0),
            )
            MOI.add_constraint(
                model,
                4.0 * Q[1] + 10.0 * Q[2] + 6.0 * Q[3],
                MOI.LessThan(0.0),
            )
        end;
        cannot_unbridge = true,
    )
    return
end

end  # module

TestVariableDotProducts.runtests()
