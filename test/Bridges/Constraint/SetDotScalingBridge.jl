# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintSetDotScaling

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

function test_scaling()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SetDotScalingBridge,
        """
        variables: x, y, z
        [x, 1.0 * y, z] in PositiveSemidefiniteConeTriangle(2)
        """,
        """
        variables: x, y, z
        [x, √2 * y, z] in ScaledPositiveSemidefiniteConeTriangle(2)
        """,
    )
    return
end

function test_scaling_vector_of_variables()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SetDotScalingBridge,
        """
        variables: x, y, z
        [x, y, z] in PositiveSemidefiniteConeTriangle(2)
        """,
        """
        variables: x, y, z
        [x, √2 * y, z] in ScaledPositiveSemidefiniteConeTriangle(2)
        """,
    )
    return
end

function test_scaling_quadratic()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SetDotScalingBridge,
        """
        variables: x, y, z
        [x, 1.0 * y * y + 1.0 * y + 3.0, z] in PositiveSemidefiniteConeTriangle(2)
        """,
        """
        variables: x, y, z
        [x, √2 * y * y + √2 * y + 4.242640687119286, z] in ScaledPositiveSemidefiniteConeTriangle(2)
        """,
    )
    return
end

function test_inverse_scaling()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SetDotInverseScalingBridge,
        """
        variables: x, y, z
        [x, √2 * y, z] in ScaledPositiveSemidefiniteConeTriangle(2)
        """,
        """
        variables: x, y, z
        [x, 1.0 * y, z] in PositiveSemidefiniteConeTriangle(2)
        """,
    )
    return
end

function test_inverse_scaling_vector_of_variables()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SetDotInverseScalingBridge,
        """
        variables: x, y, z
        [x, y, z] in ScaledPositiveSemidefiniteConeTriangle(2)
        """,
        """
        variables: x, y, z
        [x, 0.7071067811865475 * y, z] in PositiveSemidefiniteConeTriangle(2)
        """,
    )
    return
end

function test_inverse_scaling_quadratic()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SetDotInverseScalingBridge,
        """
        variables: x, y, z
        [x, √2 * y * y, z] in ScaledPositiveSemidefiniteConeTriangle(2)
        """,
        """
        variables: x, y, z
        [x, 1.0 * y * y, z] in PositiveSemidefiniteConeTriangle(2)
        """,
    )
    return
end

function test_scaling_complex()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SetDotScalingBridge,
        model -> begin
            x, y, z = MOI.add_variables(model, 3)
            MOI.add_constraint(
                model,
                MOI.Utilities.vectorize([(1.0 + 0im) * x, (1.0 * im) * y, (1.0 + 0im) * z]),
                MOI.PositiveSemidefiniteConeTriangle(2),
            )
        end,
        model -> begin
            x, y, z = MOI.add_variables(model, 3)
            MOI.add_constraint(
                model,
                MOI.Utilities.vectorize([(1.0 + 0im) * x, (√2 * im) * y, (1.0 + 0im) * z]),
                MOI.ScaledPositiveSemidefiniteConeTriangle(2),
            )
        end,
        eltype = ComplexF64,
        model_eltype = Float64,
        constraint_start = 1.2 * im
    )
    return
end


function test_set_dot_scaling_constraint_dual_start()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Constraint.SetDotScaling{Float64}(inner)
    x = MOI.add_variables(model, 3)
    c = MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.PositiveSemidefiniteConeTriangle(2),
    )
    MOI.set(model, MOI.ConstraintDualStart(), c, [1.0, 1.0, 1.0])
    @test MOI.get(model, MOI.ConstraintDualStart(), c) ≈ [1.0, 1.0, 1.0]
    F = MOI.VectorAffineFunction{Float64}
    S = MOI.Scaled{MOI.PositiveSemidefiniteConeTriangle}
    d = MOI.get(inner, MOI.ListOfConstraintIndices{F,S}())[1]
    @test MOI.get(inner, MOI.ConstraintDualStart(), d) ≈ [1.0, √2, 1.0]
    return
end

function test_set_dot_inverse_scaling_constraint_dual_start()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Constraint.SetDotInverseScaling{Float64}(inner)
    x = MOI.add_variables(model, 3)
    c = MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.ScaledPositiveSemidefiniteConeTriangle(2),
    )
    MOI.set(model, MOI.ConstraintDualStart(), c, [1.0, 1.0, 1.0])
    @test MOI.get(model, MOI.ConstraintDualStart(), c) ≈ [1.0, 1.0, 1.0]
    F = MOI.VectorAffineFunction{Float64}
    S = MOI.PositiveSemidefiniteConeTriangle
    d = MOI.get(inner, MOI.ListOfConstraintIndices{F,S}())[1]
    @test MOI.get(inner, MOI.ConstraintDualStart(), d) ≈ [1.0, 1 / √2, 1.0]
    return
end

end  # module

TestConstraintSetDotScaling.runtests()
