# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintSOS1ToMILP

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

function test_runtests_VectorOfVariables()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SOS1ToMILPBridge,
        """
        variables: x, y
        [x, y] in SOS1([1.0, 2.0])
        x in Interval(0.0, 2.0)
        y >= -1.0
        y <= 3.0
        """,
        """
        variables: x, y, z1, z2
        1.0 * z1 + 1.0 * z2 == 1.0
        x in Interval(0.0, 2.0)
        y >= -1.0
        y <= 3.0
        -1.0 * x <= 0.0
        1.0 * x + -2.0 * z1 <= 0.0
        -1.0 * y + -1.0 * z2 <= 0.0
        1.0 * y + -3.0 * z2 <= 0.0
        z1 in ZeroOne()
        z2 in ZeroOne()
        """,
    )
    return
end

function test_runtests_VectorAffineFunction()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SOS1ToMILPBridge,
        """
        variables: x, y
        [1.0 * x + 2.0, y] in SOS1([1.0, 2.0])
        x in ZeroOne()
        y in ZeroOne()
        """,
        """
        variables: x, y, z1, z2
        1.0 * z1 + 1.0 * z2 == 1.0
        -1.0 * x + 2.0 * z1 <= 2.0
        1.0 * x + -3.0 * z1 <= -2.0
        -1.0 * y <= 0.0
        1.0 * y + -1.0 * z2 <= 0.0
        x in ZeroOne()
        y in ZeroOne()
        z1 in ZeroOne()
        z2 in ZeroOne()
        """,
    )
    return
end

function test_resolve_with_modified()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.SOS1ToMILP{Int}(inner)
    x = MOI.add_variables(model, 3)
    c = MOI.add_constraint.(model, x, MOI.Interval(0, 2))
    MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.SOS1([1, 2, 3]))
    @test MOI.get(inner, MOI.NumberOfVariables()) == 3
    MOI.Bridges.final_touch(model)
    @test MOI.get(inner, MOI.NumberOfVariables()) == 6
    MOI.set(model, MOI.ConstraintSet(), c[3], MOI.Interval(0, 1))
    MOI.Bridges.final_touch(model)
    @test MOI.get(inner, MOI.NumberOfVariables()) == 6
    return
end

function test_runtests_error_variable()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.SOS1ToMILP{Int}(inner)
    x = MOI.add_variables(model, 3)
    MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.SOS1([1, 2, 3]))
    @test_throws(
        ErrorException(
            "Unable to use SOS1ToMILPBridge because element 1 in " *
            "the function has a non-finite domain: $(x[1])",
        ),
        MOI.Bridges.final_touch(model),
    )
    return
end

function test_runtests_error_affine()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.SOS1ToMILP{Int}(inner)
    x = MOI.add_variables(model, 2)
    f = MOI.Utilities.operate(vcat, Int, 2, 1 * x[1], x[2])
    MOI.add_constraint(model, f, MOI.SOS1([1, 2, 3]))
    @test_throws(
        ErrorException(
            "Unable to use SOS1ToMILPBridge because element 2 in " *
            "the function has a non-finite domain: $(1 * x[1])",
        ),
        MOI.Bridges.final_touch(model),
    )
    return
end

function test_delete_before_final_touch()
    model = MOI.Bridges.Constraint.SOS1ToMILP{Float64}(
        MOI.Utilities.Model{Float64}(),
    )
    x = MOI.add_variables(model, 2)
    c = MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.SOS1([1.0, 2.0]),
    )
    MOI.delete(model, c)
    @test !MOI.is_valid(model, c)
    return
end

end  # module

TestConstraintSOS1ToMILP.runtests()
