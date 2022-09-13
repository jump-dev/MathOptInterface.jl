# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestFeasibilityRelaxation

using Test
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

function _test_roundtrip(src_str, relaxed_str)
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, src_str)
    MOI.set(model, MOI.Utilities.FeasibilityRelaxation())
    dest = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(dest, relaxed_str)
    MOI.Bridges._test_structural_identical(model, dest)
    return
end

function test_relax_bounds()
    _test_roundtrip(
        """
        variables: x, y
        minobjective: x + y
        x >= 0.0
        y <= 0.0
        x in ZeroOne()
        y in Integer()
        """,
        """
        variables: x, y
        minobjective: x + y
        x >= 0.0
        y <= 0.0
        x in ZeroOne()
        y in Integer()
        """,
    )
    return
end

function test_relax_affine_lessthan()
    _test_roundtrip(
        """
        variables: x, y
        minobjective: x + y
        c1: x + y <= 1.0
        """,
        """
        variables: x, y, a
        minobjective: x + y + a
        c1: x + y + -1.0 * a <= 1.0
        a >= 0.0
        """,
    )
    return
end

function test_relax_affine_lessthan_max()
    _test_roundtrip(
        """
        variables: x, y
        maxobjective: x + y
        c1: x + y <= 1.0
        """,
        """
        variables: x, y, a
        maxobjective: x + y + -1.0 * a
        c1: x + y + -1.0 * a <= 1.0
        a >= 0.0
        """,
    )
    return
end

function test_relax_affine_lessthan_no_objective()
    _test_roundtrip(
        """
        variables: x, y
        c1: x + y <= 1.0
        """,
        """
        variables: x, y, a
        minobjective: 1.0 * a
        c1: x + y + -1.0 * a <= 1.0
        a >= 0.0
        """,
    )
    return
end

function test_relax_affine_lessthan_quad_objective()
    _test_roundtrip(
        """
        variables: x, y
        maxobjective: 1.0 * x * y
        c1: x + y <= 1.0
        """,
        """
        variables: x, y, a
        maxobjective: 1.0 * x * y + -1.0 * a
        c1: x + y + -1.0 * a <= 1.0
        a >= 0.0
        """,
    )
    return
end

function test_relax_affine_greaterthan()
    _test_roundtrip(
        """
        variables: x, y
        minobjective: x + y
        c1: x + y >= 1.0
        """,
        """
        variables: x, y, a
        minobjective: x + y + a
        c1: x + y + 1.0 * a >= 1.0
        a >= 0.0
        """,
    )
    return
end

function test_relax_affine_equalto()
    _test_roundtrip(
        """
        variables: x, y
        minobjective: x + y
        c1: x + y == 1.0
        """,
        """
        variables: x, y, a, b
        minobjective: x + y + a + b
        c1: x + y + 1.0 * a + -1.0 * b == 1.0
        a >= 0.0
        b >= 0.0
        """,
    )
    return
end

function test_relax_affine_interval()
    _test_roundtrip(
        """
        variables: x, y
        minobjective: x + y
        c1: x + y in Interval(5.0, 6.0)
        """,
        """
        variables: x, y, a, b
        minobjective: x + y + a + b
        c1: x + y + 1.0 * a + -1.0 * b in Interval(5.0, 6.0)
        a >= 0.0
        b >= 0.0
        """,
    )
    return
end

function test_relax_quadratic_lessthan()
    _test_roundtrip(
        """
        variables: x, y
        maxobjective: x + y
        c1: 1.0 * x * x + 2.0 * x * y <= 1.0
        """,
        """
        variables: x, y, a
        maxobjective: x + y + -1.0 * a
        c1: 1.0 * x * x + 2.0 * x * y + -1.0 * a <= 1.0
        a >= 0.0
        """,
    )
    return
end

function test_relax_quadratic_greaterthanthan()
    _test_roundtrip(
        """
        variables: x, y
        maxobjective: x + y
        c1: 1.0 * x * x + 2.0 * x * y >= 1.0
        """,
        """
        variables: x, y, a
        maxobjective: x + y + -1.0 * a
        c1: 1.0 * x * x + 2.0 * x * y + 1.0 * a >= 1.0
        a >= 0.0
        """,
    )
    return
end

function test_penalties()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, 1.0 * x, MOI.EqualTo(2.0))
    MOI.set(model, MOI.Utilities.FeasibilityRelaxation(Dict(c => 2.0)))
    @test sprint(print, model) === """
    Minimize ScalarAffineFunction{Float64}:
     0.0 + 2.0 v[2] + 2.0 v[3]

    Subject to:

    ScalarAffineFunction{Float64}-in-EqualTo{Float64}
     0.0 + 1.0 v[1] + 1.0 v[2] - 1.0 v[3] == 2.0

    VariableIndex-in-GreaterThan{Float64}
     v[2] >= 0.0
     v[3] >= 0.0
    """
    return
end

end

TestFeasibilityRelaxation.runtests()
