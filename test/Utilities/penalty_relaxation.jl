# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestPenaltyRelaxation

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

function _test_roundtrip(src_str, relaxed_str)
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, src_str)
    map = MOI.modify(model, MOI.Utilities.PenaltyRelaxation())
    for (c, v) in map
        @test v isa MOI.ScalarAffineFunction{Float64}
    end
    dest = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(dest, relaxed_str)
    MOI.Bridges._test_structural_identical(model, dest)
    return
end

function test_relax_bounds()
    src_str = """
    variables: x, y
    minobjective: x + y
    x >= 0.0
    y <= 0.0
    x in ZeroOne()
    y in Integer()
    """
    relaxed_str = """
    variables: x, y
    minobjective: x + y
    x >= 0.0
    y <= 0.0
    x in ZeroOne()
    y in Integer()
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, src_str)
    @test_logs(
        (:warn,),
        (:warn,),
        (:warn,),
        (:warn,),
        MOI.modify(model, MOI.Utilities.PenaltyRelaxation()),
    )
    dest = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(dest, relaxed_str)
    MOI.Bridges._test_structural_identical(model, dest)
    return
end

function test_relax_no_warn()
    input = """
    variables: x, y
    minobjective: x + y
    x >= 0.0
    y <= 0.0
    x in ZeroOne()
    y in Integer()
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, input)
    relaxation = MOI.Utilities.PenaltyRelaxation(; warn = false)
    @test_logs MOI.modify(model, relaxation)
    dest = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(dest, input)
    MOI.Bridges._test_structural_identical(model, dest)
    return
end

function test_relax_variable_index_objective()
    _test_roundtrip(
        """
        variables: x, y
        minobjective: x
        c1: x + y <= 1.0
        """,
        """
        variables: x, y, a
        minobjective: 1.0 * x + 1.0 * a
        c1: x + y + -1.0 * a <= 1.0
        a >= 0.0
        """,
    )
    return
end

function test_relax_scalar_nonlinear_objective()
    _test_roundtrip(
        """
        variables: x, y
        minobjective: ScalarNonlinearFunction(exp(x))
        c1: x + y <= 1.0
        """,
        """
        variables: x, y, a
        minobjective: ScalarNonlinearFunction(+(exp(x), esc(1.0 * a)))
        c1: x + y + -1.0 * a <= 1.0
        a >= 0.0
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

function test_relax_scalarnonlinear_lessthan()
    _test_roundtrip(
        """
        variables: x
        maxobjective: 1.0 * x
        c1: ScalarNonlinearFunction(log(x)) <= 1.0
        """,
        """
        variables: x, a
        maxobjective: 1.0 * x + -1.0 * a
        c1: ScalarNonlinearFunction(log(x) - a) <= 1.0
        a >= 0.0
        """,
    )
    return
end

function test_relax_scalarnonlinear_greaterthan()
    _test_roundtrip(
        """
        variables: x
        maxobjective: 1.0 * x
        c1: ScalarNonlinearFunction(log(x)) >= 1.0
        """,
        """
        variables: x, a
        maxobjective: 1.0 * x + -1.0 * a
        c1: ScalarNonlinearFunction(log(x) + a) >= 1.0
        a >= 0.0
        """,
    )
    return
end

function test_relax_scalarnonlinear_equalto()
    _test_roundtrip(
        """
        variables: x
        minobjective: 1.0 * x
        c1: ScalarNonlinearFunction(log(x)) == 1.0
        """,
        """
        variables: x, a, b
        minobjective: 1.0 * x + 1.0 * a + 1.0 * b
        c1: ScalarNonlinearFunction(+(log(x), a, -b)) == 1.0
        a >= 0.0
        b >= 0.0
        """,
    )
    return
end

function test_penalty_dict()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, 1.0 * x, MOI.EqualTo(2.0))
    map = MOI.modify(model, MOI.Utilities.PenaltyRelaxation(Dict(c => 2.0)))
    @test map[c] isa MOI.ScalarAffineFunction{Float64}
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

function test_default()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, 1.0 * x, MOI.EqualTo(2.0))
    map = MOI.modify(model, MOI.Utilities.PenaltyRelaxation(default = 2.0))
    @test map[c] isa MOI.ScalarAffineFunction{Float64}
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

function test_default_nothing()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, 1.0 * x, MOI.EqualTo(2.0))
    map = MOI.modify(model, MOI.Utilities.PenaltyRelaxation(default = nothing))
    @test !haskey(map, c)
    @test sprint(print, model) === """
    Feasibility

    Subject to:

    ScalarAffineFunction{Float64}-in-EqualTo{Float64}
     0.0 + 1.0 v[1] == 2.0
    """
    return
end

function test_brige_optimizer()
    model = MOI.instantiate(
        MOI.Utilities.Model{Float64};
        with_bridge_type = Float64,
    )
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, 1.0 * x, MOI.EqualTo(2.0))
    map = MOI.modify(model, MOI.Utilities.PenaltyRelaxation(default = 2.0))
    @test map[c] isa MOI.ScalarAffineFunction{Float64}
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

function test_caching_optimizer()
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
        MOI.Bridges.full_bridge_optimizer(
            MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}()),
            Float64,
        ),
    )
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, 1.0 * x, MOI.EqualTo(2.0))
    map = MOI.modify(model, MOI.Utilities.PenaltyRelaxation(default = 2.0))
    @test map[c] isa MOI.ScalarAffineFunction{Float64}
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

function test_scalar_penalty_relaxation()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, 1.0 * x, MOI.LessThan(2.0))
    f = MOI.modify(model, c, MOI.Utilities.ScalarPenaltyRelaxation(2.0))
    @test f isa MOI.ScalarAffineFunction{Float64}
    @test sprint(print, model) === """
    Minimize ScalarAffineFunction{Float64}:
     0.0 + 2.0 v[2]

    Subject to:

    ScalarAffineFunction{Float64}-in-LessThan{Float64}
     0.0 + 1.0 v[1] - 1.0 v[2] <= 2.0

    VariableIndex-in-GreaterThan{Float64}
     v[2] >= 0.0
    """
    return
end

function test_scalar_penalty_relaxation_vector_objective()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, 1.0 * x, MOI.LessThan(2.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = MOI.VectorOfVariables([x])
    MOI.set(model, MOI.ObjectiveFunction{MOI.VectorOfVariables}(), f)
    @test_throws(
        ErrorException(
            "Cannot perform `ScalarPenaltyRelaxation` with an objective function of type `$(typeof(f))`",
        ),
        MOI.modify(model, c, MOI.Utilities.ScalarPenaltyRelaxation(2.0)),
    )
    return
end

end  # module

TestPenaltyRelaxation.runtests()
