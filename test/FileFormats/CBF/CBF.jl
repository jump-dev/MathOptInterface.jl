# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestCBF

using Test

import MathOptInterface as MOI
import MathOptInterface.FileFormats: CBF

const MODELS_DIR = joinpath(@__DIR__, "models")

function runtests()
    for name in names(@__MODULE__, all = true)
        if startswith("$(name)", "test_")
            @testset "$name" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

function _set_var_and_con_names(model::MOI.ModelLike)
    variable_names = String[]
    for xi in MOI.get(model, MOI.ListOfVariableIndices())
        push!(variable_names, "v$(xi.value)")
        MOI.set(model, MOI.VariableName(), xi, "v$(xi.value)")
    end
    idx = 0
    constraint_names, single_variable_constraints = String[], Tuple[]
    attr = MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.Integer}()
    for ci in MOI.get(model, attr)
        idx += 1
        x = MOI.get(model, MOI.VariableName(), MOI.VariableIndex(ci.value))
        push!(single_variable_constraints, (x, MOI.Integer()))
    end
    for S in (
        MOI.Reals,
        MOI.Zeros,
        MOI.Nonnegatives,
        MOI.Nonpositives,
        MOI.SecondOrderCone,
        MOI.RotatedSecondOrderCone,
        MOI.PositiveSemidefiniteConeTriangle,
        MOI.ExponentialCone,
        MOI.DualExponentialCone,
        MOI.PowerCone{Float64},
        MOI.DualPowerCone{Float64},
    )
        for F in (MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64})
            for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
                idx += 1
                push!(constraint_names, "c$idx")
                MOI.set(model, MOI.ConstraintName(), ci, "c$idx")
            end
        end
    end
    return variable_names, constraint_names, single_variable_constraints
end

function _test_write_then_read(model_string::String)
    model1 = CBF.Model()
    MOI.Utilities.loadfromstring!(model1, model_string)
    args = _set_var_and_con_names(model1)
    io = IOBuffer()
    write(io, model1)
    model2 = CBF.Model()
    seekstart(io)
    read!(io, model2)
    _set_var_and_con_names(model2)
    return MOI.Test.util_test_models_equal(model1, model2, args...)
end

function _test_read(filename::String, model_string::String)
    model1 = CBF.Model()
    MOI.Utilities.loadfromstring!(model1, model_string)
    args = _set_var_and_con_names(model1)
    model2 = CBF.Model()
    MOI.read_from_file(model2, filename)
    _set_var_and_con_names(model2)
    MOI.Test.util_test_models_equal(model1, model2, args...)
    return
end

function test_show()
    @test sprint(summary, CBF.Model()) == "MOI.FileFormats.CBF.Model"
    return
end

function test_support_errors()
    for set in (
        MOI.EqualTo(1.0),
        MOI.LessThan(1.0),
        MOI.GreaterThan(1.0),
        MOI.Interval(1.0, 2.0),
        MOI.Semiinteger(1.0, 2.0),
        MOI.Semicontinuous(1.0, 2.0),
        MOI.ZeroOne(),
    )
        model_string = """
        variables: x
        minobjective: x
        x in $set
        """
        model = CBF.Model()
        err = MOI.UnsupportedConstraint{MOI.VariableIndex,typeof(set)}
        @test_throws err MOI.Utilities.loadfromstring!(model, model_string)
    end
    return
end

function test_read_nonempty()
    model = CBF.Model()
    MOI.add_variable(model)
    @test_throws(
        ErrorException("Cannot read in file because model is not empty."),
        MOI.read_from_file(model, joinpath(MODELS_DIR, "example_A.cbf")),
    )
    return
end

function test_read_incompatible()
    model = CBF.Model()
    @test_throws Exception MOI.read_from_file(
        model,
        joinpath(MODELS_DIR, "incompatible_version.cbf"),
    )
    return
end

function test_read_badcones()
    for filename in [
        "bad_cone_string_A.cbf",
        "bad_cone_string_B.cbf",
        "bad_cone_string_C.cbf",
        "bad_cone_string_D.cbf",
    ]
        model = CBF.Model()
        @test_throws Exception MOI.read_from_file(
            model,
            joinpath(MODELS_DIR, filename),
        )
    end
    return
end

function test_read_badpowerdim()
    for filename in ["bad_power_dim_A.cbf", "bad_power_dim_B.cbf"]
        model = CBF.Model()
        @test_throws Exception MOI.read_from_file(
            model,
            joinpath(MODELS_DIR, filename),
        )
    end
    return
end

function test_read_corrupt()
    for filename in [
        "corrupt_line_A.cbf",
        "corrupt_line_B.cbf",
        "corrupt_line_C.cbf",
        "corrupt_line_D.cbf",
        "corrupt_line_E.cbf",
    ]
        model = CBF.Model()
        @test_throws Exception MOI.read_from_file(
            model,
            joinpath(MODELS_DIR, filename),
        )
    end
    return
end

const _WRITE_READ_MODELS = [
    (
        "min VariableIndex",
        """
        variables: x
        minobjective: x
        """,
    ),
    (
        "min ScalarAffine",
        """
        variables: x, y
        minobjective: 1.2x + -1y + 1
        """,
    ),
    (
        "max VariableIndex",
        """
        variables: x
        maxobjective: x
        """,
    ),
    (
        "max ScalarAffine",
        """
        variables: x, y
        maxobjective: 1.2x + -1y + 1
        """,
    ),
    (
        "VariableIndex in Integer",
        """
        variables: x, y
        minobjective: 1.2x
        y in Integer()
        """,
    ),
    (
        "VectorOfVariables in Zeros",
        """
        variables: x, y
        minobjective: x
        c1: [x, y] in Zeros(2)
        """,
    ),
    (
        "VectorOfVariables in Nonnegatives",
        """
        variables: x, y
        minobjective: x
        c1: [x, y] in Nonnegatives(2)
        """,
    ),
    (
        "VectorOfVariables in Nonpositives",
        """
        variables: x, y
        minobjective: x
        c1: [y, x] in Nonpositives(2)
        """,
    ),
    (
        "VectorAffineFunction in Zeros",
        """
        variables: x, y
        minobjective: 1.2x
        c1: [x + 2y + -1.1, 0] in Zeros(2)
        """,
    ),
    (
        "VectorAffineFunction in Reals",
        """
        variables: x, y
        minobjective: 1.2x
        c1: [1x, 2y] in Reals(2)
        """,
    ),
    (
        "VectorAffineFunction in Nonnegatives",
        """
        variables: x, y
        minobjective: 1.2x
        c1: [1.1 * x, y + 1] in Nonnegatives(2)
        """,
    ),
    (
        "VectorAffineFunction in Nonpositives",
        """
        variables: x
        minobjective: 1.2x
        c1: [-1.1 * x + 1] in Nonpositives(1)
        """,
    ),
    (
        "VectorOfVariables in SecondOrderCone",
        """
        variables: x, y, z
        minobjective: x
        c1: [x, y, z] in SecondOrderCone(3)
        """,
    ),
    (
        "VectorOfVariables in RotatedSecondOrderCone",
        """
        variables: x, y, z
        minobjective: x
        c1: [x, y, z] in RotatedSecondOrderCone(3)
        """,
    ),
    (
        "VectorOfVariables in ExponentialCone",
        """
        variables: x, y, z
        minobjective: x
        c1: [x, y, z] in ExponentialCone()
        """,
    ),
    (
        "VectorOfVariables as function in ExponentialCone",
        """
        variables: x, y, z
        minobjective: x
        c0: [x, y, z] in Nonnegatives(3)
        c1: [x, y, z] in ExponentialCone()
        """,
    ),
    (
        "VectorOfVariables in DualExponentialCone",
        """
        variables: x, y, z
        minobjective: x
        c1: [x, y, z] in DualExponentialCone()
        """,
    ),
    (
        "VectorOfVariables as function in DualExponentialCone",
        """
        variables: x, y, z
        minobjective: x
        c0: [x, y, z] in Nonnegatives(3)
        c1: [x, y, z] in DualExponentialCone()
        """,
    ),
    (
        "VectorOfVariables in PowerCone",
        """
        variables: x, y, z
        minobjective: x
        c1: [x, y, z] in PowerCone(2.0)
        """,
    ),
    (
        "VectorOfVariables in DualPowerCone",
        """
        variables: x, y, z
        minobjective: x
        c1: [x, y, z] in DualPowerCone(2.0)
        """,
    ),
    (
        "VectorOfVariables in PositiveSemidefiniteConeTriangle",
        """
        variables: x, y, z
        minobjective: x
        c1: [x, y, z] in PositiveSemidefiniteConeTriangle(2)
        """,
    ),
    (
        "VectorAffineFunction in SecondOrderCone",
        """
        variables: x, y, z
        minobjective: 1.2x
        c1: [1.1x, y + 1, 2x + z] in SecondOrderCone(3)
        """,
    ),
    (
        "VectorAffineFunction in RotatedSecondOrderCone",
        """
        variables: x, y, z
        minobjective: 1.2x
        c1: [1.1x, y + 1, 2x + z] in RotatedSecondOrderCone(3)
        """,
    ),
    (
        "VectorAffineFunction in ExponentialCone",
        """
        variables: x, y, z
        minobjective: 1.2x
        c1: [1.1x, y + 1, 2x + z] in ExponentialCone()
        """,
    ),
    (
        "VectorAffineFunction in DualExponentialCone",
        """
        variables: x, y, z
        minobjective: 1.2x
        c1: [1.1x, y + 1, 2x + z] in DualExponentialCone()
        """,
    ),
    (
        "VectorAffineFunction in PowerCone",
        """
        variables: x, y, z
        minobjective: 1.2x
        c1: [1.1x, y + 1, 2x + z] in PowerCone(2.0)
        """,
    ),
    (
        "VectorAffineFunction in DualPowerCone",
        """
        variables: x, y, z
        minobjective: 1.2x
        c1: [1.1x, y + 1, 2x + z] in DualPowerCone(2.0)
        """,
    ),
    (
        "VectorAffineFunction in PositiveSemidefiniteConeTriangle",
        """
        variables: x, y, z
        minobjective: 1.2x
        c1: [1.1x, y + 1, 2x + z] in PositiveSemidefiniteConeTriangle(2)
        """,
    ),
]

function test_write_read_models()
    for (model_name, model_string) in _WRITE_READ_MODELS
        _test_write_then_read(model_string)
    end
    return
end

const _EXAMPLE_MODELS = [
    (
        "example_A.cbf",
        """
        variables: U, V, W, X, Y, Z, x, y, z
        minobjective: y + 2U + 2V + 2W + 2Y + 2Z
        c1: [U, V, W, X, Y, Z] in PositiveSemidefiniteConeTriangle(3)
        c2: [y + U + W + Z + -1, x + z + U + 2V + W + 2X + 2Y + Z + -0.5] in Zeros(2)
        c3: [y, x, z] in SecondOrderCone(3)
        """,
    ),
    (
        "example_B.cbf",
        """
        variables: X, Y, Z, x, y
        minobjective: 1 + x + y + X + Z
        c1: [X, Y, Z] in PositiveSemidefiniteConeTriangle(2)
        c2: [2Y + -1x + -1y] in Nonnegatives(1)
        c3: [3y + -1, x + y, 3x + -1] in PositiveSemidefiniteConeTriangle(2)
        """,
    ),
    (
        "example_C.cbf",
        """
        variables: a, b, c, d, e, f, g, h, i, j
        maxobjective: a + b + c + d + e + f + g + h + i + j + -1
        c1: [b] in Zeros(1)
        c2: [c] in Nonnegatives(1)
        c3: [d] in Nonpositives(1)
        c4: [e, f, g] in SecondOrderCone(3)
        c5: [h, i, j] in RotatedSecondOrderCone(3)
        """,
    ),
    (
        "example_D.cbf",
        """
        variables: u, v, w, x, y, z
        maxobjective: w + z
        c1: [u, v, w] in PowerCone(0.5)
        c2: [x, y, z] in DualPowerCone(0.1)
        c3: [1, u, u + v] in PowerCone(0.8)
        c4: [1, y, x + y] in DualPowerCone(0.75)
        u in Integer()
        w in Integer()
        y in Integer()
        """,
    ),
]

function test_example_models()
    for (model_name, model_string) in _EXAMPLE_MODELS
        _test_read(joinpath(MODELS_DIR, model_name), model_string)
        _test_write_then_read(model_string)
    end
    return
end

function test_write_variable_cones()
    model = CBF.Model()
    for set in (
        MOI.Zeros(2),
        MOI.Nonnegatives(3),
        MOI.PowerCone(0.74),
        MOI.Nonpositives(1),
        MOI.DualPowerCone(0.25),
        MOI.Nonnegatives(3),
        MOI.PowerCone(0.5),
        MOI.SecondOrderCone(3),
    )
        _ = MOI.add_constrained_variables(model, set)
    end
    io = IOBuffer()
    write(io, model)
    seekstart(io)
    @test read(io, String) == """
    VER
    3

    POWCONES
    2 4
    2
    0.74
    0.26
    2
    0.5
    0.5

    POW*CONES
    1 2
    2
    0.25
    0.75

    OBJSENSE
    MIN

    VAR
    21 8
    L= 2
    L+ 3
    @0:POW 3
    L- 1
    @0:POW* 3
    L+ 3
    @1:POW 3
    Q 3

    """
    return
end

function test_write_variable_cones_with_conflicting_sets()
    model = CBF.Model()
    x, _ = MOI.add_constrained_variables(model, MOI.Nonnegatives(2))
    y = MOI.add_variable(model)
    f = MOI.VectorOfVariables([y, x[2]])
    # The choice of Nonnegatives and Nonpositives is explicitly chosen because
    # Nonnegatives are parsed before Nonpositives, and it tests that we can skip
    # over a function containing `y`, and then constrain it in a later set.
    MOI.add_constraint(model, f, MOI.Nonnegatives(2))
    MOI.add_constraint(model, MOI.VectorOfVariables([y]), MOI.Nonpositives(1))
    g = 1.0 * x[1] + 2.0 * x[2] + 3.0 * y
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(g)}(), g)
    io = IOBuffer()
    write(io, model)
    seekstart(io)
    @test read(io, String) == """
    VER
    3

    OBJSENSE
    MIN

    VAR
    3 2
    L+ 2
    L- 1

    CON
    2 1
    L+ 2

    OBJACOORD
    3
    0 1.0
    1 2.0
    2 3.0

    ACOORD
    2
    0 2 1.0
    1 1 1.0

    """
    return
end

function test_roundtrip_ExponentialCone()
    model = CBF.Model()
    x, _ = MOI.add_constrained_variables(model, MOI.ExponentialCone())
    f = 1.0 * x[1] + 2.0 * x[2] + 3.0 * x[3]
    MOI.add_constraint(model, x[1], MOI.Integer())
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables([x[1], x[2], x[1]]),
        MOI.PositiveSemidefiniteConeTriangle(2),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    g = MOI.Utilities.operate(vcat, Float64, f)
    MOI.add_constraint(model, g, MOI.Zeros(1))
    io = IOBuffer()
    write(io, model)
    seekstart(io)
    @test read(io, String) == """
    VER
    3

    OBJSENSE
    MIN

    VAR
    3 1
    EXP 3

    INT
    1
    2

    PSDCON
    1
    2

    CON
    1 1
    L= 1

    OBJACOORD
    3
    2 1.0
    1 2.0
    0 3.0

    ACOORD
    3
    0 2 1.0
    0 1 2.0
    0 0 3.0

    BCOORD
    1
    0 0.0

    HCOORD
    3
    0 2 0 0 1.0
    0 1 1 0 1.0
    0 2 1 1 1.0

    """
    seekstart(io)
    model2 = CBF.Model()
    read!(io, model2)
    y = MOI.get(model2, MOI.ListOfVariableIndices())
    obj_y = MOI.get(model2, MOI.ObjectiveFunction{typeof(f)}())
    @test ≈(obj_y, 1.0 * y[1] + 2.0 * y[2] + 3.0 * y[3])
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.Integer}.(1:3)
    @test MOI.is_valid.(model2, ci) == [true, false, false]
    return
end

function test_roundtrip_DualExponentialCone()
    model = CBF.Model()
    x, _ = MOI.add_constrained_variables(model, MOI.DualExponentialCone())
    f = 1.0 * x[1] + 2.0 * x[2] + 3.0 * x[3]
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    g = MOI.Utilities.operate(vcat, Float64, f)
    MOI.add_constraint(model, g, MOI.Zeros(1))
    io = IOBuffer()
    write(io, model)
    seekstart(io)
    @test read(io, String) == """
    VER
    3

    OBJSENSE
    MIN

    VAR
    3 1
    EXP* 3

    CON
    1 1
    L= 1

    OBJACOORD
    3
    2 1.0
    1 2.0
    0 3.0

    ACOORD
    3
    0 2 1.0
    0 1 2.0
    0 0 3.0

    BCOORD
    1
    0 0.0

    """
    seekstart(io)
    model2 = CBF.Model()
    read!(io, model2)
    y = MOI.get(model2, MOI.ListOfVariableIndices())
    obj_y = MOI.get(model2, MOI.ObjectiveFunction{typeof(f)}())
    @test ≈(obj_y, 1.0 * y[1] + 2.0 * y[2] + 3.0 * y[3])
    return
end

function test_unsupported_objectives()
    model = CBF.Model()
    for (F, ret) in [
        MOI.VariableIndex => true,
        MOI.ScalarAffineFunction{Float64} => true,
        MOI.ScalarQuadraticFunction{Float64} => false,
        MOI.ScalarNonlinearFunction => false,
        MOI.VectorOfVariables => false,
        MOI.VectorAffineFunction{Float64} => false,
        MOI.VectorQuadraticFunction{Float64} => false,
        MOI.VectorNonlinearFunction => false,
    ]
        @test MOI.supports(model, MOI.ObjectiveFunction{F}()) == ret
    end
    return
end

function test_unsupported_variable_types()
    model = CBF.Model()
    @test_throws(
        MOI.UnsupportedConstraint,
        MOI.add_constrained_variable(model, MOI.Parameter(2.0)),
    )
    @test_throws(
        MOI.UnsupportedConstraint,
        MOI.add_constrained_variable(model, MOI.Semicontinuous(2.0, 3.0)),
    )
    @test_throws(
        MOI.UnsupportedConstraint,
        MOI.add_constrained_variable(model, MOI.Semiinteger(2.0, 3.0)),
    )
    return
end

end  # module

TestCBF.runtests()
