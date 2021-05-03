module TestCBF

import MathOptInterface
using Test

const MOI = MathOptInterface
const MOIU = MOI.Utilities
const CBF = MOI.FileFormats.CBF
const CBF_TEST_FILE = "test.cbf"
const MODELS_DIR = joinpath(@__DIR__, "models")

function _set_var_and_con_names(model::MOI.ModelLike)
    variable_names = String[]
    for j in MOI.get(model, MOI.ListOfVariableIndices())
        var_name_j = "v" * string(j.value)
        push!(variable_names, var_name_j)
        MOI.set(model, MOI.VariableName(), j, var_name_j)
    end

    idx = 0
    constraint_names = String[]
    single_variable_constraints = Tuple[]
    for i in MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.SingleVariable,MOI.Integer}(),
    )
        idx += 1
        x = MOI.get(model, MOI.VariableName(), MOI.VariableIndex(i.value))
        push!(single_variable_constraints, (x, MOI.Integer))
    end
    for S in [
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
        ],
        F in [MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64}]

        for i in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            idx += 1
            con_name_i = "c" * string(idx)
            push!(constraint_names, con_name_i)
            MOI.set(model, MOI.ConstraintName(), i, con_name_i)
        end
    end

    return variable_names, constraint_names, single_variable_constraints
end

function Base.isapprox(
    f::MOI.VectorOfVariables,
    g::MOI.VectorAffineFunction{Float64};
    kwargs...,
)
    return isapprox(MOI.VectorAffineFunction{Float64}(f), g)
end

function _test_write_then_read(model_string::String)
    model1 = CBF.Model()
    MOIU.loadfromstring!(model1, model_string)
    args = _set_var_and_con_names(model1)

    MOI.write_to_file(model1, CBF_TEST_FILE)
    model2 = CBF.Model()
    MOI.read_from_file(model2, CBF_TEST_FILE)
    _set_var_and_con_names(model2)

    return MOIU.test_models_equal(model1, model2, args...)
end

function _test_read(filename::String, model_string::String)
    model1 = CBF.Model()
    MOIU.loadfromstring!(model1, model_string)
    args = _set_var_and_con_names(model1)

    model2 = CBF.Model()
    MOI.read_from_file(model2, filename)
    _set_var_and_con_names(model2)

    return MOIU.test_models_equal(model1, model2, args...)
end

function test_show()
    @test sprint(show, CBF.Model()) == "A Conic Benchmark Format (CBF) model"
end

function test_support_errors()
    for set in [
        MOI.EqualTo(1.0),
        MOI.LessThan(1.0),
        MOI.GreaterThan(1.0),
        MOI.Interval(1.0, 2.0),
        MOI.Semiinteger(1.0, 2.0),
        MOI.Semicontinuous(1.0, 2.0),
        MOI.ZeroOne(),
    ]
        model_string = """
        variables: x
        minobjective: x
        x in $set
        """
        model = CBF.Model()
        err = MOI.UnsupportedConstraint{MOI.SingleVariable,typeof(set)}
        @test_throws err MOIU.loadfromstring!(model, model_string)
    end
end

function test_read_nonempty()
    model = CBF.Model()
    MOI.add_variable(model)
    @test_throws Exception MOI.read_from_file(
        model,
        joinpath(MODELS_DIR, "example1.cbf"),
    )
end

function test_read_incompatible()
    model = CBF.Model()
    @test_throws Exception MOI.read_from_file(
        model,
        joinpath(MODELS_DIR, "incompatible_version.cbf"),
    )
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
end

function test_read_badpowerdim()
    for filename in ["bad_power_dim_A.cbf", "bad_power_dim_B.cbf"]
        model = CBF.Model()
        @test_throws Exception MOI.read_from_file(
            model,
            joinpath(MODELS_DIR, filename),
        )
    end
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
end

function test_write_quadratic()
    model = CBF.Model()
    MOIU.loadfromstring!(
        model,
        """
        variables: x
        minobjective: 1 * x * x
        """,
    )
    @test_throws Exception MOI.write_to_file(model, CBF_TEST_FILE)
end

const _WRITE_READ_MODELS = [
    (
        "min SingleVariable",
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
        "max SingleVariable",
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
        "SingleVariable in Integer",
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
        "VectorOfVariables in Reals",
        """
    variables: x, y
    minobjective: x
    c1: [x, y] in Reals(2)
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
        "VectorOfVariables in DualExponentialCone",
        """
    variables: x, y, z
    minobjective: x
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
    variables: a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p
    maxobjective: a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + -1
    c1: [b] in Zeros(1)
    c2: [c] in Nonnegatives(1)
    c3: [d] in Nonpositives(1)
    c4: [e, f, g] in SecondOrderCone(3)
    c5: [h, i, j] in RotatedSecondOrderCone(3)
    c6: [m, l, k] in ExponentialCone()
    c7: [p, o, n] in DualExponentialCone()
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
end

function runtests()
    for name in names(@__MODULE__, all = true)
        if startswith("$(name)", "test_")
            @testset "$name" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    sleep(1.0)  # Allow time for unlink to happen.
    rm(CBF_TEST_FILE, force = true)
    return
end

end

TestCBF.runtests()
