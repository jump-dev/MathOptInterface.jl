module TestMPS

import MathOptInterface
using Test

const MOI = MathOptInterface
const MOIU = MOI.Utilities
const MPS = MOI.FileFormats.MPS
const MPS_TEST_FILE = "test.mps"

function _test_model_equality(model_string, variables, constraints)
    model = MPS.Model()
    MOIU.loadfromstring!(model, model_string)
    MOI.write_to_file(model, MPS_TEST_FILE)
    model_2 = MPS.Model()
    MOI.read_from_file(model_2, MPS_TEST_FILE)
    return MOIU.test_models_equal(model, model_2, variables, constraints)
end

function test_show()
    @test sprint(show, MPS.Model()) ==
          "A Mathematical Programming System (MPS) model"
end

function test_quadratic()
    model = MPS.Model()
    @test_throws(
        MOI.UnsupportedAttribute,
        MOIU.loadfromstring!(
            model,
            """
variables: x
minobjective: 1.0*x*x
""",
        )
    )
end

function test_nonempty()
    model = MPS.Model()
    @test MOI.is_empty(model)
    MOI.add_variable(model)
    @test !MOI.is_empty(model)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    MOI.add_variable(model)
    @test_throws Exception MOI.read_from_file(
        model,
        joinpath(@__DIR__, "failing_models", "bad_name.mps"),
    )
end

function test_failing_models()
    @testset "$(filename)" for filename in filter(
        f -> endswith(f, ".mps"),
        readdir(joinpath(@__DIR__, "failing_models")),
    )
        @test_throws Exception MOI.read_from_file(
            MPS.Model(),
            joinpath(@__DIR__, "failing_models", filename),
        )
    end
end

function test_empty_row_name()
    model = MPS.Model()
    x = MOI.add_variable(model)
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
        MOI.LessThan(1.0),
    )
    @test_throws Exception sprint(MPS.write_rows, model)
end

function test_sos()
    model = MPS.Model()
    x = MOI.add_variables(model, 3)
    names = Dict{MOI.VariableIndex,String}()
    for i in 1:3
        MOI.set(model, MOI.VariableName(), x[i], "x$(i)")
        names[x[i]] = "x$(i)"
    end
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.SOS1([1.5, 2.5, 3.5]),
    )
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.SOS2([1.25, 2.25, 3.25]),
    )
    @test sprint(MPS.write_sos, model, names) ==
          "SOS\n" *
          " S1 SOS1\n" *
          "    x1        1.5\n" *
          "    x2        2.5\n" *
          "    x3        3.5\n" *
          " S2 SOS2\n" *
          "    x1        1.25\n" *
          "    x2        2.25\n" *
          "    x3        3.25\n"
end

function test_maximization()
    model = MPS.Model()
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.VariableIndex}(),
        x,
    )
    @test sprint(MPS.write_columns, model, ["x"], Dict(x => "x")) ==
          "COLUMNS\n    x         OBJ       1\n"
end

function test_stacked_data()
    model = MPS.Model()
    MOI.read_from_file(model, joinpath(@__DIR__, "stacked_data.mps"))
    model_2 = MPS.Model()
    MOIU.loadfromstring!(
        model_2,
        """
variables: x, y, z
maxobjective: x + y + z + 2.5
con1: 1.0 * x in Interval(1.0, 5.0)
con2: 1.0 * x in Interval(2.0, 6.0)
con3: 1.0 * x in Interval(3.0, 7.0)
con4: 2.0 * x in Interval(4.0, 8.0)
y in Integer()
y in Interval(1.0, 4.0)
z in ZeroOne()
""",
    )
    MOI.set(model_2, MOI.Name(), "stacked_data")
    return MOIU.test_models_equal(
        model,
        model_2,
        ["x", "y", "z"],
        ["con1", "con2", "con3", "con4"],
        [
            ("y", MOI.Integer()),
            ("y", MOI.Interval{Float64}(1.0, 4.0)),
            ("z", MOI.ZeroOne()),
        ],
    )
end

function test_free_integer()
    model = MPS.Model()
    MOI.read_from_file(model, joinpath(@__DIR__, "free_integer.mps"))
    model_2 = MPS.Model()
    MOIU.loadfromstring!(
        model_2,
        """
variables: x
minobjective: x
con1: 1.0 * x >= 1.0
x in Integer()
""",
    )
    return MOIU.test_models_equal(
        model,
        model_2,
        ["x"],
        ["con1"],
        [("x", MOI.Integer())],
    )
end

function test_min_objective()
    return _test_model_equality(
        """
    variables: x
    minobjective: x
""",
        ["x"],
        String[],
    )
end

function test_objconst()
    return _test_model_equality(
        """
    variables: x
    minobjective: 1.1 * x + 1.2
""",
        ["x"],
        String[],
    )
end

function test_default_rhs_greater()
    return _test_model_equality(
        """
variables: x
minobjective: x
c1: 2.0 * x >= 0.0
""",
        ["x"],
        ["c1"],
    )
end

function test_default_rhs_less()
    return _test_model_equality(
        """
    variables: x
    minobjective: x
    c1: 2.0 * x <= 0.0
""",
        ["x"],
        ["c1"],
    )
end

function test_default_rhs_equal()
    return _test_model_equality(
        """
variables: x
minobjective: x
c1: 2.0 * x == 0.0
""",
        ["x"],
        ["c1"],
    )
end

function test_min_scalaraffine()
    return _test_model_equality(
        """
variables: x
minobjective: 1.2x
""",
        ["x"],
        String[],
    )
end

function test_scalaraffine_greaterthan()
    return _test_model_equality(
        """
variables: x
minobjective: 1.2x
c1: 1.1 * x >= 2.0
""",
        ["x"],
        ["c1"],
    )
end

function test_scalaraffine_lessthan()
    return _test_model_equality(
        """
variables: x
minobjective: 1.2x
c1: 1.1 * x <= 2.0
""",
        ["x"],
        ["c1"],
    )
end

function test_scalaraffine_equalto()
    return _test_model_equality(
        """
variables: x
minobjective: 1.2x
c1: 1.1 * x == 2.0
""",
        ["x"],
        ["c1"],
    )
end

function test_scalaraffine_interval()
    return _test_model_equality(
        """
variables: x
minobjective: 1.2x
c1: 1.1 * x in Interval(1.0, 2.0)
""",
        ["x"],
        ["c1"],
    )
end

function test_objsense_max()
    return _test_model_equality(
        """
variables: x
maxobjective: 1.2x
c1: 1.0 * x >= 0.0
""",
        ["x"],
        ["c1"],
    )
end

function test_MARKER_INT()
    model = MPS.Model()
    MOIU.loadfromstring!(
        model,
        """
variables: x, y, z
minobjective: x + y + z
x in Integer()
c2: 2 * x + -1.0 * z <= 1.0
z in ZeroOne()
x >= 1.0
""",
    )
    MOI.write_to_file(model, MPS_TEST_FILE)
    model_2 = MPS.Model()
    MOI.read_from_file(model_2, MPS_TEST_FILE)
    return MOIU.test_models_equal(
        model,
        model_2,
        ["x", "y", "z"],
        ["c2"],
        [
            ("x", MOI.Integer()),
            ("z", MOI.ZeroOne()),
            ("x", MOI.GreaterThan(1.0)),
        ],
    )
end

function test_zero_variable_bounds()
    model = MPS.Model()
    MOIU.loadfromstring!(
        model,
        """
variables: x, y, z
minobjective: x + y + z
x >= 0.0
y <= 0.0
""",
    )
    MOI.write_to_file(model, MPS_TEST_FILE)
    model_2 = MPS.Model()
    MOI.read_from_file(model_2, MPS_TEST_FILE)
    return MOIU.test_models_equal(
        model,
        model_2,
        ["x", "y", "z"],
        String[],
        [
            ("x", MOI.GreaterThan{Float64}(0.0)),
            ("y", MOI.LessThan{Float64}(0.0)),
        ],
    )
end

function test_nonzero_variable_bounds()
    model = MPS.Model()
    MOIU.loadfromstring!(
        model,
        """
variables: w, x, y, z
minobjective: w + x + y + z
x == 1.0
y >= 2.0
z <= 3.0
w in Interval(4.0, 5.0)
""",
    )
    MOI.write_to_file(model, MPS_TEST_FILE)
    model_2 = MPS.Model()
    MOI.read_from_file(model_2, MPS_TEST_FILE)
    return MOIU.test_models_equal(
        model,
        model_2,
        ["w", "x", "y", "z"],
        String[],
        [
            ("x", MOI.EqualTo{Float64}(1.0)),
            ("y", MOI.GreaterThan{Float64}(2.0)),
            ("z", MOI.LessThan{Float64}(3.0)),
            ("w", MOI.Interval{Float64}(4.0, 5.0)),
        ],
    )
end

function test_multiple_variable_bounds()
    model = MPS.Model()
    MOIU.loadfromstring!(
        model,
        """
variables: a_really_long_name
minobjective: a_really_long_name
a_really_long_name >= 1.0
a_really_long_name <= 2.0
""",
    )
    MOI.write_to_file(model, MPS_TEST_FILE)
    @test read(MPS_TEST_FILE, String) ==
          "NAME          \n" *
          "OBJSENSE MIN\n" *
          "ROWS\n" *
          " N  OBJ\n" *
          "COLUMNS\n" *
          "    a_really_long_name OBJ       1\n" *
          "RHS\n" *
          "RANGES\n" *
          "BOUNDS\n" *
          " LO bounds    a_really_long_name 1\n" *
          " UP bounds    a_really_long_name 2\n" *
          "ENDATA\n"
end

function test_unused_variable()
    # In this test, `x` will be written to the file with a 0 objective
    # coefficient since it does not appear in the objective or in the
    # constraints.
    model = MPS.Model()
    MOIU.loadfromstring!(
        model,
        """
variables: x, y
minobjective: y
c1: 2.0 * y >= 1.0
x >= 0.0
""",
    )
    MOI.write_to_file(model, MPS_TEST_FILE)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    model2 = MPS.Model()
    MOI.read_from_file(model2, MPS_TEST_FILE)
    @test MOI.get(model2, MOI.NumberOfVariables()) == 2
end

function test_names_with_spaces()
    model = MPS.Model()
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x[1, 2]")
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
        MOI.EqualTo(1.0),
    )
    MOI.set(model, MOI.ConstraintName(), c, "c c")
    @test sprint(write, model) ==
          "NAME          \n" *
          "OBJSENSE MIN\n" *
          "ROWS\n" *
          " N  OBJ\n" *
          " E  c_c\n" *
          "COLUMNS\n" *
          "    x[1,_2]   c_c       1\n" *
          "RHS\n" *
          "    rhs       c_c       1\n" *
          "RANGES\n" *
          "BOUNDS\n" *
          " FR bounds    x[1,_2]\n" *
          "ENDATA\n"
end

function test_sos_constraints()
    model = MPS.Model()
    x = MOI.add_variables(model, 3)
    MOI.set.(model, MOI.VariableName(), x, ["x1", "x2", "x3"])
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.SOS1([1.0, 2.0, 3.0]),
    )
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(reverse(x)),
        MOI.SOS2([1.2, 2.3, 3.4]),
    )
    @test sprint(write, model) ==
          "NAME          \n" *
          "OBJSENSE MIN\n" *
          "ROWS\n" *
          " N  OBJ\n" *
          "COLUMNS\n" *
          "    x1        OBJ       0\n" *
          "    x2        OBJ       0\n" *
          "    x3        OBJ       0\n" *
          "RHS\n" *
          "RANGES\n" *
          "BOUNDS\n" *
          " FR bounds    x1\n" *
          " FR bounds    x2\n" *
          " FR bounds    x3\n" *
          "SOS\n" *
          " S1 SOS1\n" *
          "    x1        1\n" *
          "    x2        2\n" *
          "    x3        3\n" *
          " S2 SOS2\n" *
          "    x3        1.2\n" *
          "    x2        2.3\n" *
          "    x1        3.4\n" *
          "ENDATA\n"
    io = IOBuffer()
    write(io, model)
    seekstart(io)
    new_model = MPS.Model()
    Base.read!(io, new_model)
    constraints = MOI.get(new_model, MOI.ListOfConstraintTypesPresent())
    @test (MOI.VectorOfVariables, MOI.SOS1{Float64}) in constraints
    @test (MOI.VectorOfVariables, MOI.SOS2{Float64}) in constraints
    return
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
    rm(MPS_TEST_FILE, force = true)
    return
end

end

TestMPS.runtests()
