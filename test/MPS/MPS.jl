const MPS = MathOptFormat.MPS

const MPS_TEST_FILE = "test.mps"

@test sprint(show, MPS.Model()) == "A Mathematical Programming System (MPS) model"

function test_model_equality(model_string, variables, constraints)
    model = MPS.Model()
    MOIU.loadfromstring!(model, model_string)
    MOI.write_to_file(model, MPS_TEST_FILE)
    model_2 = MPS.Model()
    MOI.read_from_file(model_2, MPS_TEST_FILE)
    MOIU.test_models_equal(model, model_2, variables, constraints)
end

@testset "read_from_file" begin
    file_to_read = joinpath(@__DIR__, "free_integer.mps")
    @test !MOI.is_empty(MathOptFormat.read_from_file(file_to_read))
    @test !MOI.is_empty(MathOptFormat.read_from_file(file_to_read * ".gz"))
end

@testset "Errors" begin
    failing_models_dir = joinpath(@__DIR__, "failing_models")

    @testset "Non-empty model" begin
        model = MPS.Model()
        MOI.add_variable(model)
        @test_throws Exception MOI.read_from_file(
            model, joinpath(failing_models_dir, "bad_name.mps"))
    end

    @testset "$(filename)" for filename in filter(
        f -> endswith(f, ".mps"), readdir(failing_models_dir))
        @test_throws Exception MOI.read_from_file(MPS.Model(),
            joinpath(failing_models_dir, filename))
    end
end

@testset "ROWS - empty ROW name" begin
    model = MPS.Model()
    x = MOI.add_variable(model)
    MOI.add_constraint(model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
        MOI.LessThan(1.0)
    )
    @test_throws Exception sprint(MPS.write_rows, model)
end

@testset "SOS" begin
    model = MPS.Model()
    x = MOI.add_variables(model, 3)
    for i in 1:3
        MOI.set(model, MOI.VariableName(), x[i], "x$(i)")
    end
    MOI.add_constraint(model,
        MOI.VectorOfVariables(x),
        MOI.SOS1([1.5, 2.5, 3.5])
    )
    MOI.add_constraint(model,
        MOI.VectorOfVariables(x),
        MOI.SOS2([1.25, 2.25, 3.25])
    )
    @test sprint(MPS.write_sos, model) ==
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

@testset "Maximization problems" begin
    model = MPS.Model()
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x))
    @test sprint(MPS.write_columns, model) ==
        "COLUMNS\n     x        OBJ      -1\n"
end

@testset "stacked_data" begin
    model = MPS.Model()
    MOI.read_from_file(model, joinpath(@__DIR__, "stacked_data.mps"))
    MOI.set(model, MOI.ConstraintName(), MOI.get(model,
            MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.Integer}())[1],
        "con5")
    MOI.set(model, MOI.ConstraintName(), MOI.get(model,
            MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.Interval{Float64}}())[1],
        "con6")
    model_2 = MPS.Model()
    MOIU.loadfromstring!(model_2, """
    variables: x, y
    minobjective: x + y
    con1: 1.0 * x in Interval(1.0, 5.0)
    con2: 1.0 * x in Interval(2.0, 6.0)
    con3: 1.0 * x in Interval(3.0, 7.0)
    con4: 2.0 * x in Interval(4.0, 8.0)
    con5: y in Integer()
    con6: y in Interval(1.0, 4.0)
    """)
    MOI.set(model_2, MOI.Name(), "stacked_data")
    MOIU.test_models_equal(model, model_2, ["x", "y"],
        ["con1", "con2", "con3", "con4", "con5", "con6"])
end

@testset "free_integer" begin
    model = MPS.Model()
    MOI.read_from_file(model, joinpath(@__DIR__, "free_integer.mps"))
    MOI.set(model, MOI.ConstraintName(), MOI.get(model,
            MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.Integer}())[1],
        "con2")
    model_2 = MPS.Model()
    MOIU.loadfromstring!(model_2, """
    variables: x
    minobjective: x
    con1: 1.0 * x >= 1.0
    con2: x in Integer()
    """)
    MOIU.test_models_equal(model, model_2, ["x"], ["con1", "con2"])
end

@testset "Round trips" begin
    @testset "min objective" begin
        test_model_equality("""
            variables: x
            minobjective: x
        """, ["x"], String[])
    end
    @testset "min scalaraffine" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x
        """, ["x"], String[])
    end

    @testset "ScalarAffine-in-GreaterThan" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x
            c1: 1.1 * x >= 2.0
        """, ["x"], ["c1"])
    end
    @testset "ScalarAffine-in-LessThan" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x
            c1: 1.1 * x <= 2.0
        """, ["x"], ["c1"])
    end
    @testset "ScalarAffine-in-EqualTo" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x
            c1: 1.1 * x == 2.0
        """, ["x"], ["c1"])
    end
    @testset "ScalarAffine-in-Interval" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x
            c1: 1.1 * x in Interval(1.0, 2.0)
        """, ["x"], ["c1"])
    end
    @testset "MARKER INT" begin
        model = MPS.Model()
        MOIU.loadfromstring!(model, """
            variables: x, y, z
            minobjective: x + y + z
            c1: x in Integer()
            c2: 2 * x + -1.0 * z <= 1.0
            c3: z in ZeroOne()
            c4: x >= 1.0
        """)
        MOI.write_to_file(model, MPS_TEST_FILE)
        model_2 = MPS.Model()
        MOI.read_from_file(model_2, MPS_TEST_FILE)
        for (set_type, constraint_name) in [(MOI.Integer, "c1"),
                                            (MOI.ZeroOne, "c3"),
                                            (MOI.GreaterThan{Float64}, "c4")]
            MOI.set(model_2, MOI.ConstraintName(), MOI.get(model_2,
                MOI.ListOfConstraintIndices{MOI.SingleVariable, set_type}())[1],
                constraint_name)
        end
        MOIU.test_models_equal(
            model, model_2, ["x", "y", "z"], ["c1", "c2", "c3", "c4"])
    end
    @testset "Zero variable bounds" begin
        model = MPS.Model()
        MOIU.loadfromstring!(model, """
            variables: x, y, z
            minobjective: x + y + z
            c1: x >= 0.0
            c2: y <= 0.0
        """)
        MOI.write_to_file(model, MPS_TEST_FILE)
        model_2 = MPS.Model()
        MOI.read_from_file(model_2, MPS_TEST_FILE)
        for (set_type, constraint_name) in [(MOI.GreaterThan{Float64}, "c1"),
                                            (MOI.LessThan{Float64}, "c2")]
            MOI.set(model_2, MOI.ConstraintName(), MOI.get(model_2,
                MOI.ListOfConstraintIndices{MOI.SingleVariable, set_type}())[1],
                constraint_name)
        end
        MOIU.test_models_equal(
            model, model_2, ["x", "y", "z"], ["c1", "c2"])
    end
    @testset "Non-zero variable bounds" begin
        model = MPS.Model()
        MOIU.loadfromstring!(model, """
            variables: w, x, y, z
            minobjective: w + x + y + z
            c1: x == 1.0
            c2: y >= 2.0
            c3: z <= 3.0
            c4: w in Interval(4.0, 5.0)
        """)
        MOI.write_to_file(model, MPS_TEST_FILE)
        model_2 = MPS.Model()
        MOI.read_from_file(model_2, MPS_TEST_FILE)
        for (set_type, constraint_name) in [(MOI.EqualTo{Float64}, "c1"),
                                            (MOI.GreaterThan{Float64}, "c2"),
                                            (MOI.LessThan{Float64}, "c3"),
                                            (MOI.Interval{Float64}, "c4")]
            MOI.set(model_2, MOI.ConstraintName(), MOI.get(model_2,
                MOI.ListOfConstraintIndices{MOI.SingleVariable, set_type}())[1],
                constraint_name)
        end
        MOIU.test_models_equal(
            model, model_2, ["w", "x", "y", "z"], ["c1", "c2", "c3", "c4"])
    end
end

# Clean up
sleep(1.0)  # Allow time for unlink to happen.
rm(MPS_TEST_FILE, force = true)
