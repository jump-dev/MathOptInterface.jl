import MathOptInterface
using Test

const MOI = MathOptInterface
const MOIU = MOI.Utilities
const MOF = MOI.FileFormats.MOF

const TEST_MOF_FILE = "test.mof.json"

@test sprint(show, MOF.Model()) == "A MathOptFormat Model"

include("nonlinear.jl")

struct UnsupportedSet <: MOI.AbstractSet end
struct UnsupportedFunction <: MOI.AbstractFunction end

function test_model_equality(model_string, variables, constraints; suffix = "")
    model = MOF.Model(validate = true)
    MOIU.loadfromstring!(model, model_string)
    MOI.write_to_file(model, TEST_MOF_FILE * suffix)
    model_2 = MOF.Model()
    MOI.read_from_file(model_2, TEST_MOF_FILE * suffix)
    MOIU.test_models_equal(model, model_2, variables, constraints)
    return MOF.validate(TEST_MOF_FILE * suffix)
end

@testset "Error handling: read_from_file" begin
    failing_models_dir = joinpath(@__DIR__, "failing_models")

    @testset "Non-empty model" begin
        model = MOF.Model(warn = true)
        MOI.add_variable(model)
        @test !MOI.is_empty(model)
        exception = ErrorException(
            "Cannot read model from file as destination model is not empty.",
        )
        @test_throws exception MOI.read_from_file(
            model,
            joinpath(@__DIR__, "empty_model.mof.json"),
        )
        options = MOF.get_options(model)
        @test options.warn
        MOI.empty!(model)
        @test MOI.is_empty(model)
        MOI.read_from_file(model, joinpath(@__DIR__, "empty_model.mof.json"))
        options2 = MOF.get_options(model)
        @test options2.warn
    end

    @testset "$(filename)" for filename in filter(
        f -> endswith(f, ".mof.json"),
        readdir(failing_models_dir),
    )
        @test_throws Exception MOI.read_from_file(
            MOF.Model(),
            joinpath(failing_models_dir, filename),
        )
    end
end
@testset "Names" begin
    @testset "Blank variable name" begin
        model = MOF.Model()
        variable = MOI.add_variable(model)
        @test_throws Exception MOF.moi_to_object(variable, model)
        MOI.FileFormats.create_unique_names(model, warn = true)
        @test MOF.moi_to_object(variable, model) ==
              MOF.OrderedObject("name" => "x1")
    end
    @testset "Duplicate variable name" begin
        model = MOF.Model()
        x = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), x, "x")
        y = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), y, "x")
        @test MOF.moi_to_object(x, model) == MOF.OrderedObject("name" => "x")
        @test MOF.moi_to_object(y, model) == MOF.OrderedObject("name" => "x")
        MOI.FileFormats.create_unique_names(model, warn = true)
        @test MOF.moi_to_object(x, model) == MOF.OrderedObject("name" => "x")
        @test MOF.moi_to_object(y, model) == MOF.OrderedObject("name" => "x_1")
    end
    @testset "Blank constraint name" begin
        model = MOF.Model()
        x = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), x, "x")
        c = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.ZeroOne())
        name_map = Dict(x => "x")
        MOI.FileFormats.create_unique_names(model, warn = true)
        @test MOF.moi_to_object(c, model, name_map)["name"] == "c1"
    end
    @testset "Duplicate constraint name" begin
        model = MOF.Model()
        x = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), x, "x")
        c1 = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.LessThan(1.0))
        c2 = MOI.add_constraint(
            model,
            MOI.SingleVariable(x),
            MOI.GreaterThan(0.0),
        )
        MOI.set(model, MOI.ConstraintName(), c1, "c")
        MOI.set(model, MOI.ConstraintName(), c2, "c")
        name_map = Dict(x => "x")
        @test MOF.moi_to_object(c1, model, name_map)["name"] == "c"
        @test MOF.moi_to_object(c2, model, name_map)["name"] == "c"
        MOI.FileFormats.create_unique_names(model, warn = true)
        @test MOF.moi_to_object(c1, model, name_map)["name"] == "c_1"
        @test MOF.moi_to_object(c2, model, name_map)["name"] == "c"
    end
    @testset "v0.4" begin
        filename = joinpath(@__DIR__, "v0.4.mof.json")
        model = MOF.Model(validate = true)
        @test_throws ErrorException MOI.read_from_file(model, filename)
        model = MOF.Model(validate = false)
        MOI.read_from_file(model, filename)
        @test MOI.get(model, MOI.NumberOfVariables()) == 2
    end
end
@testset "round trips" begin
    @testset "Empty model" begin
        model = MOF.Model(validate = true)
        MOI.write_to_file(model, TEST_MOF_FILE)
        model_2 = MOF.Model(validate = true)
        MOI.read_from_file(model_2, TEST_MOF_FILE)
        MOIU.test_models_equal(model, model_2, String[], String[])
    end
    @testset "FEASIBILITY_SENSE" begin
        model = MOF.Model(validate = true)
        x = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), x, "x")
        MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
        MOI.write_to_file(model, TEST_MOF_FILE)
        model_2 = MOF.Model(validate = true)
        MOI.read_from_file(model_2, TEST_MOF_FILE)
        MOIU.test_models_equal(model, model_2, ["x"], String[])
    end
    @testset "Empty function term" begin
        model = MOF.Model(validate = true)
        x = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), x, "x")
        c = MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Float64}[], 0.0),
            MOI.GreaterThan(1.0),
        )
        MOI.set(model, MOI.ConstraintName(), c, "c")
        MOI.write_to_file(model, TEST_MOF_FILE)
        model_2 = MOF.Model(validate = true)
        MOI.read_from_file(model_2, TEST_MOF_FILE)
        MOIU.test_models_equal(model, model_2, ["x"], ["c"])
    end
    @testset "min objective" begin
        test_model_equality(
            """
    variables: x
    minobjective: x
""",
            ["x"],
            String[],
        )
    end
    @testset "max objective" begin
        test_model_equality(
            """
    variables: x
    maxobjective: x
""",
            ["x"],
            String[],
            suffix = ".gz",
        )
    end
    @testset "min scalaraffine" begin
        test_model_equality(
            """
    variables: x
    minobjective: 1.2x + 0.5
""",
            ["x"],
            String[],
        )
    end
    @testset "max scalaraffine" begin
        test_model_equality(
            """
    variables: x
    maxobjective: 1.2x + 0.5
""",
            ["x"],
            String[],
            suffix = ".gz",
        )
    end
    @testset "singlevariable-in-lower" begin
        test_model_equality(
            """
    variables: x
    minobjective: 1.2x + 0.5
    c1: x >= 1.0
""",
            ["x"],
            ["c1"],
        )
    end
    @testset "singlevariable-in-upper" begin
        test_model_equality(
            """
    variables: x
    maxobjective: 1.2x + 0.5
    c1: x <= 1.0
""",
            ["x"],
            ["c1"],
            suffix = ".gz",
        )
    end
    @testset "singlevariable-in-interval" begin
        test_model_equality(
            """
    variables: x
    minobjective: 1.2x + 0.5
    c1: x in Interval(1.0, 2.0)
""",
            ["x"],
            ["c1"],
        )
    end
    @testset "singlevariable-in-equalto" begin
        test_model_equality(
            """
    variables: x
    minobjective: 1.2x + 0.5
    c1: x == 1.0
""",
            ["x"],
            ["c1"],
        )
    end
    @testset "singlevariable-in-zeroone" begin
        test_model_equality(
            """
    variables: x
    minobjective: 1.2x + 0.5
    c1: x in ZeroOne()
""",
            ["x"],
            ["c1"],
        )
    end
    @testset "singlevariable-in-integer" begin
        test_model_equality(
            """
    variables: x
    minobjective: 1.2x + 0.5
    c1: x in Integer()
""",
            ["x"],
            ["c1"],
        )
    end
    @testset "singlevariable-in-Semicontinuous" begin
        test_model_equality(
            """
    variables: x
    minobjective: 1.2x + 0.5
    c1: x in Semicontinuous(1.0, 2.0)
""",
            ["x"],
            ["c1"],
        )
    end
    @testset "singlevariable-in-Semiinteger" begin
        test_model_equality(
            """
    variables: x
    minobjective: 1.2x + 0.5
    c1: x in Semiinteger(1.0, 2.0)
""",
            ["x"],
            ["c1"],
        )
    end
    @testset "scalarquadratic-objective" begin
        test_model_equality(
            """
    variables: x
    minobjective: 1.0*x*x + -2.0x + 1.0
""",
            ["x"],
            String[],
        )
    end
    @testset "SOS1" begin
        test_model_equality(
            """
    variables: x, y, z
    minobjective: x
    c1: [x, y, z] in SOS1([1.0, 2.0, 3.0])
""",
            ["x", "y", "z"],
            ["c1"],
        )
    end
    @testset "SOS2" begin
        test_model_equality(
            """
    variables: x, y, z
    minobjective: x
    c1: [x, y, z] in SOS2([1.0, 2.0, 3.0])
""",
            ["x", "y", "z"],
            ["c1"],
        )
    end
    @testset "Reals" begin
        test_model_equality(
            """
    variables: x, y, z
    minobjective: x
    c1: [x, y, z] in Reals(3)
""",
            ["x", "y", "z"],
            ["c1"],
        )
    end
    @testset "Zeros" begin
        test_model_equality(
            """
    variables: x, y, z
    minobjective: x
    c1: [x, y, z] in Zeros(3)
""",
            ["x", "y", "z"],
            ["c1"],
        )
    end
    @testset "Nonnegatives" begin
        test_model_equality(
            """
    variables: x, y, z
    minobjective: x
    c1: [x, y, z] in Nonnegatives(3)
""",
            ["x", "y", "z"],
            ["c1"],
        )
    end
    @testset "Nonpositives" begin
        test_model_equality(
            """
    variables: x, y, z
    minobjective: x
    c1: [x, y, z] in Nonpositives(3)
""",
            ["x", "y", "z"],
            ["c1"],
        )
    end
    @testset "PowerCone" begin
        test_model_equality(
            """
    variables: x, y, z
    minobjective: x
    c1: [x, y, z] in PowerCone(2.0)
""",
            ["x", "y", "z"],
            ["c1"],
        )
    end
    @testset "DualPowerCone" begin
        test_model_equality(
            """
    variables: x, y, z
    minobjective: x
    c1: [x, y, z] in DualPowerCone(0.5)
""",
            ["x", "y", "z"],
            ["c1"],
        )
    end
    @testset "GeometricMeanCone" begin
        test_model_equality(
            """
    variables: x, y, z
    minobjective: x
    c1: [x, y, z] in GeometricMeanCone(3)
""",
            ["x", "y", "z"],
            ["c1"],
        )
    end
    @testset "Complements" begin
        test_model_equality(
            "variables: x, y\nc1: [x, y] in Complements(1)",
            ["x", "y"],
            ["c1"],
        )
    end
    @testset "vectoraffine-in-zeros" begin
        test_model_equality(
            """
    variables: x, y
    minobjective: x
    c1: [1.0x + -3.0, 2.0y + -4.0] in Zeros(2)
""",
            ["x", "y"],
            ["c1"],
        )
    end
    @testset "vectorquadratic-in-nonnegatives" begin
        test_model_equality(
            """
    variables: x, y
    minobjective: x
    c1: [1.0*x*x + -2.0x + 1.0, 2.0y + -4.0] in Nonnegatives(2)
""",
            ["x", "y"],
            ["c1"],
        )
    end
    @testset "ExponentialCone" begin
        test_model_equality(
            """
    variables: x, y, z
    minobjective: x
    c1: [x, y, z] in ExponentialCone()
""",
            ["x", "y", "z"],
            ["c1"],
        )
    end
    @testset "DualExponentialCone" begin
        test_model_equality(
            """
    variables: x, y, z
    minobjective: x
    c1: [x, y, z] in DualExponentialCone()
""",
            ["x", "y", "z"],
            ["c1"],
        )
    end
    @testset "SecondOrderCone" begin
        test_model_equality(
            """
    variables: x, y, z
    minobjective: x
    c1: [x, y, z] in SecondOrderCone(3)
""",
            ["x", "y", "z"],
            ["c1"],
        )
    end
    @testset "RotatedSecondOrderCone" begin
        test_model_equality(
            """
    variables: x, y, z
    minobjective: x
    c1: [x, y, z] in RotatedSecondOrderCone(3)
""",
            ["x", "y", "z"],
            ["c1"],
        )
    end
    @testset "PositiveSemidefiniteConeTriangle" begin
        test_model_equality(
            """
    variables: x1, x2, x3
    minobjective: x1
    c1: [x1, x2, x3] in PositiveSemidefiniteConeTriangle(2)
""",
            ["x1", "x2", "x3"],
            ["c1"],
        )
    end
    @testset "PositiveSemidefiniteConeSquare" begin
        test_model_equality(
            """
    variables: x1, x2, x3, x4
    minobjective: x1
    c1: [x1, x2, x3, x4] in PositiveSemidefiniteConeSquare(2)
""",
            ["x1", "x2", "x3", "x4"],
            ["c1"],
        )
    end
    @testset "LogDetConeTriangle" begin
        test_model_equality(
            """
    variables: t, u, x1, x2, x3
    minobjective: x1
    c1: [t, u, x1, x2, x3] in LogDetConeTriangle(2)
""",
            ["t", "u", "x1", "x2", "x3"],
            ["c1"],
        )
    end
    @testset "LogDetConeSquare" begin
        test_model_equality(
            """
    variables: t, u, x1, x2, x3, x4
    minobjective: x1
    c1: [t, u, x1, x2, x3, x4] in LogDetConeSquare(2)
""",
            ["t", "u", "x1", "x2", "x3", "x4"],
            ["c1"],
        )
    end
    @testset "RootDetConeTriangle" begin
        test_model_equality(
            """
    variables: t, x1, x2, x3
    minobjective: x1
    c1: [t, x1, x2, x3] in RootDetConeTriangle(2)
""",
            ["t", "x1", "x2", "x3"],
            ["c1"],
        )
    end
    @testset "RootDetConeSquare" begin
        test_model_equality(
            """
    variables: t, x1, x2, x3, x4
    minobjective: x1
    c1: [t, x1, x2, x3, x4] in RootDetConeSquare(2)
""",
            ["t", "x1", "x2", "x3", "x4"],
            ["c1"],
        )
    end
    @testset "IndicatorSet" begin
        test_model_equality(
            """
    variables: x, y
    minobjective: x
    c1: [x, y] in IndicatorSet{ACTIVATE_ON_ONE}(GreaterThan(1.0))
    c2: x >= 0.0
""",
            ["x", "y"],
            ["c1", "c2"],
        )

        test_model_equality(
            """
    variables: x, y
    minobjective: x
    c1: [x, y] in IndicatorSet{ACTIVATE_ON_ZERO}(GreaterThan(1.0))
    c2: x >= 0.0
""",
            ["x", "y"],
            ["c1", "c2"],
        )
    end
    @testset "NormOneCone" begin
        test_model_equality(
            """
    variables: x, y
    minobjective: x
    c1: [x, y] in NormOneCone(2)
    c2: x >= 0.0
""",
            ["x", "y"],
            ["c1", "c2"],
        )
    end
    @testset "NormInfinityCone" begin
        test_model_equality(
            """
    variables: x, y
    minobjective: x
    c1: [x, y] in NormInfinityCone(2)
    c2: x >= 0.0
""",
            ["x", "y"],
            ["c1", "c2"],
        )
    end
    @testset "RelativeEntropyCone" begin
        test_model_equality(
            """
    variables: x, y, z
    minobjective: x
    c1: [x, y, z] in RelativeEntropyCone(3)
    c2: x >= 0.0
""",
            ["x", "y", "z"],
            ["c1", "c2"],
        )
    end
    @testset "NormSpectralCone" begin
        test_model_equality(
            """
    variables: x, y, z
    minobjective: x
    c1: [x, y, z] in NormSpectralCone(1, 2)
""",
            ["x", "y", "z"],
            ["c1"],
        )
    end
    @testset "NormNuclearCone" begin
        test_model_equality(
            """
    variables: x, y, z
    minobjective: x
    c1: [x, y, z] in NormNuclearCone(1, 2)
""",
            ["x", "y", "z"],
            ["c1"],
        )
    end
    # Clean up
    sleep(1.0)  # allow time for unlink to happen
    rm(TEST_MOF_FILE, force = true)
    rm(TEST_MOF_FILE * ".gz", force = true)
end
