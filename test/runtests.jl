using MathOptFormat, Compat.Test

const MOI = MathOptFormat.MOI
const MOIU = MathOptFormat.MOIU

struct UnsupportedSet <: MOI.AbstractSet end
struct UnsupportedFunction <: MOI.AbstractFunction end

function test_model_equality(model_string, variables, constraints)
    model = MathOptFormat.Model()
    MOIU.loadfromstring!(model, model_string)
    MOI.write_to_file(model, "test.mof.json")
    model_2 = MathOptFormat.Model()
    MOI.read_from_file(model_2, "test.mof.json")
    MOIU.test_models_equal(model, model_2, variables, constraints)
end

@testset "Error handling: read_from_file" begin
    @testset "non-empty_model" begin
        model = MathOptFormat.Model()
        MOI.add_variable(model)
        @test_throws Exception MOI.read_from_file(model,
            "models/empty_model.mof.json")
    end
    @testset "unsupported_constraint_set" begin
        @test_throws Exception MOI.read_from_file(MathOptFormat.Model(),
            "models/unsupported_constraint_set.mof.json")
    end
    @testset "unsupported_constraint_function" begin
        @test_throws Exception MOI.read_from_file(MathOptFormat.Model(),
            "models/unsupported_constraint_function.mof.json")
    end
    @testset "unsupported_objective_function" begin
        @test_throws Exception MOI.read_from_file(MathOptFormat.Model(),
            "models/unsupported_objective_function.mof.json")
    end
    @testset "unsupported_objective_sense" begin
        @test_throws Exception MOI.read_from_file(MathOptFormat.Model(),
            "models/unsupported_objective_sense.mof.json")
    end
    @testset "multiple_objectives" begin
        @test_throws Exception MOI.read_from_file(MathOptFormat.Model(),
            "models/multiple_objectives.mof.json")
    end
    @testset "blank_variable_name" begin
        @test_throws Exception MOI.read_from_file(MathOptFormat.Model(),
            "models/blank_variable_name.mof.json")
    end
    @testset "missing_variable_name" begin
        @test_throws Exception MOI.read_from_file(MathOptFormat.Model(),
            "models/missing_variable_name.mof.json")
    end
end

@testset "round trips" begin
    @testset "Empty model" begin
        model = MathOptFormat.Model()
        MOI.write_to_file(model, "test.mof.json")
        model_2 = MathOptFormat.Model()
        MOI.read_from_file(model_2, "test.mof.json")
        MOIU.test_models_equal(model, model_2, String[], String[])
    end
    @testset "min objective" begin
        test_model_equality("""
            variables: x
            minobjective: x
        """, ["x"], String[])
    end
    @testset "max objective" begin
        test_model_equality("""
            variables: x
            maxobjective: x
        """, ["x"], String[])
    end
    @testset "min scalaraffine" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x + 0.5
        """, ["x"], String[])
    end
    @testset "max scalaraffine" begin
        test_model_equality("""
            variables: x
            maxobjective: 1.2x + 0.5
        """, ["x"], String[])
    end
    @testset "singlevariable-in-lower" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x + 0.5
            c1: x >= 1.0
        """, ["x"], ["c1"])
    end
    @testset "singlevariable-in-upper" begin
        test_model_equality("""
            variables: x
            maxobjective: 1.2x + 0.5
            c1: x <= 1.0
        """, ["x"], ["c1"])
    end
    @testset "singlevariable-in-interval" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x + 0.5
            c1: x in Interval(1.0, 2.0)
        """, ["x"], ["c1"])
    end
    @testset "singlevariable-in-equalto" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x + 0.5
            c1: x == 1.0
        """, ["x"], ["c1"])
    end
    @testset "singlevariable-in-zeroone" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x + 0.5
            c1: x in ZeroOne()
        """, ["x"], ["c1"])
    end
    @testset "singlevariable-in-integer" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x + 0.5
            c1: x in Integer()
        """, ["x"], ["c1"])
    end
    @testset "singlevariable-in-Semicontinuous" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x + 0.5
            c1: x in Semicontinuous(1.0, 2.0)
        """, ["x"], ["c1"])
    end
    @testset "singlevariable-in-Semiinteger" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x + 0.5
            c1: x in Semiinteger(1.0, 2.0)
        """, ["x"], ["c1"])
    end
    @testset "scalarquadratic-objective" begin
        test_model_equality("""
            variables: x
            minobjective: 1.0*x*x + -2.0x + 1.0
        """, ["x"], String[])
    end
    @testset "SOS1" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in SOS1([1.0, 2.0, 3.0])
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "SOS2" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in SOS2([1.0, 2.0, 3.0])
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "Reals" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in Reals(3)
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "Zeros" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in Zeros(3)
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "Nonnegatives" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in Nonnegatives(3)
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "Nonpositives" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in Nonpositives(3)
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "PowerCone" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in PowerCone(2.0)
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "DualPowerCone" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in DualPowerCone(0.5)
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "GeometricMeanCone" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in GeometricMeanCone(3)
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "vectoraffine-in-zeros" begin
        test_model_equality("""
            variables: x, y
            minobjective: x
            c1: [1.0x + -3.0, 2.0y + -4.0] in Zeros(2)
        """, ["x", "y"], ["c1"])
    end
    @testset "vectorquadratic-in-nonnegatives" begin
        test_model_equality("""
            variables: x, y
            minobjective: x
            c1: [1.0*x*x + -2.0x + 1.0, 2.0y + -4.0] in Nonnegatives(2)
        """, ["x", "y"], ["c1"])
    end
    @testset "ExponentialCone" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in ExponentialCone()
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "DualExponentialCone" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in DualExponentialCone()
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "SecondOrderCone" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in SecondOrderCone(3)
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "RotatedSecondOrderCone" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in RotatedSecondOrderCone(3)
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "PositiveSemidefiniteConeTriangle" begin
        test_model_equality("""
            variables: x1, x2, x3
            minobjective: x1
            c1: [x1, x2, x3] in PositiveSemidefiniteConeTriangle(2)
        """, ["x1", "x2", "x3"], ["c1"])
    end
    @testset "PositiveSemidefiniteConeSquare" begin
        test_model_equality("""
            variables: x1, x2, x3, x4
            minobjective: x1
            c1: [x1, x2, x3, x4] in PositiveSemidefiniteConeSquare(2)
        """, ["x1", "x2", "x3", "x4"], ["c1"])
    end
    @testset "LogDetConeTriangle" begin
        test_model_equality("""
            variables: t, x1, x2, x3
            minobjective: x1
            c1: [t, x1, x2, x3] in LogDetConeTriangle(2)
        """, ["t", "x1", "x2", "x3"], ["c1"])
    end
    @testset "LogDetConeSquare" begin
        test_model_equality("""
            variables: t, x1, x2, x3, x4
            minobjective: x1
            c1: [t, x1, x2, x3, x4] in LogDetConeSquare(2)
        """, ["t", "x1", "x2", "x3", "x4"], ["c1"])
    end
    @testset "RootDetConeTriangle" begin
        test_model_equality("""
            variables: t, x1, x2, x3
            minobjective: x1
            c1: [t, x1, x2, x3] in RootDetConeTriangle(2)
        """, ["t", "x1", "x2", "x3"], ["c1"])
    end
    @testset "RootDetConeSquare" begin
        test_model_equality("""
            variables: t, x1, x2, x3, x4
            minobjective: x1
            c1: [t, x1, x2, x3, x4] in RootDetConeSquare(2)
        """, ["t", "x1", "x2", "x3", "x4"], ["c1"])
    end

    # Clean up
    sleep(1.0)  # allow time for unlink to happen
    rm("test.mof.json", force=true)
end
