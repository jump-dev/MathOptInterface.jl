module TestLP

import MathOptInterface
using Test

const MOI = MathOptInterface
const MOIU = MOI.Utilities
const LP = MOI.FileFormats.LP
const LP_TEST_FILE = "test.lp"

function test_show()
    @test sprint(show, LP.Model()) == "A .LP-file model"
end

function test_comprehensive_write()
    model = LP.Model()
    MOIU.loadfromstring!(
        model,
        """
variables: a, x, y, z
minobjective: x
x >= -1.0
x <= 2.0
y == 3.0
z in Interval(4.0, 5.0)
c5: 1.1x + 0.0 <= 5.1
c6: 1.3x + -1.4 >= -0.1
c7: 1.5a + 1.6 == 0.2
c8: 1.7a + 1.8 in Interval(0.3, 0.4)
x in ZeroOne()
y in Integer()
""",
    )
    MOI.write_to_file(model, LP_TEST_FILE)
    @test read(LP_TEST_FILE, String) ==
          "minimize\n" *
          "obj: x\n" *
          "subject to\n" *
          "c5: 1.1 x <= 5.1\n" *
          "c6: -1.4 + 1.3 x >= -0.1\n" *
          "c7: 1.6 + 1.5 a = 0.2\n" *
          "c8: 0.3 <= 1.8 + 1.7 a <= 0.4\n" *
          "Bounds\n" *
          "x <= 2\n" *
          "x >= -1\n" *
          "y = 3\n" *
          "4 <= z <= 5\n" *
          "a free\n" *
          "General\n" *
          "y\n" *
          "Binary\n" *
          "x\n" *
          "End\n"

    @test !MOI.is_empty(model)
    MOI.empty!(model)
    @test MOI.is_empty(model)
end

function test_name_sanitization_start()
    for starting_letter in [".", "0", "E", "e"]
        model = LP.Model()
        MOI.Utilities.loadfromstring!(
            model,
            """
variables: x
minobjective: x
c1: 1.0 * x >= -1.0
""",
        )
        x = MOI.get(model, MOI.VariableIndex, "x")
        MOI.set(model, MOI.VariableName(), x, starting_letter * "x")
        c = MOI.get(model, MOI.ConstraintIndex, "c1")
        MOI.set(model, MOI.ConstraintName(), c, starting_letter * "c1")
        MOI.write_to_file(model, LP_TEST_FILE)
        @test read(LP_TEST_FILE, String) ==
              "minimize\n" *
              "obj: _$(starting_letter)x\n" *
              "subject to\n" *
              "_$(starting_letter)c1: 1 _$(starting_letter)x >= -1\n" *
              "Bounds\n" *
              "_$(starting_letter)x free\n" *
              "End\n"
    end
end

function test_name_sanitization_illegal()
    for illegal_letter in ["[", "]", "*", "^"]
        model = LP.Model()
        MOI.Utilities.loadfromstring!(
            model,
            """
variables: x
minobjective: x
c1: 1.0 * x >= -1.0
""",
        )
        x = MOI.get(model, MOI.VariableIndex, "x")
        MOI.set(model, MOI.VariableName(), x, "x$(illegal_letter)y")
        c = MOI.get(model, MOI.ConstraintIndex, "c1")
        MOI.set(model, MOI.ConstraintName(), c, "c$(illegal_letter)d")
        MOI.write_to_file(model, LP_TEST_FILE)
        @test read(LP_TEST_FILE, String) ==
              "minimize\n" *
              "obj: x_y\n" *
              "subject to\n" *
              "c_d: 1 x_y >= -1\n" *
              "Bounds\n" *
              "x_y free\n" *
              "End\n"
    end
end

function test_name_sanitization_duplicate()
    model = LP.Model()
    MOI.Utilities.loadfromstring!(
        model,
        """
variables: a, b, c, d
minobjective: a + b + c + d
c1: a + b + c + d >= -1.0
""",
    )
    variables = MOI.get(model, MOI.ListOfVariableIndices())
    MOI.set.(model, MOI.VariableName(), variables, ["a[", "a]", "a*", "a^"])
    MOI.write_to_file(model, LP_TEST_FILE)
    @test read(LP_TEST_FILE, String) ==
          "minimize\n" *
          "obj: 1 a_ + 1 a__1 + 1 a__2 + 1 a__3\n" *
          "subject to\n" *
          "c1: 1 a_ + 1 a__1 + 1 a__2 + 1 a__3 >= -1\n" *
          "Bounds\n" *
          "a_ free\n" *
          "a__1 free\n" *
          "a__2 free\n" *
          "a__3 free\n" *
          "End\n"
end

function test_name_sanitization_length()
    model = LP.Model(maximum_length = 3)
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "abcdefg")
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    MOI.write_to_file(model, LP_TEST_FILE)
    @test read(LP_TEST_FILE, String) ==
          "minimize\n" *
          "obj: abc\n" *
          "subject to\n" *
          "Bounds\n" *
          "abc free\n" *
          "End\n"
end

function test_name_sanitization_too_long()
    model = LP.Model(maximum_length = 3)
    MOI.Utilities.loadfromstring!(
        model,
        """
variables: abcd, abce
minobjective: abcd + abce
c1: abcd + abce >= -1.0
""",
    )
    MOI.write_to_file(model, LP_TEST_FILE)
    @test read(LP_TEST_FILE, String) ==
          "minimize\n" *
          "obj: 1 abc + 1 abc_1\n" *
          "subject to\n" *
          "c1: 1 abc + 1 abc_1 >= -1\n" *
          "Bounds\n" *
          "abc free\n" *
          "abc_1 free\n" *
          "End\n"
end

function test_name_sanitization_other()
    model = LP.Model()
    MOIU.loadfromstring!(
        model,
        """
variables: x
maxobjective: 2.0 * x + -1.0
""",
    )
    MOI.write_to_file(model, LP_TEST_FILE)
    @test read(LP_TEST_FILE, String) ==
          "maximize\n" *
          "obj: -1 + 2 x\n" *
          "subject to\n" *
          "Bounds\n" *
          "x free\n" *
          "End\n"
end

function test_free_variables()
    model = LP.Model()
    MOIU.loadfromstring!(
        model,
        """
variables: x, y, z
maxobjective: x
x in ZeroOne()
y in Integer()
""",
    )
    MOI.write_to_file(model, LP_TEST_FILE)
    @test read(LP_TEST_FILE, String) ==
          "maximize\n" *
          "obj: x\n" *
          "subject to\n" *
          "Bounds\n" *
          "y free\n" *
          "z free\n" *
          "General\n" *
          "y\n" *
          "Binary\n" *
          "x\n" *
          "End\n"
end

function test_quadratic_objective()
    model = LP.Model()
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

function test_read_example_lo1()
    model = LP.Model()
    MOI.read_from_file(model, joinpath(@__DIR__, "models", "example_lo1.lp"))
    @test MOI.get(model, MOI.NumberOfVariables()) == 4
    constraints = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) in
          constraints
    @test (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) in
          constraints
    @test (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) in
          constraints
    @test (MOI.VariableIndex, MOI.GreaterThan{Float64}) in constraints
    @test (MOI.VariableIndex, MOI.Interval{Float64}) in constraints
    return nothing
end

function test_read_model2()
    model = LP.Model()
    MOI.read_from_file(model, joinpath(@__DIR__, "models", "model2.lp"))
    @test MOI.get(model, MOI.NumberOfVariables()) == 8
    constraints = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) in
          constraints
    @test (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) in
          constraints
    @test (MOI.VariableIndex, MOI.GreaterThan{Float64}) in constraints
    @test (MOI.VariableIndex, MOI.Interval{Float64}) in constraints
    @test (MathOptInterface.VariableIndex, MathOptInterface.Integer) in
          constraints
    @test (MathOptInterface.VariableIndex, MathOptInterface.ZeroOne) in
          constraints
    # Adicionar testes dos bounds de V8
    @test MOI.get(model, MOI.VariableName(), MOI.VariableIndex(8)) == "V8"
    @test model.variables.lower[8] == -Inf
    @test model.variables.upper[8] == -3
    obj_type = MOI.get(model, MOI.ObjectiveFunctionType())
    obj_func = MOI.get(model, MOI.ObjectiveFunction{obj_type}())
    @test obj_func.constant == 2.5
    return nothing
end

function test_read_model1_tricky()
    model = LP.Model()
    MOI.read_from_file(model, joinpath(@__DIR__, "models", "model1_tricky.lp"))
    @test MOI.get(model, MOI.NumberOfVariables()) == 8
    var_names = MOI.get.(model, MOI.VariableName(), MOI.VariableIndex.(1:8))
    @test Set(var_names) ==
          Set(["Var4", "V5", "V1", "V2", "V3", "V6", "V7", "V8"])
    return nothing
end

function test_read_corrupt()
    model = LP.Model()
    @test_throws ErrorException MOI.read_from_file(
        model,
        joinpath(@__DIR__, "models", "corrupt.lp"),
    )
    return nothing
end

function test_read_invalid_variable_name()
    model = LP.Model()
    @test_throws ErrorException MOI.read_from_file(
        model,
        joinpath(@__DIR__, "models", "invalid_variable_name.lp"),
    )
    return nothing
end

function test_read_invalid_affine_term_objective()
    model = LP.Model()
    @test_throws ErrorException MOI.read_from_file(
        model,
        joinpath(@__DIR__, "models", "invalid_affine_term_objective.lp"),
    )
    return nothing
end

function test_read_invalid_affine_term_constraint()
    model = LP.Model()
    @test_throws ErrorException MOI.read_from_file(
        model,
        joinpath(@__DIR__, "models", "invalid_affine_term_constraint.lp"),
    )
    return nothing
end

function test_read_invalid_sos_set()
    model = LP.Model()
    @test_throws ErrorException MOI.read_from_file(
        model,
        joinpath(@__DIR__, "models", "invalid_sos_set.lp"),
    )
    return nothing
end

function test_read_invalid_sos_constraint()
    model = LP.Model()
    @test_throws ErrorException MOI.read_from_file(
        model,
        joinpath(@__DIR__, "models", "invalid_sos_constraint.lp"),
    )
    return nothing
end

function test_read_invalid_bound()
    model = LP.Model()
    @test_throws ErrorException MOI.read_from_file(
        model,
        joinpath(@__DIR__, "models", "invalid_bound.lp"),
    )
    return nothing
end

function test_read_invalid_constraint()
    model = LP.Model()
    @test_throws ErrorException MOI.read_from_file(
        model,
        joinpath(@__DIR__, "models", "invalid_constraint.lp"),
    )
    return nothing
end

function test_read_model1()
    model = LP.Model()
    MOI.read_from_file(model, joinpath(@__DIR__, "models", "model1.lp"))
    constraints = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) in
          constraints
    @test (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) in
          constraints
    @test (MOI.VariableIndex, MOI.GreaterThan{Float64}) in constraints
    @test (MOI.VariableIndex, MOI.Interval{Float64}) in constraints
    @test (MathOptInterface.VariableIndex, MathOptInterface.Integer) in
          constraints
    @test (MathOptInterface.VariableIndex, MathOptInterface.ZeroOne) in
          constraints
    @test (
        MathOptInterface.VectorOfVariables,
        MathOptInterface.SOS1{Float64},
    ) in constraints
    @test (
        MathOptInterface.VectorOfVariables,
        MathOptInterface.SOS2{Float64},
    ) in constraints
    return nothing
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
    rm(LP_TEST_FILE, force = true)
    return
end

end

TestLP.runtests()
