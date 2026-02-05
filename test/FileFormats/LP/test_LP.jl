# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestLP

using Test

import MathOptInterface as MOI
import MathOptInterface.FileFormats: LP

const LP_TEST_FILE = "test.lp"

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

function test_show()
    @test sprint(summary, LP.Model()) == "MOI.FileFormats.LP.Model"
    return
end

function test_comprehensive_write()
    model = LP.Model()
    MOI.Utilities.loadfromstring!(
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
c11: [x, y, z] in SOS1{Float64}([1.0, 2.0, 3.0])
c12: [x, y, z] in SOS2{Float64}([3.3, 1.1, 2.2])
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
          "a free\n" *
          "-1 <= x <= 2\n" *
          "y = 3\n" *
          "4 <= z <= 5\n" *
          "General\n" *
          "y\n" *
          "Binary\n" *
          "x\n" *
          "SOS\n" *
          "c11: S1:: x:1.0 y:2.0 z:3.0\n" *
          "c12: S2:: x:3.3 y:1.1 z:2.2\n" *
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
    MOI.Utilities.loadfromstring!(
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
    MOI.Utilities.loadfromstring!(
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

function test_free_variables_reading()
    for case in ["free", "Free", "FreE", "frEe"]
        io = IOBuffer("Minimize\nobj: x\nsubject to\nBounds\nx $case\nEnd")
        model = MOI.FileFormats.LP.Model()
        read!(io, model)
        out = IOBuffer()
        write(out, model)
        seekstart(out)
        file = read(out, String)
        @test occursin("Bounds\nx free\nEnd", file)
    end
    return
end

function test_integer_variables_reading()
    for case in ["general", "GeneRalS", "Integer", "InTegeRs"]
        io = IOBuffer("Minimize\nobj: x\nsubject to\n$case\nx\nEnd")
        model = MOI.FileFormats.LP.Model()
        read!(io, model)
        out = IOBuffer()
        write(out, model)
        seekstart(out)
        file = read(out, String)
        @test occursin("General\nx\nEnd", file)
    end
    return
end

function test_quadratic_objective_diag()
    model = LP.Model()
    MOI.Utilities.loadfromstring!(
        model,
        """
variables: x
minobjective: 1.0*x*x
""",
    )
    MOI.write_to_file(model, LP_TEST_FILE)
    @test read(LP_TEST_FILE, String) ==
          "minimize\n" *
          "obj: [ 2 x ^ 2 ]/2\n" *
          "subject to\n" *
          "Bounds\n" *
          "x free\n" *
          "End\n"
    return
end

function test_quadratic_objective_off_diag()
    model = LP.Model()
    MOI.Utilities.loadfromstring!(
        model,
        """
variables: x, y
minobjective: 1.1 * x + 1.2 * y + 1.5*x*y + 1.3
""",
    )
    MOI.write_to_file(model, LP_TEST_FILE)
    @test read(LP_TEST_FILE, String) ==
          "minimize\n" *
          "obj: 1.3 + 1.1 x + 1.2 y + [ 3 x * y ]/2\n" *
          "subject to\n" *
          "Bounds\n" *
          "x free\n" *
          "y free\n" *
          "End\n"
    return
end

function test_quadratic_objective_complicated()
    model = LP.Model()
    MOI.Utilities.loadfromstring!(
        model,
        """
variables: x, y
minobjective: 1.1 * x + 1.2 * y + -1.1 * x * x + 1.5*x*y + 1.3
""",
    )
    MOI.write_to_file(model, LP_TEST_FILE)
    @test read(LP_TEST_FILE, String) ==
          "minimize\n" *
          "obj: 1.3 + 1.1 x + 1.2 y + [ -2.2 x ^ 2 + 3 x * y ]/2\n" *
          "subject to\n" *
          "Bounds\n" *
          "x free\n" *
          "y free\n" *
          "End\n"
    return
end

function test_quadratic_constraint_diag()
    model = LP.Model()
    MOI.Utilities.loadfromstring!(
        model,
        """
variables: x
c: 1.0*x*x <= 1.4
""",
    )
    MOI.write_to_file(model, LP_TEST_FILE)
    @test read(LP_TEST_FILE, String) ==
          "minimize\n" *
          "obj: \n" *
          "subject to\n" *
          "c: [ 1 x ^ 2 ] <= 1.4\n" *
          "Bounds\n" *
          "x free\n" *
          "End\n"
    return
end

function test_quadratic_constraint_off_diag()
    model = LP.Model()
    MOI.Utilities.loadfromstring!(
        model,
        """
variables: x, y
c: 1.1 * x + 1.2 * y + 1.5*x*y + 1.3 == 1.5
""",
    )
    MOI.write_to_file(model, LP_TEST_FILE)
    @test read(LP_TEST_FILE, String) ==
          "minimize\n" *
          "obj: \n" *
          "subject to\n" *
          "c: 1.3 + 1.1 x + 1.2 y + [ 1.5 x * y ] = 1.5\n" *
          "Bounds\n" *
          "x free\n" *
          "y free\n" *
          "End\n"
    return
end

function test_quadratic_constraint_complicated()
    model = LP.Model()
    MOI.Utilities.loadfromstring!(
        model,
        """
variables: x, y
c: 1.1 * x + 1.2 * y + -1.1 * x * x + 1.5*x*y + 1.3 in Interval(-1.1, 1.4)
""",
    )
    MOI.write_to_file(model, LP_TEST_FILE)
    @test read(LP_TEST_FILE, String) ==
          "minimize\n" *
          "obj: \n" *
          "subject to\n" *
          "c: -1.1 <= 1.3 + 1.1 x + 1.2 y + [ -1.1 x ^ 2 + 1.5 x * y ] <= 1.4\n" *
          "Bounds\n" *
          "x free\n" *
          "y free\n" *
          "End\n"
    return
end

function test_write_indicator()
    model = LP.Model()
    MOI.Utilities.loadfromstring!(
        model,
        """
        variables: x, z
        c1: [z, x] in Indicator{ACTIVATE_ON_ONE}(LessThan(0.0))
        c2: [z, x] in Indicator{ACTIVATE_ON_ZERO}(GreaterThan(2.0))
        c3: [z, x] in Indicator{ACTIVATE_ON_ONE}(EqualTo(1.2))

        c4: [z, 2.0 * x] in Indicator{ACTIVATE_ON_ONE}(LessThan(0.0))
        c5: [z, 3.0 * x] in Indicator{ACTIVATE_ON_ZERO}(GreaterThan(2.0))
        c6: [1.0 * z, x] in Indicator{ACTIVATE_ON_ONE}(EqualTo(1.2))
        z in ZeroOne()
        """,
    )
    MOI.write_to_file(model, LP_TEST_FILE)
    @test read(LP_TEST_FILE, String) ==
          "minimize\n" *
          "obj: \n" *
          "subject to\n" *
          "c4:  z = 1 -> 2 x <= 0\n" *
          "c1: z = 1 -> x <= 0\n" *
          "c5:  z = 0 -> 3 x >= 2\n" *
          "c2: z = 0 -> x >= 2\n" *
          "c6:  z = 1 -> 1 x = 1.2\n" *
          "c3: z = 1 -> x = 1.2\n" *
          "Bounds\n" *
          "x free\n" *
          "Binary\n" *
          "z\n" *
          "End\n"
    return
end

###
### Read tests
###

macro test_parse_error(result, expr)
    return quote
        ret = try
            $(esc(expr))
        catch err
            sprint(showerror, err)
        end
        @test ret == $(esc(result))
    end
end

function test_read_invalid()
    dir = joinpath(@__DIR__, "models")
    @test_parse_error(
        """
        Error parsing LP file on line 7:
        C: 1 x <= 2
        ^
        Got an identifier with value `C`. No file contents are allowed after `end`.""",
        MOI.read_from_file(LP.Model(), joinpath(dir, "invalid_after_end.lp")),
    )
    @test_parse_error(
        """
        Error parsing LP file on line 6:
         x1 != 10
            ^
        Got an identifier with value `!`. We expected this to be an inequality like `>=`, `<=`, or `==`.""",
        MOI.read_from_file(LP.Model(), joinpath(dir, "invalid_bound_2.lp")),
    )
    @test_parse_error(
        """
        Error parsing LP file on line 6:
        c2:  2 x1 + x2 + c2: + 3 x3 + x4 >= 15
                           ^
        Got the symbol `:`. We expected this to be an inequality like `>=`, `<=`, or `==`.""",
        MOI.read_from_file(LP.Model(), joinpath(dir, "invalid_constraint.lp")),
    )
    @test_parse_error(
        """
        Error parsing LP file on line 10:
         csos2: S2::
                    ^
        Got a new line. SOS constraints cannot be spread across lines.""",
        MOI.read_from_file(
            LP.Model(),
            joinpath(dir, "invalid_sos_constraint.lp"),
        ),
    )
    @test_parse_error(
        """
        Error parsing LP file on line 10:
         csos2: S3:: V2:2 V4:1 V5:2.5
                ^
        Got an identifier with value `S3`. This must be either `S1` for SOS-I or `S2` for SOS-II.""",
        MOI.read_from_file(LP.Model(), joinpath(dir, "invalid_sos_set.lp")),
    )
    @test_parse_error(
        """
        Error parsing LP file on line 3:
        obj: 3 1x1 + x2 + 5 x3 + x4
               ^
        Got a number with value `1`. We expected this token to be a keyword defining a new section.""",
        MOI.read_from_file(
            LP.Model(),
            joinpath(dir, "invalid_variable_name.lp"),
        ),
    )
    return
end

function test_read_unexpected_line()
    io = IOBuffer("MinimizeSubject to x + y = 0")
    model = LP.Model()
    @test_parse_error(
        """
        Error parsing LP file on line 1:
        MinimizeSubject to x + y = 0
        ^
        Got an identifier with value `MinimizeSubject`. We expected this token to be a keyword defining a new section.""",
        read!(io, model),
    )
    return
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
    @test (MOI.VariableIndex, MOI.LessThan{Float64}) in constraints
    @test !((MOI.VariableIndex, MOI.Interval{Float64}) in constraints)
    io = IOBuffer()
    write(io, model)
    seekstart(io)
    file = read(io, String)
    @test occursin("maximize", file)
    @test occursin("obj: 3 x1 + 1 x2 + 5 x3 + 1 x4", file)
    @test occursin("c1: 3 x1 + 1 x2 + 2 x3 = 30", file)
    @test occursin("c2: 2 x1 + 1 x2 + 3 x3 + 1 x4 >= 15", file)
    @test occursin("c3: 2 x2 + 3 x4 <= 25", file)
    @test occursin("x1 >= 0", file)
    @test occursin("0 <= x2 <= 10", file)
    @test occursin("x3 >= 0", file)
    @test occursin("x4 >= 0", file)
    return
end

function test_read_model1_tricky()
    model = LP.Model()
    MOI.read_from_file(model, joinpath(@__DIR__, "models", "model1_tricky.lp"))
    @test MOI.get(model, MOI.NumberOfVariables()) == 8
    var_names = MOI.get.(model, MOI.VariableName(), MOI.VariableIndex.(1:8))
    @test Set(var_names) ==
          Set(["Var4", "V5", "V1", "V2", "V3", "V6", "V7", "V8"])
    io = IOBuffer()
    write(io, model)
    seekstart(io)
    file = read(io, String)
    @test occursin("maximize", file)
    @test occursin("obj: -1 Var4 + 1 V5 + [ 1 Var4 ^ 2 - 1.2 V5 * V1 ]/2", file)
    @test occursin("CON3: 1 V3 <= 2.5", file)
    @test occursin("CON4: 1 V5 + 1 V6 + 1 V7 <= 1", file)
    @test occursin("CON1: 1 V1 >= 0", file)
    @test occursin("CON5: [ 1 Var4 ^ 2 - 1.2 V5 * V1 ] <= 0", file)
    @test occursin("1 V2 >= 2", file)
    @test occursin("-infinity <= V1 <= 3", file)
    @test occursin("Var4 >= 5.5", file)
    @test occursin("V3 >= -3", file)
    @test occursin("V5 = 1", file)
    @test occursin("0 <= V2 <= 3", file)
    @test occursin("V6 free", file)
    @test occursin("0 <= V7 <= 1", file)
    @test occursin("0 <= V8 <= 1", file)
    @test occursin("\nVar4\n", file)
    @test occursin("\nV5\n", file)
    @test occursin("\nV6\n", file)
    @test occursin("Binary\nV8\n", file)
    @test occursin("sos1: S1:: V1:1.0 V2:2.0 V3:3.0", file)
    @test occursin("sos2: S2:: V1:8.5 V2:10.2 V3:18.3", file)
    return
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
    @test (MOI.VariableIndex, MOI.LessThan{Float64}) in constraints
    @test !((MOI.VariableIndex, MOI.Interval{Float64}) in constraints)
    @test (MOI.VariableIndex, MOI.Integer) in constraints
    @test (MOI.VariableIndex, MOI.ZeroOne) in constraints
    @test (MOI.VectorOfVariables, MOI.SOS1{Float64}) in constraints
    @test (MOI.VectorOfVariables, MOI.SOS2{Float64}) in constraints
    return
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
    @test (MOI.VariableIndex, MOI.LessThan{Float64}) in constraints
    @test !((MOI.VariableIndex, MOI.Interval{Float64}) in constraints)
    @test (MOI.VariableIndex, MOI.Integer) in constraints
    @test (MOI.VariableIndex, MOI.ZeroOne) in constraints
    @test MOI.get(model, MOI.VariableName(), MOI.VariableIndex(2)) == "V5"
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.LessThan{Float64}}(2)
    @test MOI.get(model, MOI.ConstraintSet(), ci) == MOI.LessThan(1.0)
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.GreaterThan{Float64}}(2)
    @test MOI.get(model, MOI.ConstraintSet(), ci) == MOI.GreaterThan(0.0)
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.Interval{Float64}}(2)
    @test !MOI.is_valid(model, ci)
    @test MOI.get(model, MOI.VariableName(), MOI.VariableIndex(8)) == "V8"
    @test model.variables.lower[8] == -Inf
    @test model.variables.upper[8] == -3
    obj_type = MOI.get(model, MOI.ObjectiveFunctionType())
    obj_func = MOI.get(model, MOI.ObjectiveFunction{obj_type}())
    @test obj_func.constant == 2.5
    return
end

function test_read_objective_sense()
    cases = Dict(
        "max" => MOI.MAX_SENSE,
        "maximize" => MOI.MAX_SENSE,
        "maximise" => MOI.MAX_SENSE,
        "maximum" => MOI.MAX_SENSE,
        "min" => MOI.MIN_SENSE,
        "minimize" => MOI.MIN_SENSE,
        "minimise" => MOI.MIN_SENSE,
        "minimum" => MOI.MIN_SENSE,
    )
    for (sense, result) in cases
        model = LP.Model()
        io = IOBuffer("$sense\nx")
        read!(io, model)
        @test MOI.get(model, MOI.ObjectiveSense()) == result
    end
    return
end

function test_read_nonempty_model()
    filename = joinpath(@__DIR__, "models", "model2.lp")
    model = LP.Model()
    MOI.read_from_file(model, filename)
    @test_throws(
        ErrorException("Cannot read in file because model is not empty."),
        MOI.read_from_file(model, filename),
    )
    return
end

function test_read_maximum_length_error()
    filename = joinpath(@__DIR__, "models", "model2.lp")
    model = LP.Model(; maximum_length = 1)
    contents = try
        MOI.read_from_file(model, filename)
    catch err
        sprint(showerror, err)
    end
    @test contents == """
    Error parsing LP file on line 2:
    obj: - 2 - 1 V4 + 1 V5 + 3 + 2 - 0.5
                 ^
    Got an identifier with value `V4`. Name (V4) exceeds maximum length (1)"""
    return
end

function test_default_bound()
    io = IOBuffer("minimize\nobj: x + y")
    model = LP.Model()
    MOI.read!(io, model)
    x = MOI.get(model, MOI.ListOfVariableIndices())
    F, S = MOI.VariableIndex, MOI.GreaterThan{Float64}
    c = [MOI.ConstraintIndex{F,S}(xi.value) for xi in x]
    sets = MOI.get.(model, MOI.ConstraintSet(), c)
    @test all(s -> s == MOI.GreaterThan(0.0), sets)
    @test length(sets) == 2
    return
end

function test_default_bound_double_bound()
    io = IOBuffer("minimize\nobj: x\nsubject to\nbounds\n x <= -1\n x >= -2")
    model = LP.Model()
    MOI.read!(io, model)
    x = first(MOI.get(model, MOI.ListOfVariableIndices()))
    F = MOI.VariableIndex
    @test MOI.get(
        model,
        MOI.ConstraintSet(),
        MOI.ConstraintIndex{F,MOI.GreaterThan{Float64}}(x.value),
    ) == MOI.GreaterThan(-2.0)
    @test MOI.get(
        model,
        MOI.ConstraintSet(),
        MOI.ConstraintIndex{F,MOI.LessThan{Float64}}(x.value),
    ) == MOI.LessThan(-1.0)
    return
end

function test_infinite_interval()
    model = LP.Model()
    x = MOI.add_variable(model)
    MOI.add_constraint(model, 1.0 * x, MOI.Interval(-Inf, Inf))
    MOI.add_constraint(model, 1.0 * x, MOI.Interval(-Inf, 1.0))
    MOI.add_constraint(model, 1.0 * x, MOI.Interval(2.0, Inf))
    MOI.add_constraint(model, 1.0 * x, MOI.Interval(3.0, 4.0))
    @test sprint(write, model) ==
          "minimize\n" *
          "obj: \n" *
          "subject to\n" *
          "c1: -inf <= 1 x1 <= inf\n" *
          "c2: -inf <= 1 x1 <= 1\n" *
          "c3: 2 <= 1 x1 <= inf\n" *
          "c4: 3 <= 1 x1 <= 4\n" *
          "Bounds\n" *
          "x1 free\n" *
          "End\n"

    return
end

function test_read_tricky_quadratic()
    model = LP.Model()
    filename = joinpath(@__DIR__, "models", "tricky_quadratic.lp")
    MOI.read_from_file(model, filename)
    io = IOBuffer()
    write(io, model)
    seekstart(io)
    file = read(io, String)
    @test occursin("minimize", file)
    @test occursin("obj: [ 2 x ^ 2 + 2 x * y ]/2", file)
    @test occursin("c1: [ 1 x ^ 2 - 1 x * y ] <= 0", file)
    @test occursin("c2: [ 0.5 x ^ 2 - 0.5 x * y ] <= 0", file)
    @test occursin("c3: [ 0.5 x ^ 2 - 0.5 x * y ] <= 0", file)
    @test occursin("x free", file)
    @test occursin("y free", file)
    return
end

function test_quadratic_newline_edge_cases()
    for case in [
        "+\n[ x^2 ]/2",
        "+ [\n x^2 ]/2",
        "+ [ x^2\n]/2",
        "+ [ x^2 ]\n/2",
        "+ [ x^2\n]\n/2",
        "+ \n[ x^2\n]\n/2",
        "+\n[ x^2 ]/2 \\ comment\n",
        "+ [ \\comment\n x^2 ]/2",
        "+ [ x^2\n]/2 \\ comment\n",
        "+ [ x^2 ]\n/2 \\ comment\n",
        "+ [ x^2\n]\n/2 \\ comment\n",
        "+ \n[ x^2\n]\n/2 \\ comment\n",
    ]
        io = IOBuffer("Minimize\nobj: x $(case)\nEnd")
        model = MOI.FileFormats.LP.Model()
        read!(io, model)
        out = IOBuffer()
        write(out, model)
        seekstart(out)
        file = read(out, String)
        @test occursin("obj: 1 x + [ 1 x ^ 2 ]/2", file)
    end
    for case in [
        "+\n[ x^2 ]",
        "+ [\n x^2 ]",
        "+ [ x^2\n]",
        "+ [ x^2 ]",
        "+ [ x^2\n]",
        "+ \n[ x^2\n]",
        "+\n[ x^2 ] \\ comment",
        "+ [ \\comment\n x^2 ]",
        "+ [ x^2\n] \\ comment",
        "+ [ x^2 ] \\ comment",
        "+ [ x^2\n] \\ comment",
        "+ \n[ x^2\n] \\ comment",
    ]
        io = IOBuffer("Minimize\nobj: x $(case)\nEnd")
        model = MOI.FileFormats.LP.Model()
        read!(io, model)
        out = IOBuffer()
        write(out, model)
        seekstart(out)
        file = read(out, String)
        @test occursin("obj: 1 x + [ 2 x ^ 2 ]/2", file)
    end
    return
end

function test_newline_inequality()
    for case in [
        "x + y <= 2",
        "x + y <=\n2",
        "x + y\n<= 2",
        "x +\n y <= 2",
        "x\n+ y <= 2",
    ]
        io = IOBuffer("Minimize\nobj: x\nSubject to\nc1: $(case)\nEnd")
        model = MOI.FileFormats.LP.Model()
        read!(io, model)
        out = IOBuffer()
        write(out, model)
        seekstart(out)
        file = read(out, String)
        @test occursin("c1: 1 x + 1 y <= 2", file)
    end
    return
end

function test_wrong_way_bounds()
    for (case, result) in [
        "x >= 2" => "x >= 2",
        "x <= 2" => "0 <= x <= 2",
        "x == 2" => "x = 2",
        "x > 2" => "x >= 2",
        "x < 2" => "0 <= x <= 2",
        "x = 2" => "x = 2",
        "2 >= x" => "0 <= x <= 2",
        "2 <= x" => "x >= 2",
        "2 == x" => "x = 2",
        "2 > x" => "0 <= x <= 2",
        "2 < x" => "x >= 2",
        "2 = x" => "x = 2",
    ]
        io = IOBuffer("Minimize\nobj: x\nSubject to\nBounds\n$(case)\nEnd")
        model = MOI.FileFormats.LP.Model()
        read!(io, model)
        out = IOBuffer()
        write(out, model)
        seekstart(out)
        file = read(out, String)
        @test occursin("Bounds\n$result", file)
    end
    return
end

function test_variable_coefficient_variable()
    io = IOBuffer("Minimize\nobj: x -1 y\nEnd")
    model = MOI.FileFormats.LP.Model()
    read!(io, model)
    out = IOBuffer()
    write(out, model)
    seekstart(out)
    file = read(out, String)
    @test file ==
          "minimize\nobj: 1 x - 1 y\nsubject to\nBounds\nx >= 0\ny >= 0\nEnd\n"
    return
end

function _test_round_trip(bound, needle)
    model = MOI.FileFormats.LP.Model()
    io = IOBuffer()
    write(io, "Minimize\nobj: x\nBounds\n$bound\nEnd")
    seekstart(io)
    read!(io, model)
    seekstart(io)
    write(io, model)
    seekstart(io)
    file = read(io, String)
    @test occursin(needle, file)
    return
end

function test_reading_bounds()
    # Test lower bound
    _test_round_trip("x >= 1", "Bounds\nx >= 1\nEnd")
    _test_round_trip("x >= 0", "Bounds\nx >= 0\nEnd")
    _test_round_trip("x >= -1", "Bounds\nx >= -1\nEnd")
    _test_round_trip("x > 1", "Bounds\nx >= 1\nEnd")
    _test_round_trip("x > 0", "Bounds\nx >= 0\nEnd")
    _test_round_trip("x > -1", "Bounds\nx >= -1\nEnd")
    # Test reversed lower bound
    _test_round_trip("1 <= x", "Bounds\nx >= 1\nEnd")
    _test_round_trip("0 <= x", "Bounds\nx >= 0\nEnd")
    _test_round_trip("-1 <= x", "Bounds\nx >= -1\nEnd")
    _test_round_trip("1 < x", "Bounds\nx >= 1\nEnd")
    _test_round_trip("0 < x", "Bounds\nx >= 0\nEnd")
    _test_round_trip("-1 < x", "Bounds\nx >= -1\nEnd")
    # Test upper bound
    _test_round_trip("x <= 1", "Bounds\n0 <= x <= 1\nEnd")
    _test_round_trip("x <= 0", "Bounds\nx = 0\nEnd")
    _test_round_trip("x <= -1", "Bounds\n-infinity <= x <= -1\nEnd")
    _test_round_trip("x < 1", "Bounds\n0 <= x <= 1\nEnd")
    _test_round_trip("x < 0", "Bounds\nx = 0\nEnd")
    _test_round_trip("x < -1", "Bounds\n-infinity <= x <= -1\nEnd")
    # Test reversed upper bound
    _test_round_trip("1 >= x", "Bounds\n0 <= x <= 1\nEnd")
    _test_round_trip("0 >= x", "Bounds\nx = 0\nEnd")
    _test_round_trip("-1 >= x", "Bounds\n-infinity <= x <= -1\nEnd")
    _test_round_trip("1 > x", "Bounds\n0 <= x <= 1\nEnd")
    _test_round_trip("0 > x", "Bounds\nx = 0\nEnd")
    _test_round_trip("-1 > x", "Bounds\n-infinity <= x <= -1\nEnd")
    # Test equality
    _test_round_trip("x == 1", "Bounds\nx = 1\nEnd")
    _test_round_trip("x == 0", "Bounds\nx = 0\nEnd")
    _test_round_trip("x == -1", "Bounds\nx = -1\nEnd")
    _test_round_trip("1 = x", "Bounds\nx = 1\nEnd")
    _test_round_trip("0 = x", "Bounds\nx = 0\nEnd")
    _test_round_trip("-1 = x", "Bounds\nx = -1\nEnd")
    # Test interval
    _test_round_trip("0 <= x <= 1", "Bounds\n0 <= x <= 1\nEnd")
    _test_round_trip("-1 <= x <= 1", "Bounds\n-1 <= x <= 1\nEnd")
    _test_round_trip("-2 <= x <= -1", "Bounds\n-2 <= x <= -1\nEnd")
    # Test reversed interval
    _test_round_trip("1 >= x >= 0", "Bounds\n0 <= x <= 1\nEnd")
    _test_round_trip("1 >= x >= -1", "Bounds\n-1 <= x <= 1\nEnd")
    _test_round_trip("-1 >= x >= -2", "Bounds\n-2 <= x <= -1\nEnd")
    # Test double-sided equality
    _test_round_trip("1 <= x <= 1", "Bounds\nx = 1\nEnd")
    _test_round_trip("0 <= x <= 0", "Bounds\nx = 0\nEnd")
    _test_round_trip("-2 <= x <= -2", "Bounds\nx = -2\nEnd")
    # Test upper then lower
    _test_round_trip("x <= 1\nx >= 0", "Bounds\n0 <= x <= 1\nEnd")
    _test_round_trip("x <= 2\nx >= 1", "Bounds\n1 <= x <= 2\nEnd")
    _test_round_trip("x <= 2\nx >= -1", "Bounds\n-1 <= x <= 2\nEnd")
    # Test lower then upper
    _test_round_trip("x >= 0\nx <= 1", "Bounds\n0 <= x <= 1\nEnd")
    _test_round_trip("x >= 1\nx <= 2", "Bounds\n1 <= x <= 2\nEnd")
    _test_round_trip("x >= -1\nx <= 2", "Bounds\n-1 <= x <= 2\nEnd")
    return
end

function _test_read_quadratic(input::String, output::String = input)
    io = IOBuffer()
    input_text = """
    minimize
    obj: $input
    subject to
    Bounds
    x >= 0
    y >= 0
    End
    """
    write(io, input_text)
    model = MOI.FileFormats.LP.Model()
    seekstart(io)
    read!(io, model)
    out = IOBuffer()
    write(out, model)
    seekstart(out)
    output_text = """
    minimize
    obj: $output
    subject to
    Bounds
    x >= 0
    y >= 0
    End
    """
    @test read(out, String) == output_text
    return
end

function test_read_quadratic()
    _test_read_quadratic("1 x + 1 y + [ 1 x * y + 1 y ^ 2 ]/2")
    _test_read_quadratic("1 x + 1 y + [ 2 x * y + 1 y ^ 2 ]/2")
    _test_read_quadratic("1 x + 1 y + [ 1 x * y + 2 y ^ 2 ]/2")
    _test_read_quadratic("1 x + 1 y + [ 2 x * y + 2 y ^ 2 ]/2")
    _test_read_quadratic(
        "1 x + 1 y + [ 1 x * y + 1 y ^ 2 ]",
        "1 x + 1 y + [ 2 x * y + 2 y ^ 2 ]/2",
    )
    _test_read_quadratic(
        "1 x + 1 y + [ 2 x * y + 1 y ^ 2 ]",
        "1 x + 1 y + [ 4 x * y + 2 y ^ 2 ]/2",
    )
    _test_read_quadratic(
        "1 x + 1 y + [ 1 x * y + 2 y ^ 2 ]",
        "1 x + 1 y + [ 2 x * y + 4 y ^ 2 ]/2",
    )
    _test_read_quadratic(
        "1 x + 1 y + [ 2 x * y + 2 y ^ 2 ]",
        "1 x + 1 y + [ 4 x * y + 4 y ^ 2 ]/2",
    )
    _test_read_quadratic(
        "[ 1.0 x^2 \n+ 1.0 x * y\n+ 1.0 y * y ]/2",
        "[ 1 x ^ 2 + 1 x * y + 1 y ^ 2 ]/2",
    )
    _test_read_quadratic(
        "[ 1.0 x^2 \n- 1.0 x * y\n+ 1.0 y * y ]/2",
        "[ 1 x ^ 2 - 1 x * y + 1 y ^ 2 ]/2",
    )
    _test_read_quadratic(
        "[ 1.0 x^2 -\n 1.0 x * y +\n1.0 y * y ]/2",
        "[ 1 x ^ 2 - 1 x * y + 1 y ^ 2 ]/2",
    )
    _test_read_quadratic(
        "[ 1.0 x^2 + 1.0 x * y\n+ 1.0 y * y ]/2",
        "[ 1 x ^ 2 + 1 x * y + 1 y ^ 2 ]/2",
    )
    return
end

function test_read_newline_breaks()
    io = IOBuffer()
    input_text = """
    minimize
    obj: 1 x + 2 y
        + 3 z
    subject to
    c: x -
    y
    + z ==
    0
    Bounds
    x >= 0 -1 <= y
    +1 <= z <= +2
    End
    """
    write(io, input_text)
    model = MOI.FileFormats.LP.Model()
    seekstart(io)
    read!(io, model)
    out = IOBuffer()
    write(out, model)
    seekstart(out)
    output_text = """
    minimize
    obj: 1 x + 2 y + 3 z
    subject to
    c: 1 x - 1 y + 1 z = 0
    Bounds
    x >= 0
    y >= -1
    1 <= z <= 2
    End
    """
    @test read(out, String) == output_text
    return
end

function test_read_variable_bounds()
    io = IOBuffer("""
    maximize
    obj: 1 x1
    subject to
    bounds
    -infinity <= x1 <= +infinity
    -infinity <= x2 <= 1
    -infinity <= x3 <= -1
    -1 <= x4 <= +infinity
    1 <= x5 <= +infinity
    -1 <= x6 <= 1
    1 <= x7 <= 1
    end
    """)
    model = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_LP)
    read!(io, model)
    io = IOBuffer()
    write(io, model)
    seekstart(io)
    @test read(io, String) == """
    maximize
    obj: 1 x1
    subject to
    Bounds
    x1 free
    -infinity <= x2 <= 1
    -infinity <= x3 <= -1
    x4 >= -1
    x5 >= 1
    -1 <= x6 <= 1
    x7 = 1
    End
    """
    return
end

function test_read_indicator()
    io = IOBuffer("""
    minimize
    obj: 1 x
    subject to
    c: z = 1 -> x >= 0
    d: z = 0 -> x - y <= 1.2
    bounds
    x free
    z free
    binary
    z
    end
    """)
    model = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_LP)
    read!(io, model)
    io = IOBuffer()
    write(io, model)
    seekstart(io)
    @test read(io, String) == """
    minimize
    obj: 1 x
    subject to
    d:  z = 0 -> 1 x - 1 y <= 1.2
    c:  z = 1 -> 1 x >= 0
    Bounds
    x free
    y >= 0
    Binary
    z
    End
    """
    return
end

function test_VectorAffineFunction_SOS()
    model = MOI.FileFormats.LP.Model()
    F = MOI.VectorAffineFunction{Float64}
    @test !MOI.supports_constraint(model, F, MOI.SOS1{Float64})
    @test !MOI.supports_constraint(model, F, MOI.SOS2{Float64})
    return
end

function test_invalid_token_in_sos()
    model = LP.Model()
    io = IOBuffer("""
                  minimize
                  obj: x + y
                  subject to
                  SOS
                  c11: S1:: x 1.0 y 2.0
                  """)
    contents = try
        read!(io, model)
    catch err
        sprint(showerror, err)
    end
    @test contents == """
    Error parsing LP file on line 5:
    c11: S1:: x 1.0 y 2.0
                ^
    Got a number with value `1.0`. We expected this token to be the symbol `:`."""
    return
end

function test_unable_to_parse_bound()
    io = IOBuffer("""
                  minimize
                  obj: 1 x
                  subject to
                  bounds
                  x
                  end
                  """)
    model = LP.Model()
    @test_parse_error(
        """
        Error parsing LP file on line 6:
        end
        ^
        Got a keyword defining a new section with value `END`. We expected this to be an inequality like `>=`, `<=`, or `==`.""",
        read!(io, model),
    )
    return
end

function test_unsupported_variable_types()
    model = LP.Model()
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

function _test_int_round_trip(src)
    model = MOI.FileFormats.LP.Model(; coefficient_type = Int)
    io = IOBuffer()
    write(io, src)
    seekstart(io)
    read!(io, model)
    seekstart(io)
    write(io, model)
    seekstart(io)
    file = read(io, String)
    @test file == src
    return
end

function test_int_round_trip()
    for src in [
        """
        minimize
        obj: 1 + 2 x
        subject to
        c: 3 x >= 2
        Bounds
        x >= 0
        End
        """,
        """
        minimize
        obj: 1 x + [ 1 x ^ 2 ]/2
        subject to
        c: [ 1 x ^ 2 ] <= 1
        Bounds
        x >= 0
        End
        """,
        """
        minimize
        obj: [ 2 x ^ 2 ]/2
        subject to
        c: [ 2 x ^ 2 ] <= 1
        Bounds
        x >= 0
        End
        """,
        """
        minimize
        obj: [ 3 x ^ 2 + 4 x * y + 5 y ^ 2 ]/2
        subject to
        c: [ 3 x ^ 2 + 4 x * y + 5 y ^ 2 ] <= 1
        Bounds
        x >= 0
        y >= 1
        End
        """,
    ]
        _test_int_round_trip(src)
    end
    return
end

function test_unsupported_objectives()
    model = LP.Model()
    for (F, ret) in [
        MOI.VariableIndex => true,
        MOI.ScalarAffineFunction{Float64} => true,
        MOI.ScalarQuadraticFunction{Float64} => true,
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

function test_subject_to_name()
    for (case, err) in [
        "subject to" => false,
        "Subject To" => false,
        "such that" => false,
        "Such That" => false,
        "st" => false,
        "s.t." => false,
        "st." => false,
        "subject that" => true,
        "subject\nto" => true,
        "s. t." => true,
        "such to" => true,
    ]
        io = IOBuffer("Minimize\nobj: x\n$case\n2x == 1\nBounds\nx free\nEnd")
        seekstart(io)
        model = MOI.FileFormats.LP.Model()
        if err
            @test_parse_error(
                """
                Error parsing LP file on line 3:
                $(first(split(case, "\n")))
                ^
                Got an identifier with value `$(first(split(case)))`. We expected this token to be a keyword defining a new section.""",
                read!(io, model),
            )
        else
            read!(io, model)
            out = IOBuffer()
            write(out, model)
            seekstart(out)
            file = read(out, String)
            @test occursin("subject to\nc1: 2 x = 1\n", file)
        end
    end
    return
end

function test_parse_identifier()
    cache = LP._ReadCache(LP.Model{Float64}())
    for input in [
        "x",
        "X",
        "e",
        "abc!\"D",
        "π",
        "𝔼1π!~a",
        "x!\"#\$%&()/,.;?@_`'{}|~",
        "aAc2",
    ]
        io = IOBuffer(input)
        state = LP._LexerState(io)
        x = LP._parse_identifier(state, cache)
        @test cache.variable_name_to_index[input] == x
    end
    for input in ["2", "2x"]
        state = LP._LexerState(IOBuffer(input))
        @test_parse_error(
            """
            Error parsing LP file on line 1:
            $(input)
            ^
            Got a number with value `2`. We expected this token to be an identifier.""",
            LP._parse_identifier(state, cache),
        )
    end
    state = LP._LexerState(IOBuffer(".x"))
    @test_parse_error(
        """
        Error parsing LP file on line 1:
        .x
        ^
        Got a token with value `.`. This character is not supported at the start of an identifier.""",
        LP._parse_identifier(state, cache),
    )
    state = LP._LexerState(IOBuffer("❤x"))
    @test_parse_error(
        """
        Error parsing LP file on line 1:
        ❤x
        ^
        Got a token with value `❤`. This character is not supported in an LP file.""",
        LP._parse_identifier(state, cache),
    )
    return
end

function test_parse_number()
    cache = LP._ReadCache(LP.Model{Float64}())
    for (input, result) in [
        "1" => 1.0,
        "02" => 2.0,
        "- 1" => -1.0,
        "- -1" => 1.0,
        "+ 1" => 1.0,
        "+ -1" => -1.0,
        "- + 1" => -1.0,
        "+ + 1" => 1.0,
        "+ - + 1" => -1.0,
        "+ - + -1" => 1.0,
        "inf" => Inf,
        "-inf" => -Inf,
        "- inf" => - Inf,
        "iNf" => Inf,
        "iNfinitY" => Inf,
        "infinity" => Inf,
        "1.23e+01" => 12.3,
        "1.23e-1" => 0.123,
        "1.23E-1" => 0.123,
        "1.23E+3" => 1230.0,
    ]
        io = IOBuffer(input)
        state = LP._LexerState(io)
        @test LP._parse_number(state, cache) == result
    end
    for (input, reason) in [
        "x" => "We expected this to be a number.",
        "abc" => "We expected this to be a number.",
        "ten" => "We expected this to be a number.",
        "1.1.1" => "We were unable to parse this as a number.",
        "1eE1" => "We were unable to parse this as a number.",
    ]
        io = IOBuffer(input)
        state = LP._LexerState(io)
        @test_parse_error(
            """
            Error parsing LP file on line 1:
            $input
            ^
            Got an identifier with value `$input`. $reason""",
            LP._parse_number(state, cache),
        )
    end
    return
end

function test_parse_quad_term()
    cache = LP._ReadCache(LP.Model{Float64}())
    # Diagonal
    for (input, coef) in [
        "x * x" => 2.0,
        "\nx * x" => 2.0,
        "x\n * x" => 2.0,
        "x * \n x" => 2.0,
        "x^2" => 2.0,
        "x ^ 2" => 2.0,
        "+ x * x" => 2.0,
        "+ 2 * x * x" => 4.0,
        "- x * x" => -2.0,
        "- 2 * x * x" => -4.0,
        "-2 x * x" => -4.0,
        "2.2 x * x" => 4.4,
    ]
        io = IOBuffer(input)
        state = LP._LexerState(io)
        term = LP._parse_quadratic_term(state, cache, 1.0)
        x = cache.variable_name_to_index["x"]
        @test term == MOI.ScalarQuadraticTerm(coef, x, x)
        seekstart(io)
        term = LP._parse_quadratic_term(state, cache, -1.0)
        @test term == MOI.ScalarQuadraticTerm(-coef, x, x)
    end
    # Off-diagonal
    for (input, coef) in [
        "x * y" => 1.0,
        "\nx * y" => 1.0,
        "x\n * y" => 1.0,
        "x * \n y" => 1.0,
        "+ x * y" => 1.0,
        "+ 2 * x * y" => 2.0,
        "- x * y" => -1.0,
        "- 2 * x * y" => -2.0,
        "2.2 * x * y" => 2.2,
        "2.2 x * y" => 2.2,
    ]
        io = IOBuffer(input)
        state = LP._LexerState(io)
        term = LP._parse_quadratic_term(state, cache, 1.0)
        x = cache.variable_name_to_index["x"]
        y = cache.variable_name_to_index["y"]
        @test term == MOI.ScalarQuadraticTerm(coef, x, y)
        seekstart(io)
        term = LP._parse_quadratic_term(state, cache, -1.0)
        @test term == MOI.ScalarQuadraticTerm(-coef, x, y)
    end
    for (input, reason) in [
        "x^" => " ^\nGot a token with value `EOF`. Unexpected end to the file. We weren't finished yet.",
        "x^x" => "  ^\nGot an identifier with value `x`. We expected this token to be a number.",
        "x^0" => "  ^\nGot a number with value `0`. Only `^ 2` is supported.",
        "x^1" => "  ^\nGot a number with value `1`. Only `^ 2` is supported.",
        "x^3" => "  ^\nGot a number with value `3`. Only `^ 2` is supported.",
        "x * 2  * x" => "    ^\nGot a number with value `2`. We expected this token to be an identifier.",
    ]
        io = IOBuffer(input)
        state = LP._LexerState(io)
        @test_parse_error(
            """
            Error parsing LP file on line 1:
            $input
            $reason""",
            LP._parse_quadratic_term(state, cache, -1.0),
        )
    end
    return
end

function test_parse_term()
    cache = LP._ReadCache(LP.Model{Float64}())
    for (input, coef) in [
        "x" => 1.0,
        "+ x" => 1.0,
        "- x" => -1.0,
        "- -x" => 1.0,
        "+ -x" => -1.0,
        "2x" => 2.0,
        "2.0 x" => 2.0,
        "3.0 x" => 3.0,
        "2.0 * x" => 2.0,
        "3.2 * x" => 3.2,
    ]
        io = IOBuffer(input)
        state = LP._LexerState(io)
        term = LP._parse_term(state, cache, 1.0)
        x = cache.variable_name_to_index["x"]
        @test term == MOI.ScalarAffineTerm(coef, x)
        seekstart(io)
        term = LP._parse_term(state, cache, -1.0)
        @test term == MOI.ScalarAffineTerm(-coef, x)
    end
    for (input, reason) in [">= 1" => "Got the symbol `>=`."]
        io = IOBuffer(input)
        state = LP._LexerState(io)
        @test_parse_error(
            """
            Error parsing LP file on line 1:
            $input
            ^
            $reason We expected this to be a new term in the expression.""",
            LP._parse_term(state, cache, 1.0),
        )
    end
    return
end

function test_parse_quad_expression()
    cache = LP._ReadCache(LP.Model{Float64}())
    for (input, reason) in [
        "x^2" => "^\nGot an identifier with value `x`. We expected this token to be the symbol `[`.",
        "[ x^2 ]/" => "       ^\nGot a token with value `EOF`. Unexpected end to the file. We weren't finished yet.",
        "[ x^2 ]/3" => "        ^\nGot a number with value `3`. The only supported value here is `] / 2`.",
    ]
        io = IOBuffer(input)
        state = LP._LexerState(io)
        @test_parse_error(
            """
            Error parsing LP file on line 1:
            $input
            $reason""",
            LP._parse_quadratic_expression(state, cache, 1.0),
        )
    end
    return
end

function test_parse_set_prefix()
    cache = LP._ReadCache(LP.Model{Float64}())
    for (input, set) in [
        "1.0 <=" => MOI.GreaterThan(1.0),
        "1.0 <" => MOI.GreaterThan(1.0),
        "1.0 >=" => MOI.LessThan(1.0),
        "1.0 >" => MOI.LessThan(1.0),
        "1.0 ==" => MOI.EqualTo(1.0),
        "1.0 =" => MOI.EqualTo(1.0),
        # Theirs not to reason why, theirs but to do and
        "1.0 =<" => MOI.GreaterThan(1.0),
        "1.0 =>" => MOI.LessThan(1.0),
    ]
        io = IOBuffer(input)
        state = LP._LexerState(io)
        @test LP._parse_set_prefix(state, cache) == set
    end
    io = IOBuffer("1.0 ->")
    state = LP._LexerState(io)
    @test_parse_error(
        """
        Error parsing LP file on line 1:
        1.0 ->
            ^
        Got the symbol `->`. We expected this to be an inequality like `>=`, `<=`, or `==`.""",
        LP._parse_set_prefix(state, cache),
    )
    return
end

function test_parse_set_sufffix()
    cache = LP._ReadCache(LP.Model{Float64}())
    for (input, set) in [
        "free" => nothing,
        "Free" => nothing,
        ">= 1.0" => MOI.GreaterThan(1.0),
        "> 1.0" => MOI.GreaterThan(1.0),
        "<= 1.0" => MOI.LessThan(1.0),
        "< 1.0" => MOI.LessThan(1.0),
        "== 1.0" => MOI.EqualTo(1.0),
        "= 1.0" => MOI.EqualTo(1.0),
        # Theirs not to reason why, theirs but to do and
        "=< 1.0" => MOI.LessThan(1.0),
        "=> 1.0" => MOI.GreaterThan(1.0),
    ]
        io = IOBuffer(input)
        state = LP._LexerState(io)
        @test LP._parse_set_suffix(state, cache) == set
    end
    io = IOBuffer("-> 1")
    state = LP._LexerState(io)
    @test_parse_error(
        """
        Error parsing LP file on line 1:
        -> 1
        ^
        Got the symbol `->`. We expected this to be an inequality like `>=`, `<=`, or `==`.""",
        LP._parse_set_suffix(state, cache),
    )
    return
end

function test_new_line_edge_cases()
    target = "minimize\nobj: 1 x + 1 y\nsubject to\nc: 1 x <= 1\nBounds\nx >= 0\ny >= 0\nEnd\n"
    for input in [
        # Good
        "minimize\nobj: x + y\nsubject to\nc: x <= 1\nend",
        # Blank lines at the start
        "\n\nminimize\nobj: x + y\nsubject to\nc: x <= 1\nend",
        # Blank lines after minimize
        "minimize\n\n\nobj: x + y\nsubject to\nc: x <= 1\nend",
        # Blank lines between obj: and expression
        "minimize\nobj:\n\nx + y\nsubject to\nc: x <= 1\nend",
        # Blank lines throughout objective expression
        "minimize\nobj:\nx \n+ \ny\nsubject to\nc: x <= 1\nend",
        # Blank lines after objective expression
        "minimize\nobj: x + y\n\nsubject to\nc: x <= 1\nend",
        # Blank lines after subject to
        "minimize\nobj: x + y\nsubject to\n\n\nc: x <= 1\nend",
        # Blank lines in constraint
        "minimize\nobj: x + y\nsubject to\nc:\nx\n<=\n1\nend",
        # Blank lines around end
        "minimize\nobj: x + y\nsubject to\nc: x <= 1\n\nend\n\n\n",
        # Comment before newline
        "minimize\\comment\nobj: x + y\nsubject to\nc: x <= 1\nend",
        "minimize\nobj: x + y\\comment\nsubject to\nc: x <= 1\nend",
        "minimize\nobj: x + y\nsubject to\\comment\nc: x <= 1\nend",
        "minimize\nobj: x + y\nsubject to\nc: x <= 1\\comment\nend",
        "minimize\nobj: x + y\nsubject to\nc: x <= 1\nend\\comment",
    ]
        io = IOBuffer(input)
        model = LP.Model()
        MOI.read!(io, model)
        @test sprint(MOI.write, model) == target
    end
    return
end

function test_new_line_edge_cases_sos()
    target = "minimize\nobj: 1 x + 1 y\nsubject to\nBounds\nx >= 0\ny >= 0\nSOS\nc: S1:: x:1.0 y:2.0\nEnd\n"
    for input in [
        "minimize\nobj: x + y\nsubject to\nc: S1:: x:1 y:2\nend",
        "minimize\nobj: x + y\nsubject to\nc: S1:: x:1 y:2\n\nend",
        "minimize\nobj: x + y\nsubject to\n\nc: S1:: x:1 y:2\n\nend",
        "minimize\nobj: x + y\nsubject to\n\nc: S1:: x:1 y:2\\comment\nend",
    ]
        io = IOBuffer(input)
        model = LP.Model()
        MOI.read!(io, model)
        @test sprint(MOI.write, model) == target
    end
    return
end

function test_missing_new_line_edge_cases()
    for input in [
        # No newline between objective sense and objective
        "minimize x",
        "maximize x",
        "maximize c: x",
        # No new line between objective and subject to
        "maximize\nobj: x subject to",
        # No new line between subject to and constraint
        "maximize\nobj: x\nsubject to c: x >= 0",
    ]
        io = IOBuffer(input)
        model = LP.Model()
        MOI.read!(io, model)
        @test MOI.get(model, MOI.VariableIndex, "x") isa MOI.VariableIndex
    end
    return
end

function test_parse_keyword_edge_cases_identifier_is_keyword()
    for name in ["max", "min", "st", "such", "bounds", "obj", "free"]
        io = IOBuffer("""
        maximize
        obj: $name
        subject to
        $name <= 1
        bounds
        $name free
        end
        """)
        model = LP.Model()
        MOI.read!(io, model)
        x = only(MOI.get(model, MOI.ListOfVariableIndices()))
        @test MOI.get(model, MOI.VariableName(), x) == name
    end
    return
end

function test_parse_keyword_subject_to_errors()
    for line in ["subject", "subject too"]
        io = IOBuffer("""
        maximize
        obj: x
        $line
        x <= 1
        bounds
        x free
        end
        """)
        model = LP.Model()
        @test_parse_error(
            """
            Error parsing LP file on line 3:
            $line
            ^
            Got an identifier with value `subject`. We expected this token to be a keyword defining a new section.""",
            MOI.read!(io, model),
        )
    end
    return
end

function test_parse_newline_in_objective_expression()
    for obj in ["2 x", "\n2 x", "2\nx", "2*\nx", "2\n*x", "2\n\n*\n\n\nx\n"]
        io = IOBuffer("""
        maximize
        obj: $obj
        subject to
        bounds
        x free
        end
        """)
        model = LP.Model()
        MOI.read!(io, model)
        x = MOI.get(model, MOI.VariableIndex, "x")
        f = 2.0 * x
        g = MOI.get(model, MOI.ObjectiveFunction{typeof(f)}())
        @test isapprox(f, g)
    end
    return
end

function test_parse_subject_eof()
    io = IOBuffer("maximize\nobj:\nsubject")
    model = LP.Model()
    MOI.read!(io, model)
    x = MOI.get(model, MOI.VariableIndex, "subject")
    @test x isa MOI.VariableIndex
    return
end

function test_parse_expr_eof()
    io = IOBuffer("maximize\nobj: x + 2\n")
    model = LP.Model()
    MOI.read!(io, model)
    x = MOI.get(model, MOI.VariableIndex, "x")
    f = 1.0 * x + 2.0
    g = MOI.get(model, MOI.ObjectiveFunction{typeof(f)}())
    @test isapprox(f, g)
    return
end

function test_parse_quadratic_expr_eof()
    io = IOBuffer("maximize\nobj: [x * x]\n")
    model = LP.Model()
    MOI.read!(io, model)
    x = MOI.get(model, MOI.VariableIndex, "x")
    f = 1.0 * x * x
    g = MOI.get(model, MOI.ObjectiveFunction{typeof(f)}())
    @test isapprox(f, g)
    return
end

function test_ambiguous_case_1()
    # Xpress allows this. We currently do too.
    for kw in ("subject to", "such that", "st")
        io = IOBuffer("maximize obj: x $kw c: x <= 1\nend")
        model = LP.Model()
        MOI.read!(io, model)
        @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
        F, S = MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}
        @test isa(
            MOI.get(model, MOI.ConstraintIndex, "c"),
            MOI.ConstraintIndex{F,S},
        )
    end
    return
end

function test_ambiguous_case_2()
    # Xpress allows this. We currently parse this as two "<keyword-constraint>"
    # sections, and think that there is no objective function.
    io = IOBuffer("min\nst\nst\nst >= 0\nend")
    model = LP.Model()
    MOI.read!(io, model)
    st = MOI.get(model, MOI.VariableIndex, "st")
    @test_broken MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    f = 1.0 * st
    @test_broken isapprox(MOI.get(model, MOI.ObjectiveFunction{typeof(f)}()), f)
    return
end

function test_ambiguous_case_3()
    # Gurobi doesn't allow this, but Xpress does. We do.
    io = IOBuffer("min\nobj: end\nsubject to\nc: end <= 1\nend")
    model = LP.Model()
    MOI.read!(io, model)
    x = MOI.get(model, MOI.VariableIndex, "end")
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    f = 1.0 * x
    @test isapprox(MOI.get(model, MOI.ObjectiveFunction{typeof(f)}()), f)
    return
end

function test_get_line_about_pos()
    for (input, output) in [
        "x" => ("x", 1),
        "\nx" => ("x", 1),
        "x\n" => ("x", 1),
        "x + y\n" => ("x + y", 1),
        "\ny + x\n" => ("y + x", 5),
        "\n❤ + x\n" => ("❤ + x", 5),
        "♡♡♡♡♡♡♡♡♡♡ + x + ♡♡♡♡♡♡♡♡♡♡" => ("♡♡ + x + ♡♡♡", 6),
        "♡♡♡♡♡♡♡♡♡\n♡ + x + ♡\n♡♡♡♡♡♡♡♡♡" => ("♡ + x + ♡", 5),
    ]
        io = IOBuffer(input)
        readuntil(io, 'x')
        pos = position(io)
        @test output == LP._get_line_about_pos(IOBuffer(input), pos, 10)
    end
    return
end

end  # module

TestLP.runtests()
