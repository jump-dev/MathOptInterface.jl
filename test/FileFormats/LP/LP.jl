# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestLP

import MathOptInterface
using Test

const MOI = MathOptInterface
const LP = MOI.FileFormats.LP
const LP_TEST_FILE = "test.lp"

function test_show()
    @test sprint(show, LP.Model()) == "A .LP-file model"
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
          "x <= 2\n" *
          "x >= -1\n" *
          "y = 3\n" *
          "4 <= z <= 5\n" *
          "a free\n" *
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

function test_quadratic_objective()
    model = LP.Model()
    @test_throws(
        MOI.UnsupportedAttribute,
        MOI.Utilities.loadfromstring!(
            model,
            """
variables: x
minobjective: 1.0*x*x
""",
        )
    )
end

###
### Read tests
###

function test_read_invalid()
    models = joinpath(@__DIR__, "models")
    for filename in filter(f -> startswith(f, "invalid_"), readdir(models))
        model = LP.Model()
        @test_throws(
            ErrorException,
            MOI.read_from_file(model, joinpath(models, filename)),
        )
    end
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
    @test (MOI.VariableIndex, MOI.Interval{Float64}) in constraints
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
    @test occursin("obj: -1 Var4 + 1 V5", file)
    @test occursin("CON3: 1 V3 <= 2.5", file)
    @test occursin("CON4: 1 V5 + 1 V6 + 1 V7 <= 1", file)
    @test occursin("CON1: 1 V1 >= 0", file)
    @test occursin("R1: 1 V2 >= 2", file)
    @test occursin("V1 <= 3", file)
    @test occursin("Var4 >= 5.5", file)
    @test occursin("V3 >= -3", file)
    @test occursin("V5 = 1", file)
    @test occursin("0 <= V2 <= 3", file)
    @test occursin("0 <= V7 <= 1", file)
    @test occursin("0 <= V8 <= 1", file)
    @test occursin("V6 free", file)
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
    return
end

function test_read_objective_sense()
    model = LP.Model()
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
        LP._set_objective_sense(LP._KW_OBJECTIVE, model, sense)
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
    @test_throws(
        ErrorException("Name exceeds maximum length: V4"),
        MOI.read_from_file(model, filename),
    )
    return
end

function test_default_bound()
    io = IOBuffer()
    write(io, "minimize\nobj: x + y")
    seekstart(io)
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
    io = IOBuffer()
    write(io, "minimize\nobj: x\nsubject to\nbounds\n x <= -1\n x >= -2")
    seekstart(io)
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
