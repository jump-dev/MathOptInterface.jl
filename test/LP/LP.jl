const LP = MathOptFormat.LP

const LP_TEST_FILE = "test.lp"

@test sprint(show, LP.Model()) == "A .LP-file model"

@testset "write_to_file" begin
    @testset "comprehensive write" begin
        model = LP.Model()
        MOIU.loadfromstring!(model, """
        variables: a, x, y, z
        minobjective: x
        c1: x >= -1.0
        c2: x <= 2.0
        c3: y == 3.0
        c4: z in Interval(4.0, 5.0)
        c5: 1.1x + 0.0 <= 5.1
        c6: 1.3x + -1.4 >= -0.1
        c7: 1.5a + 1.6 == 0.2
        c8: 1.7a + 1.8 in Interval(0.3, 0.4)
        c9: x in ZeroOne()
        c10: y in Integer()
        """)
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
            "General\n" *
            "y\n" *
            "Binary\n" *
            "x\n" *
            "End\n"

        @test !MOI.is_empty(model)
        MOI.empty!(model)
        @test MOI.is_empty(model)
    end

    @testset "Name sanitisation" begin
        @testset "sanitized_name" begin
            max_length = 15
            o = LP.Options(max_length, true, false, Set{Char}(), Set{Char}())

            @test LP.sanitized_name("x", o) == "x"
            @test LP.sanitized_name(repeat("x", max_length), o) == repeat("x", max_length)

            too_long = repeat("x", max_length + 1)
            @test (@test_logs (:warn, "Name $(too_long) too long (length: $(length(too_long)); maximum: $(max_length)). Truncating.") LP.sanitized_name(too_long, o)) == repeat("x", max_length)

            @test (@test_logs (:warn, "Name .x cannot start with a period, a number, e, or E. Prepending an underscore to name.") LP.sanitized_name(".x", o)) == "_.x"
            @test (@test_logs (:warn, "Name 0x cannot start with a period, a number, e, or E. Prepending an underscore to name.") LP.sanitized_name("0x", o)) == "_0x"
            @test (@test_logs (:warn, "Name Ex cannot start with a period, a number, e, or E. Prepending an underscore to name.") LP.sanitized_name("Ex", o)) == "_Ex"
            @test (@test_logs (:warn, "Name ex cannot start with a period, a number, e, or E. Prepending an underscore to name.") LP.sanitized_name("ex", o)) == "_ex"

            @test (@test_logs (:warn, "Name x*ds contains an illegal character: \"*\". Removing the offending character from name.") LP.sanitized_name("x*ds", o)) == "x_ds"
            @test (@test_logs (:warn, "Name x^ contains an illegal character: \"^\". Removing the offending character from name.") LP.sanitized_name("x^", o)) == "x_"
            @test (@test_logs (:warn, "Name x*ds[1] contains an illegal character: \"*\". Removing the offending character from name.") LP.sanitized_name("x*ds[1]", o)) == "x_ds_1_"
            @test (@test_logs(
                    (:warn, "Name Ex*ds[1] cannot start with a period, a number, e, or E. Prepending an underscore to name."),
                    (:warn, "Name _Ex*ds[1] contains an illegal character: \"*\". Removing the offending character from name."),
                    LP.sanitized_name("Ex*ds[1]", o))
                ) == "_Ex_ds_1_"
        end

        @testset "Whole chain" begin
            model = LP.Model(warn=true)
            MOIU.loadfromstring!(model, """
            variables: a
            minobjective: a
            c1: a >= -1.0
            """)
            MOI.set(model, MOI.VariableName(), MOI.get(model, MOI.ListOfVariableIndices())[1], "a[]")

            @test_logs (:warn, "Name a[] contains an illegal character: \"[\". Removing the offending character from name.") MOI.write_to_file(model, LP_TEST_FILE)
        end

        @testset "Warn multiple times" begin
            model = LP.Model(warn=true)
            MOIU.loadfromstring!(model, """
            variables: a, b, c
            minobjective: a + b + c
            c1: a + b + c >= -1.0
            """)
            MOI.set(model, MOI.VariableName(), MOI.get(model, MOI.ListOfVariableIndices())[1], "a[]")
            MOI.set(model, MOI.VariableName(), MOI.get(model, MOI.ListOfVariableIndices())[2], "b[]")
            MOI.set(model, MOI.VariableName(), MOI.get(model, MOI.ListOfVariableIndices())[3], "c[]")

            @test_logs(
                (:warn, "Name a[] contains an illegal character: \"[\". Removing the offending character from name."),
                (:warn, "Name b[] contains an illegal character: \"[\". Removing the offending character from name."),
                (:warn, "Name c[] contains an illegal character: \"[\". Removing the offending character from name."),
                MOI.write_to_file(model, LP_TEST_FILE))
        end

        @testset "Warn once" begin
            model = LP.Model(warn_once=true)
            MOIU.loadfromstring!(model, """
            variables: a, b, c
            minobjective: a + b + c
            c1: a + b + c >= -1.0
            """)
            MOI.set(model, MOI.VariableName(), MOI.get(model, MOI.ListOfVariableIndices())[1], "a[]")
            MOI.set(model, MOI.VariableName(), MOI.get(model, MOI.ListOfVariableIndices())[2], "b[]")
            MOI.set(model, MOI.VariableName(), MOI.get(model, MOI.ListOfVariableIndices())[3], "c[]")

            @test_logs (:warn, "Name a[] contains an illegal character: \"[\". Removing the offending character from name.") MOI.write_to_file(model, LP_TEST_FILE)
        end

        @testset "Duplicate names after sanitization" begin
            model = LP.Model(warn=true)
            MOIU.loadfromstring!(model, """
            variables: a, b, c, d
            minobjective: a + b + c + d
            c1: a + b + c + d >= -1.0
            """)
            MOI.set(model, MOI.VariableName(), MOI.get(model, MOI.ListOfVariableIndices())[1], "a[")
            MOI.set(model, MOI.VariableName(), MOI.get(model, MOI.ListOfVariableIndices())[2], "a]")
            MOI.set(model, MOI.VariableName(), MOI.get(model, MOI.ListOfVariableIndices())[3], "a*")
            MOI.set(model, MOI.VariableName(), MOI.get(model, MOI.ListOfVariableIndices())[4], "a^")

            @test_logs (:warn, "Name a[ contains an illegal character: \"[\". Removing the offending character from name.") (:warn, "Name a] contains an illegal character: \"]\". Removing the offending character from name.") (:warn, "Name a* contains an illegal character: \"*\". Removing the offending character from name.") (:warn, "Name a^ contains an illegal character: \"^\". Removing the offending character from name.") MOI.write_to_file(model, LP_TEST_FILE)
            @test read(LP_TEST_FILE, String) ==
                "minimize\n" *
                "obj: 1 a_ + 1 a__1 + 1 a__2 + 1 a__3\n" *
                "subject to\n" *
                "c1: 1 a_ + 1 a__1 + 1 a__2 + 1 a__3 >= -1\n" *
                "Bounds\n" *
                "End\n"
        end

        @testset "Too long duplicate names after sanitization" begin
            max_length = 15

            model = LP.Model(maximum_length=max_length, warn=true)
            MOIU.loadfromstring!(model, """
            variables: a, b, c, d
            minobjective: a + b + c + d
            c1: a + b + c + d >= -1.0
            """)
            MOI.set(model, MOI.VariableName(), MOI.get(model, MOI.ListOfVariableIndices())[1], "a[")
            MOI.set(model, MOI.VariableName(), MOI.get(model, MOI.ListOfVariableIndices())[2], "a]")
            MOI.set(model, MOI.VariableName(), MOI.get(model, MOI.ListOfVariableIndices())[3], "a*" * repeat("x", max_length + 1))
            MOI.set(model, MOI.VariableName(), MOI.get(model, MOI.ListOfVariableIndices())[4], "a^" * repeat("x", max_length + 1))

            @test_logs(
                (:warn, "Name a[ contains an illegal character: \"[\". Removing the offending character from name."),
                (:warn, "Name a] contains an illegal character: \"]\". Removing the offending character from name."),
                (:warn, "Name a*$(repeat("x", max_length + 1)) contains an illegal character: \"*\". Removing the offending character from name."),
                (:warn, "Name a_$(repeat("x", max_length + 1)) too long (length: $(2 + max_length + 1); maximum: $(max_length)). Truncating."),
                (:warn, "Name a^$(repeat("x", max_length + 1)) contains an illegal character: \"^\". Removing the offending character from name."),
                (:warn, "Name a_$(repeat("x", max_length + 1)) too long (length: $(2 + max_length + 1); maximum: $(max_length)). Truncating."),
                MOI.write_to_file(model, LP_TEST_FILE))
            @test read(LP_TEST_FILE, String) ==
                "minimize\n" *
                "obj: 1 a_ + 1 a__1 + 1 a_$(repeat("x", max_length - 2)) + 1 a_$(repeat("x", max_length - 4))_1\n" *
                "subject to\n" *
                "c1: 1 a_ + 1 a__1 + 1 a_$(repeat("x", max_length - 2)) + 1 a_$(repeat("x", max_length - 4))_1 >= -1\n" *
                "Bounds\n" *
                "End\n"
        end

        @testset "Many too long duplicate names after sanitization" begin
            max_length = 6

            model = LP.Model(maximum_length=max_length, warn=true)
            MOIU.loadfromstring!(model, """
            variables: a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r
            minobjective: a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r
            c1: a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r >= -1.0
            """)
            for i in 1:18
                MOI.set(model, MOI.VariableName(), MOI.get(model, MOI.ListOfVariableIndices())[i], repeat("x", max_length + 1))
            end

            @test_logs(
                (:warn, "Duplicate name xxxxxxx detected for variable MathOptInterface.VariableIndex(2). Renamed to xxxxxxx_1."),
                (:warn, "Duplicate name xxxxxxx detected for variable MathOptInterface.VariableIndex(3). Renamed to xxxxxxx_2."),
                (:warn, "Duplicate name xxxxxxx detected for variable MathOptInterface.VariableIndex(4). Renamed to xxxxxxx_3."),
                (:warn, "Duplicate name xxxxxxx detected for variable MathOptInterface.VariableIndex(5). Renamed to xxxxxxx_4."),
                (:warn, "Duplicate name xxxxxxx detected for variable MathOptInterface.VariableIndex(6). Renamed to xxxxxxx_5."),
                (:warn, "Duplicate name xxxxxxx detected for variable MathOptInterface.VariableIndex(7). Renamed to xxxxxxx_6."),
                (:warn, "Duplicate name xxxxxxx detected for variable MathOptInterface.VariableIndex(8). Renamed to xxxxxxx_7."),
                (:warn, "Duplicate name xxxxxxx detected for variable MathOptInterface.VariableIndex(9). Renamed to xxxxxxx_8."),
                (:warn, "Duplicate name xxxxxxx detected for variable MathOptInterface.VariableIndex(10). Renamed to xxxxxxx_9."),
                (:warn, "Duplicate name xxxxxxx detected for variable MathOptInterface.VariableIndex(11). Renamed to xxxxxxx_10."),
                (:warn, "Duplicate name xxxxxxx detected for variable MathOptInterface.VariableIndex(12). Renamed to xxxxxxx_11."),
                (:warn, "Duplicate name xxxxxxx detected for variable MathOptInterface.VariableIndex(13). Renamed to xxxxxxx_12."),
                (:warn, "Duplicate name xxxxxxx detected for variable MathOptInterface.VariableIndex(14). Renamed to xxxxxxx_13."),
                (:warn, "Duplicate name xxxxxxx detected for variable MathOptInterface.VariableIndex(15). Renamed to xxxxxxx_14."),
                (:warn, "Duplicate name xxxxxxx detected for variable MathOptInterface.VariableIndex(16). Renamed to xxxxxxx_15."),
                (:warn, "Duplicate name xxxxxxx detected for variable MathOptInterface.VariableIndex(17). Renamed to xxxxxxx_16."),
                (:warn, "Duplicate name xxxxxxx detected for variable MathOptInterface.VariableIndex(18). Renamed to xxxxxxx_17."),
                (:warn, "Name $(repeat("x", max_length + 1)) too long (length: $(1 + max_length); maximum: $(max_length)). Truncating."),
                (:warn, "Name $(repeat("x", max_length + 1))_1 too long (length: $(3 + max_length); maximum: $(max_length)). Truncating."),
                (:warn, "Name $(repeat("x", max_length + 1))_2 too long (length: $(3 + max_length); maximum: $(max_length)). Truncating."),
                (:warn, "Name $(repeat("x", max_length + 1))_3 too long (length: $(3 + max_length); maximum: $(max_length)). Truncating."),
                (:warn, "Name $(repeat("x", max_length + 1))_4 too long (length: $(3 + max_length); maximum: $(max_length)). Truncating."),
                (:warn, "Name $(repeat("x", max_length + 1))_5 too long (length: $(3 + max_length); maximum: $(max_length)). Truncating."),
                (:warn, "Name $(repeat("x", max_length + 1))_6 too long (length: $(3 + max_length); maximum: $(max_length)). Truncating."),
                (:warn, "Name $(repeat("x", max_length + 1))_7 too long (length: $(3 + max_length); maximum: $(max_length)). Truncating."),
                (:warn, "Name $(repeat("x", max_length + 1))_8 too long (length: $(3 + max_length); maximum: $(max_length)). Truncating."),
                (:warn, "Name $(repeat("x", max_length + 1))_9 too long (length: $(3 + max_length); maximum: $(max_length)). Truncating."),
                (:warn, "Name $(repeat("x", max_length + 1))_10 too long (length: $(4 + max_length); maximum: $(max_length)). Truncating."),
                (:warn, "Name $(repeat("x", max_length + 1))_11 too long (length: $(4 + max_length); maximum: $(max_length)). Truncating."),
                (:warn, "Name $(repeat("x", max_length + 1))_12 too long (length: $(4 + max_length); maximum: $(max_length)). Truncating."),
                (:warn, "Name $(repeat("x", max_length + 1))_13 too long (length: $(4 + max_length); maximum: $(max_length)). Truncating."),
                (:warn, "Name $(repeat("x", max_length + 1))_14 too long (length: $(4 + max_length); maximum: $(max_length)). Truncating."),
                (:warn, "Name $(repeat("x", max_length + 1))_15 too long (length: $(4 + max_length); maximum: $(max_length)). Truncating."),
                (:warn, "Name $(repeat("x", max_length + 1))_16 too long (length: $(4 + max_length); maximum: $(max_length)). Truncating."),
                (:warn, "Name $(repeat("x", max_length + 1))_17 too long (length: $(4 + max_length); maximum: $(max_length)). Truncating."),
                MOI.write_to_file(model, LP_TEST_FILE))

            expr = "1 " * repeat("x", max_length) * " + " *
                join(["1 " * repeat("x", max_length - 2) * "_" * string(i) for i in 1:9], " + ") * " + " *
                join(["1 " * repeat("x", max_length - 3) * "_" * string(i) for i in 10:17], " + ")
            @test read(LP_TEST_FILE, String) ==
                "minimize\n" *
                "obj: $(expr)\n" *
                "subject to\n" *
                "c1: $(expr) >= -1\n" *
                "Bounds\n" *
                "End\n"
        end
    end

    @testset "other features" begin
        model = LP.Model()
        MOIU.loadfromstring!(model, """
        variables: x
        maxobjective: 2.0 * x + -1.0
        """)
        MOI.write_to_file(model, LP_TEST_FILE)
        @test read(LP_TEST_FILE, String) ==
            "maximize\n" *
            "obj: -1 + 2 x\n" *
            "subject to\n" *
            "Bounds\n" *
            "End\n"
    end
end

@testset "read_from_file" begin
    model = LP.Model()
    exception = ErrorException("Read from file is not implemented for LP files.")
    @test_throws exception MOI.read_from_file(model, LP_TEST_FILE)
    @test_throws exception MathOptFormat.read_from_file(LP_TEST_FILE)
end

# Clean up
sleep(1.0)  # Allow time for unlink to happen.
rm(LP_TEST_FILE, force = true)
