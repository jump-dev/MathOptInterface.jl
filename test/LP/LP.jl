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
            "c5: 0 + 1.1x <= 5.1\n" *
            "c6: -1.4 + 1.3x >= -0.1\n" *
            "c7: 1.6 + 1.5a == 0.2\n" *
            "c8: 0.3 <= 1.8 + 1.7a <= 0.4\n" *
            "Bounds\n" *
            "x <= 2\n" *
            "x >= -1\n" *
            "y == 3\n" *
            "4 <= z <= 5\n" *
            "General\n" *
            "y\n" *
            "Binary\n" *
            "x\n"
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
            "obj: -1 + 2x\n" *
            "subject to\n" *
            "Bounds\n"
    end
end

@testset "read_from_file" begin
    model = LP.Model()
    exception = ErrorException("Read from file is not implemented for LP files.")
    if VERSION < v"0.7"
        @test_throws Exception MOI.read_from_file(model, LP_TEST_FILE)
        @test_throws Exception MathOptFormat.read_from_file(LP_TEST_FILE)
    else
        @test_throws exception MOI.read_from_file(model, LP_TEST_FILE)
        @test_throws exception MathOptFormat.read_from_file(LP_TEST_FILE)
    end
end

# Clean up
sleep(1.0)  # Allow time for unlink to happen.
rm(LP_TEST_FILE, force = true)
