using MathOptFormat, Base.Test
const MOF = MathOptFormat
const MOI = MathOptFormat.MathOptInterface
const JSON = MOF.JSON

function stringify(m::MOF.MOFFile)
    io = IOBuffer()
    MOF.save(io, m, 1)
    String(take!(io))
end

function getproblem(file::String)
    replace(readstring(problempath(file)), "\r\n", "\n")
end
problempath(prob::String) = joinpath(@__DIR__, "problems", prob)

@testset "MOFFile" begin

    @testset "JSON.json(::MOFFile)" begin
        m = MOF.MOFFile()
        @test JSON.json(m.d) == "{\"version\":\"0.0\",\"sense\":\"min\",\"variables\":[],\"objective\":{},\"constraints\":[]}"
    end

end

@testset "Sets" begin
    @test JSON.json(MOF.Object(MOI.EqualTo(3.0)))             == "{\"head\":\"EqualTo\",\"value\":3.0}"
    @test JSON.json(MOF.Object(MOI.LessThan(3.0)))            == "{\"head\":\"LessThan\",\"upper\":3.0}"
    @test JSON.json(MOF.Object(MOI.GreaterThan(3.0)))         == "{\"head\":\"GreaterThan\",\"lower\":3.0}"
    @test JSON.json(MOF.Object(MOI.Interval(3.0, 4.0)))       == "{\"head\":\"Interval\",\"lower\":3.0,\"upper\":4.0}"
    @test JSON.json(MOF.Object(MOI.Reals(2)))                 == "{\"head\":\"Reals\",\"dim\":2}"
    @test JSON.json(MOF.Object(MOI.Zeros(2)))                 == "{\"head\":\"Zeros\",\"dim\":2}"
    @test JSON.json(MOF.Object(MOI.Nonpositives(2)))          == "{\"head\":\"Nonpositives\",\"dim\":2}"
    @test JSON.json(MOF.Object(MOI.Nonnegatives(2)))          == "{\"head\":\"Nonnegatives\",\"dim\":2}"
    @test JSON.json(MOF.Object(MOI.Semicontinuous(2.5, 3.0))) == "{\"head\":\"Semicontinuous\",\"l\":2.5,\"u\":3.0}"
    @test JSON.json(MOF.Object(MOI.Semiinteger(2, 5)))        == "{\"head\":\"Semiinteger\",\"l\":2,\"u\":5}"
    @test JSON.json(MOF.Object(MOI.SOS1([1,2])))            == "{\"head\":\"SOSI\",\"weights\":[1,2]}"
    @test JSON.json(MOF.Object(MOI.SOS2([3,4,5])))            == "{\"head\":\"SOSII\",\"weights\":[3,4,5]}"
    # test cones
end

@testset "Functions" begin

    @testset "SingleVariable" begin
        v = MOI.VariableReference(1)
        m = MOF.MOFFile()
        @test JSON.json(MOF.Object!(m, MOI.SingleVariable(v))) == "{\"head\":\"SingleVariable\",\"variable\":\"x1\"}"
    end

    @testset "VectorOfVariables" begin
        v = MOI.VariableReference(1)
        m = MOF.MOFFile()
        @test JSON.json(MOF.Object!(m, MOI.VectorOfVariables([v,v]))) == "{\"head\":\"VectorOfVariables\",\"variables\":[\"x1\",\"x1\"]}"
    end

    @testset "ScalarAffineFunction" begin
        v = MOI.VariableReference(1)
        m = MOF.MOFFile()
        f = MOI.ScalarAffineFunction([v, v], [1.0, 2.0], 3.0)
        @test JSON.json(MOF.Object!(m, f)) == "{\"head\":\"ScalarAffineFunction\",\"variables\":[\"x1\",\"x1\"],\"coefficients\":[1.0,2.0],\"constant\":3.0}"
    end
end

@testset "getname!" begin
    v = MOI.VariableReference(1)
    m = MOF.MOFFile()
    @test MOF.getname!(m, v) == "x1"
    @test length(m.d["variables"]) == 1
    @test length(keys(m.ext)) == 1
    @test MOF.getname!(m, v) == "x1"
    @test length(m.d["variables"]) == 1
    @test length(keys(m.ext)) == 1
end

@testset "OptimizationSense" begin
    @test MOF.Object(MOI.MinSense) == "min"
    @test MOF.Object(MOI.MaxSense) == "max"
end

@testset "Write Examples" begin

    @testset "1.mof.json" begin
        # min 1x + 2x + 3
        # s.t        x >= 3
        m = MOF.MOFFile()
        v = MOI.VariableReference(1)
        f = MOI.ScalarAffineFunction([v, v], [1.0, 2.0], 3.0)
        MOI.setobjective!(m, MOI.MinSense, f)

        MOI.addconstraint!(m,
            MOI.ScalarAffineFunction([v], [1.0], 0.0),
            MOI.GreaterThan(3.0),
            "firstconstraint"
        )

        @test stringify(m) == getproblem("1.mof.json")
    end

    @testset "2.mof.json" begin
        # min 2x - y
        # s.t 2x + 1 == 0
        #      x ∈ Z
        #      y ∈ {0, 1}
        m = MOF.MOFFile()
        x = MOI.VariableReference(1)
        y = MOI.VariableReference(2)
        c = MOI.ScalarAffineFunction([x, y], [2.0, -1.0], 0.0)
        MOI.setobjective!(m, MOI.MaxSense, c)
        MOI.addconstraint!(m,
            MOI.ScalarAffineFunction([x], [2.0], 1.0),
            MOI.EqualTo(3.0)
        )
        MOI.addconstraint!(m, MOI.SingleVariable(x), MOI.Integer())
        MOI.addconstraint!(m, MOI.SingleVariable(y), MOI.ZeroOne())

        @test stringify(m) == getproblem("2.mof.json")
    end

    @testset "3.mof.json" begin
        m = MOF.MOFFile()

        (x1, x2, x3) = MOI.addvariables!(m, 3)

        MOI.setobjective!(m, MOI.MaxSense,
            MOI.ScalarQuadraticFunction(
                [x1, x2],
                [2.0, -1.0],
                [x1],
                [x1],
                [1.0],
                0.0
            )
        )

        MOI.addconstraint!(m,
            MOI.VectorOfVariables([x1, x2, x3]),
            MOI.SOS2([1.0, 2.0, 3.0])
        )

        @test stringify(m) == getproblem("3.mof.json")
    end

    @testset "linear7.mof.json" begin
        # Min  x - y
        # s.t. 0.0 <= x          (c1)
        #             y <= 0.0   (c2)
        m = MOF.MOFFile()
        x = MOI.addvariable!(m, "x")
        y = MOI.addvariable!(m, "y")
        MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([x, y], [1.0, -1.0], 0.0))
        MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[x],[1.0],[0.0]), MOI.Nonnegatives(1))
        MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[y],[1.0],[0.0]), MOI.Nonpositives(1))
        @test stringify(m) == getproblem("linear7.mof.json")
    end

    @testset "qp1.mof.json" begin
        # simple quadratic objective
        # Min x^2 + xy + y^2 + yz + z^2
        # st  x + 2y + 3z >= 4 (c1)
        #     x +  y      >= 1 (c2)
        #     x,y \in R
        m = MOF.MOFFile()

        v = MOI.addvariables!(m, 3)

        MOI.addconstraint!(m,
            MOI.ScalarAffineFunction(v, [1.0,2.0,3.0], 0.0),
            MOI.GreaterThan(4.0)
        )

        MOI.addconstraint!(m,
            MOI.ScalarAffineFunction([v[1],v[2]], [1.0,1.0], 0.0),
            MOI.GreaterThan(1.0)
        )

        MOI.setobjective!(m, MOI.MinSense,
            MOI.ScalarQuadraticFunction(
                MOI.VariableReference[],
                Float64[],
                v[[1,1,2,2,3]],
                v[[1,2,2,3,3]],
                [2.0, 1.0, 2.0, 1.0, 2.0],
                0.0
            )
        )

        @test stringify(m) == getproblem("qp1.mof.json")
    end
end

@testset "Read-Write Examples" begin
    for prob in [
            "1", "2", "3", "linear7", "qp1", "LIN1", "LIN2", "linear1", "linear2", "mip01", "sos1"
            ]
        @testset "$(prob)" begin
            file_representation = getproblem("$(prob).mof.json")
            moffile = MOF.MOFFile(problempath("$(prob).mof.json"))
            @test stringify(moffile) == file_representation
            model =  MOI.SolverInstance(moffile, MOF.MOFWriter())
            @test stringify(model) == file_representation
            model2 =  MOI.SolverInstance(problempath("$(prob).mof.json"), MOF.MOFWriter())
            @test stringify(model2) == file_representation
        end
    end
end
