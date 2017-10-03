using MathOptFormat, Base.Test
const MOF = MathOptFormat
const MOI = MathOptFormat.MathOptInterface
const JSON = MOF.JSON

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

@testset "Native Interface" begin
    @testset "Sets" begin
        @test JSON.json(MOF.equalto(3.0)) == "{\"head\":\"EqualTo\",\"value\":3.0}"
        @test JSON.json(MOF.lessthan(3.0)) == "{\"head\":\"LessThan\",\"value\":3.0}"
        @test JSON.json(MOF.greaterthan(3.0)) == "{\"head\":\"GreaterThan\",\"value\":3.0}"
        @test JSON.json(MOF.interval(3.0, 4.0)) == "{\"head\":\"Interval\",\"lower\":3.0,\"upper\":4.0}"
        @test JSON.json(MOF.reals(3)) == "{\"head\":\"Reals\",\"dimension\":3}"
        @test JSON.json(MOF.zeros(3)) == "{\"head\":\"Zeros\",\"dimension\":3}"
        @test JSON.json(MOF.nonnegatives(3)) == "{\"head\":\"Nonnegatives\",\"dimension\":3}"
        @test JSON.json(MOF.nonpositives(3)) == "{\"head\":\"Nonpositives\",\"dimension\":3}"
        @test JSON.json(MOF.semicontinuous(3.0, 4.0)) == "{\"head\":\"Semicontinuous\",\"lower\":3.0,\"upper\":4.0}"
        @test JSON.json(MOF.sos1([1,2])) == "{\"head\":\"SOSI\",\"weights\":[1,2]}"
        @test JSON.json(MOF.sos2([3,4,5])) == "{\"head\":\"SOSII\",\"weights\":[3,4,5]}"
    end

    @testset "Functions" begin

        @testset "variable" begin
            @test JSON.json(MOF.variable("x1")) == "{\"head\":\"variable\",\"name\":\"x1\"}"
        end

        @testset "variableset" begin
            @test JSON.json(MOF.variableset(["x1", "x1"])) == "{\"head\":\"variableset\",\"names\":[\"x1\",\"x1\"]}"
        end

        @testset "affine" begin
            @test JSON.json(MOF.affine(["x1", "x1"], [1.0, 2.0], 3.0)) == "{\"head\":\"affine\",\"variables\":[\"x1\",\"x1\"],\"coefficients\":[1.0,2.0],\"constant\":3.0}"
        end
    end

    @testset "Examples" begin
        @testset "1.mof.json" begin
            # min 1x + 2x + 3
            # s.t        x >= 3
            m = MOF.MOFFile()
            MOF.addvariable!(m, "x1")
            MOF.setobjective!(m, "min", MOF.affine(["x1", "x1"], [1.0, 2.0], 3.0))
            MOF.addconstraint!(m, MOF.affine(["x1"], [1.0], 0.0), MOF.greaterthan(3.0))
            io = IOBuffer()
            MOF.save(io, m, 1)
            @test String(take!(io)) == getproblem("1.mof.json")
        end
        @testset "2.mof.json" begin
            # min 2x - y
            # s.t 2x + 1 == 0
            #      x ∈ Z
            m = MOF.MOFFile()
            MOF.addvariable!(m, "x1")
            MOF.addvariable!(m, "x2")
            MOF.setobjective!(m, "max", MOF.affine(["x1", "x2"], [2.0, -1.0], 0.0))
            MOF.addconstraint!(m, MOF.affine(["x1"], [2.0], 1.0), MOF.equalto(3.0))
            MOF.addconstraint!(m, MOF.variable("x1"), MOF.integer())
            MOF.addconstraint!(m, MOF.variable("x2"), MOF.zeroone())
            io = IOBuffer()
            MOF.save(io, m, 1)
            @test String(take!(io)) == getproblem("2.mof.json")
        end

        @testset "3.mof.json" begin
            m = MOF.MOFFile()
            MOF.addvariable!(m, "x1")
            MOF.addvariable!(m, "x2")
            MOF.addvariable!(m, "x3")
            MOF.setobjective!(m, "max", MOF.quadratic(["x1", "x2"], [2.0, -1.0], ["x1"], ["x1"], [1.0], 0.0))
            MOF.addconstraint!(m, MOF.variableset(["x1", "x2", "x3"]), MOF.sos2([1.0, 2.0, 3.0]))
            io = IOBuffer()
            MOF.save(io, m, 1)
            @test String(take!(io)) == getproblem("3.mof.json")
        end

        @testset "linear7.mof.json" begin
            # Min  x - y
            # s.t. 0.0 <= x          (c1)
            #             y <= 0.0   (c2)
            m = MOF.MOFFile()
            MOF.addvariable!(m, "x")
            MOF.addvariable!(m, "y")
            MOF.setobjective!(m, "min", MOF.affine(["x", "y"], [1.0, -1.0], 0.0))
            MOF.addconstraint!(m, MOF.vectoraffine([1],["x"],[1.0],[0.0]), MOF.nonnegatives(1))
            MOF.addconstraint!(m, MOF.vectoraffine([1],["y"],[1.0],[0.0]), MOF.nonpositives(1))
            io = IOBuffer()
            MOF.save(io, m, 1)
            @test String(take!(io)) == getproblem("linear7.mof.json")
        end

        @testset "qp1.mof.json" begin
            # simple quadratic objective
            # Min x^2 + xy + y^2 + yz + z^2
            # st  x + 2y + 3z >= 4 (c1)
            #     x +  y      >= 1 (c2)
            #     x,y \in R
            m = MOF.MOFFile()
            v = [MOI.VariableReference(1), MOI.VariableReference(2), MOI.VariableReference(3)]
            cf1 = MOI.ScalarAffineFunction(v, [1.0,2.0,3.0], 0.0)
            c1 = MOI.addconstraint!(m, cf1, MOI.GreaterThan(4.0))

            c2 = MOI.addconstraint!(m, MOI.ScalarAffineFunction([v[1],v[2]], [1.0,1.0], 0.0), MOI.GreaterThan(1.0))

            obj = MOI.ScalarQuadraticFunction(MOI.VariableReference[], Float64[], v[[1,1,2,2,3]], v[[1,2,2,3,3]], [2.0, 1.0, 2.0, 1.0, 2.0], 0.0)

            MOI.setobjective!(m, MOI.MinSense, obj)

            io = IOBuffer()
            MOF.save(io, m, 1)
            @test String(take!(io)) == getproblem("qp1.mof.json")
        end


    end
end

@testset "MOI Interface" begin

    @testset "getvariable!" begin
        v = MOI.VariableReference(1)
        m = MOF.MOFFile()
        @test MOF.getvariable!(m, v) == "x1"
        @test length(m.d["variables"]) == 1
        @test length(keys(m.ext)) == 1
        @test MOF.getvariable!(m, v) == "x1"
        @test length(m.d["variables"]) == 1
        @test length(keys(m.ext)) == 1
    end

    @testset "OptimizationSense" begin
        @test MOF.Object(MOI.MinSense) == "min"
        @test MOF.Object(MOI.MaxSense) == "max"
    end

    @testset "Sets" begin
        @test MOF.Object(MOI.EqualTo(3.0))     == MOF.equalto(3.0)
        @test MOF.Object(MOI.LessThan(3.0))    == MOF.lessthan(3.0)
        @test MOF.Object(MOI.GreaterThan(3.0)) == MOF.greaterthan(3.0)
        @test MOF.Object(MOI.Interval(3.0, 4.0)) == MOF.interval(3.0, 4.0)
        @test MOF.Object(MOI.Reals(2)) == MOF.reals(2)
        @test MOF.Object(MOI.Zeros(2)) == MOF.zeros(2)
        @test MOF.Object(MOI.Nonpositives(2)) == MOF.nonpositives(2)
        @test MOF.Object(MOI.Nonnegatives(2)) == MOF.nonnegatives(2)
        @test MOF.Object(MOI.Semicontinuous(2.5, 3.0)) == MOF.semicontinuous(2.5, 3.0)
        @test MOF.Object(MOI.Semiinteger(2, 5)) == MOF.semiinteger(2, 5)
        @test MOF.Object(MOI.SOS1([1,3,5])) == MOF.sos1([1,3,5])
        @test MOF.Object(MOI.SOS2([1,3,5])) == MOF.sos2([1,3,5])
    end

    @testset "Functions" begin

        @testset "SingleVariable" begin
            v = MOI.VariableReference(1)
            m = MOF.MOFFile()
            @test MOF.Object!(m, MOI.SingleVariable(v)) == MOF.variable("x1")
        end

        @testset "VectorOfVariables" begin
            v = MOI.VariableReference(1)
            m = MOF.MOFFile()
            @test MOF.Object!(m, MOI.VectorOfVariables([v,v])) == MOF.variableset(["x1", "x1"])
        end

        @testset "ScalarAffineFunction" begin
            v = MOI.VariableReference(1)
            m = MOF.MOFFile()
            f = MOI.ScalarAffineFunction([v, v], [1.0, 2.0], 3.0)

            @test MOF.Object!(m, f) == MOF.affine(["x1", "x1"], [1.0, 2.0], 3.0)
        end
    end

    @testset "Write Examples" begin

        @testset "1.mof.json" begin
            v = MOI.VariableReference(1)
            m = MOF.MOFFile()
            f = MOI.ScalarAffineFunction([v, v], [1.0, 2.0], 3.0)
            MOI.setobjective!(m, MOI.MinSense, f)
            f2 = MOI.ScalarAffineFunction([v], [1.0], 0.0)
            MOI.addconstraint!(m, f2, MOI.GreaterThan(3.0))
            io = IOBuffer()
            MOF.save(io, m, 1)
            @test String(take!(io)) == getproblem("1.mof.json")
        end

        @testset "2.mof.json" begin
            x = MOI.VariableReference(1)
            y = MOI.VariableReference(2)
            m = MOF.MOFFile()
            c = MOI.ScalarAffineFunction([x, y], [2.0, -1.0], 0.0)
            MOI.setobjective!(m, MOI.MaxSense, c)
            a1 = MOI.ScalarAffineFunction([x], [2.0], 1.0)
            MOI.addconstraint!(m, a1, MOI.EqualTo(3.0))
            MOI.addconstraint!(m, MOI.SingleVariable(x), MOI.Integer())
            MOI.addconstraint!(m, MOI.SingleVariable(y), MOI.ZeroOne())

            io = IOBuffer()
            MOF.save(io, m, 1)
            @test String(take!(io)) == getproblem("2.mof.json")
        end
    end
end

@testset "Read Examples" begin
    for prob in ["1.mof.json", "2.mof.json", "3.mof.json", "linear7.mof.json", "qp1.mof.json"]
        @testset "$(prob)" begin
            io = IOBuffer()
            MOF.save(io, MOF.MOFFile(problempath(prob)), 1)
            @test String(take!(io)) == getproblem(prob)
        end
    end
end

include("solver_required.jl")
