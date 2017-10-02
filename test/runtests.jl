using MathOptFormat, Base.Test
const MOF = MathOptFormat
const MOI = MathOptFormat.MathOptInterface
const JSON = MOF.JSON

function problem(file::String)
    replace(readstring(joinpath(@__DIR__, "problems", file)), "\r\n", "\n")
end

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
    end
    @testset "Functions" begin

        @testset "variable" begin
            @test JSON.json(MOF.variable("x1")) == "{\"head\":\"variable\",\"name\":\"x1\"}"
        end

        @testset "variableset" begin
            @test JSON.json(MOF.variableset(["x1", "x1"])) == "{\"head\":\"variableset\",\"names\":[\"x1\",\"x1\"]}"
        end

        @testset "linear" begin
            @test JSON.json(MOF.linear(["x1", "x1"], [1.0, 2.0], 3.0)) == "{\"head\":\"linear\",\"variables\":[\"x1\",\"x1\"],\"coefficients\":[1.0,2.0],\"constant\":3.0}"
        end
    end

    @testset "Examples" begin
        @testset "1.mof.json" begin
            m = MOF.MOFFile()
            MOF.addvariable!(m, "x1")
            MOF.setobjective!(m, "min", MOF.linear(["x1", "x1"], [1.0, 2.0], 3.0))
            MOF.addconstraint!(m, MOF.linear(["x1"], [1.0], 0.0), MOF.lessthan(3.0))
            io = IOBuffer()
            MOF.save(io, m)
            @test String(take!(io)) == problem("1.mof.json")
        end
        @testset "2.mof.json" begin
            m = MOF.MOFFile()
            MOF.addvariable!(m, "x1")
            MOF.addvariable!(m, "x2")
            MOF.setobjective!(m, "max", MOF.linear(["x1", "x2"], [2.0, -1.0], 0.0))
            MOF.addconstraint!(m, MOF.linear(["x1"], [2.0], 1.0), MOF.equalto(3.0))
            MOF.addconstraint!(m, MOF.variable("x1"), MOF.integer())
            io = IOBuffer()
            MOF.save(io, m)
            @test String(take!(io)) == problem("2.mof.json")
        end

    end
end

@testset "MOI Interface" begin

    @testset "getvariable!" begin
        v = MOI.VariableReference(1)
        m = MOF.MOFFile()
        @test MOF.getvariable!(m, v) == "x1"
        @test length(m.d["variables"]) == 1
        @test length(keys(m.variables)) == 1
        @test MOF.getvariable!(m, v) == "x1"
        @test length(m.d["variables"]) == 1
        @test length(keys(m.variables)) == 1
    end

    @testset "OptimizationSense" begin
        @test MOF.Object(MOI.MinSense) == "min"
        @test MOF.Object(MOI.MaxSense) == "max"
    end

    @testset "Sets" begin
        @test JSON.json(MOF.Object(MOI.EqualTo(3.0)))     == "{\"head\":\"EqualTo\",\"value\":3.0}"
        @test JSON.json(MOF.Object(MOI.LessThan(3.0)))    == "{\"head\":\"LessThan\",\"value\":3.0}"
        @test JSON.json(MOF.Object(MOI.GreaterThan(3.0))) == "{\"head\":\"GreaterThan\",\"value\":3.0}"
    end

    @testset "Functions" begin

        @testset "SingleVariable" begin
            v = MOI.VariableReference(1)
            m = MOF.MOFFile()
            @test JSON.json(MOF.Object!(m, MOI.SingleVariable(v))) == "{\"head\":\"variable\",\"name\":\"x1\"}"
        end

        @testset "VectorOfVariables" begin
            v = MOI.VariableReference(1)
            m = MOF.MOFFile()
            @test JSON.json(MOF.Object!(m, MOI.VectorOfVariables([v,v]))) == "{\"head\":\"variableset\",\"names\":[\"x1\",\"x1\"]}"
        end

        @testset "ScalarAffineFunction" begin
            v = MOI.VariableReference(1)
            m = MOF.MOFFile()
            f = MOI.ScalarAffineFunction([v, v], [1.0, 2.0], 3.0)

            @test JSON.json(MOF.Object!(m, f)) == "{\"head\":\"linear\",\"variables\":[\"x1\",\"x1\"],\"coefficients\":[1.0,2.0],\"constant\":3.0}"
        end
    end

    @testset "Examples" begin

        @testset "1.mof.json" begin
            v = MOI.VariableReference(1)
            m = MOF.MOFFile()
            f = MOI.ScalarAffineFunction([v, v], [1.0, 2.0], 3.0)
            MOI.setobjective!(m, MOI.MinSense, f)
            f2 = MOI.ScalarAffineFunction([v], [1.0], 0.0)
            MOI.addconstraint!(m, f2, MOI.LessThan(3.0))
            io = IOBuffer()
            MOF.save(io, m)
            @test String(take!(io)) == problem("1.mof.json")
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

            io = IOBuffer()
            MOF.save(io, m)
            @test String(take!(io)) == problem("2.mof.json")
        end
    end
end
