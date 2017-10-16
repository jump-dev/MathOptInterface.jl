using MathOptFormat, Base.Test
const MOF = MathOptFormat
const MOI = MathOptFormat.MathOptInterface
const JSON = MOF.JSON

function stringify(m::MOF.MOFFile)
    io = IOBuffer()
    MOI.writeproblem(m, io, 1)
    String(take!(io))
end

function getproblem(file::String)
    replace(read(problempath(file), String), "\r\n", "\n")
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
    @test JSON.json(MOF.Object(MOI.Reals(2)))                 == "{\"head\":\"Reals\",\"dimension\":2}"
    @test JSON.json(MOF.Object(MOI.Zeros(2)))                 == "{\"head\":\"Zeros\",\"dimension\":2}"
    @test JSON.json(MOF.Object(MOI.Nonpositives(2)))          == "{\"head\":\"Nonpositives\",\"dimension\":2}"
    @test JSON.json(MOF.Object(MOI.Nonnegatives(2)))          == "{\"head\":\"Nonnegatives\",\"dimension\":2}"
    @test JSON.json(MOF.Object(MOI.Semicontinuous(2.5, 3.0))) == "{\"head\":\"Semicontinuous\",\"lower\":2.5,\"upper\":3.0}"
    @test JSON.json(MOF.Object(MOI.Semiinteger(2, 5)))        == "{\"head\":\"Semiinteger\",\"lower\":2,\"upper\":5}"
    @test JSON.json(MOF.Object(MOI.SOS1([1,2])))            == "{\"head\":\"SOS1\",\"weights\":[1,2]}"
    @test JSON.json(MOF.Object(MOI.SOS2([3,4,5])))            == "{\"head\":\"SOS2\",\"weights\":[3,4,5]}"

    @test JSON.json(MOF.Object(MOI.Interval(-Inf, Inf)))       == "{\"head\":\"Interval\",\"lower\":\"-inf\",\"upper\":\"+inf\"}"
    @test JSON.json(MOF.Object(MOI.Semicontinuous(-Inf, Inf)))       == "{\"head\":\"Semicontinuous\",\"lower\":\"-inf\",\"upper\":\"+inf\"}"
    @test JSON.json(MOF.Object(MOI.Semiinteger(-Inf, Inf)))       == "{\"head\":\"Semiinteger\",\"lower\":\"-inf\",\"upper\":\"+inf\"}"

    @test JSON.json(MOF.Object(MOI.SecondOrderCone(2)))       == "{\"head\":\"SecondOrderCone\",\"dimension\":2}"
    @test JSON.json(MOF.Object(MOI.RotatedSecondOrderCone(2)))       == "{\"head\":\"RotatedSecondOrderCone\",\"dimension\":2}"

    @test JSON.json(MOF.Object(MOI.ExponentialCone()))       == "{\"head\":\"ExponentialCone\"}"
    @test JSON.json(MOF.Object(MOI.DualExponentialCone()))       == "{\"head\":\"DualExponentialCone\"}"

    @test JSON.json(MOF.Object(MOI.PowerCone(0.5)))       == "{\"head\":\"PowerCone\",\"exponent\":0.5}"
    @test JSON.json(MOF.Object(MOI.DualPowerCone(0.5)))       == "{\"head\":\"DualPowerCone\",\"exponent\":0.5}"

    @test JSON.json(MOF.Object(MOI.PositiveSemidefiniteConeTriangle(2)))       == "{\"head\":\"PositiveSemidefiniteConeTriangle\",\"dimension\":2}"
    @test JSON.json(MOF.Object(MOI.PositiveSemidefiniteConeScaled(2)))       == "{\"head\":\"PositiveSemidefiniteConeScaled\",\"dimension\":2}"
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

    @testset "VectorAffineFunction" begin
        v = MOI.VariableReference(1)
        m = MOF.MOFFile()
        f = MOI.VectorAffineFunction([1, 1], [v, v], [1.0, 2.0], [3.0])
        @test JSON.json(MOF.Object!(m, f)) == "{\"head\":\"VectorAffineFunction\",\"outputindex\":[1,1],\"variables\":[\"x1\",\"x1\"],\"coefficients\":[1.0,2.0],\"constant\":[3.0]}"
    end

    @testset "ScalarQuadraticFunction" begin
        v = MOI.VariableReference(1)
        m = MOF.MOFFile()
        f = MOI.ScalarQuadraticFunction([v], [1.0], [v], [v], [2.0], 3.0)
        @test JSON.json(MOF.Object!(m, f)) == "{\"head\":\"ScalarQuadraticFunction\",\"affine_variables\":[\"x1\"],\"affine_coefficients\":[1.0],\"quadratic_rowvariables\":[\"x1\"],\"quadratic_colvariables\":[\"x1\"],\"quadratic_coefficients\":[2.0],\"constant\":3.0}"
    end

    @testset "VectorQuadraticFunction" begin
        v = MOI.VariableReference(1)
        m = MOF.MOFFile()
        f = MOI.VectorQuadraticFunction([1], [v], [1.0], [1], [v], [v], [2.0], [3.0])
        @test JSON.json(MOF.Object!(m, f)) == "{\"head\":\"VectorQuadraticFunction\",\"affine_outputindex\":[1],\"affine_variables\":[\"x1\"],\"affine_coefficients\":[1.0],\"quadratic_outputindex\":[1],\"quadratic_rowvariables\":[\"x1\"],\"quadratic_colvariables\":[\"x1\"],\"quadratic_coefficients\":[2.0],\"constant\":[3.0]}"
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
        solver = MOF.MOFWriter()
        m = MOI.SolverInstance(solver)
        v = MOI.addvariable!(m)
        f = MOI.ScalarAffineFunction([v, v], [1.0, 2.0], 3.0)
        MOI.setattribute!(m, MOI.ObjectiveFunction(),f)
        MOI.setattribute!(m, MOI.ObjectiveSense(), MOI.MinSense)
        @test MOI.canaddconstraint(m,
            MOI.ScalarAffineFunction([v], [1.0], 0.0),
            MOI.GreaterThan(3.0))
        c1 = MOI.addconstraint!(m,
            MOI.ScalarAffineFunction([v], [1.0], 0.0),
            MOI.GreaterThan(3.0),
            "firstconstraint"
        )
        @test typeof(c1) == MOI.ConstraintReference{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}
        @test stringify(m) == getproblem("1.mof.json")
        @test MOI.canmodifyconstraint(m, c1, MOI.GreaterThan(4.0))
        MOI.modifyconstraint!(m, c1, MOI.GreaterThan(4.0))
        @test stringify(m) == getproblem("1a.mof.json")
        @test MOI.canmodifyconstraint(m, c1, MOI.ScalarAffineFunction([v], [2.0], 1.0))
        MOI.modifyconstraint!(m, c1, MOI.ScalarAffineFunction([v], [2.0], 1.0))
        @test stringify(m) == getproblem("1b.mof.json")
        @test MOI.canmodifyconstraint(m, c1, MOI.ScalarConstantChange(1.5))
        MOI.modifyconstraint!(m, c1, MOI.ScalarConstantChange(1.5))
        @test stringify(m) == getproblem("1c.mof.json")
        @test MOI.candelete(m, c1)
        MOI.delete!(m, c1)
        @test stringify(m) == getproblem("1d.mof.json")
        @test MOI.canaddconstraint(m,
            MOI.SingleVariable(v),
            MOI.Semicontinuous(1.0, 5.0)
        )
        c2 = MOI.addconstraint!(m,
            MOI.SingleVariable(v),
            MOI.Semicontinuous(1.0, 5.0)
        )
        u = MOI.addvariable!(m)
        @test MOI.canaddconstraint(m,
            MOI.SingleVariable(u),
            MOI.Semiinteger(2, 6)
        )
        c3 = MOI.addconstraint!(m,
            MOI.SingleVariable(u),
            MOI.Semiinteger(2, 6)
        )
        @test stringify(m) == getproblem("1e.mof.json")
        @test MOI.candelete(m, c2)
        MOI.delete!(m, c2)
        @test stringify(m) == getproblem("1f.mof.json")
        @test MOI.cantransformconstraint(m,
            c3,
            MOI.Integer()
        )
        c4 = MOI.transformconstraint!(m,
            c3,
            MOI.Integer()
        )
        @test stringify(m) == getproblem("1g.mof.json")
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
        MOI.setattribute!(m, MOI.ObjectiveFunction(), c)
        MOI.setattribute!(m, MOI.ObjectiveSense(), MOI.MaxSense)
        @test MOI.canaddconstraint(m,
            MOI.ScalarAffineFunction([x], [2.0], 1.0),
            MOI.EqualTo(3.0)
        )
        c1 = MOI.addconstraint!(m,
            MOI.ScalarAffineFunction([x], [2.0], 1.0),
            MOI.EqualTo(3.0)
        )
        @test typeof(c1) == MOI.ConstraintReference{MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}}
        @test MOI.canaddconstraint(m, MOI.SingleVariable(x), MOI.Integer())
        MOI.addconstraint!(m, MOI.SingleVariable(x), MOI.Integer())
        @test MOI.canaddconstraint(m, MOI.SingleVariable(y), MOI.ZeroOne())
        MOI.addconstraint!(m, MOI.SingleVariable(y), MOI.ZeroOne())

        @test stringify(m) == getproblem("2.mof.json")
    end

    @testset "3.mof.json" begin
        m = MOF.MOFFile()

        (x1, x2, x3) = MOI.addvariables!(m, 3)
        MOI.setattribute!(m, MOI.ObjectiveFunction(),
            MOI.ScalarQuadraticFunction(
                [x1, x2],
                [2.0, -1.0],
                [x1],
                [x1],
                [1.0],
                0.0
            )
        )
        MOI.setattribute!(m, MOI.ObjectiveSense(), MOI.MaxSense)

        @test MOI.canaddconstraint(m,
            MOI.VectorOfVariables([x1, x2, x3]),
            MOI.SOS2([1.0, 2.0, 3.0])
        )
        c1 = MOI.addconstraint!(m,
            MOI.VectorOfVariables([x1, x2, x3]),
            MOI.SOS2([1.0, 2.0, 3.0])
        )
        @test typeof(c1) == MOI.ConstraintReference{MOI.VectorOfVariables, MOI.SOS2{Float64}}

        @test stringify(m) == getproblem("3.mof.json")
    end

    @testset "linear7.mof.json" begin
        # Min  x - y
        # s.t. 0.0 <= x          (c1)
        #             y <= 0.0   (c2)
        m = MOF.MOFFile()
        x = MOI.addvariable!(m, "x")
        y = MOI.addvariable!(m, "y")
        MOI.setattribute!(m, MOI.ObjectiveFunction(),
            MOI.ScalarAffineFunction([x, y], [1.0, -1.0], 0.0)
        )
        MOI.setattribute!(m, MOI.ObjectiveSense(), MOI.MinSense)
        @test MOI.canaddconstraint(m, MOI.VectorAffineFunction([1],[x],[1.0],[0.0]), MOI.Nonnegatives(1))
        MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[x],[1.0],[0.0]), MOI.Nonnegatives(1))

        @test MOI.canaddconstraint(m, MOI.VectorAffineFunction([1],[y],[1.0],[0.0]), MOI.Nonpositives(1))
        c = MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[y],[1.0],[0.0]), MOI.Nonpositives(1))
        MOI.writeproblem(m, problempath("linear7.mof.json"), 1)
        @test stringify(m) == getproblem("linear7.mof.json")

        @test MOI.canmodifyconstraint(m, c, MOI.VectorConstantChange([1.0]))
        MOI.modifyconstraint!(m, c, MOI.VectorConstantChange([1.0]))
        MOI.writeproblem(m, problempath("linear7a.mof.json"), 1)
        @test stringify(m) == getproblem("linear7a.mof.json")
    end

    @testset "qp1.mof.json" begin
        # simple quadratic objective
        # Min x^2 + xy + y^2 + yz + z^2
        # st  x + 2y + 3z >= 4 (c1)
        #     x +  y      >= 1 (c2)
        #     x,y \in R
        m = MOF.MOFFile()

        v = MOI.addvariables!(m, 3)

        @test MOI.canaddconstraint(m,
            MOI.ScalarAffineFunction(v, [1.0,2.0,3.0], 0.0),
            MOI.GreaterThan(4.0)
        )
        MOI.addconstraint!(m,
            MOI.ScalarAffineFunction(v, [1.0,2.0,3.0], 0.0),
            MOI.GreaterThan(4.0)
        )
        @test MOI.canaddconstraint(m,
            MOI.ScalarAffineFunction([v[1],v[2]], [1.0,1.0], 0.0),
            MOI.GreaterThan(1.0)
        )
        MOI.addconstraint!(m,
            MOI.ScalarAffineFunction([v[1],v[2]], [1.0,1.0], 0.0),
            MOI.GreaterThan(1.0)
        )
        MOI.setattribute!(m, MOI.ObjectiveFunction(),
            MOI.ScalarQuadraticFunction(
                MOI.VariableReference[],
                Float64[],
                v[[1,1,2,2,3]],
                v[[1,2,2,3,3]],
                [2.0, 1.0, 2.0, 1.0, 2.0],
                0.0
            )
        )
        MOI.setattribute!(m, MOI.ObjectiveSense(), MOI.MinSense)
        @test stringify(m) == getproblem("qp1.mof.json")
    end

    @testset "qcp.mof.json" begin
        # quadratic constraint
        # Max x + y
        # st  - x + y >= 0 (c1[1])
        #       x + y >= 0 (c1[2])
        #     0.5x^2 + y <= 2 (c2)

        m = MOF.MOFFile()

        x = MOI.addvariable!(m)
        y = MOI.addvariable!(m)

        @test MOI.canaddconstraint(m, MOI.VectorAffineFunction([1,1,2,2], [x,y,x,y],[-1.0,1.0,1.0,1.0], [0.0,0.0]), MOI.Nonnegatives(2))
        MOI.addconstraint!(m, MOI.VectorAffineFunction([1,1,2,2], [x,y,x,y],[-1.0,1.0,1.0,1.0], [0.0,0.0]), MOI.Nonnegatives(2))

        f = MOI.VectorQuadraticFunction([1], [y],[1.0], [1], [x],[x],[1.0], [0.0])
        @test MOI.canaddconstraint(m, f, MOI.LessThan(2.0))
        MOI.addconstraint!(m, f, MOI.LessThan(2.0))

        MOI.setattribute!(m, MOI.ObjectiveFunction(), MOI.ScalarAffineFunction([x,y], [1.0,1.0], 0.0))
        MOI.setattribute!(m, MOI.ObjectiveSense(), MOI.MaxSense)
        # MOI.writeproblem(m, "test/problems/qcp.mof.json",1)
        @test stringify(m) == getproblem("qcp.mof.json")
    end

    @testset "conic.mof.json" begin
        # an unrealistic model to test functionality
        m = MOF.MOFFile()

        v = MOI.addvariables!(m, 3)
        # reals
        @test MOI.canaddconstraint(m,
            MOI.VectorOfVariables(v),
            MOI.Reals(3)
        )
        MOI.addconstraint!(m,
            MOI.VectorOfVariables(v),
            MOI.Reals(3)
        )
        # second order
        @test MOI.canaddconstraint(m,
            MOI.VectorOfVariables(v),
            MOI.SecondOrderCone(3)
        )
        MOI.addconstraint!(m,
            MOI.VectorOfVariables(v),
            MOI.SecondOrderCone(3)
        )
        # rotated second order
        @test MOI.canaddconstraint(m,
            MOI.VectorOfVariables(v),
            MOI.RotatedSecondOrderCone(3)
        )
        MOI.addconstraint!(m,
            MOI.VectorOfVariables(v),
            MOI.RotatedSecondOrderCone(3)
        )
        # exponential
        @test MOI.canaddconstraint(m,
            MOI.VectorOfVariables(v),
            MOI.ExponentialCone()
        )
        MOI.addconstraint!(m,
            MOI.VectorOfVariables(v),
            MOI.ExponentialCone()
        )
        # dual exponential
        @test MOI.canaddconstraint(m,
            MOI.VectorOfVariables(v),
            MOI.DualExponentialCone()
        )
        MOI.addconstraint!(m,
            MOI.VectorOfVariables(v),
            MOI.DualExponentialCone()
        )
        # power
        @test MOI.canaddconstraint(m,
            MOI.VectorOfVariables(v),
            MOI.PowerCone(1.5)
        )
        MOI.addconstraint!(m,
            MOI.VectorOfVariables(v),
            MOI.PowerCone(1.5)
        )
        # dual power
        @test MOI.canaddconstraint(m,
            MOI.VectorOfVariables(v),
            MOI.DualPowerCone(1.5)
        )
        MOI.addconstraint!(m,
            MOI.VectorOfVariables(v),
            MOI.DualPowerCone(1.5)
        )
        # psd triangle
        @test MOI.canaddconstraint(m,
            MOI.VectorOfVariables(v),
            MOI.PositiveSemidefiniteConeTriangle(3)
        )
        MOI.addconstraint!(m,
            MOI.VectorOfVariables(v),
            MOI.PositiveSemidefiniteConeTriangle(3)
        )
        # psd triangle scales
        @test MOI.canaddconstraint(m,
            MOI.VectorOfVariables(v),
            MOI.PositiveSemidefiniteConeScaled(3)
        )
        MOI.addconstraint!(m,
            MOI.VectorOfVariables(v),
            MOI.PositiveSemidefiniteConeScaled(3)
        )
        MOI.setattribute!(m, MOI.ObjectiveFunction(),
            MOI.ScalarAffineFunction(
                MOI.VariableReference[],
                Float64[],
                0.0
            )
        )
        MOI.setattribute!(m, MOI.ObjectiveSense(), MOI.MinSense)

        # MOI.writeproblem(m, "test/problems/conic.mof.json", 1)
        @test stringify(m) == getproblem("conic.mof.json")
    end
end

@testset "Read-Write Examples" begin
    for prob in [
            "1","1a","1b","1c","1d","1e","1f", "2", "3", "linear7", "linear7a", "qp1", "qcp", "LIN1", "LIN2", "linear1", "linear2", "mip01", "sos1", "conic"
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
