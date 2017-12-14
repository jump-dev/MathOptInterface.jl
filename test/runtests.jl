using MathOptFormat, Base.Test
const MOF = MathOptFormat
const MOI = MathOptFormat.MathOptInterface
const JSON = MOF.JSON

# a switch to update the example files
# if the format changes
const WRITEFILES = true

function stringify(instance::MOF.MOFInstance, indent::Int=1)
    io = IOBuffer()
    MOI.write(instance, io, indent)
    String(take!(io))
end

function getproblem(file::String)
    replace(read(problempath(file), String), "\r\n", "\n")
end
problempath(prob::String) = joinpath(@__DIR__, "problems", prob)

@testset "floatify" begin
    # just to get 100% coverage
    @test MOF.floatify(1) == 1.0
end

@testset "MOFInstance" begin

    @testset "JSON.json(::MOFInstance)" begin
        instance = MOF.MOFInstance()
        MOI.addvariable!(instance)
        MOI.write(instance, problempath("test.mof.json"))
        @test getproblem("test.mof.json") == "{\"version\":\"0.0\",\"sense\":\"min\",\"variables\":[{\"name\":\"x1\"}],\"objective\":{\"head\":\"ScalarAffineFunction\",\"variables\":[],\"coefficients\":[],\"constant\":0.0},\"constraints\":[]}"
        # test different ways to read a model from file
        m2 = MOF.MOFInstance(problempath("test.mof.json"))
        m3 = MOF.MOFInstance()
        MOI.read!(m3, problempath("test.mof.json"))
        @test stringify(instance) == stringify(m2) == stringify(m3)
        # model already exists
        @test_throws Exception MOI.read!(m3, problempath("test.mof.json"))
        rm(problempath("test.mof.json"))
    end

end

@testset "Sets" begin
    @test JSON.json(MOF.object(MOI.EqualTo(3.0)))             == "{\"head\":\"EqualTo\",\"value\":3.0}"
    @test JSON.json(MOF.object(MOI.LessThan(3.0)))            == "{\"head\":\"LessThan\",\"upper\":3.0}"
    @test JSON.json(MOF.object(MOI.GreaterThan(3.0)))         == "{\"head\":\"GreaterThan\",\"lower\":3.0}"
    @test JSON.json(MOF.object(MOI.Interval(3.0, 4.0)))       == "{\"head\":\"Interval\",\"lower\":3.0,\"upper\":4.0}"
    @test JSON.json(MOF.object(MOI.Reals(2)))                 == "{\"head\":\"Reals\",\"dimension\":2}"
    @test JSON.json(MOF.object(MOI.Zeros(2)))                 == "{\"head\":\"Zeros\",\"dimension\":2}"
    @test JSON.json(MOF.object(MOI.Nonpositives(2)))          == "{\"head\":\"Nonpositives\",\"dimension\":2}"
    @test JSON.json(MOF.object(MOI.Nonnegatives(2)))          == "{\"head\":\"Nonnegatives\",\"dimension\":2}"
    @test JSON.json(MOF.object(MOI.Semicontinuous(2.5, 3.0))) == "{\"head\":\"Semicontinuous\",\"lower\":2.5,\"upper\":3.0}"
    @test JSON.json(MOF.object(MOI.Semiinteger(2, 5)))        == "{\"head\":\"Semiinteger\",\"lower\":2,\"upper\":5}"
    @test JSON.json(MOF.object(MOI.SOS1([1,2])))            == "{\"head\":\"SOS1\",\"weights\":[1,2]}"
    @test JSON.json(MOF.object(MOI.SOS2([3,4,5])))            == "{\"head\":\"SOS2\",\"weights\":[3,4,5]}"

    @test JSON.json(MOF.object(MOI.Interval(-Inf, Inf)))       == "{\"head\":\"Interval\",\"lower\":\"-inf\",\"upper\":\"+inf\"}"
    @test JSON.json(MOF.object(MOI.Semicontinuous(-Inf, Inf)))       == "{\"head\":\"Semicontinuous\",\"lower\":\"-inf\",\"upper\":\"+inf\"}"
    @test JSON.json(MOF.object(MOI.Semiinteger(-Inf, Inf)))       == "{\"head\":\"Semiinteger\",\"lower\":\"-inf\",\"upper\":\"+inf\"}"

    @test JSON.json(MOF.object(MOI.SecondOrderCone(2)))       == "{\"head\":\"SecondOrderCone\",\"dimension\":2}"
    @test JSON.json(MOF.object(MOI.RotatedSecondOrderCone(2)))       == "{\"head\":\"RotatedSecondOrderCone\",\"dimension\":2}"

    @test JSON.json(MOF.object(MOI.ExponentialCone()))       == "{\"head\":\"ExponentialCone\"}"
    @test JSON.json(MOF.object(MOI.DualExponentialCone()))       == "{\"head\":\"DualExponentialCone\"}"

    @test JSON.json(MOF.object(MOI.PowerCone(0.5)))       == "{\"head\":\"PowerCone\",\"exponent\":0.5}"
    @test JSON.json(MOF.object(MOI.DualPowerCone(0.5)))       == "{\"head\":\"DualPowerCone\",\"exponent\":0.5}"

    @test JSON.json(MOF.object(MOI.PositiveSemidefiniteConeTriangle(2)))       == "{\"head\":\"PositiveSemidefiniteConeTriangle\",\"dimension\":2}"
    @test JSON.json(MOF.object(MOI.PositiveSemidefiniteConeSquare(2)))       == "{\"head\":\"PositiveSemidefiniteConeSquare\",\"dimension\":2}"

    @test JSON.json(MOF.object(MOI.GeometricMeanCone(2)))       == "{\"head\":\"GeometricMeanCone\",\"dimension\":2}"
    @test JSON.json(MOF.object(MOI.LogDetConeTriangle(2)))       == "{\"head\":\"LogDetConeTriangle\",\"dimension\":2}"
    @test JSON.json(MOF.object(MOI.LogDetConeSquare(2)))       == "{\"head\":\"LogDetConeSquare\",\"dimension\":2}"
    @test JSON.json(MOF.object(MOI.RootDetConeTriangle(2)))       == "{\"head\":\"RootDetConeTriangle\",\"dimension\":2}"
    @test JSON.json(MOF.object(MOI.RootDetConeSquare(2)))       == "{\"head\":\"RootDetConeSquare\",\"dimension\":2}"
end

@testset "Functions" begin

    @testset "SingleVariable" begin
        instance = MOF.MOFInstance()
        v = MOI.addvariable!(instance)
        @test JSON.json(MOF.object!(instance, MOI.SingleVariable(v))) == "{\"head\":\"SingleVariable\",\"variable\":\"x1\"}"
    end

    @testset "VectorOfVariables" begin
        instance = MOF.MOFInstance()
        v = MOI.addvariable!(instance)
        @test JSON.json(MOF.object!(instance, MOI.VectorOfVariables([v,v]))) == "{\"head\":\"VectorOfVariables\",\"variables\":[\"x1\",\"x1\"]}"
    end

    @testset "ScalarAffineFunction" begin
        instance = MOF.MOFInstance()
        v = MOI.addvariable!(instance)
        f = MOI.ScalarAffineFunction([v, v], [1.0, 2.0], 3.0)
        @test JSON.json(MOF.object!(instance, f)) == "{\"head\":\"ScalarAffineFunction\",\"variables\":[\"x1\",\"x1\"],\"coefficients\":[1.0,2.0],\"constant\":3.0}"
    end

    @testset "VectorAffineFunction" begin
        instance = MOF.MOFInstance()
        v = MOI.addvariable!(instance)
        f = MOI.VectorAffineFunction([1, 1], [v, v], [1.0, 2.0], [3.0])
        @test JSON.json(MOF.object!(instance, f)) == "{\"head\":\"VectorAffineFunction\",\"outputindex\":[1,1],\"variables\":[\"x1\",\"x1\"],\"coefficients\":[1.0,2.0],\"constant\":[3.0]}"
    end

    @testset "ScalarQuadraticFunction" begin
        instance = MOF.MOFInstance()
        v = MOI.addvariable!(instance)
        f = MOI.ScalarQuadraticFunction([v], [1.0], [v], [v], [2.0], 3.0)
        @test JSON.json(MOF.object!(instance, f)) == "{\"head\":\"ScalarQuadraticFunction\",\"affine_variables\":[\"x1\"],\"affine_coefficients\":[1.0],\"quadratic_rowvariables\":[\"x1\"],\"quadratic_colvariables\":[\"x1\"],\"quadratic_coefficients\":[2.0],\"constant\":3.0}"
    end

    @testset "VectorQuadraticFunction" begin
        instance = MOF.MOFInstance()
        v = MOI.addvariable!(instance)
        f = MOI.VectorQuadraticFunction([1], [v], [1.0], [1], [v], [v], [2.0], [3.0])
        @test JSON.json(MOF.object!(instance, f)) == "{\"head\":\"VectorQuadraticFunction\",\"affine_outputindex\":[1],\"affine_variables\":[\"x1\"],\"affine_coefficients\":[1.0],\"quadratic_outputindex\":[1],\"quadratic_rowvariables\":[\"x1\"],\"quadratic_colvariables\":[\"x1\"],\"quadratic_coefficients\":[2.0],\"constant\":[3.0]}"
    end
end

@testset "OptimizationSense" begin
    @test MOF.object(MOI.MinSense) == "min"
    @test MOF.object(MOI.MaxSense) == "max"
    @test_throws Exception MOF.object(MOI.FeasibilitySense)
    instance = MOF.MOFInstance()
    instance["sense"] = "Max"
    @test_throws Exception MOI.get(instance, MOI.ObjectiveSense())
end

@testset "Write Examples" begin

    @testset "1.mof.json" begin
        # min 1x + 2x + 3
        # s.t        x >= 3
        instance = MOF.MOFInstance()
        v = MOI.addvariable!(instance)
        f = MOI.ScalarAffineFunction([v, v], [1.0, 2.0], 3.0)
        MOI.set!(instance, MOI.ObjectiveFunction(),f)
        MOI.set!(instance, MOI.ObjectiveSense(), MOI.MinSense)
        @test MOI.canaddconstraint(instance,
            MOI.ScalarAffineFunction([v], [1.0], 0.0),
            MOI.GreaterThan(3.0))
        c1 = MOI.addconstraint!(instance,
            MOI.ScalarAffineFunction([v], [1.0], 0.0),
            MOI.GreaterThan(3.0)
        )
        @test MOI.canset(instance, MOI.ConstraintName(), c1)
        MOI.set!(instance, MOI.ConstraintName(), c1, "firstconstraint")
        @test typeof(c1) == MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}
        WRITEFILES && MOI.write(instance, problempath("1.mof.json"), 1)
        @test stringify(instance) == getproblem("1.mof.json")
        @test MOI.canmodifyconstraint(instance, c1, MOI.GreaterThan(4.0))
        MOI.modifyconstraint!(instance, c1, MOI.GreaterThan(4.0))
        WRITEFILES && MOI.write(instance, problempath("1a.mof.json"), 1)
        @test stringify(instance) == getproblem("1a.mof.json")
        @test MOI.canmodifyconstraint(instance, c1, MOI.ScalarAffineFunction([v], [2.0], 1.0))
        MOI.modifyconstraint!(instance, c1, MOI.ScalarAffineFunction([v], [2.0], 1.0))
        WRITEFILES && MOI.write(instance, problempath("1b.mof.json"), 1)
        @test stringify(instance) == getproblem("1b.mof.json")
        @test MOI.canmodifyconstraint(instance, c1, MOI.ScalarConstantChange(1.5))
        MOI.modifyconstraint!(instance, c1, MOI.ScalarConstantChange(1.5))
        WRITEFILES && MOI.write(instance, problempath("1c.mof.json"), 1)
        @test stringify(instance) == getproblem("1c.mof.json")
        @test MOI.candelete(instance, c1)
        MOI.delete!(instance, c1)
        WRITEFILES && MOI.write(instance, problempath("1d.mof.json"), 1)
        @test stringify(instance) == getproblem("1d.mof.json")
        @test MOI.canaddconstraint(instance,
            MOI.SingleVariable(v),
            MOI.Semicontinuous(1.0, 5.0)
        )
        c2 = MOI.addconstraint!(instance,
            MOI.SingleVariable(v),
            MOI.Semicontinuous(1.0, 5.0)
        )
        u = MOI.addvariable!(instance)
        @test MOI.canaddconstraint(instance,
            MOI.SingleVariable(u),
            MOI.Semiinteger(2, 6)
        )
        c3 = MOI.addconstraint!(instance,
            MOI.SingleVariable(u),
            MOI.Semiinteger(2, 6)
        )
        WRITEFILES && MOI.write(instance, problempath("1e.mof.json"), 1)
        @test stringify(instance) == getproblem("1e.mof.json")
        @test MOI.candelete(instance, c2)
        MOI.delete!(instance, c2)
        WRITEFILES && MOI.write(instance, problempath("1f.mof.json"), 1)
        @test stringify(instance) == getproblem("1f.mof.json")
        @test MOI.cantransformconstraint(instance,
            c3,
            MOI.Integer()
        )
        c4 = MOI.transformconstraint!(instance,
            c3,
            MOI.Integer()
        )
        @test MOI.canget(instance, MOI.ConstraintSet(), c4)
        @test MOI.get(instance, MOI.ConstraintSet(), c4) == MOI.Integer()
        @test MOI.canget(instance, MOI.ConstraintFunction(), c4)
        @test MOI.get(instance, MOI.ConstraintFunction(), c4) == MOI.SingleVariable(u)
        WRITEFILES && MOI.write(instance, problempath("1g.mof.json"), 1)
        @test stringify(instance) == getproblem("1g.mof.json")
    end

    @testset "2.mof.json" begin
        # min 2x - y
        # s.t 2x + 1 == 0
        #      x ∈ Z
        #      y ∈ {0, 1}
        instance = MOF.MOFInstance()
        x = MOI.addvariable!(instance)
        y = MOI.addvariable!(instance)
        @test MOI.canget(instance, MOI.NumberOfVariables())
        @test MOI.get(instance, MOI.NumberOfVariables()) == 2

        @test MOI.canget(instance, MOI.ListOfVariableIndices())
        @test MOI.get(instance, MOI.ListOfVariableIndices()) == [x,y]

        c = MOI.ScalarAffineFunction([x, y], [2.0, -1.0], 0.0)
        MOI.set!(instance, MOI.ObjectiveFunction(), c)
        MOI.set!(instance, MOI.ObjectiveSense(), MOI.MaxSense)
        @test MOI.canaddconstraint(instance,
            MOI.ScalarAffineFunction([x], [2.0], 1.0),
            MOI.EqualTo(3.0)
        )
        c1 = MOI.addconstraint!(instance,
            MOI.ScalarAffineFunction([x], [2.0], 1.0),
            MOI.EqualTo(3.0)
        )
        @test typeof(c1) == MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}}
        @test MOI.canaddconstraint(instance, MOI.SingleVariable(x), MOI.Integer())
        MOI.addconstraint!(instance, MOI.SingleVariable(x), MOI.Integer())
        @test MOI.canaddconstraint(instance, MOI.SingleVariable(y), MOI.ZeroOne())
        MOI.addconstraint!(instance, MOI.SingleVariable(y), MOI.ZeroOne())
        WRITEFILES && MOI.write(instance, problempath("2.mof.json"), 1)
        @test stringify(instance) == getproblem("2.mof.json")
        @test MOI.canset(instance, MOI.VariablePrimalStart(), x)
        MOI.set!(instance, MOI.VariablePrimalStart(), x, 1.0)
        WRITEFILES && MOI.write(instance, problempath("2a.mof.json"), 1)
        @test stringify(instance) == getproblem("2a.mof.json")
        @test MOI.canset(instance, MOI.ConstraintPrimalStart(), c1)
        MOI.set!(instance, MOI.ConstraintPrimalStart(), c1, 1.0)
        WRITEFILES && MOI.write(instance, problempath("2b.mof.json"), 1)
        @test stringify(instance) == getproblem("2b.mof.json")
        @test MOI.canset(instance, MOI.ConstraintDualStart(), c1)
        MOI.set!(instance, MOI.ConstraintDualStart(), c1, -1.0)
        WRITEFILES && MOI.write(instance, problempath("2c.mof.json"), 1)
        @test stringify(instance) == getproblem("2c.mof.json")

        @test MOI.canget(instance, MOI.VariablePrimalStart(), x)
        @test MOI.get(instance, MOI.VariablePrimalStart(), x) == 1.0
        @test MOI.canget(instance, MOI.VariableName(), x)
        @test MOI.get(instance, MOI.VariableName(), x) == "x1"
        @test MOI.canget(instance, MOI.VariableIndex, "x1")
        @test MOI.get(instance, MOI.VariableIndex, "x1") == x
        @test MOI.canset(instance, MOI.VariableName(), x)
        MOI.set!(instance, MOI.VariableName(), x, "y")
        @test MOI.canget(instance, MOI.VariableIndex, "y")
        @test MOI.canget(instance, MOI.VariableIndex, "x1") == false
        @test MOI.get(instance, MOI.VariableIndex, "y") == x

        @test MOI.canget(instance, MOI.ConstraintName(), c1)
        @test MOI.get(instance, MOI.ConstraintName(), c1) == "c1"
        @test MOI.canget(instance, MOI.ConstraintPrimalStart(), c1)
        @test MOI.get(instance, MOI.ConstraintPrimalStart(), c1) == 1.0
        @test MOI.canget(instance, MOI.ConstraintDualStart(), c1)
        @test MOI.get(instance, MOI.ConstraintDualStart(), c1) == -1.0

        WRITEFILES && MOI.write(instance, problempath("2d.mof.json"), 1)
        @test stringify(instance) == getproblem("2d.mof.json")
    end

    @testset "3.mof.json" begin
        instance = MOF.MOFInstance()

        (x1, x2, x3) = MOI.addvariables!(instance, 3)
        MOI.set!(instance, MOI.ObjectiveFunction(),
            MOI.ScalarQuadraticFunction(
                [x1, x2],
                [2.0, -1.0],
                [x1],
                [x1],
                [1.0],
                0.0
            )
        )
        @test MOI.canset(instance, MOI.ObjectiveSense(), MOI.MaxSense)
        MOI.set!(instance, MOI.ObjectiveSense(), MOI.MaxSense)
        @test MOI.canget(instance, MOI.ObjectiveSense())
        @test MOI.get(instance, MOI.ObjectiveSense()) == MOI.MaxSense

        @test MOI.canaddconstraint(instance,
            MOI.VectorOfVariables([x1, x2, x3]),
            MOI.SOS2([1.0, 2.0, 3.0])
        )
        c1 = MOI.addconstraint!(instance,
            MOI.VectorOfVariables([x1, x2, x3]),
            MOI.SOS2([1.0, 2.0, 3.0])
        )
        @test typeof(c1) == MOI.ConstraintIndex{MOI.VectorOfVariables, MOI.SOS2{Float64}}
        WRITEFILES && MOI.write(instance, problempath("3.mof.json"), 1)
        @test stringify(instance) == getproblem("3.mof.json")
    end

    @testset "linear7.mof.json" begin
        # Min  x - y
        # s.t. 0.0 <= x          (c1)
        #             y <= 0.0   (c2)
        instance = MOF.MOFInstance()
        x = MOI.addvariable!(instance)
        y = MOI.addvariable!(instance)
        @test MOI.canset(instance, MOI.VariableName(), x)
        MOI.set!(instance, MOI.VariableName(), x, "x")
        @test MOI.canset(instance, MOI.VariableName(), y)
        MOI.set!(instance, MOI.VariableName(), y, "y")

        @test MOI.canset(instance, MOI.ObjectiveFunction(),
            MOI.ScalarAffineFunction([x, y], [1.0, -1.0], 0.0)
        )
        obj = MOI.ScalarAffineFunction([x, y], [1.0, -1.0], 0.0)
        MOI.set!(instance, MOI.ObjectiveFunction(), obj)
        @test MOI.canget(instance, MOI.ObjectiveFunction())
        obj2 = MOI.get(instance, MOI.ObjectiveFunction())
        @test obj.variables == obj2.variables
        @test obj.coefficients == obj2.coefficients
        @test obj.constant == obj2.constant
        MOI.set!(instance, MOI.ObjectiveSense(), MOI.MinSense)
        @test MOI.canget(instance, MOI.ObjectiveSense())
        @test MOI.get(instance, MOI.ObjectiveSense()) == MOI.MinSense

        @test MOI.canaddconstraint(instance, MOI.VectorAffineFunction([1],[x],[1.0],[0.0]), MOI.Nonnegatives(1))
        MOI.addconstraint!(instance, MOI.VectorAffineFunction([1],[x],[1.0],[0.0]), MOI.Nonnegatives(1))

        @test MOI.canaddconstraint(instance, MOI.VectorAffineFunction([1],[y],[1.0],[0.0]), MOI.Nonpositives(1))
        c = MOI.addconstraint!(instance, MOI.VectorAffineFunction([1],[y],[1.0],[0.0]), MOI.Nonpositives(1))
        WRITEFILES && MOI.write(instance, problempath("linear7.mof.json"), 1)
        @test stringify(instance) == getproblem("linear7.mof.json")

        @test MOI.canmodifyconstraint(instance, c, MOI.VectorConstantChange([1.0]))
        MOI.modifyconstraint!(instance, c, MOI.VectorConstantChange([1.0]))
        WRITEFILES && MOI.write(instance, problempath("linear7a.mof.json"), 1)
        @test stringify(instance) == getproblem("linear7a.mof.json")
    end

    @testset "qp1.mof.json" begin
        # simple quadratic objective
        # Min x^2 + xy + y^2 + yz + z^2
        # st  x + 2y + 3z >= 4 (c1)
        #     x +  y      >= 1 (c2)
        #     x,y \in R
        instance = MOF.MOFInstance()

        v = MOI.addvariables!(instance, 3)

        @test MOI.canaddconstraint(instance,
            MOI.ScalarAffineFunction(v, [1.0,2.0,3.0], 0.0),
            MOI.GreaterThan(4.0)
        )
        MOI.addconstraint!(instance,
            MOI.ScalarAffineFunction(v, [1.0,2.0,3.0], 0.0),
            MOI.GreaterThan(4.0)
        )
        @test MOI.canaddconstraint(instance,
            MOI.ScalarAffineFunction([v[1],v[2]], [1.0,1.0], 0.0),
            MOI.GreaterThan(1.0)
        )
        MOI.addconstraint!(instance,
            MOI.ScalarAffineFunction([v[1],v[2]], [1.0,1.0], 0.0),
            MOI.GreaterThan(1.0)
        )
        MOI.set!(instance, MOI.ObjectiveFunction(),
            MOI.ScalarQuadraticFunction(
                MOI.VariableIndex[],
                Float64[],
                v[[1,1,2,2,3]],
                v[[1,2,2,3,3]],
                [2.0, 1.0, 2.0, 1.0, 2.0],
                0.0
            )
        )
        MOI.set!(instance, MOI.ObjectiveSense(), MOI.MinSense)
        WRITEFILES && MOI.write(instance, problempath("qp1.mof.json"), 1)
        @test stringify(instance) == getproblem("qp1.mof.json")
    end

    @testset "qcp.mof.json" begin
        # quadratic constraint
        # Max x + y
        # st  - x + y >= 0 (c1[1])
        #       x + y >= 0 (c1[2])
        #     0.5x^2 + y <= 2 (c2)

        instance = MOF.MOFInstance()

        x = MOI.addvariable!(instance)
        y = MOI.addvariable!(instance)

        @test MOI.canaddconstraint(instance, MOI.VectorAffineFunction([1,1,2,2], [x,y,x,y],[-1.0,1.0,1.0,1.0], [0.0,0.0]), MOI.Nonnegatives(2))
        MOI.addconstraint!(instance, MOI.VectorAffineFunction([1,1,2,2], [x,y,x,y],[-1.0,1.0,1.0,1.0], [0.0,0.0]), MOI.Nonnegatives(2))

        f = MOI.VectorQuadraticFunction([1], [y],[1.0], [1], [x],[x],[1.0], [0.0])
        @test MOI.canaddconstraint(instance, f, MOI.LessThan(2.0))
        MOI.addconstraint!(instance, f, MOI.LessThan(2.0))

        MOI.set!(instance, MOI.ObjectiveFunction(), MOI.ScalarAffineFunction([x,y], [1.0,1.0], 0.0))
        MOI.set!(instance, MOI.ObjectiveSense(), MOI.MaxSense)
        WRITEFILES && MOI.write(instance, problempath("qcp.mof.json"), 1)
        @test stringify(instance) == getproblem("qcp.mof.json")
    end

    @testset "conic.mof.json" begin
        # an unrealistic model to test functionality
        instance = MOF.MOFInstance()

        v = MOI.addvariables!(instance, 3)
        # reals
        @test MOI.canaddconstraint(instance,
            MOI.VectorOfVariables(v),
            MOI.Reals(3)
        )
        MOI.addconstraint!(instance,
            MOI.VectorOfVariables(v),
            MOI.Reals(3)
        )
        # second order
        @test MOI.canaddconstraint(instance,
            MOI.VectorOfVariables(v),
            MOI.SecondOrderCone(3)
        )
        MOI.addconstraint!(instance,
            MOI.VectorOfVariables(v),
            MOI.SecondOrderCone(3)
        )
        # rotated second order
        @test MOI.canaddconstraint(instance,
            MOI.VectorOfVariables(v),
            MOI.RotatedSecondOrderCone(3)
        )
        MOI.addconstraint!(instance,
            MOI.VectorOfVariables(v),
            MOI.RotatedSecondOrderCone(3)
        )
        # exponential
        @test MOI.canaddconstraint(instance,
            MOI.VectorOfVariables(v),
            MOI.ExponentialCone()
        )
        MOI.addconstraint!(instance,
            MOI.VectorOfVariables(v),
            MOI.ExponentialCone()
        )
        # dual exponential
        @test MOI.canaddconstraint(instance,
            MOI.VectorOfVariables(v),
            MOI.DualExponentialCone()
        )
        MOI.addconstraint!(instance,
            MOI.VectorOfVariables(v),
            MOI.DualExponentialCone()
        )
        # power
        @test MOI.canaddconstraint(instance,
            MOI.VectorOfVariables(v),
            MOI.PowerCone(1.5)
        )
        MOI.addconstraint!(instance,
            MOI.VectorOfVariables(v),
            MOI.PowerCone(1.5)
        )
        # dual power
        @test MOI.canaddconstraint(instance,
            MOI.VectorOfVariables(v),
            MOI.DualPowerCone(1.5)
        )
        MOI.addconstraint!(instance,
            MOI.VectorOfVariables(v),
            MOI.DualPowerCone(1.5)
        )
        # psd triangle
        @test MOI.canaddconstraint(instance,
            MOI.VectorOfVariables(v),
            MOI.PositiveSemidefiniteConeTriangle(3)
        )
        MOI.addconstraint!(instance,
            MOI.VectorOfVariables(v),
            MOI.PositiveSemidefiniteConeTriangle(3)
        )
        # psd square
        @test MOI.canaddconstraint(instance,
            MOI.VectorOfVariables(v),
            MOI.PositiveSemidefiniteConeSquare(3)
        )
        MOI.addconstraint!(instance,
            MOI.VectorOfVariables(v),
            MOI.PositiveSemidefiniteConeSquare(3)
        )
        # geom mean
        @test MOI.canaddconstraint(instance,
            MOI.VectorOfVariables(v),
            MOI.GeometricMeanCone(3)
        )
        MOI.addconstraint!(instance,
            MOI.VectorOfVariables(v),
            MOI.GeometricMeanCone(3)
        )
        # logdet
        @test MOI.canaddconstraint(instance,
            MOI.VectorOfVariables(v),
            MOI.LogDetConeTriangle(3)
        )
        MOI.addconstraint!(instance,
            MOI.VectorOfVariables(v),
            MOI.LogDetConeTriangle(3)
        )
        # logdet square
        @test MOI.canaddconstraint(instance,
            MOI.VectorOfVariables(v),
            MOI.LogDetConeSquare(3)
        )
        MOI.addconstraint!(instance,
            MOI.VectorOfVariables(v),
            MOI.LogDetConeSquare(3)
        )
        # rootdet
        @test MOI.canaddconstraint(instance,
            MOI.VectorOfVariables(v),
            MOI.RootDetConeTriangle(3)
        )
        MOI.addconstraint!(instance,
            MOI.VectorOfVariables(v),
            MOI.RootDetConeTriangle(3)
        )
        # rootdet square
        @test MOI.canaddconstraint(instance,
            MOI.VectorOfVariables(v),
            MOI.RootDetConeSquare(3)
        )
        MOI.addconstraint!(instance,
            MOI.VectorOfVariables(v),
            MOI.RootDetConeSquare(3)
        )

        MOI.set!(instance, MOI.ObjectiveFunction(),
            MOI.ScalarAffineFunction(
                MOI.VariableIndex[],
                Float64[],
                0.0
            )
        )
        MOI.set!(instance, MOI.ObjectiveSense(), MOI.MinSense)

        WRITEFILES && MOI.write(instance, problempath("conic.mof.json"), 1)
        @test stringify(instance) == getproblem("conic.mof.json")
    end
end

@testset "Read-Write Examples" begin
    for prob in [
            "1","1a","1b","1c","1d","1e","1f", "2", "2a", "2b", "2c", "2d", "3", "linear7", "linear7a", "qp1", "qcp", "LIN1", "LIN2", "linear1", "linear2", "mip01", "sos1", "conic"
            ]
        @testset "$(prob)" begin
            file_representation = getproblem("$(prob).mof.json")
            # load from file
            problem = MOF.MOFInstance(problempath("$(prob).mof.json"))
            @test stringify(problem) == file_representation

            model =  MOF.MOFInstance()
            MOI.read!(model, problempath("$(prob).mof.json"))
            @test stringify(model) == file_representation

            model2 = MOF.MOFInstance()
            MOI.copy!(model2, model)
            @test stringify(model2) == file_representation
        end
    end
end
