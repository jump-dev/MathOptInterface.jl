
structeq(a::T, b::T) where {T} = all(f->getfield(a, f) == getfield(b, f), fieldnames(T))

@testset "parsefunction" begin
    @test structeq(MOIU.parsefunction(:x), MOIU.ParsedSingleVariable(:x))
    @test structeq(MOIU.parsefunction(:([x,y,z])), MOIU.ParsedVectorOfVariables([:x,:y,:z]))
    @test structeq(MOIU.parsefunction(:(x + y + 2.0)), MOIU.ParsedScalarAffineFunction([:x,:y],[1.0,1.0],2.0))
    @test structeq(MOIU.parsefunction(:(x + -3y + 2.0)), MOIU.ParsedScalarAffineFunction([:x,:y],[1.0,-3.0],2.0))
    @test structeq(MOIU.parsefunction(:(2*x*y + y + 1.0)), MOIU.ParsedScalarQuadraticFunction([:y],[1.0],[:x],[:y],[2.0],1.0))
    @test_throws AssertionError MOIU.parsefunction(:(x - y))

    @test structeq(MOIU.parsefunction(:([x, 2x+y+5.0])), MOIU.ParsedVectorAffineFunction([1,2,2],[:x,:x,:y],[1.0,2.0,1.0],[0.0,5.0]))
    @test structeq(MOIU.parsefunction(:([x, 2x+y+5.0, 1*x*x])), MOIU.ParsedVectorQuadraticFunction([1,2,2],[:x,:x,:y],[1.0,2.0,1.0],[3],[:x],[:x],[2.0],[0.0,5.0,0.0]))

end

@testset "separatelabel" begin
    @test MOIU.separatelabel(:(variables: x)) == (:variables, :x)
    @test MOIU.separatelabel(:(variables: x, y)) == (:variables, :((x,y)))
    @test MOIU.separatelabel(:(minobjective: x + y)) == (:minobjective, :(x+y))
    @test MOIU.separatelabel(:(con1: 2x <= 1)) == (:con1, :(2x <= 1))
    @test MOIU.separatelabel(:(con1: [x,y] in S)) == (:con1, :([x,y] in S))
end

@MOIU.model GeneralModel (ZeroOne, Integer) (EqualTo, GreaterThan, LessThan, Interval) (Zeros, Nonnegatives, Nonpositives, SecondOrderCone, RotatedSecondOrderCone, PositiveSemidefiniteConeTriangle) () (SingleVariable,) (ScalarAffineFunction,ScalarQuadraticFunction) (VectorOfVariables,) (VectorAffineFunction,)

@testset "loadfromstring" begin
    @testset "one variable" begin
        s = """
        variables: x
        bound: x >= 1.0
        """
        model = GeneralModel{Float64}()
        x = MOI.addvariable!(model)
        MOI.set!(model, MOI.VariableName(), x, "x")
        bound = MOI.addconstraint!(model, MOI.SingleVariable(x), MOI.GreaterThan(1.0))
        MOI.set!(model, MOI.ConstraintName(), bound, "bound")

        model2 = GeneralModel{Float64}()
        MOIU.loadfromstring!(model2, s)
        MOIU.test_models_equal(model, model2, ["x"], ["bound"])
    end

    @testset "linear constraints" begin
        s = """
        variables: x, y
        linear1: x + y >= 1.0
        linear2: x + y <= 1.0
        linear3: x + y == 1.0
        """
        model = GeneralModel{Float64}()
        x = MOI.addvariable!(model)
        y = MOI.addvariable!(model)
        MOI.set!(model, MOI.VariableName(), x, "x")
        MOI.set!(model, MOI.VariableName(), y, "y")
        linear1 = MOI.addconstraint!(model, MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x), MOI.ScalarAffineTerm(1.0, y)], 0.0), MOI.GreaterThan(1.0))
        linear2 = MOI.addconstraint!(model, MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x), MOI.ScalarAffineTerm(1.0, y)], 0.0), MOI.LessThan(1.0))
        linear3 = MOI.addconstraint!(model, MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x), MOI.ScalarAffineTerm(1.0, y)], 0.0), MOI.EqualTo(1.0))
        MOI.set!(model, MOI.ConstraintName(), linear1, "linear1")
        MOI.set!(model, MOI.ConstraintName(), linear2, "linear2")
        MOI.set!(model, MOI.ConstraintName(), linear3, "linear3")

        model2 = GeneralModel{Float64}()
        MOIU.loadfromstring!(model2, s)
        MOIU.test_models_equal(model, model2, ["x", "y"], ["linear1", "linear2", "linear3"])
    end

    @testset "minimization: linear objective" begin
        s = """
        variables: x, y
        minobjective: x + -2y + 1.0
        """
        model = GeneralModel{Float64}()
        x = MOI.addvariable!(model)
        y = MOI.addvariable!(model)
        MOI.set!(model, MOI.VariableName(), x, "x")
        MOI.set!(model, MOI.VariableName(), y, "y")
        MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x), MOI.ScalarAffineTerm(-2.0, y)], 1.0))
        MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

        model2 = GeneralModel{Float64}()
        MOIU.loadfromstring!(model2, s)
        MOIU.test_models_equal(model, model2, ["x", "y"], String[])
    end

    @testset "maximization: linear objective" begin
        s = """
        variables: x, y
        maxobjective: x + -2y + 1.0
        """
        model = GeneralModel{Float64}()
        x = MOI.addvariable!(model)
        y = MOI.addvariable!(model)
        MOI.set!(model, MOI.VariableName(), x, "x")
        MOI.set!(model, MOI.VariableName(), y, "y")
        MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x), MOI.ScalarAffineTerm(-2.0, y)], 1.0))
        MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)

        model2 = GeneralModel{Float64}()
        MOIU.loadfromstring!(model2, s)
        MOIU.test_models_equal(model, model2, ["x", "y"], String[])
    end

    @testset "SOC constraints" begin
        s = """
        variables: x, y, z
        varsoc: [x,y,z] in SecondOrderCone(3)
        affsoc: [2x,y+1,-1*z] in SecondOrderCone(3)
        affsoc2: [1.0,2.0,3.0] in SecondOrderCone(3)
        """
        model = GeneralModel{Float64}()
        x = MOI.addvariable!(model)
        y = MOI.addvariable!(model)
        z = MOI.addvariable!(model)
        MOI.set!(model, MOI.VariableName(), x, "x")
        MOI.set!(model, MOI.VariableName(), y, "y")
        MOI.set!(model, MOI.VariableName(), z, "z")
        varsoc = MOI.addconstraint!(model, MOI.VectorOfVariables([x,y,z]), MOI.SecondOrderCone(3))
        affsoc = MOI.addconstraint!(model, MOI.VectorAffineFunction([1,2,3],[x,y,z],[2.0,1.0,-1.0],[0.0,1.0,0.0]), MOI.SecondOrderCone(3))
        affsoc2 = MOI.addconstraint!(model, MOI.VectorAffineFunction(Int[],MOI.VariableIndex[],Float64[],[1.0,2.0,3.0]), MOI.SecondOrderCone(3))
        MOI.set!(model, MOI.ConstraintName(), varsoc, "varsoc")
        MOI.set!(model, MOI.ConstraintName(), affsoc, "affsoc")
        MOI.set!(model, MOI.ConstraintName(), affsoc2, "affsoc2")

        model2 = GeneralModel{Float64}()
        MOIU.loadfromstring!(model2, s)
        MOIU.test_models_equal(model, model2, ["x", "y", "z"], ["varsoc", "affsoc", "affsoc2"])
    end

end
