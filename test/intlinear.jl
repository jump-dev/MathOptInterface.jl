using MathOptInterface
MOI = MathOptInterface

using MathOptInterfaceUtilities

function int1test(solver::MOI.AbstractSolver, ε=Base.rtoldefault(Float64))
    @testset "MIP01 from CPLEX.jl" begin
        # an example on mixed integer programming
        #
        #   maximize x + 2 y + 5 z
        #
        #   s.t.  x + y + z <= 10
        #         x + 2 y + z <= 15
        #
        #         x is continuous: 0 <= x <= 5
        #         y is integer: 0 <= y <= 10
        #         z is binary

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64}), (MOI.SingleVariable, MOI.ZeroOne), (MOI.SingleVariable, MOI.Integer)])

        m = MOI.SolverInstance(solver)

        v = MOI.addvariables!(m, 3)
        @test MOI.getattribute(m, MOI.NumberOfVariables()) == 3

        cf = MOI.ScalarAffineFunction(v, [1.0,1.0,1.0], 0.0)
        c = MOI.addconstraint!(m, cf, MOI.LessThan(10.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

        cf2 = MOI.ScalarAffineFunction(v, [1.0,3.0,1.0], 0.0)
        c2 = MOI.addconstraint!(m, cf, MOI.LessThan(15.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 2


        MOI.addconstraint!(m, MOI.SingleVariable(v[1]), MOI.Interval(0.0, 5.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.Interval{Float64}}()) == 1

        MOI.addconstraint!(m, MOI.SingleVariable(v[2]), MOI.Interval(0.0, 10.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.Interval{Float64}}()) == 1
        MOI.addconstraint!(m, MOI.SingleVariable(v[2]), MOI.Integer())
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.Integer}()) == 1

        MOI.addconstraint!(m, MOI.SingleVariable(v[3]), MOI.ZeroOne()))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.ZeroOne}()) == 1


        objf = MOI.ScalarAffineFunction(v, [1.0, 2.0, 5.0], 0.0)
        MOI.setobjective!(m, MOI.MaxSense, objf)

        @test MOI.getattribute(m, MOI.Sense()) == MOI.MaxSense

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.ResultCount())
        @test MOI.getattribute(m, MOI.ResultCount()) == 1

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 19 atol=ε

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
        @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [4,5,1] atol=ε

        @test MOI.cangetattribute(m, MOI.ConstraintPrimal(), c)
        @test MOI.getattribute(m, MOI.ConstraintPrimal(), c) ≈ 10 atol=ε

        @test MOI.cangetattribute(m, MOI.ConstraintPrimal(), c2)
        @test MOI.getattribute(m, MOI.ConstraintPrimal(), c2) ≈ 15 atol=ε

        @test MOI.cangetattribute(m, MOI.DualStatus()) == false
    end
end

# Mixed-integer linear problems

function knapsacktest(solver::MOI.AbstractSolver, eps=Base.rtoldefault(Float64))
    @testset "Knapsack model" begin
        # integer knapsack problem
        # max 5a + 3b + 2c + 7d + 4e
        # st  2a + 8b + 4c + 2d + 5e <= 10
        #                  a,b,c,d,e ∈ binary

        m = MOI.SolverInstance(solver)

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.SingleVariable,MOI.ZeroOne),(MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64})])

        v = MOI.addvariables!(m, 5)
        @test MOI.getattribute(m, MOI.NumberOfVariables()) == 5

        for vi in v
            MOI.addconstraint!(m, MOI.SingleVariable(vi), MOI.ZeroOne())
        end
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.ZeroOne}()) == 5
        c = MOI.addconstraint!(m, MOI.ScalarAffineFunction(v, [2.0, 8.0, 4.0, 2.0, 5.0], 0.0), MOI.LessThan(10.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

        MOI.setobjective!(m, MOI.MaxSense, MOI.ScalarAffineFunction(v, [5.0, 3.0, 2.0, 7.0, 4.0], 0.0))

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 16 atol=eps

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
        @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [1, 0, 0, 1, 1] atol=eps
    end
end

function intlineartest(solver::MOI.AbstractSolver, eps=Base.rtoldefault(Float64))
    knapsacktest(solver, ɛ)

    # TODO more test sets here
end
