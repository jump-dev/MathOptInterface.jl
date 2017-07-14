# Mixed-integer linear problems

function intlineartest(solver::MOI.AbstractSolver, eps=Base.rtoldefault(Float64))
    @testset "Knapsack model" begin
        # integer knapsack problem
        # max 5a + 3b + 2c + 7d + 4e
        # st  2a + 8b + 4c + 2d + 5e <= 10
        #                  a,b,c,d,e ∈ binary

        m = MOI.SolverInstance(solver)

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction, [(MOI.ScalarVariablewiseFunction,MOI.ZeroOne),(MOI.ScalarAffineFunction{Float64},MOI.LessThan)])

        v = MOI.addvariables!(m, 5)
        @test MOI.getattribute(m, MOI.NumberOfVariables()) == 5

        for vi in v
            MOI.addconstraint!(m, MOI.ScalarVariablewiseFunction(vi), MOI.ZeroOne())
        end
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarVariablewiseFunction,MOI.ZeroOne}()) == 5
        c = MOI.addconstraint!(m, MOI.ScalarAffineFunction(v, [2.0, 8.0, 4.0, 2.0, 5.0], 0.0), MOI.LessThan(10))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.ZeroOne}()) == 1

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

    # TODO more test sets here
end
