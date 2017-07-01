# Mixed-integer linear problems

function intlineartest(solver::AbstractSolver, eps=Base.rtoldefault(Float64))
    @testset "Set up model, variables, constraint, objective" begin
        # integer knapsack problem
        # max 5a + 3b + 2c + 7d + 4e
        # st  2a + 8b + 4c + 2d + 5e <= 10
        #                  a,b,c,d,e ∈ binary

        m = Model(solver)

        v = addvariables!(m, 5)
        @test getattribute(m, VariableCount()) == 5

        @test getattribute(m, SupportsVariablewiseConstraint{ZeroOne}())
        for vi in v
            addconstraint!(m, vi, ZeroOne())
        end
        c = addconstraint!(m, 10, v, -[2, 8, 4, 2, 5], NonNegative(1))
        @test getattribute(m, ConstraintCount()) == 6

        setattribute!(m, Sense(), MaxSense)
        setobjective!(m, 0.0, v, [5, 3, 2, 7, 4])
    end

    @testset "Solve model, check results" begin
        optimize!(m)

        @test cangetattribute(m, TerminationStatus())
        @test getattribute(m, TerminationStatus()) == Success

        @test cangetattribute(m, PrimalStatus())
        @test getattribute(m, PrimalStatus()) == FeasiblePoint

        @test cangetattribute(m, ObjectiveValue())
        @test isapprox(getattribute(m, ObjectiveValue()), -16, atol=eps)

        @test cangetattribute(m, VariablePrimal(), v)
        @test isapprox(getattribute(m, VariablePrimal(), v), [1, 0, 0, 1, 1], atol=eps)
    end

    # TODO more test sets here
end
