# Continuous conic problems

function contconictest(solver::AbstractSolver, eps=Base.rtoldefault(Float64))
    @testset "Set up model, variables, constraint, objective" begin
        # linear conic problem
        # min -3x - 2y - 4z
        # st    x +  y +  z == 3
        #            y +  z == 2
        #       x>=0 y>=0 z>=0
        # Opt obj = -11, soln x = 1, y = 0, z = 2

        m = Model(solver)

        v = addvariables!(m, 3)
        @test getattribute(m, VariableCount()) == 3

        @test getattribute(m, SupportsAffineConstraint{NonNegative}())
        vc = addconstraint!(m, v, NonNegative(3))
        c = addconstraint!(m, [3, 2], -[1 1 1; 0 1 1], Zero(2))
        @test getattribute(m, ConstraintCount()) == 4

        setattribute!(m, Sense(), MinSense)
        setobjective!(m, 1, 0, v, [-3 -2, -4])
    end

    @testset "Solve model, check results" begin
        optimize!(m)

        @test cangetattribute(m, TerminationStatus())
        @test getattribute(m, TerminationStatus()) == Success

        @test cangetattribute(m, PrimalStatus())
        @test getattribute(m, PrimalStatus()) == FeasiblePoint
        @test cangetattribute(m, DualStatus())
        @test getattribute(m, DualStatus()) == FeasiblePoint

        @test cangetattribute(m, ObjectiveValue())
        @test isapprox(getattribute(m, ObjectiveValue()), -11, atol=eps)

        @test cangetattribute(m, VariablePrimal(), v)
        @test isapprox(getattribute(m, VariablePrimal(), v), [1, 0, 2], atol=eps)

        @test cangetattribute(m, ConstraintDual(), c)
        @test isapprox(getattribute(m, ConstraintDual(), c), [3, 1], atol=eps)

        # TODO var dual and con primal
    end

    # TODO more test sets here
end
