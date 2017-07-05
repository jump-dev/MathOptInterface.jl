# Continuous conic problems

function contconictest(solver::MOI.AbstractSolver, ε=Base.rtoldefault(Float64))
    @testset "Linear model" begin
        # linear conic problem
        # min -3x - 2y - 4z
        # st    x +  y +  z == 3
        #            y +  z == 2
        #       x>=0 y>=0 z>=0
        # Opt obj = -11, soln x = 1, y = 0, z = 2

        @test MOI.supportsproblem(solver, ScalarAffineFunction, [(MOI.VectorVariablewiseFunction,MOI.NonNegative),(MOI.VectorAffineFunction{Float64},MOI.NonNegative)])

        m = MOI.SolverInstance(solver)

        v = MOI.addvariables!(m, 3)
        @test MOI.getattribute(m, MOI.VariableCount()) == 3

        vc = MOI.addconstraint!(m, MOI.VectorVariablewiseFunction(v), MOI.NonNegative(3))
        c = MOI.addconstraint!(m, MOI.VectorAffineFunction([1,1,1,2,2], [v;v[2];v[3]], ones(5), [-3.0,-2.0]), MOI.Zero(2))
        @test MOI.getattribute(m, MOI.ConstraintCount()) == 2

        setattribute!(m, MOI.Sense(), MOI.MinSense)
        setobjective!(m, ScalarAffineFunction(v, [-3.0, -2.0, -4.0], 0.0))

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.cangetattribute(m, MOI.DualStatus())
        @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ -11 atol=ε

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
        @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [1, 0, 2] atol=ε

        @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
        @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ [3, 1] atol=ε

        # TODO var dual and con primal
    end

    # TODO more models
end
