# Continuous linear problems

function contlineartest(solver::MOI.AbstractSolver, ε=Base.rtoldefault(Float64))
    @testset "Model 1" begin
        # simple 2 variable, 1 constraint problem
        # min -x
        # st   x + y <= 1   (x + y - 1 ∈ NonPositive)
        #       x, y >= 0   (x, y ∈ MOI.NonNegative)

        m = MOI.SolverInstance(solver)

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction, [(MOI.ScalarAffineFunction{Float64},MOI.NonPositive),(MOI.ScalarVariablewiseFunction,MOI.GreaterThan)])

        v = MOI.addvariables!(m, 2)
        @test MOI.getattribute(m, MOI.VariableCount()) == 2

        c = MOI.addconstraint!(m, MOI.ScalarAffineFunction(v, [1.0,1.0], -1.0), MOI.NonPositive(1))
        @test MOI.getattribute(m, MOI.ConstraintCount()) == 1

        vc1 = MOI.addconstraint!(m, MOI.ScalarVariablewiseFunction(v[1]), MOI.GreaterThan(0))
        vc2 = MOI.addconstraint!(m, MOI.ScalarVariablewiseFunction(v[2]), MOI.GreaterThan(0))
        @test MOI.getattribute(m, MOI.ConstraintCount()) == 3

        MOI.setattribute!(m, MOI.Sense(), MOI.MinSense)
        MOI.setobjective!(m, MOI.ScalarAffineFunction(v, [-1.0,0.0], 0.0))
        # TODO query objective
        # (b, a_varref, a_coef, qi, qj, qc) = getobjective(m)
        # @test b ≈ 0 atol=ε
        # @test a_varref == v
        # @test a_coef ≈ [-1, 0] atol=ε

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.cangetattribute(m, MOI.DualStatus())
        @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ -1 atol=ε

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
        @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [1, 0] atol=ε
        # TODO var dual?

        @test MOI.cangetattribute(m, MOI.ConstraintPrimal(), c)
        @test MOI.getattribute(m, MOI.ConstraintPrimal(), c) ≈ [1] atol=ε
        @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
        @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ [-1] atol=ε

        # change objective to Max +x

        @test MOI.cansetattribute(m, MOI.Sense())
        MOI.setattribute!(m, MOI.Sense(), MOI.MaxSense)
        MOI.setobjective!(m, MOI.ScalarAffineFunction(v, [1.0,0.0], 0.0))

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.cangetattribute(m, MOI.DualStatus())
        @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 1 atol=ε

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
        @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [1, 0] atol=ε

        @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
        @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ [1] atol=ε
    end

    # TODO more test sets here
end
