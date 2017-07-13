# Continuous linear problems

function contlineartest(solver::MOI.AbstractSolver, ε=Base.rtoldefault(Float64))
    @testset "Basic solve, query, resolve" begin
        # simple 2 variable, 1 constraint problem
        # min -x
        # st   x + y <= 1   (x + y - 1 ∈ NonPositive)
        #       x, y >= 0   (x, y ∈ NonNegative)

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction, [(MOI.ScalarAffineFunction{Float64},MOI.LessThan),(MOI.ScalarVariablewiseFunction,MOI.GreaterThan)])

        m = MOI.SolverInstance(solver)

        v = MOI.addvariables!(m, 2)
        @test MOI.getattribute(m, MOI.VariableCount()) == 2

        cf = MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0)
        c = MOI.addconstraint!(m, cf, MOI.LessThan(1.0))
        @test MOI.getattribute(m, MOI.ConstraintCount()) == 1

        vc1 = MOI.addconstraint!(m, MOI.ScalarVariablewiseFunction(v[1]), MOI.GreaterThan(0.0))
        vc2 = MOI.addconstraint!(m, MOI.ScalarVariablewiseFunction(v[2]), MOI.GreaterThan(0.0))
        @test MOI.getattribute(m, MOI.ConstraintCount()) == 3

        objf = MOI.ScalarAffineFunction(v, [-1.0,0.0], 0.0)
        MOI.setobjective!(m, MOI.MinSense, obj)

        @test MOI.getattribute(m, MOI.Sense()) == MOI.MinSense

        if MOI.cangetattribute(m, MOI.ObjectiveFunction())
            @test objf ≈ MPI.getattribute(m, MOI.ObjectiveFunction())
        end

        if MOI.cangetattribute(m, MOI.ConstraintFunction(), c)
            @test cf ≈ MOI.getattribute(m, MOI.ConstraintFunction(), c)
        end

        if MOI.cangetattribute(m, MOI.ConstraintSet(), c)
            s = MOI.getattribute(m, MOI.ConstraintSet(), c)
            @test s == MOI.LessThan(1.0)
        end

        if MOI.cangetattribute(m, MOI.ConstraintSet(), vc1)
            s = MOI.getattribute(m, MOI.ConstraintSet(), vc1)
            @test s == MOI.GreaterThan(0.0)
        end
        if MOI.cangetattribute(m, MOI.ConstraintSet(), vc2)
            s = MOI.getattribute(m, MOI.ConstraintSet(), vc2)
            @test s == MOI.GreaterThan(0.0)
        end

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ -1 atol=ε

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
        @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [1, 0] atol=ε

        @test MOI.cangetattribute(m, MOI.ConstraintPrimal(), c)
        @test MOI.getattribute(m, MOI.ConstraintPrimal(), c) ≈ 1 atol=ε

        if MOI.getattribute(solver, MOI.SupportsDuals())
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
            @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ -1 atol=ε

            # reduced costs
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc1)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc1) ≈ 0 atol=ε
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc2)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc2) ≈ 1 atol=ε
        end

        # change objective to Max +x

        objf = MOI.ScalarAffineFunction(v, [1.0,0.0], 0.0)
        MOI.setobjective!(m, MOI.MaxSense, objf)

        if MOI.cangetattribute(m, MOI.ObjectiveFunction())
            @test objf ≈ MPI.getattribute(m, MOI.ObjectiveFunction())
        end

        @test MOI.getattribute(m, MOI.Sense()) == MOI.MaxSense

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 1 atol=ε

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
        @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [1, 0] atol=ε

        if MOI.getattribute(solver, MOI.SupportsDuals())
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
            @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ -1 atol=ε

            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc1)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc1) ≈ 0 atol=ε
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc2)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc2) ≈ 1 atol=ε
        end

    end

    @testset "Modify GreaterThan and LessThan sets as bounds" begin

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction, [(MOI.ScalarVariablewiseFunction{Float64},MOI.NonPositive),(MOI.ScalarVariablewiseFunction{Float64},MOI.LessThan)])

        m = MOI.SolverInstance(solver)

        x = MOI.addvariable!(m)
        y = MOI.addvariable!(m)

        # Min  x - y
        # s.t. 0.0 <= x          (c1)
        #             y <= 0.0   (c2)

        MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([x,y], [1.0, -1.0], 0.0))

        c1 = MOI.addconstraint!(m, MOI.ScalarVariablewiseFunction(x), GreaterThan(0.0))
        c2 = MOI.addconstraint!(m, MOI.ScalarVariablewiseFunction(y), LessThan(0.0))

        MOI.optimize!(m)

        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 0.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 0.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0.0 atol=ε

        # Min  x - y
        # s.t. 100.0 <= x
        #               y <= 0.0
        MOI.modifyconstraint!(m, c1, GreaterThan(100.0))
        MOI.optimize!(m)
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 100.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 100.0 atol=eps
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0.0 atol=eps

        # Min  x - y
        # s.t. 100.0 <= x
        #               y <= -100.0
        MOI.modifyconstraint!(m, c2, LessThan(-100.0))
        MOI.optimize!(m)
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 200.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 100.0 atol=eps
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ -100.0 atol=eps

    end


    @testset "Modify GreaterThan and LessThan sets as linear constraints" begin

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction, [(MOI.ScalarAffineFunction{Float64},MOI.NonPositive),(MOI.ScalarAffineFunction{Float64},MOI.LessThan)])

        m = MOI.SolverInstance(solver)

        x = MOI.addvariable!(m)
        y = MOI.addvariable!(m)

        # Min  x - y
        # s.t. 0.0 <= x          (c1)
        #             y <= 0.0   (c2)

        MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([x,y], [1.0, -1.0], 0.0))

        c1 = MOI.addconstraint!(m, MOI.ScalarAffineFunction([x],[1.0],0.0), GreaterThan(0.0))
        c2 = MOI.addconstraint!(m, MOI.ScalarAffineFunction([y],[1.0],0.0), LessThan(0.0))

        MOI.optimize!(m)

        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 0.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 0.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0.0 atol=ε

        # Min  x - y
        # s.t. 100.0 <= x
        #               y <= 0.0
        MOI.modifyconstraint!(m, c1, GreaterThan(100.0))
        MOI.optimize!(m)
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 100.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 100.0 atol=eps
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0.0 atol=eps

        # Min  x - y
        # s.t. 100.0 <= x
        #               y <= -100.0
        MOI.modifyconstraint!(m, c2, LessThan(-100.0))
        MOI.optimize!(m)
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 200.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 100.0 atol=eps
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ -100.0 atol=eps

    end

    @testset "Modify constants in Nonnegative and Nonpositive" begin

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction, [(MOI.VectorAffineFunction{Float64},MOI.Nonpositive),(MOI.VectorAffineFunction{Float64},MOI.Nonpositive)])

        m = MOI.SolverInstance(solver)

        x = MOI.addvariable!(m)
        y = MOI.addvariable!(m)

        # Min  x - y
        # s.t. 0.0 <= x          (c1)
        #             y <= 0.0   (c2)

        MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([x,y], [1.0, -1.0], 0.0))

        c1 = MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[x],[1.0],0.0), Nonnegative(1))
        c2 = MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[y],[1.0],0.0), Nonpositive(1))

        MOI.optimize!(m)

        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 0.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 0.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0.0 atol=ε

        # Min  x - y
        # s.t. 100.0 <= x
        #               y <= 0.0
        MOI.modifyconstraint!(m, c1, ScalarConstantChange(-100.0))
        MOI.optimize!(m)
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 100.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 100.0 atol=eps
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0.0 atol=eps

        # Min  x - y
        # s.t. 100.0 <= x
        #               y <= -100.0
        MOI.modifyconstraint!(m, c2, ScalarConstantChange(100.0))
        MOI.optimize!(m)
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 200.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 100.0 atol=eps
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ -100.0 atol=eps

    end

    # TODO more test sets here
end
