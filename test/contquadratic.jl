using MathOptInterface
MOI = MathOptInterface

using MathOptInterfaceUtilities # Defines isapprox for ScalarQuadraticFunction

# Continuous quadratic problems

function qp1test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "QP1 - Quadratic objective" begin
        # simple quadratic objective
        # Min x^2 + xy + y^2 + yz + z^2
        # st  x + 2y + 3z >= 4 (c1)
        #     x +  y      >= 1 (c2)
        #     x,y \in R

        @test MOI.supportsproblem(solver, MOI.ScalarQuadraticFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64})])

        m = MOI.SolverInstance(solver)

        v = MOI.addvariables!(m, 3)
        @test MOI.getattribute(m, MOI.NumberOfVariables()) == 3

        cf1 = MOI.ScalarAffineFunction(v, [1.0,2.0,3.0], 0.0)
        c1 = MOI.addconstraint!(m, cf1, MOI.GreaterThan(4.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}()) == 1

        c2 = MOI.addconstraint!(m, MOI.ScalarAffineFunction([v[1],v[2]], [1.0,1.0], 0.0), MOI.GreaterThan(1.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}()) == 2

        obj = MOI.ScalarQuadraticFunction(MOI.VariableReference[], Float64[], v[[1,1,2,2,3]], v[[1,2,2,3,3]], [2.0, 1.0, 2.0, 1.0, 2.0], 0.0)
        MOI.setobjective!(m, MOI.MinSense, obj)
        @test MOI.getattribute(m, MOI.ObjectiveSense()) == MOI.MinSense

        if MOI.cangetattribute(m, MOI.ObjectiveFunction())
            @test obj ≈ MOI.getattribute(m, MOI.ObjectiveFunction())
        end

        if MOI.cangetattribute(m, MOI.ConstraintFunction(), c1)
            @test cf1 ≈ MOI.getattribute(m, MOI.ConstraintFunction(), c1)
        end

        if MOI.cangetattribute(m, MOI.ConstraintSet(), c1)
            @test MOI.GreaterThan(4.0) == MOI.getattribute(m, MOI.ConstraintSet(), c1)
        end

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 130/70 atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
        @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [0.5714285714285715,0.4285714285714285,0.8571428571428572] atol=atol rtol=rtol
    end
end

function qp2test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "QP2" begin
        # same as QP0 but with duplicate terms
        # then change the objective and sense
        # simple quadratic objective
        # Min x^2 + xy + y^2 + yz + z^2
        # st  x + 2y + 3z >= 4 (c1)
        #     x +  y      >= 1 (c2)
        #     x,y \in R

        @test MOI.supportsproblem(solver, MOI.ScalarQuadraticFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64})])

        m = MOI.SolverInstance(solver)

        v = MOI.addvariables!(m, 3)
        @test MOI.getattribute(m, MOI.NumberOfVariables()) == 3

        c1f = MOI.ScalarAffineFunction(v, [1.0,2.0,3.0], 0.0)
        c1 = MOI.addconstraint!(m, c1f, MOI.GreaterThan(4.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}()) == 1

        c2 = MOI.addconstraint!(m, MOI.ScalarAffineFunction([v[1],v[2]], [1.0,1.0], 0.0), MOI.GreaterThan(1.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}()) == 2

        obj = MOI.ScalarQuadraticFunction(v, [0.0,0.0,0.0],[v[1], v[1], v[1], v[2], v[2], v[3], v[3]], [v[1], v[2], v[2], v[2], v[3], v[3], v[3]], [2.0, 0.5, 0.5, 2.0, 1.0, 1.0, 1.0], 0.0)
        MOI.setobjective!(m, MOI.MinSense, obj)
        @test MOI.getattribute(m, MOI.ObjectiveSense()) == MOI.MinSense

        if MOI.cangetattribute(m, MOI.ObjectiveFunction())
            @test obj ≈ MOI.getattribute(m, MOI.ObjectiveFunction())
        end

        if MOI.cangetattribute(m, MOI.ConstraintFunction(), c1)
            @test c1f ≈ MOI.getattribute(m, MOI.ConstraintFunction(), c1)
        end

        if MOI.cangetattribute(m, MOI.ConstraintSet(), c1)
            @test MOI.GreaterThan(4.0) == MOI.getattribute(m, MOI.ConstraintSet(), c1)
        end

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 130/70 atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
        @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [0.5714285714285715,0.4285714285714285,0.8571428571428572] atol=atol rtol=rtol

        # change objective to Max -2(x^2 + xy + y^2 + yz + z^2)
        obj2 = MOI.ScalarQuadraticFunction(v, [0.0,0.0,0.0],[v[1], v[1], v[1], v[2], v[2], v[3], v[3]], [v[1], v[2], v[2], v[2], v[3], v[3], v[3]], [-4.0, -1.0, -1.0, -4.0, -2.0, -2.0, -2.0], 0.0)
        MOI.setobjective!(m, MOI.MaxSense, obj2)
        @test MOI.getattribute(m, MOI.ObjectiveSense()) == MOI.MaxSense

        if MOI.cangetattribute(m, MOI.ObjectiveFunction())
            @test obj2 ≈ MOI.getattribute(m, MOI.ObjectiveFunction())
        end

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ -2*130/70 atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
        @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [0.5714285714285715,0.4285714285714285,0.8571428571428572] atol=atol rtol=rtol
    end
end

function qp3test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "qp3test - Linear Quadratic objective" begin
        # simple quadratic objective
        #    minimize 2 x^2 + y^2 + xy + x + y + 1
        #       s.t.  x, y >= 0
        #             x + y = 1

        @test MOI.supportsproblem(solver, MOI.ScalarQuadraticFunction{Float64},
            [
                (MOI.SingleVariable,MOI.GreaterThan{Float64}),
                (MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64})
            ]
        )

        m = MOI.SolverInstance(solver)
        x = MOI.addvariable!(m)
        y = MOI.addvariable!(m)

        MOI.addconstraint!(m,
            MOI.ScalarAffineFunction([x,y], [1.0,1.0], 0.0),
            MOI.EqualTo(1.0)
        )

        MOI.addconstraint!(m, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
        MOI.addconstraint!(m, MOI.SingleVariable(y), MOI.GreaterThan(0.0))

        MOI.setobjective!(m,
            MOI.MinSense,
            MOI.ScalarQuadraticFunction(
                [x,y], [1.0,1.0],
                [x,y,x], [x,y,y], [4.0, 2.0, 1.0],
                1.0
            )
        )

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 2.875 atol=atol rtol=rtol
        @test MOI.cangetattribute(m, MOI.VariablePrimal(), [x,y])
        @test MOI.getattribute(m, MOI.VariablePrimal(), [x,y]) ≈ [0.25, 0.75] atol=atol rtol=rtol

        # change back to linear
        #        max 2x + y + 1
        #       s.t.  x, y >= 0
        #             x + y = 1
        # (x,y) = (1,0), obj = 3
        objf = MOI.ScalarAffineFunction([x,y], [2.0,1.0], 1.0)
        MOI.setobjective!(m, MOI.MaxSense, objf)

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 3.0 atol=atol rtol=rtol
        @test MOI.cangetattribute(m, MOI.VariablePrimal(), [x,y])
        @test MOI.getattribute(m, MOI.VariablePrimal(), [x,y]) ≈ [1.0, 0.0] atol=atol rtol=rtol

    end
end


function qptests(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "Quadratic Programs (quad. objective)" begin
        qp1test(solver, atol=atol, rtol=rtol)
        qp2test(solver, atol=atol, rtol=rtol)
        qp3test(solver, atol=atol, rtol=rtol)
    end
end

#=
    Quadratically constrained programs
=#

function qcp1test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "qcp1" begin
        # quadratic constraint
        # Max x + y
        # st  - x + y >= 0 (c1[1])
        #       x + y >= 0 (c1[2])
        #     0.5x^2 + y <= 2 (c2)

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.VectorAffineFunction{Float64},MOI.Nonnegatives),(MOI.ScalarQuadraticFunction{Float64},MOI.LessThan{Float64})])

        m = MOI.SolverInstance(solver)

        x = MOI.addvariable!(m)
        y = MOI.addvariable!(m)
        @test MOI.getattribute(m, MOI.NumberOfVariables()) == 2

        c1 = MOI.addconstraint!(m, MOI.VectorAffineFunction([1,1,2,2], [x,y,x,y],[-1.0,1.0,1.0,1.0], [0.0,0.0]), MOI.Nonnegatives(2))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives}()) == 1

        c2f = MOI.ScalarQuadraticFunction([y],[1.0],[x],[x],[1.0], 0.0)
        c2 = MOI.addconstraint!(m, c2f, MOI.LessThan(2.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}}()) == 1

        MOI.setobjective!(m, MOI.MaxSense, MOI.ScalarAffineFunction([x,y], [1.0,1.0], 0.0))
        @test MOI.getattribute(m, MOI.ObjectiveSense()) == MOI.MaxSense

        if MOI.cangetattribute(m, MOI.ConstraintFunction(), c2)
            @test c2f ≈ MOI.getattribute(m, MOI.ConstraintFunction(), c2)
        end

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 2.25 atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), [x,y])
        @test MOI.getattribute(m, MOI.VariablePrimal(), [x,y]) ≈ [0.5,1.75] atol=atol rtol=rtol

        # try delete quadratic constraint and go back to linear

        # MOI.delete!(m, c2)
        #
        # MOI.optimize!(m)
        #
        # @test MOI.cangetattribute(m, MOI.TerminationStatus())
        # @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        #
        # @test MOI.cangetattribute(m, MOI.PrimalStatus())
        # @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
        #
        # @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        # @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 0.0 atol=atol rtol=rtol
    end
end


function qcp2test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "qcp2" begin
        # Max x
        # s.t. x^2 <= 2 (c)

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarQuadraticFunction{Float64},MOI.LessThan{Float64})])

        m = MOI.SolverInstance(solver)

        x = MOI.addvariable!(m)
        @test MOI.getattribute(m, MOI.NumberOfVariables()) == 1

        cf = MOI.ScalarQuadraticFunction(MOI.VariableReference[x],Float64[0.0],[x],[x],[1.0], 0.0)
        c = MOI.addconstraint!(m, cf, MOI.LessThan(2.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}}()) == 1

        MOI.setobjective!(m, MOI.MaxSense, MOI.ScalarAffineFunction([x], [1.0], 0.0))
        @test MOI.getattribute(m, MOI.ObjectiveSense()) == MOI.MaxSense

        if MOI.cangetattribute(m, MOI.ConstraintFunction(), c)
            @test cf ≈ MOI.getattribute(m, MOI.ConstraintFunction(), c)
        end

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.DualStatus())
        @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ sqrt(2) atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), x)
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ sqrt(2) atol=atol rtol=rtol

        # TODO - duals
        # @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
        # @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ 0.5/sqrt(2) atol=atol rtol=rtol
    end
end

function qcp3test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "qcp3" begin
        # Min -x
        # s.t. x^2 <= 2

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarQuadraticFunction{Float64},MOI.LessThan{Float64})])

        m = MOI.SolverInstance(solver)

        x = MOI.addvariable!(m)
        @test MOI.getattribute(m, MOI.NumberOfVariables()) == 1

        cf = MOI.ScalarQuadraticFunction(MOI.VariableReference[],Float64[],[x],[x],[1.0], 0.0)
        c = MOI.addconstraint!(m, cf, MOI.LessThan(2.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}}()) == 1

        MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([x], [-1.0], 0.0))
        @test MOI.getattribute(m, MOI.ObjectiveSense()) == MOI.MinSense

        if MOI.cangetattribute(m, MOI.ConstraintFunction(), c)
            @test cf ≈ MOI.getattribute(m, MOI.ConstraintFunction(), c)
        end

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.DualStatus())
        @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ -sqrt(2) atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), x)
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ sqrt(2) atol=atol rtol=rtol

        # TODO - duals
        # @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
        # @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ -0.5/sqrt(2) atol=atol rtol=rtol
    end
end

function qcptests(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "Quadratic Constrainted Programs (quad. constraints only)" begin
        qcp1test(solver, atol=atol, rtol=rtol)
        qcp2test(solver, atol=atol, rtol=rtol)
        qcp3test(solver, atol=atol, rtol=rtol)
    end
end

#=
    SOCP
=#

function socp1test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "socp1" begin
        # min t
        # s.t. x + y >= 1 (c1)
        #      x^2 + y^2 <= t^2 (c2)
        #      t >= 0 (bound)

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarQuadraticFunction{Float64},MOI.LessThan{Float64}), (MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}), (MOI.SingleVariable,MOI.GreaterThan{Float64})])

        m = MOI.SolverInstance(solver)

        x = MOI.addvariable!(m)
        y = MOI.addvariable!(m)
        t = MOI.addvariable!(m)
        @test MOI.getattribute(m, MOI.NumberOfVariables()) == 3

        c1f = MOI.ScalarAffineFunction([x,y],[1.0, 1.0], 0.0)
        c1 = MOI.addconstraint!(m, c1f, MOI.GreaterThan(1.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}()) == 1

        c2f = MOI.ScalarQuadraticFunction(MOI.VariableReference[],Float64[],[x,y,t],[x,y,t],[1.0,1.0,-1.0], 0.0)
        c2 = MOI.addconstraint!(m, c2f, MOI.LessThan(0.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}}()) == 1

        bound = MOI.addconstraint!(m, MOI.SingleVariable(t), MOI.GreaterThan(0.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable, MOI.GreaterThan{Float64}}()) == 1

        MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([t], [1.0], 0.0))
        @test MOI.getattribute(m, MOI.ObjectiveSense()) == MOI.MinSense

        if MOI.cangetattribute(m, MOI.ConstraintFunction(), c1)
            @test c1f ≈ MOI.getattribute(m, MOI.ConstraintFunction(), c1)
        end

        if MOI.cangetattribute(m, MOI.ConstraintFunction(), c2)
            @test c2f ≈ MOI.getattribute(m, MOI.ConstraintFunction(), c2)
        end

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ sqrt(1/2) atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), [x,y,t])
        @test MOI.getattribute(m, MOI.VariablePrimal(), [x,y,t]) ≈ [0.5,0.5,sqrt(1/2)] atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), [t,x,y,t])
        @test MOI.getattribute(m, MOI.VariablePrimal(), [t,x,y,t]) ≈ [sqrt(1/2),0.5,0.5,sqrt(1/2)] atol=atol rtol=rtol
    end
end

function socptests(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "Second Order Cone Programs" begin
        socp1test(solver, atol=atol, rtol=rtol)
    end
end

function contquadratictests(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    qptests(solver, atol=atol, rtol=rtol)
    qcptests(solver, atol=atol, rtol=rtol)
    socptests(solver, atol=atol, rtol=rtol)
end
