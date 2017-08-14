using MathOptInterface
MOI = MathOptInterface

using MathOptInterfaceUtilities # Defines isapprox for ScalarAffineFunction

# Continuous linear problems

function linear1test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "Basic solve, query, resolve" begin
        # simple 2 variable, 1 constraint problem
        # min -x
        # st   x + y <= 1   (x + y - 1 ∈ Nonpositives)
        #       x, y >= 0   (x, y ∈ Nonnegatives)

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64})])

        @test MOI.getattribute(solver, MOI.SupportsAddConstraintAfterSolve())
        @test MOI.getattribute(solver, MOI.SupportsAddVariableAfterSolve())
        @test MOI.getattribute(solver, MOI.SupportsDeleteConstraint())

        m = MOI.SolverInstance(solver)

        v = MOI.addvariables!(m, 2)
        @test MOI.getattribute(m, MOI.NumberOfVariables()) == 2

        cf = MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0)
        c = MOI.addconstraint!(m, cf, MOI.LessThan(1.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

        vc1 = MOI.addconstraint!(m, MOI.SingleVariable(v[1]), MOI.GreaterThan(0.0))
        # test fallback
        vc2 = MOI.addconstraint!(m, v[2], MOI.GreaterThan(0.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

        objf = MOI.ScalarAffineFunction(v, [-1.0,0.0], 0.0)
        MOI.setobjective!(m, MOI.MinSense, objf)

        @test MOI.getattribute(m, MOI.ObjectiveSense()) == MOI.MinSense

        if MOI.cangetattribute(m, MOI.ObjectiveFunction())
            @test objf ≈ MOI.getattribute(m, MOI.ObjectiveFunction())
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
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ -1 atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
        @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [1, 0] atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.ConstraintPrimal(), c)
        @test MOI.getattribute(m, MOI.ConstraintPrimal(), c) ≈ 1 atol=atol rtol=rtol

        if MOI.getattribute(solver, MOI.SupportsDuals())
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
            @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol

            # reduced costs
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc1)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc2)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc2) ≈ 1 atol=atol rtol=rtol
        end

        # change objective to Max +x

        objf = MOI.ScalarAffineFunction(v, [1.0,0.0], 0.0)
        MOI.setobjective!(m, MOI.MaxSense, objf)

        if MOI.cangetattribute(m, MOI.ObjectiveFunction())
            @test objf ≈ MOI.getattribute(m, MOI.ObjectiveFunction())
        end

        @test MOI.getattribute(m, MOI.ObjectiveSense()) == MOI.MaxSense

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 1 atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
        @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [1, 0] atol=atol rtol=rtol

        if MOI.getattribute(solver, MOI.SupportsDuals())
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
            @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol

            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc1)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc2)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc2) ≈ 1 atol=atol rtol=rtol
        end

        # add new variable to get :
        # max x + 2z
        # s.t. x + y + z <= 1
        # x,y,z >= 0

        z = MOI.addvariable!(m)
        push!(v, z)
        @test v[3] == z

        vc3 = MOI.addconstraint!(m, MOI.SingleVariable(v[3]), MOI.GreaterThan(0.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 3

        MOI.modifyconstraint!(m, c, MOI.ScalarCoefficientChange{Float64}(z, 1.0))
        MOI.modifyobjective!(m, MOI.ScalarCoefficientChange{Float64}(z, 2.0))

        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 3

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.ResultCount())
        @test MOI.getattribute(m, MOI.ResultCount()) >= 1

        @test MOI.cangetattribute(m, MOI.PrimalStatus(1))
        @test MOI.getattribute(m, MOI.PrimalStatus(1)) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
        @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [0, 0, 1] atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.ConstraintPrimal(), c)
        @test MOI.getattribute(m, MOI.ConstraintPrimal(), c) ≈ 1 atol=atol rtol=rtol

        if MOI.getattribute(solver, MOI.SupportsDuals())
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
            @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ -2 atol=atol rtol=rtol

            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc1)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc1) ≈ 1 atol=atol rtol=rtol
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc2)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc2) ≈ 2 atol=atol rtol=rtol
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc3)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc3) ≈ 0 atol=atol rtol=rtol
        end

        # setting lb of x to -1 to get :
        # max x + 2z
        # s.t. x + y + z <= 1
        # x >= -1
        # y,z >= 0

        MOI.modifyconstraint!(m, vc1, MOI.GreaterThan(-1.0))

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.ResultCount())
        @test MOI.getattribute(m, MOI.ResultCount()) >= 1

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 3 atol=atol rtol=rtol

        # put lb of x back to 0 and fix z to zero to get :
        # max x + 2z
        # s.t. x + y + z <= 1
        # x, y >= 0, z = 0

        MOI.modifyconstraint!(m, vc1, MOI.GreaterThan(0.0))
        MOI.delete!(m, vc3)
        vc3 = MOI.addconstraint!(m, MOI.SingleVariable(v[3]), MOI.EqualTo(0.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.ResultCount())
        @test MOI.getattribute(m, MOI.ResultCount()) >= 1

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 1 atol=atol rtol=rtol

        # modify affine linear constraint set to be == 2 to get :
        # max x + 2z
        # s.t. x + y + z == 2
        # x,y >= 0, z = 0

        MOI.delete!(m, c)
        cf = MOI.ScalarAffineFunction(v, [1.0,1.0,1.0], 0.0)
        c = MOI.addconstraint!(m, cf, MOI.EqualTo(2.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 0
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64}}()) == 1

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.ResultCount())
        @test MOI.getattribute(m, MOI.ResultCount()) >= 1

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

        # modify objective function to x + 2y to get :
        # max x + 2y
        # s.t. x + y + z == 2
        # x,y >= 0, z = 0

        objf = MOI.ScalarAffineFunction(v, [1.0,2.0,0.0], 0.0)
        MOI.setobjective!(m, MOI.MaxSense, objf)

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.ResultCount())
        @test MOI.getattribute(m, MOI.ResultCount()) >= 1

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 4 atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
        @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [0, 2, 0] atol=atol rtol=rtol

        # add constraint x - y >= 0 to get :
        # max x+2y
        # s.t. x + y + z == 2
        # x - y >= 0
        # x,y >= 0, z = 0

        cf2 = MOI.ScalarAffineFunction(v, [1.0, -1.0, 0.0], 0.0)
        c2 = MOI.addconstraint!(m, cf2, MOI.GreaterThan(0.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64}}()) == 1
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}}()) == 1
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 0

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.ResultCount())
        @test MOI.getattribute(m, MOI.ResultCount()) >= 1

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 3 atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
        @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [1, 1, 0] atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.ConstraintPrimal(), c)
        @test MOI.getattribute(m, MOI.ConstraintPrimal(), c) ≈ 2 atol=atol rtol=rtol

        if MOI.getattribute(solver, MOI.SupportsDuals())
            @test MOI.cangetattribute(m, MOI.DualStatus(1))
            @test MOI.getattribute(m, MOI.DualStatus(1)) == MOI.FeasiblePoint

            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
            @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ -1.5 atol=atol rtol=rtol
            @test MOI.getattribute(m, MOI.ConstraintDual(), c2) ≈ 0.5 atol=atol rtol=rtol

            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc1)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc2)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc2) ≈ 0 atol=atol rtol=rtol
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc3)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc3) ≈ 1.5 atol=atol rtol=rtol
        end
    end
end

function linear2test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "addvariable! (one by one) interface" begin
        # Min -x
        # s.t. x + y <= 1
        # x, y >= 0

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64})])

        m = MOI.SolverInstance(solver)

        x = MOI.addvariable!(m)
        y = MOI.addvariable!(m)

        @test MOI.getattribute(m, MOI.NumberOfVariables()) == 2

        cf = MOI.ScalarAffineFunction([x, y], [1.0,1.0], 0.0)
        c = MOI.addconstraint!(m, cf, MOI.LessThan(1.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

        vc1 = MOI.addconstraint!(m, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
        vc2 = MOI.addconstraint!(m, MOI.SingleVariable(y), MOI.GreaterThan(0.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

        objf = MOI.ScalarAffineFunction([x, y], [-1.0,0.0], 0.0)
        MOI.setobjective!(m, MOI.MinSense, objf)

        @test MOI.getattribute(m, MOI.ObjectiveSense()) == MOI.MinSense

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ -1 atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), x)
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 1 atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), y)
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0 atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.ConstraintPrimal(), c)
        @test MOI.getattribute(m, MOI.ConstraintPrimal(), c) ≈ 1 atol=atol rtol=rtol

        if MOI.getattribute(solver, MOI.SupportsDuals())
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
            @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol

            # reduced costs
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc1)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc2)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc2) ≈ 1 atol=atol rtol=rtol
        end
    end
end

function linear3test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "Issue #40 from Gurobi.jl" begin
        # min  x
        # s.t. x >= 0
        #      x >= 3

        m = MOI.SolverInstance(solver)

        x = MOI.addvariable!(m)
        @test MOI.getattribute(m, MOI.NumberOfVariables()) == 1

        MOI.addconstraint!(m, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
        cf = MOI.ScalarAffineFunction([x], [1.0], 0.0)
        MOI.addconstraint!(m, cf, MOI.GreaterThan(3.0))

        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 1
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}}()) == 1

        objf = MOI.ScalarAffineFunction([x], [1.0], 0.0)
        MOI.setobjective!(m, MOI.MinSense, objf)

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.ResultCount())
        @test MOI.getattribute(m, MOI.ResultCount()) >= 1

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 3 atol=atol rtol=rtol

        # max  x
        # s.t. x <= 0
        #      x <= 3

        m = MOI.SolverInstance(solver)

        x = MOI.addvariable!(m)
        @test MOI.getattribute(m, MOI.NumberOfVariables()) == 1

        MOI.addconstraint!(m, MOI.SingleVariable(x), MOI.LessThan(0.0))
        cf = MOI.ScalarAffineFunction([x], [1.0], 0.0)
        MOI.addconstraint!(m, cf, MOI.LessThan(3.0))

        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.LessThan{Float64}}()) == 1
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

        objf = MOI.ScalarAffineFunction([x], [1.0], 0.0)
        MOI.setobjective!(m, MOI.MaxSense, objf)

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.ResultCount())
        @test MOI.getattribute(m, MOI.ResultCount()) >= 1

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 0 atol=atol rtol=rtol
    end
end

function linear4test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "Modify GreaterThan and LessThan sets as bounds" begin

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.SingleVariable,MOI.GreaterThan{Float64}),(MOI.SingleVariable,MOI.LessThan{Float64})])

        m = MOI.SolverInstance(solver)

        x = MOI.addvariable!(m)
        y = MOI.addvariable!(m)

        # Min  x - y
        # s.t. 0.0 <= x          (c1)
        #             y <= 0.0   (c2)

        MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([x,y], [1.0, -1.0], 0.0))

        c1 = MOI.addconstraint!(m, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
        c2 = MOI.addconstraint!(m, MOI.SingleVariable(y), MOI.LessThan(0.0))

        MOI.optimize!(m)

        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 0.0 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 0.0 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0.0 atol=atol rtol=rtol

        # Min  x - y
        # s.t. 100.0 <= x
        #               y <= 0.0
        MOI.modifyconstraint!(m, c1, MOI.GreaterThan(100.0))
        MOI.optimize!(m)
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 100.0 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 100.0 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0.0 atol=atol rtol=rtol

        # Min  x - y
        # s.t. 100.0 <= x
        #               y <= -100.0
        MOI.modifyconstraint!(m, c2, MOI.LessThan(-100.0))
        MOI.optimize!(m)
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 200.0 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 100.0 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ -100.0 atol=atol rtol=rtol

    end
end

function linear5test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "Change coeffs, del constr, del var" begin
        @test MOI.getattribute(solver, MOI.SupportsDeleteVariable())
        #####################################
        # Start from simple LP
        # Solve it
        # Copy and solve again
        # Chg coeff, solve, change back solve
        # del constr and solve
        # del var and solve

        #   maximize x + y
        #
        #   s.t. 2 x + 1 y <= 4
        #        1 x + 2 y <= 4
        #        x >= 0, y >= 0
        #
        #   solution: x = 1.3333333, y = 1.3333333, objv = 2.66666666

        m = MOI.SolverInstance(solver)

        x = MOI.addvariable!(m)
        y = MOI.addvariable!(m)

        @test MOI.getattribute(m, MOI.NumberOfVariables()) == 2

        cf1 = MOI.ScalarAffineFunction([x, y], [2.0,1.0], 0.0)
        cf2 = MOI.ScalarAffineFunction([x, y], [1.0,2.0], 0.0)

        c1 = MOI.addconstraint!(m, cf1, MOI.LessThan(4.0))
        c2 = MOI.addconstraint!(m, cf2, MOI.LessThan(4.0))

        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 2

        vc1 = MOI.addconstraint!(m, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
        vc2 = MOI.addconstraint!(m, MOI.SingleVariable(y), MOI.GreaterThan(0.0))

        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

        objf = MOI.ScalarAffineFunction([x, y], [1.0,1.0], 0.0)
        MOI.setobjective!(m, MOI.MaxSense, objf)

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 8/3 atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), [x, y])
        @test MOI.getattribute(m, MOI.VariablePrimal(), [x, y]) ≈ [4/3, 4/3] atol=atol rtol=rtol

        # copy and solve again
        # missing test

        # change coeff
        #   maximize x + y
        #
        #   s.t. 2 x + 3 y <= 4
        #        1 x + 2 y <= 4
        #        x >= 0, y >= 0
        #
        #   solution: x = 2, y = 0, objv = 2


        MOI.modifyconstraint!(m, c1, MOI.ScalarCoefficientChange(y, 3.0))
        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), [x, y])
        @test MOI.getattribute(m, MOI.VariablePrimal(), [x, y]) ≈ [2.0, 0.0] atol=atol rtol=rtol

        # delconstrs and solve
        #   maximize x + y
        #
        #   s.t. 1 x + 2 y <= 4
        #        x >= 0, y >= 0
        #
        #   solution: x = 4, y = 0, objv = 4

        MOI.delete!(m, c1)
        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 4 atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), [x, y])
        @test MOI.getattribute(m, MOI.VariablePrimal(), [x, y]) ≈ [4.0, 0.0] atol=atol rtol=rtol

        # delvars and solve
        #   maximize y
        #
        #   s.t.  2 y <= 4
        #           y >= 0
        #
        #   solution: y = 2, objv = 2

        MOI.delete!(m, x)
        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), y)
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 2.0 atol=atol rtol=rtol

    end
end

function linear6test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "Modify GreaterThan and LessThan sets as linear constraints" begin

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}),(MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64})])

        m = MOI.SolverInstance(solver)

        x = MOI.addvariable!(m)
        y = MOI.addvariable!(m)

        # Min  x - y
        # s.t. 0.0 <= x          (c1)
        #             y <= 0.0   (c2)

        MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([x,y], [1.0, -1.0], 0.0))

        c1 = MOI.addconstraint!(m, MOI.ScalarAffineFunction([x],[1.0],0.0), MOI.GreaterThan(0.0))
        c2 = MOI.addconstraint!(m, MOI.ScalarAffineFunction([y],[1.0],0.0), MOI.LessThan(0.0))

        MOI.optimize!(m)

        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 0.0 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 0.0 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0.0 atol=atol rtol=rtol

        # Min  x - y
        # s.t. 100.0 <= x
        #               y <= 0.0
        MOI.modifyconstraint!(m, c1, MOI.GreaterThan(100.0))
        MOI.optimize!(m)
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 100.0 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 100.0 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0.0 atol=atol rtol=rtol

        # Min  x - y
        # s.t. 100.0 <= x
        #               y <= -100.0
        MOI.modifyconstraint!(m, c2, MOI.LessThan(-100.0))
        MOI.optimize!(m)
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 200.0 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 100.0 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ -100.0 atol=atol rtol=rtol

    end
end

function linear7test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "Modify constants in Nonnegatives and Nonpositives" begin

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.VectorAffineFunction{Float64},MOI.Nonpositives),(MOI.VectorAffineFunction{Float64},MOI.Nonpositives)])

        m = MOI.SolverInstance(solver)

        x = MOI.addvariable!(m)
        y = MOI.addvariable!(m)

        # Min  x - y
        # s.t. 0.0 <= x          (c1)
        #             y <= 0.0   (c2)

        MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([x,y], [1.0, -1.0], 0.0))

        c1 = MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[x],[1.0],[0.0]), MOI.Nonnegatives(1))
        c2 = MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[y],[1.0],[0.0]), MOI.Nonpositives(1))

        MOI.optimize!(m)

        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 0.0 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 0.0 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0.0 atol=atol rtol=rtol

        # Min  x - y
        # s.t. 100.0 <= x
        #               y <= 0.0
        MOI.modifyconstraint!(m, c1, MOI.VectorConstantChange([-100.0]))
        MOI.optimize!(m)
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 100.0 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 100.0 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0.0 atol=atol rtol=rtol

        # Min  x - y
        # s.t. 100.0 <= x
        #               y <= -100.0
        MOI.modifyconstraint!(m, c2, MOI.VectorConstantChange([100.0]))
        MOI.optimize!(m)
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 200.0 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 100.0 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ -100.0 atol=atol rtol=rtol

    end
end

function linear8test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "test infeasible problem" begin
        # min x
        # s.t. 2x+y <= -1
        # x,y >= 0
        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64})])

        m = MOI.SolverInstance(solver)
        x = MOI.addvariable!(m)
        y = MOI.addvariable!(m)
        c = MOI.addconstraint!(m, MOI.ScalarAffineFunction([x,y], [2.0,1.0], 0.0), MOI.LessThan(-1.0))
        bndx = MOI.addconstraint!(m, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
        bndy = MOI.addconstraint!(m, MOI.SingleVariable(y), MOI.GreaterThan(0.0))
        MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([x], [1.0], 0.0))
        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.ResultCount())
        if MOI.getattribute(m, MOI.ResultCount()) >= 1
            # solver returned an infeasibility ray
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.InfeasibilityCertificate
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
            @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol
            # TODO: farkas dual on bounds
            # @test MOI.getattribute(m, MOI.ConstraintDual(), x) ≈ ? atol=atol rtol=rtol
            # @test MOI.getattribute(m, MOI.ConstraintDual(), y) ≈ ? atol=atol rtol=rtol
        else
            # solver returned nothing
            @test MOI.getattribute(m, MOI.ResultCount()) == 0
            @test MOI.cangetattribute(m, MOI.PrimalStatus(1)) == false
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.InfeasibleNoResult ||
                MOI.getattribute(m, MOI.TerminationStatus()) == MOI.InfeasibleOrUnbounded
        end
    end
    @testset "test unbounded problem" begin
        # min -x-y
        # s.t. -x+2y <= 0
        # x,y >= 0
        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64})])

        m = MOI.SolverInstance(solver)
        x = MOI.addvariable!(m)
        y = MOI.addvariable!(m)
        MOI.addconstraint!(m, MOI.ScalarAffineFunction([x,y], [-1.0,2.0], 0.0), MOI.LessThan(0.0))
        MOI.addconstraint!(m, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
        MOI.addconstraint!(m, MOI.SingleVariable(y), MOI.GreaterThan(0.0))
        MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([x, y], [-1.0, -1.0], 0.0))
        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.ResultCount())
        if MOI.getattribute(m, MOI.ResultCount()) >= 1
            # solver returned an unbounded ray
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
            @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.InfeasibilityCertificate
        else
            # solver returned nothing
            @test MOI.getattribute(m, MOI.ResultCount()) == 0
            @test MOI.cangetattribute(m, MOI.PrimalStatus(1)) == false
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.UnboundedNoResult ||
                MOI.getattribute(m, MOI.TerminationStatus()) == MOI.InfeasibleOrUnbounded
        end
    end
    @testset "unbounded problem with unique ray" begin
        # min -x-y
        # s.t. x-y == 0
        # x,y >= 0
        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64})])

        m = MOI.SolverInstance(solver)
        x = MOI.addvariable!(m)
        y = MOI.addvariable!(m)
        MOI.addconstraint!(m, MOI.ScalarAffineFunction([x,y], [1.0,-1.0], 0.0), MOI.EqualTo(0.0))
        MOI.addconstraint!(m, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
        MOI.addconstraint!(m, MOI.SingleVariable(y), MOI.GreaterThan(0.0))
        MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([x, y], [-1.0, -1.0], 0.0))
        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.ResultCount())
        if MOI.getattribute(m, MOI.ResultCount()) > 0
            # solver returned an unbounded ray
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
            @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.InfeasibilityCertificate
            @test MOI.cangetattribute(m, MOI.VariablePrimal(), [x, y])
            ray = MOI.getattribute(m, MOI.VariablePrimal(), [x,y])
            @test ray[1] ≈ ray[2] atol=atol rtol=rtol

        else
            # solver returned nothing
            @test MOI.getattribute(m, MOI.ResultCount()) == 0
            @test MOI.cangetattribute(m, MOI.PrimalStatus(1)) == false
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.UnboundedNoResult ||
                MOI.getattribute(m, MOI.TerminationStatus()) == MOI.InfeasibleOrUnbounded
        end
    end
end

function linear9test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "addconstraints" begin
        #   maximize 1000 x + 350 y
        #
        #       s.t.                x >= 30
        #                           y >= 0
        #                 x -   1.5 y >= 0
        #            12   x +   8   y <= 1000
        #            1000 x + 300   y <= 70000
        #
        #   solution: (59.0909, 36.3636)
        #   objv: 71818.1818
        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64},
            [
                (MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}),
                (MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}),
                (MOI.SingleVariable,MOI.GreaterThan{Float64})
            ]
        )

        m = MOI.SolverInstance(solver)
        x = MOI.addvariable!(m)
        y = MOI.addvariable!(m)

        MOI.addconstraints!(m,
            [MOI.SingleVariable(x), MOI.SingleVariable(y)],
            [MOI.GreaterThan(30.0), MOI.GreaterThan(0.0)]
        )

        MOI.addconstraints!(m,
            [MOI.ScalarAffineFunction([x, y], [1.0, -1.5], 0.0)],
            [MOI.GreaterThan(0.0)]
        )

        MOI.addconstraints!(m,
            [
                MOI.ScalarAffineFunction([x, y], [12.0, 8.0], 0.0),
                MOI.ScalarAffineFunction([x, y], [1_000.0, 300.0], 0.0)
            ],
            [
                MOI.LessThan(1_000.0),
                MOI.LessThan(70_000.0)
            ]
        )

        MOI.setobjective!(m, MOI.MaxSense,
            MOI.ScalarAffineFunction([x, y], [1_000.0, 350.0], 0.0)
        )

        MOI.optimize!(m)

        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 79e4/11 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 650/11 atol=atol rtol=rtol
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 400/11 atol=atol rtol=rtol
    end

end

function linear10test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "ranged constraints" begin
        #   maximize x + y
        #
        #       s.t.  5 <= x + y <= 10
        #                  x,  y >= 0
        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64},
            [
                (MOI.ScalarAffineFunction{Float64},MOI.Interval{Float64}),
                (MOI.SingleVariable,MOI.GreaterThan{Float64})
            ]
        )

        m = MOI.SolverInstance(solver)
        x = MOI.addvariable!(m)
        y = MOI.addvariable!(m)

        MOI.addconstraints!(m,
            [MOI.SingleVariable(x), MOI.SingleVariable(y)],
            [MOI.GreaterThan(0.0), MOI.GreaterThan(0.0)]
        )

        c = MOI.addconstraint!(m, MOI.ScalarAffineFunction([x,y], [1.0, 1.0], 0.0), MOI.Interval(5.0, 10.0))

        MOI.setobjective!(m, MOI.MaxSense,
            MOI.ScalarAffineFunction([x, y], [1.0, 1.0], 0.0)
        )

        MOI.optimize!(m)

        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 10.0 atol=atol rtol=rtol

        if MOI.getattribute(solver, MOI.SupportsDuals())
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
            @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol
        end

        MOI.setobjective!(m, MOI.MinSense,
            MOI.ScalarAffineFunction([x, y], [1.0, 1.0], 0.0)
        )

        MOI.optimize!(m)

        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 5.0 atol=atol rtol=rtol

        if MOI.getattribute(solver, MOI.SupportsDuals())
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
            @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ 1 atol=atol rtol=rtol
        end

        MOI.modifyconstraint!(m, c, MOI.Interval(2.0, 12.0))
        MOI.optimize!(m)

        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 2.0 atol=atol rtol=rtol

        MOI.setobjective!(m, MOI.MaxSense,
            MOI.ScalarAffineFunction([x, y], [1.0, 1.0], 0.0)
        )

        MOI.optimize!(m)

        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 12.0 atol=atol rtol=rtol
    end
end

function linear11test(solver::MOI.AbstractSolver, ε=Base.rtoldefault(Float64))
    @testset "Test changing constraint sense" begin
        # simple 2 variable, 1 constraint problem
        # min x + y
        # st   x + y >= 1
        #      x + y >= 2
        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64},
            [
                (MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}),
                (MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64})
            ]
        )

        m = MOI.SolverInstance(solver)

        v = MOI.addvariables!(m, 2)

        c1 = MOI.addconstraint!(m, MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0), MOI.GreaterThan(1.0))
        c2 = MOI.addconstraint!(m, MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0), MOI.GreaterThan(2.0))

        MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0))

        MOI.optimize!(m)

        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 2.0 atol=ε

        c3 = MOI.transformconstraint!(m, c2, MOI.LessThan(2.0))

        @test isa(c3, MOI.ConstraintReference{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}})
        @test MOI.isvalid(m, c2) == false
        @test MOI.isvalid(m, c3) == true
        
        MOI.optimize!(m)

        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 1.0 atol=ε
    end
end

function contlineartest(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    linear1test(solver, atol=atol, rtol=rtol)
    linear2test(solver, atol=atol, rtol=rtol)
    linear3test(solver, atol=atol, rtol=rtol)
    linear4test(solver, atol=atol, rtol=rtol)
    linear5test(solver, atol=atol, rtol=rtol)
    linear6test(solver, atol=atol, rtol=rtol)
    linear7test(solver, atol=atol, rtol=rtol)
    linear8test(solver, atol=atol, rtol=rtol)
    linear9test(solver, atol=atol, rtol=rtol)
    linear10test(solver, atol=atol, rtol=rtol)
    linear11test(solver, atol=atol, rtol=rtol)
end
