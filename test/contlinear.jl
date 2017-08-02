using MathOptInterface
MOI = MathOptInterface

using MathOptInterfaceUtilities # Defines isapprox for ScalarAffineFunction

# Continuous linear problems

function linear1test(solver::MOI.AbstractSolver, ε=Base.rtoldefault(Float64))
    @testset "Basic solve, query, resolve" begin
        # simple 2 variable, 1 constraint problem
        # min -x
        # st   x + y <= 1   (x + y - 1 ∈ Nonpositives)
        #       x, y >= 0   (x, y ∈ Nonnegatives)

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64})])

        m = MOI.SolverInstance(solver)

        v = MOI.addvariables!(m, 2)
        @test MOI.getattribute(m, MOI.NumberOfVariables()) == 2

        cf = MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0)
        c = MOI.addconstraint!(m, cf, MOI.LessThan(1.0))
        @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

        vc1 = MOI.addconstraint!(m, MOI.SingleVariable(v[1]), MOI.GreaterThan(0.0))
        vc2 = MOI.addconstraint!(m, MOI.SingleVariable(v[2]), MOI.GreaterThan(0.0))
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
            @test objf ≈ MOI.getattribute(m, MOI.ObjectiveFunction())
        end

        @test MOI.getattribute(m, MOI.ObjectiveSense()) == MOI.MaxSense

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

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 2 atol=ε

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
        @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [0, 0, 1] atol=ε

        @test MOI.cangetattribute(m, MOI.ConstraintPrimal(), c)
        @test MOI.getattribute(m, MOI.ConstraintPrimal(), c) ≈ 1 atol=ε

        if MOI.getattribute(solver, MOI.SupportsDuals())
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
            @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ -2 atol=ε

            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc1)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc1) ≈ 1 atol=ε
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc2)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc2) ≈ 2 atol=ε
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc3)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc3) ≈ 0 atol=ε
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

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 3 atol=ε

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

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 1 atol=ε

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

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 2 atol=ε

        # modify objective function to x + 2y to get :
        # max x + 2y
        # s.t. x + y + z == 2
        # x,y >= 0, z = 0

        objf = MOI.ScalarAffineFunction(v, [1.0,2.0,0.0], 0.0)
        MOI.setobjective!(m, MOI.MaxSense, objf)

        MOI.optimize!(m)

        @test MOI.cangetattribute(m, MOI.TerminationStatus())
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 4 atol=ε

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
        @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [0, 2, 0] atol=ε

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

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 3 atol=ε

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
        @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [1, 1, 0] atol=ε

        @test MOI.cangetattribute(m, MOI.ConstraintPrimal(), c)
        @test MOI.getattribute(m, MOI.ConstraintPrimal(), c) ≈ 2 atol=ε

        if MOI.getattribute(solver, MOI.SupportsDuals())
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint

            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
            @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ -1.5 atol=ε
            @test MOI.getattribute(m, MOI.ConstraintDual(), c2) ≈ 0.5 atol=ε

            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc1)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc1) ≈ 0 atol=ε
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc2)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc2) ≈ 0 atol=ε
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc3)
            @test MOI.getattribute(m, MOI.ConstraintDual(), vc3) ≈ 1.5 atol=ε
        end
    end
end

function linear2test(solver::MOI.AbstractSolver, ε=Base.rtoldefault(Float64))
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
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ -1 atol=ε

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), x)
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 1 atol=ε

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), y)
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0 atol=ε

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
    end
end

function linear3test(solver::MOI.AbstractSolver, ε=Base.rtoldefault(Float64))
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

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 3 atol=ε

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

        @test MOI.cangetattribute(m, MOI.PrimalStatus())
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.cangetattribute(m, MOI.ObjectiveValue())
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 0 atol=ε
    end
end

function linear4test(solver::MOI.AbstractSolver, ε=Base.rtoldefault(Float64))
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

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 0.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 0.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0.0 atol=ε

        # Min  x - y
        # s.t. 100.0 <= x
        #               y <= 0.0
        MOI.modifyconstraint!(m, c1, MOI.GreaterThan(100.0))
        MOI.optimize!(m)
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 100.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 100.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0.0 atol=ε

        # Min  x - y
        # s.t. 100.0 <= x
        #               y <= -100.0
        MOI.modifyconstraint!(m, c2, MOI.LessThan(-100.0))
        MOI.optimize!(m)
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 200.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 100.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ -100.0 atol=ε

    end
end

function linear5test(solver::MOI.AbstractSolver, ε=Base.rtoldefault(Float64))
    @testset "Change coeffs, del constr, del var" begin

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
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 8/3 atol=ε

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), [x, y])
        @test MOI.getattribute(m, MOI.VariablePrimal(), [x, y]) ≈ [4/3, 4/3] atol=ε

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
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 2 atol=ε

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), [x, y])
        @test MOI.getattribute(m, MOI.VariablePrimal(), [x, y]) ≈ [2.0, 0.0] atol=ɛ

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
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 4 atol=ε

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), [x, y])
        @test MOI.getattribute(m, MOI.VariablePrimal(), [x, y]) ≈ [4.0, 0.0] atol=ε

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
        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 2 atol=ε

        @test MOI.cangetattribute(m, MOI.VariablePrimal(), y)
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 2.0 atol=ε

    end
end

function linear6test(solver::MOI.AbstractSolver, ε=Base.rtoldefault(Float64))
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

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 0.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 0.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0.0 atol=ε

        # Min  x - y
        # s.t. 100.0 <= x
        #               y <= 0.0
        MOI.modifyconstraint!(m, c1, MOI.GreaterThan(100.0))
        MOI.optimize!(m)
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 100.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 100.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0.0 atol=ε

        # Min  x - y
        # s.t. 100.0 <= x
        #               y <= -100.0
        MOI.modifyconstraint!(m, c2, MOI.LessThan(-100.0))
        MOI.optimize!(m)
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 200.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 100.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ -100.0 atol=ε

    end
end

function linear7test(solver::MOI.AbstractSolver, ε=Base.rtoldefault(Float64))
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

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 0.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 0.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0.0 atol=ε

        # Min  x - y
        # s.t. 100.0 <= x
        #               y <= 0.0
        MOI.modifyconstraint!(m, c1, MOI.VectorConstantChange([-100.0]))
        MOI.optimize!(m)
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 100.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 100.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 0.0 atol=ε

        # Min  x - y
        # s.t. 100.0 <= x
        #               y <= -100.0
        MOI.modifyconstraint!(m, c2, MOI.VectorConstantChange([100.0]))
        MOI.optimize!(m)
        @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
        @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 200.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 100.0 atol=ε
        @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ -100.0 atol=ε

    end
end

function contlineartest(solver::MOI.AbstractSolver, ε=Base.rtoldefault(Float64))
    linear1test(solver, ε)
    linear2test(solver, ε)
    linear3test(solver, ε)
    linear4test(solver, ε)
    linear5test(solver, ε)
    linear6test(solver, ε)
    linear7test(solver, ε)
end
