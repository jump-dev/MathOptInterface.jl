using MathOptInterface
MOI = MathOptInterface

using MathOptInterfaceUtilities # Defines isapprox for ScalarAffineFunction

# Continuous linear problems

# Basic solver, query, resolve
function linear1test(solver::MOI.AbstractSolver, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # simple 2 variable, 1 constraint problem
    # min -x
    # st   x + y <= 1   (x + y - 1 ∈ Nonpositives)
    #       x, y >= 0   (x, y ∈ Nonnegatives)

    @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64})])

    @test MOI.get(solver, MOI.SupportsAddConstraintAfterSolve())
    @test MOI.get(solver, MOI.SupportsAddVariableAfterSolve())
    @test MOI.get(solver, MOI.SupportsDeleteConstraint())

    instance = MOI.SolverInstance(solver)

    v = MOI.addvariables!(instance, 2)
    @test MOI.get(instance, MOI.NumberOfVariables()) == 2

    cf = MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0)
    c = MOI.addconstraint!(instance, cf, MOI.LessThan(1.0))
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

    vc1 = MOI.addconstraint!(instance, MOI.SingleVariable(v[1]), MOI.GreaterThan(0.0))
    # test fallback
    vc2 = MOI.addconstraint!(instance, v[2], MOI.GreaterThan(0.0))
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

    objf = MOI.ScalarAffineFunction(v, [-1.0,0.0], 0.0)
    MOI.set!(instance, MOI.ObjectiveFunction(), objf)
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MinSense)

    @test MOI.get(instance, MOI.ObjectiveSense()) == MOI.MinSense

    if config.query
        @test MOI.canget(instance, MOI.ObjectiveFunction())
        @test objf ≈ MOI.get(instance, MOI.ObjectiveFunction())

        @test MOI.canget(instance, MOI.ConstraintFunction(), c)
        @test cf ≈ MOI.get(instance, MOI.ConstraintFunction(), c)

        @test MOI.canget(instance, MOI.ConstraintSet(), c)
        s = MOI.get(instance, MOI.ConstraintSet(), c)
        @test s == MOI.LessThan(1.0)

        @test MOI.canget(instance, MOI.ConstraintSet(), vc1)
        s = MOI.get(instance, MOI.ConstraintSet(), vc1)
        @test s == MOI.GreaterThan(0.0)

        @test MOI.canget(instance, MOI.ConstraintSet(), vc2)
        s = MOI.get(instance, MOI.ConstraintSet(), vc2)
        @test s == MOI.GreaterThan(0.0)
    end

    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.TerminationStatus())
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

    @test MOI.canget(instance, MOI.PrimalStatus())
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.canget(instance, MOI.ObjectiveValue())
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ -1 atol=atol rtol=rtol

    @test MOI.canget(instance, MOI.VariablePrimal(), v)
    @test MOI.get(instance, MOI.VariablePrimal(), v) ≈ [1, 0] atol=atol rtol=rtol

    @test MOI.canget(instance, MOI.ConstraintPrimal(), c)
    @test MOI.get(instance, MOI.ConstraintPrimal(), c) ≈ 1 atol=atol rtol=rtol

    if config.duals
        @test MOI.canget(instance, MOI.DualStatus())
        @test MOI.get(instance, MOI.DualStatus()) == MOI.FeasiblePoint
        @test MOI.canget(instance, MOI.ConstraintDual(), c)
        @test MOI.get(instance, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol

        # reduced costs
        @test MOI.canget(instance, MOI.ConstraintDual(), vc1)
        @test MOI.get(instance, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
        @test MOI.canget(instance, MOI.ConstraintDual(), vc2)
        @test MOI.get(instance, MOI.ConstraintDual(), vc2) ≈ 1 atol=atol rtol=rtol
    end

    # change objective to Max +x

    objf = MOI.ScalarAffineFunction(v, [1.0,0.0], 0.0)
    MOI.set!(instance, MOI.ObjectiveFunction(), objf)
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MaxSense)

    if MOI.canget(instance, MOI.ObjectiveFunction())
        @test objf ≈ MOI.get(instance, MOI.ObjectiveFunction())
    end

    @test MOI.get(instance, MOI.ObjectiveSense()) == MOI.MaxSense

    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.TerminationStatus())
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

    @test MOI.canget(instance, MOI.PrimalStatus())
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.canget(instance, MOI.ObjectiveValue())
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 1 atol=atol rtol=rtol

    @test MOI.canget(instance, MOI.VariablePrimal(), v)
    @test MOI.get(instance, MOI.VariablePrimal(), v) ≈ [1, 0] atol=atol rtol=rtol

    if config.duals
        @test MOI.canget(instance, MOI.DualStatus())
        @test MOI.get(instance, MOI.DualStatus()) == MOI.FeasiblePoint
        @test MOI.canget(instance, MOI.ConstraintDual(), c)
        @test MOI.get(instance, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol

        @test MOI.canget(instance, MOI.ConstraintDual(), vc1)
        @test MOI.get(instance, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
        @test MOI.canget(instance, MOI.ConstraintDual(), vc2)
        @test MOI.get(instance, MOI.ConstraintDual(), vc2) ≈ 1 atol=atol rtol=rtol
    end

    # add new variable to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x,y,z >= 0

    z = MOI.addvariable!(instance)
    push!(v, z)
    @test v[3] == z

    vc3 = MOI.addconstraint!(instance, MOI.SingleVariable(v[3]), MOI.GreaterThan(0.0))
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 3

    @test MOI.canmodifyconstraint(instance, c, MOI.ScalarCoefficientChange{Float64}(z, 1.0))
    MOI.modifyconstraint!(instance, c, MOI.ScalarCoefficientChange{Float64}(z, 1.0))

    @test MOI.canmodifyobjective(instance, MOI.ScalarCoefficientChange{Float64}(z, 2.0))
    MOI.modifyobjective!(instance, MOI.ScalarCoefficientChange{Float64}(z, 2.0))

    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 3

    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.TerminationStatus())
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

    @test MOI.canget(instance, MOI.ResultCount())
    @test MOI.get(instance, MOI.ResultCount()) >= 1

    @test MOI.canget(instance, MOI.PrimalStatus(1))
    @test MOI.get(instance, MOI.PrimalStatus(1)) == MOI.FeasiblePoint

    @test MOI.canget(instance, MOI.ObjectiveValue())
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

    @test MOI.canget(instance, MOI.VariablePrimal(), v)
    @test MOI.get(instance, MOI.VariablePrimal(), v) ≈ [0, 0, 1] atol=atol rtol=rtol

    @test MOI.canget(instance, MOI.ConstraintPrimal(), c)
    @test MOI.get(instance, MOI.ConstraintPrimal(), c) ≈ 1 atol=atol rtol=rtol

    if config.duals
        @test MOI.canget(instance, MOI.DualStatus())
        @test MOI.get(instance, MOI.DualStatus()) == MOI.FeasiblePoint
        @test MOI.canget(instance, MOI.ConstraintDual(), c)
        @test MOI.get(instance, MOI.ConstraintDual(), c) ≈ -2 atol=atol rtol=rtol

        @test MOI.canget(instance, MOI.ConstraintDual(), vc1)
        @test MOI.get(instance, MOI.ConstraintDual(), vc1) ≈ 1 atol=atol rtol=rtol
        @test MOI.canget(instance, MOI.ConstraintDual(), vc2)
        @test MOI.get(instance, MOI.ConstraintDual(), vc2) ≈ 2 atol=atol rtol=rtol
        @test MOI.canget(instance, MOI.ConstraintDual(), vc3)
        @test MOI.get(instance, MOI.ConstraintDual(), vc3) ≈ 0 atol=atol rtol=rtol
    end

    # setting lb of x to -1 to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x >= -1
    # y,z >= 0
    @test MOI.canmodifyconstraint(instance, vc1, MOI.GreaterThan(-1.0))
    MOI.modifyconstraint!(instance, vc1, MOI.GreaterThan(-1.0))

    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.TerminationStatus())
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

    @test MOI.canget(instance, MOI.ResultCount())
    @test MOI.get(instance, MOI.ResultCount()) >= 1

    @test MOI.canget(instance, MOI.PrimalStatus())
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.canget(instance, MOI.ObjectiveValue())
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 3 atol=atol rtol=rtol

    # put lb of x back to 0 and fix z to zero to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x, y >= 0, z = 0
    @test MOI.canmodifyconstraint(instance, vc1, MOI.GreaterThan(0.0))
    MOI.modifyconstraint!(instance, vc1, MOI.GreaterThan(0.0))

    @test MOI.candelete(instance, vc3)
    MOI.delete!(instance, vc3)

    vc3 = MOI.addconstraint!(instance, MOI.SingleVariable(v[3]), MOI.EqualTo(0.0))
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.TerminationStatus())
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

    @test MOI.canget(instance, MOI.ResultCount())
    @test MOI.get(instance, MOI.ResultCount()) >= 1

    @test MOI.canget(instance, MOI.PrimalStatus())
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.canget(instance, MOI.ObjectiveValue())
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 1 atol=atol rtol=rtol

    # modify affine linear constraint set to be == 2 to get :
    # max x + 2z
    # s.t. x + y + z == 2
    # x,y >= 0, z = 0
    @test MOI.candelete(instance, c)
    MOI.delete!(instance, c)
    cf = MOI.ScalarAffineFunction(v, [1.0,1.0,1.0], 0.0)
    c = MOI.addconstraint!(instance, cf, MOI.EqualTo(2.0))
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 0
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64}}()) == 1

    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.TerminationStatus())
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

    @test MOI.canget(instance, MOI.ResultCount())
    @test MOI.get(instance, MOI.ResultCount()) >= 1

    @test MOI.canget(instance, MOI.PrimalStatus())
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.canget(instance, MOI.ObjectiveValue())
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

    # modify objective function to x + 2y to get :
    # max x + 2y
    # s.t. x + y + z == 2
    # x,y >= 0, z = 0

    objf = MOI.ScalarAffineFunction(v, [1.0,2.0,0.0], 0.0)
    MOI.set!(instance, MOI.ObjectiveFunction(), objf)
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MaxSense)

    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.TerminationStatus())
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

    @test MOI.canget(instance, MOI.ResultCount())
    @test MOI.get(instance, MOI.ResultCount()) >= 1

    @test MOI.canget(instance, MOI.PrimalStatus())
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.canget(instance, MOI.ObjectiveValue())
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 4 atol=atol rtol=rtol

    @test MOI.canget(instance, MOI.VariablePrimal(), v)
    @test MOI.get(instance, MOI.VariablePrimal(), v) ≈ [0, 2, 0] atol=atol rtol=rtol

    # add constraint x - y >= 0 to get :
    # max x+2y
    # s.t. x + y + z == 2
    # x - y >= 0
    # x,y >= 0, z = 0

    cf2 = MOI.ScalarAffineFunction(v, [1.0, -1.0, 0.0], 0.0)
    c2 = MOI.addconstraint!(instance, cf2, MOI.GreaterThan(0.0))
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64}}()) == 1
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}}()) == 1
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 0

    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.TerminationStatus())
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

    @test MOI.canget(instance, MOI.ResultCount())
    @test MOI.get(instance, MOI.ResultCount()) >= 1

    @test MOI.canget(instance, MOI.PrimalStatus())
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.canget(instance, MOI.ObjectiveValue())
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 3 atol=atol rtol=rtol

    @test MOI.canget(instance, MOI.VariablePrimal(), v)
    @test MOI.get(instance, MOI.VariablePrimal(), v) ≈ [1, 1, 0] atol=atol rtol=rtol

    @test MOI.canget(instance, MOI.ConstraintPrimal(), c)
    @test MOI.get(instance, MOI.ConstraintPrimal(), c) ≈ 2 atol=atol rtol=rtol

    if config.duals
        @test MOI.canget(instance, MOI.DualStatus(1))
        @test MOI.get(instance, MOI.DualStatus(1)) == MOI.FeasiblePoint

        @test MOI.canget(instance, MOI.ConstraintDual(), c)
        @test MOI.get(instance, MOI.ConstraintDual(), c) ≈ -1.5 atol=atol rtol=rtol
        @test MOI.get(instance, MOI.ConstraintDual(), c2) ≈ 0.5 atol=atol rtol=rtol

        @test MOI.canget(instance, MOI.ConstraintDual(), vc1)
        @test MOI.get(instance, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
        @test MOI.canget(instance, MOI.ConstraintDual(), vc2)
        @test MOI.get(instance, MOI.ConstraintDual(), vc2) ≈ 0 atol=atol rtol=rtol
        @test MOI.canget(instance, MOI.ConstraintDual(), vc3)
        @test MOI.get(instance, MOI.ConstraintDual(), vc3) ≈ 1.5 atol=atol rtol=rtol
    end
end

# addvariable! (one by one)
function linear2test(solver::MOI.AbstractSolver, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # Min -x
    # s.t. x + y <= 1
    # x, y >= 0

    @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64})])

    instance = MOI.SolverInstance(solver)

    x = MOI.addvariable!(instance)
    y = MOI.addvariable!(instance)

    @test MOI.get(instance, MOI.NumberOfVariables()) == 2

    cf = MOI.ScalarAffineFunction([x, y], [1.0,1.0], 0.0)
    c = MOI.addconstraint!(instance, cf, MOI.LessThan(1.0))
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

    vc1 = MOI.addconstraint!(instance, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    vc2 = MOI.addconstraint!(instance, MOI.SingleVariable(y), MOI.GreaterThan(0.0))
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

    objf = MOI.ScalarAffineFunction([x, y], [-1.0,0.0], 0.0)
    MOI.set!(instance, MOI.ObjectiveFunction(), objf)
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MinSense)

    @test MOI.get(instance, MOI.ObjectiveSense()) == MOI.MinSense

    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.TerminationStatus())
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

    @test MOI.canget(instance, MOI.PrimalStatus())
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.canget(instance, MOI.ObjectiveValue())
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ -1 atol=atol rtol=rtol

    @test MOI.canget(instance, MOI.VariablePrimal(), x)
    @test MOI.get(instance, MOI.VariablePrimal(), x) ≈ 1 atol=atol rtol=rtol

    @test MOI.canget(instance, MOI.VariablePrimal(), y)
    @test MOI.get(instance, MOI.VariablePrimal(), y) ≈ 0 atol=atol rtol=rtol

    @test MOI.canget(instance, MOI.ConstraintPrimal(), c)
    @test MOI.get(instance, MOI.ConstraintPrimal(), c) ≈ 1 atol=atol rtol=rtol

    if config.duals
        @test MOI.canget(instance, MOI.DualStatus())
        @test MOI.get(instance, MOI.DualStatus()) == MOI.FeasiblePoint
        @test MOI.canget(instance, MOI.ConstraintDual(), c)
        @test MOI.get(instance, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol

        # reduced costs
        @test MOI.canget(instance, MOI.ConstraintDual(), vc1)
        @test MOI.get(instance, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
        @test MOI.canget(instance, MOI.ConstraintDual(), vc2)
        @test MOI.get(instance, MOI.ConstraintDual(), vc2) ≈ 1 atol=atol rtol=rtol
    end
end

# Issue #40 from Gurobi.jl
function linear3test(solver::MOI.AbstractSolver, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # min  x
    # s.t. x >= 0
    #      x >= 3

    instance = MOI.SolverInstance(solver)

    x = MOI.addvariable!(instance)
    @test MOI.get(instance, MOI.NumberOfVariables()) == 1

    MOI.addconstraint!(instance, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    cf = MOI.ScalarAffineFunction([x], [1.0], 0.0)
    MOI.addconstraint!(instance, cf, MOI.GreaterThan(3.0))

    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 1
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}}()) == 1

    objf = MOI.ScalarAffineFunction([x], [1.0], 0.0)
    MOI.set!(instance, MOI.ObjectiveFunction(), objf)
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MinSense)

    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.TerminationStatus())
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

    @test MOI.canget(instance, MOI.ResultCount())
    @test MOI.get(instance, MOI.ResultCount()) >= 1

    @test MOI.canget(instance, MOI.PrimalStatus())
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.canget(instance, MOI.ObjectiveValue())
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 3 atol=atol rtol=rtol

    # max  x
    # s.t. x <= 0
    #      x <= 3

    instance = MOI.SolverInstance(solver)

    x = MOI.addvariable!(instance)
    @test MOI.get(instance, MOI.NumberOfVariables()) == 1

    MOI.addconstraint!(instance, MOI.SingleVariable(x), MOI.LessThan(0.0))
    cf = MOI.ScalarAffineFunction([x], [1.0], 0.0)
    MOI.addconstraint!(instance, cf, MOI.LessThan(3.0))

    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.LessThan{Float64}}()) == 1
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

    objf = MOI.ScalarAffineFunction([x], [1.0], 0.0)
    MOI.set!(instance, MOI.ObjectiveFunction(), objf)
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MaxSense)

    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.TerminationStatus())
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

    @test MOI.canget(instance, MOI.ResultCount())
    @test MOI.get(instance, MOI.ResultCount()) >= 1

    @test MOI.canget(instance, MOI.PrimalStatus())
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.canget(instance, MOI.ObjectiveValue())
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 0 atol=atol rtol=rtol
end

# Modify GreaterThan and LessThan sets as bounds
function linear4test(solver::MOI.AbstractSolver, config::TestConfig)
    atol = config.atol
    rtol = config.rtol

    @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.SingleVariable,MOI.GreaterThan{Float64}),(MOI.SingleVariable,MOI.LessThan{Float64})])

    instance = MOI.SolverInstance(solver)

    x = MOI.addvariable!(instance)
    y = MOI.addvariable!(instance)

    # Min  x - y
    # s.t. 0.0 <= x          (c1)
    #             y <= 0.0   (c2)

    MOI.set!(instance, MOI.ObjectiveFunction(), MOI.ScalarAffineFunction([x,y], [1.0, -1.0], 0.0))
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MinSense)

    c1 = MOI.addconstraint!(instance, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    c2 = MOI.addconstraint!(instance, MOI.SingleVariable(y), MOI.LessThan(0.0))

    MOI.optimize!(instance)

    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 0.0 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), x) ≈ 0.0 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), y) ≈ 0.0 atol=atol rtol=rtol

    # Min  x - y
    # s.t. 100.0 <= x
    #               y <= 0.0
    @test MOI.canmodifyconstraint(instance, c1, MOI.GreaterThan(100.0))
    MOI.modifyconstraint!(instance, c1, MOI.GreaterThan(100.0))
    MOI.optimize!(instance)
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 100.0 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), x) ≈ 100.0 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), y) ≈ 0.0 atol=atol rtol=rtol

    # Min  x - y
    # s.t. 100.0 <= x
    #               y <= -100.0
    @test MOI.canmodifyconstraint(instance, c2, MOI.LessThan(-100.0))
    MOI.modifyconstraint!(instance, c2, MOI.LessThan(-100.0))
    MOI.optimize!(instance)
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 200.0 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), x) ≈ 100.0 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), y) ≈ -100.0 atol=atol rtol=rtol
end

# Change coeffs, del constr, del var
function linear5test(solver::MOI.AbstractSolver, config::TestConfig)
    @test MOI.get(solver, MOI.SupportsDeleteVariable())
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

    instance = MOI.SolverInstance(solver)

    x = MOI.addvariable!(instance)
    y = MOI.addvariable!(instance)

    @test MOI.get(instance, MOI.NumberOfVariables()) == 2

    cf1 = MOI.ScalarAffineFunction([x, y], [2.0,1.0], 0.0)
    cf2 = MOI.ScalarAffineFunction([x, y], [1.0,2.0], 0.0)

    c1 = MOI.addconstraint!(instance, cf1, MOI.LessThan(4.0))
    c2 = MOI.addconstraint!(instance, cf2, MOI.LessThan(4.0))

    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 2

    vc1 = MOI.addconstraint!(instance, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    vc2 = MOI.addconstraint!(instance, MOI.SingleVariable(y), MOI.GreaterThan(0.0))

    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

    objf = MOI.ScalarAffineFunction([x, y], [1.0,1.0], 0.0)
    MOI.set!(instance, MOI.ObjectiveFunction(), objf)
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MaxSense)

    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.TerminationStatus())
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

    @test MOI.canget(instance, MOI.PrimalStatus())
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.canget(instance, MOI.ObjectiveValue())
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 8/3 atol=atol rtol=rtol

    @test MOI.canget(instance, MOI.VariablePrimal(), [x, y])
    @test MOI.get(instance, MOI.VariablePrimal(), [x, y]) ≈ [4/3, 4/3] atol=atol rtol=rtol

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

    @test MOI.canmodifyconstraint(instance, c1, MOI.ScalarCoefficientChange(y, 3.0))
    MOI.modifyconstraint!(instance, c1, MOI.ScalarCoefficientChange(y, 3.0))
    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.TerminationStatus())
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

    @test MOI.canget(instance, MOI.PrimalStatus())
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.canget(instance, MOI.ObjectiveValue())
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

    @test MOI.canget(instance, MOI.VariablePrimal(), [x, y])
    @test MOI.get(instance, MOI.VariablePrimal(), [x, y]) ≈ [2.0, 0.0] atol=atol rtol=rtol

    # delconstrs and solve
    #   maximize x + y
    #
    #   s.t. 1 x + 2 y <= 4
    #        x >= 0, y >= 0
    #
    #   solution: x = 4, y = 0, objv = 4
    @test MOI.candelete(instance, c1)
    MOI.delete!(instance, c1)

    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.TerminationStatus())
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

    @test MOI.canget(instance, MOI.PrimalStatus())
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.canget(instance, MOI.ObjectiveValue())
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 4 atol=atol rtol=rtol

    @test MOI.canget(instance, MOI.VariablePrimal(), [x, y])
    @test MOI.get(instance, MOI.VariablePrimal(), [x, y]) ≈ [4.0, 0.0] atol=atol rtol=rtol

    # delvars and solve
    #   maximize y
    #
    #   s.t.  2 y <= 4
    #           y >= 0
    #
    #   solution: y = 2, objv = 2
    @test MOI.candelete(instance, x)
    MOI.delete!(instance, x)

    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.TerminationStatus())
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

    @test MOI.canget(instance, MOI.PrimalStatus())
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.canget(instance, MOI.ObjectiveValue())
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

    @test MOI.canget(instance, MOI.VariablePrimal(), y)
    @test MOI.get(instance, MOI.VariablePrimal(), y) ≈ 2.0 atol=atol rtol=rtol
end

# Modify GreaterThan and LessThan sets as linear constraints
function linear6test(solver::MOI.AbstractSolver, config::TestConfig)
    atol = config.atol
    rtol = config.rtol

    @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}),(MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64})])

    instance = MOI.SolverInstance(solver)

    x = MOI.addvariable!(instance)
    y = MOI.addvariable!(instance)

    # Min  x - y
    # s.t. 0.0 <= x          (c1)
    #             y <= 0.0   (c2)

    MOI.set!(instance, MOI.ObjectiveFunction(), MOI.ScalarAffineFunction([x,y], [1.0, -1.0], 0.0))
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MinSense)

    c1 = MOI.addconstraint!(instance, MOI.ScalarAffineFunction([x],[1.0],0.0), MOI.GreaterThan(0.0))
    c2 = MOI.addconstraint!(instance, MOI.ScalarAffineFunction([y],[1.0],0.0), MOI.LessThan(0.0))

    MOI.optimize!(instance)

    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 0.0 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), x) ≈ 0.0 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), y) ≈ 0.0 atol=atol rtol=rtol

    # Min  x - y
    # s.t. 100.0 <= x
    #               y <= 0.0
    @test MOI.canmodifyconstraint(instance, c1, MOI.GreaterThan(100.0))
    MOI.modifyconstraint!(instance, c1, MOI.GreaterThan(100.0))
    MOI.optimize!(instance)
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 100.0 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), x) ≈ 100.0 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), y) ≈ 0.0 atol=atol rtol=rtol

    # Min  x - y
    # s.t. 100.0 <= x
    #               y <= -100.0
    @test MOI.canmodifyconstraint(instance, c2, MOI.LessThan(-100.0))
    MOI.modifyconstraint!(instance, c2, MOI.LessThan(-100.0))
    MOI.optimize!(instance)
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 200.0 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), x) ≈ 100.0 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), y) ≈ -100.0 atol=atol rtol=rtol
end

# Modify constants in Nonnegatives and Nonpositives
function linear7test(solver::MOI.AbstractSolver, config::TestConfig)
    atol = config.atol
    rtol = config.rtol

    @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.VectorAffineFunction{Float64},MOI.Nonpositives),(MOI.VectorAffineFunction{Float64},MOI.Nonpositives)])

    instance = MOI.SolverInstance(solver)

    x = MOI.addvariable!(instance)
    y = MOI.addvariable!(instance)

    # Min  x - y
    # s.t. 0.0 <= x          (c1)
    #             y <= 0.0   (c2)

    MOI.set!(instance, MOI.ObjectiveFunction(), MOI.ScalarAffineFunction([x,y], [1.0, -1.0], 0.0))
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MinSense)

    c1 = MOI.addconstraint!(instance, MOI.VectorAffineFunction([1],[x],[1.0],[0.0]), MOI.Nonnegatives(1))
    c2 = MOI.addconstraint!(instance, MOI.VectorAffineFunction([1],[y],[1.0],[0.0]), MOI.Nonpositives(1))

    MOI.optimize!(instance)

    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 0.0 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), x) ≈ 0.0 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), y) ≈ 0.0 atol=atol rtol=rtol

    # Min  x - y
    # s.t. 100.0 <= x
    #               y <= 0.0
    @test MOI.canmodifyconstraint(instance, c1, MOI.VectorConstantChange([-100.0]))
    MOI.modifyconstraint!(instance, c1, MOI.VectorConstantChange([-100.0]))
    MOI.optimize!(instance)
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 100.0 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), x) ≈ 100.0 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), y) ≈ 0.0 atol=atol rtol=rtol

    # Min  x - y
    # s.t. 100.0 <= x
    #               y <= -100.0
    @test MOI.canmodifyconstraint(instance, c2, MOI.VectorConstantChange([100.0]))
    MOI.modifyconstraint!(instance, c2, MOI.VectorConstantChange([100.0]))
    MOI.optimize!(instance)
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 200.0 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), x) ≈ 100.0 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), y) ≈ -100.0 atol=atol rtol=rtol
end

# infeasible problem
function linear8atest(solver::MOI.AbstractSolver, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # min x
    # s.t. 2x+y <= -1
    # x,y >= 0
    @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64})])

    instance = MOI.SolverInstance(solver)
    x = MOI.addvariable!(instance)
    y = MOI.addvariable!(instance)
    c = MOI.addconstraint!(instance, MOI.ScalarAffineFunction([x,y], [2.0,1.0], 0.0), MOI.LessThan(-1.0))
    bndx = MOI.addconstraint!(instance, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    bndy = MOI.addconstraint!(instance, MOI.SingleVariable(y), MOI.GreaterThan(0.0))
    MOI.set!(instance, MOI.ObjectiveFunction(), MOI.ScalarAffineFunction([x], [1.0], 0.0))
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MinSense)
    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.ResultCount())
    if config.infeas_certificates
        # solver returned an infeasibility ray
        @test MOI.get(instance, MOI.ResultCount()) >= 1
        @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(instance, MOI.DualStatus()) == MOI.InfeasibilityCertificate
        @test MOI.canget(instance, MOI.ConstraintDual(), c)
        cd = MOI.get(instance, MOI.ConstraintDual(), c)
        @test cd < -atol
        # TODO: farkas dual on bounds - see #127
        # xd = MOI.get(instance, MOI.ConstraintDual(), bndx)
        # yd = MOI.get(instance, MOI.ConstraintDual(), bndy)
        # @test xd > atol
        # @test yd > atol
        # @test yd ≈ -cd atol=atol rtol=rtol
        # @test xd ≈ -2cd atol=atol rtol=rtol
    else
        # solver returned nothing
        @test MOI.get(instance, MOI.ResultCount()) == 0
        @test MOI.canget(instance, MOI.PrimalStatus(1)) == false
        @test MOI.get(instance, MOI.TerminationStatus()) == MOI.InfeasibleNoResult ||
            MOI.get(instance, MOI.TerminationStatus()) == MOI.InfeasibleOrUnbounded
    end
end

# unbounded problem
function linear8btest(solver::MOI.AbstractSolver, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # min -x-y
    # s.t. -x+2y <= 0
    # x,y >= 0
    @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64})])

    instance = MOI.SolverInstance(solver)
    x = MOI.addvariable!(instance)
    y = MOI.addvariable!(instance)
    MOI.addconstraint!(instance, MOI.ScalarAffineFunction([x,y], [-1.0,2.0], 0.0), MOI.LessThan(0.0))
    MOI.addconstraint!(instance, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    MOI.addconstraint!(instance, MOI.SingleVariable(y), MOI.GreaterThan(0.0))
    MOI.set!(instance, MOI.ObjectiveFunction(), MOI.ScalarAffineFunction([x, y], [-1.0, -1.0], 0.0))
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MinSense)
    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.ResultCount())
    if config.infeas_certificates
        # solver returned an unbounded ray
        @test MOI.get(instance, MOI.ResultCount()) >= 1
        @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(instance, MOI.PrimalStatus()) == MOI.InfeasibilityCertificate
    else
        # solver returned nothing
        @test MOI.get(instance, MOI.ResultCount()) == 0
        @test MOI.canget(instance, MOI.PrimalStatus(1)) == false
        @test MOI.get(instance, MOI.TerminationStatus()) == MOI.UnboundedNoResult ||
            MOI.get(instance, MOI.TerminationStatus()) == MOI.InfeasibleOrUnbounded
    end
end

# unbounded problem with unique ray
function linear8ctest(solver::MOI.AbstractSolver, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # min -x-y
    # s.t. x-y == 0
    # x,y >= 0
    @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64})])

    instance = MOI.SolverInstance(solver)
    x = MOI.addvariable!(instance)
    y = MOI.addvariable!(instance)
    MOI.addconstraint!(instance, MOI.ScalarAffineFunction([x,y], [1.0,-1.0], 0.0), MOI.EqualTo(0.0))
    MOI.addconstraint!(instance, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    MOI.addconstraint!(instance, MOI.SingleVariable(y), MOI.GreaterThan(0.0))
    MOI.set!(instance, MOI.ObjectiveFunction(),MOI.ScalarAffineFunction([x, y], [-1.0, -1.0], 0.0))
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MinSense)
    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.ResultCount())
    if config.infeas_certificates
        # solver returned an unbounded ray
        @test MOI.get(instance, MOI.ResultCount()) >= 1
        @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(instance, MOI.PrimalStatus()) == MOI.InfeasibilityCertificate
        @test MOI.canget(instance, MOI.VariablePrimal(), [x, y])
        ray = MOI.get(instance, MOI.VariablePrimal(), [x,y])
        @test ray[1] ≈ ray[2] atol=atol rtol=rtol

    else
        # solver returned nothing
        @test MOI.get(instance, MOI.ResultCount()) == 0
        @test MOI.canget(instance, MOI.PrimalStatus(1)) == false
        @test MOI.get(instance, MOI.TerminationStatus()) == MOI.UnboundedNoResult ||
            MOI.get(instance, MOI.TerminationStatus()) == MOI.InfeasibleOrUnbounded
    end
end

# addconstraints
function linear9test(solver::MOI.AbstractSolver, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
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

    instance = MOI.SolverInstance(solver)
    x = MOI.addvariable!(instance)
    y = MOI.addvariable!(instance)

    MOI.addconstraints!(instance,
        [MOI.SingleVariable(x), MOI.SingleVariable(y)],
        [MOI.GreaterThan(30.0), MOI.GreaterThan(0.0)]
    )

    MOI.addconstraints!(instance,
        [MOI.ScalarAffineFunction([x, y], [1.0, -1.5], 0.0)],
        [MOI.GreaterThan(0.0)]
    )

    MOI.addconstraints!(instance,
        [
            MOI.ScalarAffineFunction([x, y], [12.0, 8.0], 0.0),
            MOI.ScalarAffineFunction([x, y], [1_000.0, 300.0], 0.0)
        ],
        [
            MOI.LessThan(1_000.0),
            MOI.LessThan(70_000.0)
        ]
    )

    MOI.set!(instance, MOI.ObjectiveFunction(),
                      MOI.ScalarAffineFunction([x, y], [1_000.0, 350.0], 0.0))
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MaxSense)

    MOI.optimize!(instance)

    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 79e4/11 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), x) ≈ 650/11 atol=atol rtol=rtol
    @test MOI.get(instance, MOI.VariablePrimal(), y) ≈ 400/11 atol=atol rtol=rtol
end

# ranged constraints
function linear10test(solver::MOI.AbstractSolver, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
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

    instance = MOI.SolverInstance(solver)
    x = MOI.addvariable!(instance)
    y = MOI.addvariable!(instance)

    MOI.addconstraints!(instance,
        [MOI.SingleVariable(x), MOI.SingleVariable(y)],
        [MOI.GreaterThan(0.0), MOI.GreaterThan(0.0)]
    )

    c = MOI.addconstraint!(instance, MOI.ScalarAffineFunction([x,y], [1.0, 1.0], 0.0), MOI.Interval(5.0, 10.0))

    MOI.set!(instance, MOI.ObjectiveFunction(), MOI.ScalarAffineFunction([x, y], [1.0, 1.0], 0.0))
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MaxSense)

    MOI.optimize!(instance)

    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 10.0 atol=atol rtol=rtol

    if config.duals
        @test MOI.get(instance, MOI.ResultCount()) >= 1
        @test MOI.canget(instance, MOI.DualStatus())
        @test MOI.get(instance, MOI.DualStatus()) == MOI.FeasiblePoint
        @test MOI.canget(instance, MOI.ConstraintDual(), c)
        @test MOI.get(instance, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol
    end

    MOI.set!(instance, MOI.ObjectiveFunction(), MOI.ScalarAffineFunction([x, y], [1.0, 1.0], 0.0))
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MinSense)

    MOI.optimize!(instance)

    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 5.0 atol=atol rtol=rtol

    if config.duals
        @test MOI.get(instance, MOI.ResultCount()) >= 1
        @test MOI.canget(instance, MOI.DualStatus())
        @test MOI.get(instance, MOI.DualStatus()) == MOI.FeasiblePoint
        @test MOI.canget(instance, MOI.ConstraintDual(), c)
        @test MOI.get(instance, MOI.ConstraintDual(), c) ≈ 1 atol=atol rtol=rtol
    end

    @test MOI.canmodifyconstraint(instance, c, MOI.Interval(2.0, 12.0))
    MOI.modifyconstraint!(instance, c, MOI.Interval(2.0, 12.0))
    MOI.optimize!(instance)

    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 2.0 atol=atol rtol=rtol

    MOI.set!(instance, MOI.ObjectiveFunction(), MOI.ScalarAffineFunction([x, y], [1.0, 1.0], 0.0))
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MaxSense)

    MOI.optimize!(instance)

    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 12.0 atol=atol rtol=rtol
end

# changing constraint sense
function linear11test(solver::MOI.AbstractSolver, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
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

    instance = MOI.SolverInstance(solver)

    v = MOI.addvariables!(instance, 2)

    c1 = MOI.addconstraint!(instance, MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0), MOI.GreaterThan(1.0))
    c2 = MOI.addconstraint!(instance, MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0), MOI.GreaterThan(2.0))

    MOI.set!(instance, MOI.ObjectiveFunction(), MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0))
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MinSense)

    MOI.optimize!(instance)

    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 2.0 atol=atol rtol=rtol

    @test MOI.cantransformconstraint(instance, c2, MOI.LessThan(2.0))
    c3 = MOI.transformconstraint!(instance, c2, MOI.LessThan(2.0))

    @test isa(c3, MOI.ConstraintReference{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}})
    @test MOI.isvalid(instance, c2) == false
    @test MOI.isvalid(instance, c3) == true

    MOI.optimize!(instance)

    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint
    @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 1.0 atol=atol rtol=rtol
end

# infeasible problem with 2 linear constraints
function linear12test(solver::MOI.AbstractSolver, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # min x
    # s.t. 2x-3y <= -7
    #      y <= 2
    # x,y >= 0
    @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64})])

    instance = MOI.SolverInstance(solver)
    x = MOI.addvariable!(instance)
    y = MOI.addvariable!(instance)
    c1 = MOI.addconstraint!(instance, MOI.ScalarAffineFunction([x,y], [2.0,-3.0], 0.0), MOI.LessThan(-7.0))
    c2 = MOI.addconstraint!(instance, MOI.ScalarAffineFunction([y], [1.0], 0.0), MOI.LessThan(2.0))
    bndx = MOI.addconstraint!(instance, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    bndy = MOI.addconstraint!(instance, MOI.SingleVariable(y), MOI.GreaterThan(0.0))
    MOI.set!(instance, MOI.ObjectiveFunction(), MOI.ScalarAffineFunction([x], [1.0], 0.0))
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MinSense)
    MOI.optimize!(instance)

    @test MOI.canget(instance, MOI.ResultCount())
    if config.infeas_certificates
        # solver returned an infeasibility ray
        @test MOI.get(instance, MOI.ResultCount()) >= 1
        @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(instance, MOI.DualStatus()) == MOI.InfeasibilityCertificate
        @test MOI.canget(instance, MOI.ConstraintDual(), c1)
        cd1 = MOI.get(instance, MOI.ConstraintDual(), c1)
        cd2 = MOI.get(instance, MOI.ConstraintDual(), c2)
        bndxd = MOI.get(instance, MOI.ConstraintDual(), bndx)
        bndyd = MOI.get(instance, MOI.ConstraintDual(), bndy)
        @test cd1 < - atol
        @test cd2 < - atol
        @test - 3 * cd1 + cd2 ≈ -bndyd atol=atol rtol=rtol
        @test 2 * cd1 ≈ -bndxd atol=atol rtol=rtol
        @test -7 * cd1 + 2 * cd2 > atol
    else
        # solver returned nothing
        @test MOI.get(instance, MOI.ResultCount()) == 0
        @test MOI.canget(instance, MOI.PrimalStatus(1)) == false
        @test MOI.get(instance, MOI.TerminationStatus()) == MOI.InfeasibleNoResult ||
            MOI.get(instance, MOI.TerminationStatus()) == MOI.InfeasibleOrUnbounded
    end
end

const contlineartests = Dict("linear1" => linear1test,
                              "linear2" => linear2test,
                              "linear3" => linear3test,
                              "linear4" => linear4test,
                              "linear5" => linear5test,
                              "linear6" => linear6test,
                              "linear7" => linear7test,
                              "linear8a" => linear8atest,
                              "linear8b" => linear8btest,
                              "linear8c" => linear8ctest,
                              "linear9" => linear9test,
                              "linear10" => linear10test,
                              "linear11" => linear11test,
                              "linear12" => linear12test)

function contlineartest(solver::MOI.AbstractSolver, config::TestConfig, exclude::Vector{String} = [])
    for (name,f) in contlineartests
        if name in exclude
            continue
        end
        @testset "$name" f(solver, config)
    end
end
