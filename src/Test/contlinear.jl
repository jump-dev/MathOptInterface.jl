# Continuous linear problems

# Basic solver, query, resolve
function linear1test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # simple 2 variable, 1 constraint problem
    # min -x
    # st   x + y <= 1   (x + y - 1 ∈ Nonpositives)
    #       x, y >= 0   (x, y ∈ Nonnegatives)

    #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64})])

    #@test MOI.get(model, MOI.SupportsAddConstraintAfterSolve())
    #@test MOI.get(model, MOI.SupportsAddVariableAfterSolve())
    #@test MOI.get(model, MOI.SupportsDeleteConstraint())

    MOI.empty!(model)
    @test MOI.isempty(model)

    @test MOI.canaddvariable(model)
    v = MOI.addvariables!(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2

    cf = MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0)
    @test MOI.canaddconstraint(model, typeof(cf), MOI.LessThan{Float64})
    c = MOI.addconstraint!(model, cf, MOI.LessThan(1.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    vc1 = MOI.addconstraint!(model, MOI.SingleVariable(v[1]), MOI.GreaterThan(0.0))
    # test fallback
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    vc2 = MOI.addconstraint!(model, v[2], MOI.GreaterThan(0.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

    # note: adding some redundant zero coefficients to catch solvers that don't handle duplicate coefficients correctly:
    objf = MOI.ScalarAffineFunction([v; v; v], [0.0,0.0,-1.0,0.0,0.0,0.0], 0.0)
    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MinSense

    if config.query
        @test MOI.canget(model, MOI.ListOfVariableIndices())
        vrs = MOI.get(model, MOI.ListOfVariableIndices())
        @test vrs == v || vrs == reverse(v)

        @test !MOI.canget(model, MOI.ObjectiveFunction{MOI.SingleVariable}())
        @test MOI.canget(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
        @test objf ≈ MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())

        @test MOI.canget(model, MOI.ConstraintFunction(), typeof(c))
        @test cf ≈ MOI.get(model, MOI.ConstraintFunction(), c)

        @test MOI.canget(model, MOI.ConstraintSet(), typeof(c))
        s = MOI.get(model, MOI.ConstraintSet(), c)
        @test s == MOI.LessThan(1.0)

        @test MOI.canget(model, MOI.ConstraintSet(), typeof(vc1))
        s = MOI.get(model, MOI.ConstraintSet(), vc1)
        @test s == MOI.GreaterThan(0.0)

        @test MOI.canget(model, MOI.ConstraintSet(), typeof(vc2))
        s = MOI.get(model, MOI.ConstraintSet(), vc2)
        @test s == MOI.GreaterThan(0.0)
    end

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -1 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1, 0] atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c))
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 1 atol=atol rtol=rtol

        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c))
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol

            # reduced costs
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc1))
            @test MOI.get(model, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc2))
            @test MOI.get(model, MOI.ConstraintDual(), vc2) ≈ 1 atol=atol rtol=rtol
        end
    end

    # change objective to Max +x

    objf = MOI.ScalarAffineFunction(v, [1.0,0.0], 0.0)
    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if config.query
        @test MOI.canget(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
        @test objf ≈ MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    end

    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MaxSense

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1, 0] atol=atol rtol=rtol

        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c))
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol

            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc1))
            @test MOI.get(model, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc2))
            @test MOI.get(model, MOI.ConstraintDual(), vc2) ≈ 1 atol=atol rtol=rtol
        end
    end

    # add new variable to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x,y,z >= 0

    @test MOI.canaddvariable(model)
    z = MOI.addvariable!(model)
    push!(v, z)
    @test v[3] == z

    if config.query
        # Test that the modifcation of v has not affected the model
        @test MOI.canget(model, MOI.ConstraintFunction(), typeof(c))
        vars = MOI.get(model, MOI.ConstraintFunction(), c).variables
        @test vars == [v[1], v[2]] || vars == [v[2], v[1]]
        @test MOI.canget(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
        @test MOI.ScalarAffineFunction([v[1]], [1.0], 0.0) ≈ MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    end

    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    vc3 = MOI.addconstraint!(model, MOI.SingleVariable(v[3]), MOI.GreaterThan(0.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 3

    @test MOI.canmodifyconstraint(model, c, MOI.ScalarCoefficientChange{Float64})
    MOI.modifyconstraint!(model, c, MOI.ScalarCoefficientChange{Float64}(z, 1.0))

    @test MOI.canmodifyobjective(model, MOI.ScalarCoefficientChange{Float64})
    MOI.modifyobjective!(model, MOI.ScalarCoefficientChange{Float64}(z, 2.0))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 3

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.ResultCount())
        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.canget(model, MOI.PrimalStatus(1))
        @test MOI.get(model, MOI.PrimalStatus(1)) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [0, 0, 1] atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c))
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 1 atol=atol rtol=rtol

        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c))
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -2 atol=atol rtol=rtol

            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc1))
            @test MOI.get(model, MOI.ConstraintDual(), vc1) ≈ 1 atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc2))
            @test MOI.get(model, MOI.ConstraintDual(), vc2) ≈ 2 atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc3))
            @test MOI.get(model, MOI.ConstraintDual(), vc3) ≈ 0 atol=atol rtol=rtol
        end
    end

    # setting lb of x to -1 to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x >= -1
    # y,z >= 0
    @test MOI.canmodifyconstraint(model, vc1, MOI.GreaterThan{Float64})
    MOI.modifyconstraint!(model, vc1, MOI.GreaterThan(-1.0))

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.ResultCount())
        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [-1, 0, 2] atol=atol rtol=rtol
    end

    # put lb of x back to 0 and fix z to zero to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x, y >= 0, z = 0
    @test MOI.canmodifyconstraint(model, vc1, MOI.GreaterThan{Float64})
    MOI.modifyconstraint!(model, vc1, MOI.GreaterThan(0.0))

    @test MOI.candelete(model, vc3)
    MOI.delete!(model, vc3)

    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.EqualTo{Float64})
    vc3 = MOI.addconstraint!(model, MOI.SingleVariable(v[3]), MOI.EqualTo(0.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.ResultCount())
        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1, 0, 0] atol=atol rtol=rtol
    end

    # modify affine linear constraint set to be == 2 to get :
    # max x + 2z
    # s.t. x + y + z == 2
    # x,y >= 0, z = 0
    @test MOI.candelete(model, c)
    MOI.delete!(model, c)
    # note: adding some redundant zero coefficients to catch solvers that don't handle duplicate coefficients correctly:
    cf = MOI.ScalarAffineFunction([v; v; v], [0.0,0.0,0.0,1.0,1.0,1.0,0.0,0.0,0.0], 0.0)
    @test MOI.canaddconstraint(model, typeof(cf), MOI.EqualTo{Float64})
    c = MOI.addconstraint!(model, cf, MOI.EqualTo(2.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 0
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64}}()) == 1

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.ResultCount())
        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [2, 0, 0] atol=atol rtol=rtol
    end

    # modify objective function to x + 2y to get :
    # max x + 2y
    # s.t. x + y + z == 2
    # x,y >= 0, z = 0

    objf = MOI.ScalarAffineFunction(v, [1.0,2.0,0.0], 0.0)
    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.ResultCount())
        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 4 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [0, 2, 0] atol=atol rtol=rtol
    end

    # add constraint x - y >= 0 to get :
    # max x+2y
    # s.t. x + y + z == 2
    # x - y >= 0
    # x,y >= 0, z = 0

    cf2 = MOI.ScalarAffineFunction(v, [1.0, -1.0, 0.0], 0.0)
    @test MOI.canaddconstraint(model, typeof(cf2), MOI.GreaterThan{Float64})
    c2 = MOI.addconstraint!(model, cf2, MOI.GreaterThan(0.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 0

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.ResultCount())
        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1, 1, 0] atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c))
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 2 atol=atol rtol=rtol

        if config.duals
            @test MOI.canget(model, MOI.DualStatus(1))
            @test MOI.get(model, MOI.DualStatus(1)) == MOI.FeasiblePoint

            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c))
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1.5 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ 0.5 atol=atol rtol=rtol

            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc1))
            @test MOI.get(model, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc2))
            @test MOI.get(model, MOI.ConstraintDual(), vc2) ≈ 0 atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc3))
            @test MOI.get(model, MOI.ConstraintDual(), vc3) ≈ 1.5 atol=atol rtol=rtol
        end
    end

    if config.query
        @test MOI.canget(model, MOI.ConstraintFunction(), typeof(c2))
        @test MOI.get(model, MOI.ConstraintFunction(), c2) ≈ cf2
    end

    # delete variable x to get :
    # max 2y
    # s.t. y + z == 2
    # - y >= 0
    # y >= 0, z = 0

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2
    MOI.delete!(model, v[1])
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 1

    if config.query
        @test MOI.get(model, MOI.ConstraintFunction(), c2) ≈ MOI.ScalarAffineFunction([v[2], z], [-1.0, 0.0], 0.0)

        @test MOI.canget(model, MOI.ListOfVariableIndices())
        vrs = MOI.get(model, MOI.ListOfVariableIndices())
        @test vrs == [v[2], z] || vrs == [z, v[2]]
        @test MOI.canget(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
        @test MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()) ≈ MOI.ScalarAffineFunction([v[2], z], [2.0, 0.0], 0.0)
    end
end

# addvariable! (one by one)
function linear2test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # Min -x
    # s.t. x + y <= 1
    # x, y >= 0

    #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64})])

    MOI.empty!(model)
    @test MOI.isempty(model)

    @test MOI.canaddvariable(model)
    x = MOI.addvariable!(model)
    @test MOI.canaddvariable(model)
    y = MOI.addvariable!(model)

    @test MOI.get(model, MOI.NumberOfVariables()) == 2

    cf = MOI.ScalarAffineFunction([x, y], [1.0,1.0], 0.0)
    @test MOI.canaddconstraint(model, typeof(cf), MOI.LessThan{Float64})
    c = MOI.addconstraint!(model, cf, MOI.LessThan(1.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    vc1 = MOI.addconstraint!(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    vc2 = MOI.addconstraint!(model, MOI.SingleVariable(y), MOI.GreaterThan(0.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

    objf = MOI.ScalarAffineFunction([x, y], [-1.0,0.0], 0.0)
    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MinSense

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -1 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 1 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 0 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c))
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 1 atol=atol rtol=rtol

        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c))
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol

            # reduced costs
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc1))
            @test MOI.get(model, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc2))
            @test MOI.get(model, MOI.ConstraintDual(), vc2) ≈ 1 atol=atol rtol=rtol
        end
    end
end

# Issue #40 from Gurobi.jl
function linear3test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # min  x
    # s.t. x >= 0
    #      x >= 3

    MOI.empty!(model)
    @test MOI.isempty(model)

    @test MOI.canaddvariable(model)
    x = MOI.addvariable!(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1

    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    MOI.addconstraint!(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    cf = MOI.ScalarAffineFunction([x], [1.0], 0.0)
    @test MOI.canaddconstraint(model, typeof(cf), MOI.GreaterThan{Float64})
    MOI.addconstraint!(model, cf, MOI.GreaterThan(3.0))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}}()) == 1

    objf = MOI.ScalarAffineFunction([x], [1.0], 0.0)
    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.ResultCount())
        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 3 atol=atol rtol=rtol
    end

    # max  x
    # s.t. x <= 0
    #      x <= 3

    MOI.empty!(model)
    @test MOI.isempty(model)

    @test MOI.canaddvariable(model)
    x = MOI.addvariable!(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1

    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.LessThan{Float64})
    MOI.addconstraint!(model, MOI.SingleVariable(x), MOI.LessThan(0.0))
    cf = MOI.ScalarAffineFunction([x], [1.0], 0.0)
    @test MOI.canaddconstraint(model, typeof(cf), MOI.LessThan{Float64})
    MOI.addconstraint!(model, cf, MOI.LessThan(3.0))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.LessThan{Float64}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

    objf = MOI.ScalarAffineFunction([x], [1.0], 0.0)
    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.ResultCount())
        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 0 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 0 atol=atol rtol=rtol
    end
end

# Modify GreaterThan and LessThan sets as bounds
function linear4test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol

    #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64}, [(MOI.SingleVariable,MOI.GreaterThan{Float64}),(MOI.SingleVariable,MOI.LessThan{Float64})])

    MOI.empty!(model)
    @test MOI.isempty(model)

    @test MOI.canaddvariable(model)
    x = MOI.addvariable!(model)
    @test MOI.canaddvariable(model)
    y = MOI.addvariable!(model)

    # Min  x - y
    # s.t. 0.0 <= x          (c1)
    #             y <= 0.0   (c2)

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([x,y], [1.0, -1.0], 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    c1 = MOI.addconstraint!(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.LessThan{Float64})
    c2 = MOI.addconstraint!(model, MOI.SingleVariable(y), MOI.LessThan(0.0))

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 0.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 0.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 0.0 atol=atol rtol=rtol
    end

    # Min  x - y
    # s.t. 100.0 <= x
    #               y <= 0.0
    @test MOI.canmodifyconstraint(model, c1, MOI.GreaterThan{Float64})
    MOI.modifyconstraint!(model, c1, MOI.GreaterThan(100.0))
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 100.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 100.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 0.0 atol=atol rtol=rtol
    end

    # Min  x - y
    # s.t. 100.0 <= x
    #               y <= -100.0
    @test MOI.canmodifyconstraint(model, c2, MOI.LessThan{Float64})
    MOI.modifyconstraint!(model, c2, MOI.LessThan(-100.0))
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 200.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 100.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ -100.0 atol=atol rtol=rtol
    end
end

# Change coeffs, del constr, del var
function linear5test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    #@test MOI.get(model, MOI.SupportsDeleteVariable())
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

    MOI.empty!(model)
    @test MOI.isempty(model)

    @test MOI.canaddvariable(model)
    x = MOI.addvariable!(model)
    @test MOI.canaddvariable(model)
    y = MOI.addvariable!(model)

    @test MOI.get(model, MOI.NumberOfVariables()) == 2

    cf1 = MOI.ScalarAffineFunction([x, y], [2.0,1.0], 0.0)
    cf2 = MOI.ScalarAffineFunction([x, y], [1.0,2.0], 0.0)

    @test MOI.canaddconstraint(model, typeof(cf1), MOI.LessThan{Float64})
    c1 = MOI.addconstraint!(model, cf1, MOI.LessThan(4.0))
    @test MOI.canaddconstraint(model, typeof(cf2), MOI.LessThan{Float64})
    c2 = MOI.addconstraint!(model, cf2, MOI.LessThan(4.0))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 2

    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    vc1 = MOI.addconstraint!(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    vc2 = MOI.addconstraint!(model, MOI.SingleVariable(y), MOI.GreaterThan(0.0))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

    objf = MOI.ScalarAffineFunction([x, y], [1.0,1.0], 0.0)
    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 8/3 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), [x, y]) ≈ [4/3, 4/3] atol=atol rtol=rtol
    end

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

    @test MOI.canmodifyconstraint(model, c1, MOI.ScalarCoefficientChange{Float64})
    MOI.modifyconstraint!(model, c1, MOI.ScalarCoefficientChange(y, 3.0))
    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), [x, y]) ≈ [2.0, 0.0] atol=atol rtol=rtol
    end

    # delconstrs and solve
    #   maximize x + y
    #
    #   s.t. 1 x + 2 y <= 4
    #        x >= 0, y >= 0
    #
    #   solution: x = 4, y = 0, objv = 4
    @test MOI.candelete(model, c1)
    MOI.delete!(model, c1)

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 4 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), [x, y]) ≈ [4.0, 0.0] atol=atol rtol=rtol
    end

    # delvars and solve
    #   maximize y
    #
    #   s.t.  2 y <= 4
    #           y >= 0
    #
    #   solution: y = 2, objv = 2
    @test MOI.candelete(model, x)
    MOI.delete!(model, x)

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 2.0 atol=atol rtol=rtol
    end
end

# Modify GreaterThan and LessThan sets as linear constraints
function linear6test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol

    #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}),(MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64})])

    MOI.empty!(model)
    @test MOI.isempty(model)

    @test MOI.canaddvariable(model)
    x = MOI.addvariable!(model)
    @test MOI.canaddvariable(model)
    y = MOI.addvariable!(model)

    # Min  x - y
    # s.t. 0.0 <= x          (c1)
    #             y <= 0.0   (c2)

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([x,y], [1.0, -1.0], 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})
    c1 = MOI.addconstraint!(model, MOI.ScalarAffineFunction([x],[1.0],0.0), MOI.GreaterThan(0.0))
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    c2 = MOI.addconstraint!(model, MOI.ScalarAffineFunction([y],[1.0],0.0), MOI.LessThan(0.0))

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 0.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 0.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 0.0 atol=atol rtol=rtol
    end

    # Min  x - y
    # s.t. 100.0 <= x
    #               y <= 0.0
    @test MOI.canmodifyconstraint(model, c1, MOI.GreaterThan{Float64})
    MOI.modifyconstraint!(model, c1, MOI.GreaterThan(100.0))
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 100.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 100.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 0.0 atol=atol rtol=rtol
    end

    # Min  x - y
    # s.t. 100.0 <= x
    #               y <= -100.0
    @test MOI.canmodifyconstraint(model, c2, MOI.LessThan{Float64})
    MOI.modifyconstraint!(model, c2, MOI.LessThan(-100.0))
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 200.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 100.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ -100.0 atol=atol rtol=rtol
    end
end

# Modify constants in Nonnegatives and Nonpositives
function linear7test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol

    #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64}, [(MOI.VectorAffineFunction{Float64},MOI.Nonpositives),(MOI.VectorAffineFunction{Float64},MOI.Nonpositives)])

    MOI.empty!(model)
    @test MOI.isempty(model)

    @test MOI.canaddvariable(model)
    x = MOI.addvariable!(model)
    @test MOI.canaddvariable(model)
    y = MOI.addvariable!(model)

    # Min  x - y
    # s.t. 0.0 <= x          (c1)
    #             y <= 0.0   (c2)

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([x,y], [1.0, -1.0], 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)
    c1 = MOI.addconstraint!(model, MOI.VectorAffineFunction([1],[x],[1.0],[0.0]), MOI.Nonnegatives(1))
    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonpositives)
    c2 = MOI.addconstraint!(model, MOI.VectorAffineFunction([1],[y],[1.0],[0.0]), MOI.Nonpositives(1))

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 0.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 0.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 0.0 atol=atol rtol=rtol
    end

    # Min  x - y
    # s.t. 100.0 <= x
    #               y <= 0.0
    @test MOI.canmodifyconstraint(model, c1, MOI.VectorConstantChange{Float64})
    MOI.modifyconstraint!(model, c1, MOI.VectorConstantChange([-100.0]))
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 100.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 100.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 0.0 atol=atol rtol=rtol
    end

    # Min  x - y
    # s.t. 100.0 <= x
    #               y <= -100.0
    @test MOI.canmodifyconstraint(model, c2, MOI.VectorConstantChange{Float64})
    MOI.modifyconstraint!(model, c2, MOI.VectorConstantChange([100.0]))
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 200.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 100.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ -100.0 atol=atol rtol=rtol
    end
end

# infeasible problem
function linear8atest(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # min x
    # s.t. 2x+y <= -1
    # x,y >= 0
    #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64})])

    MOI.empty!(model)
    @test MOI.isempty(model)

    @test MOI.canaddvariable(model)
    x = MOI.addvariable!(model)
    @test MOI.canaddvariable(model)
    y = MOI.addvariable!(model)
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    c = MOI.addconstraint!(model, MOI.ScalarAffineFunction([x,y], [2.0,1.0], 0.0), MOI.LessThan(-1.0))
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    bndx = MOI.addconstraint!(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    bndy = MOI.addconstraint!(model, MOI.SingleVariable(y), MOI.GreaterThan(0.0))

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([x], [1.0], 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.ResultCount())
        if config.infeas_certificates
            # solver returned an infeasibility ray
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
            @test MOI.get(model, MOI.DualStatus()) == MOI.InfeasibilityCertificate
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c))
            cd = MOI.get(model, MOI.ConstraintDual(), c)
            @test cd < -atol
            # TODO: farkas dual on bounds - see #127
            # xd = MOI.get(model, MOI.ConstraintDual(), bndx)
            # yd = MOI.get(model, MOI.ConstraintDual(), bndy)
            # @test xd > atol
            # @test yd > atol
            # @test yd ≈ -cd atol=atol rtol=rtol
            # @test xd ≈ -2cd atol=atol rtol=rtol
        else
            # solver returned nothing
            @test MOI.get(model, MOI.ResultCount()) == 0
            @test MOI.canget(model, MOI.PrimalStatus(1)) == false
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.InfeasibleNoResult ||
                MOI.get(model, MOI.TerminationStatus()) == MOI.InfeasibleOrUnbounded
        end
    end
end

# unbounded problem
function linear8btest(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # min -x-y
    # s.t. -x+2y <= 0
    # x,y >= 0
    #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64})])

    MOI.empty!(model)
    @test MOI.isempty(model)

    @test MOI.canaddvariable(model)
    x = MOI.addvariable!(model)
    @test MOI.canaddvariable(model)
    y = MOI.addvariable!(model)
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    MOI.addconstraint!(model, MOI.ScalarAffineFunction([x,y], [-1.0,2.0], 0.0), MOI.LessThan(0.0))
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    MOI.addconstraint!(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    MOI.addconstraint!(model, MOI.SingleVariable(y), MOI.GreaterThan(0.0))

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([x, y], [-1.0, -1.0], 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.ResultCount())
        if config.infeas_certificates
            # solver returned an unbounded ray
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.InfeasibilityCertificate
        else
            # solver returned nothing
            @test MOI.get(model, MOI.ResultCount()) == 0
            @test MOI.canget(model, MOI.PrimalStatus(1)) == false
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.UnboundedNoResult ||
                MOI.get(model, MOI.TerminationStatus()) == MOI.InfeasibleOrUnbounded
        end
    end
end

# unbounded problem with unique ray
function linear8ctest(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # min -x-y
    # s.t. x-y == 0
    # x,y >= 0
    #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64})])

    MOI.empty!(model)
    @test MOI.isempty(model)

    @test MOI.canaddvariable(model)
    x = MOI.addvariable!(model)
    @test MOI.canaddvariable(model)
    y = MOI.addvariable!(model)
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
    MOI.addconstraint!(model, MOI.ScalarAffineFunction([x,y], [1.0,-1.0], 0.0), MOI.EqualTo(0.0))
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    MOI.addconstraint!(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    MOI.addconstraint!(model, MOI.SingleVariable(y), MOI.GreaterThan(0.0))

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),MOI.ScalarAffineFunction([x, y], [-1.0, -1.0], 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.ResultCount())
        if config.infeas_certificates
            # solver returned an unbounded ray
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.InfeasibilityCertificate
            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            ray = MOI.get(model, MOI.VariablePrimal(), [x,y])
            @test ray[1] ≈ ray[2] atol=atol rtol=rtol

        else
            # solver returned nothing
            @test MOI.get(model, MOI.ResultCount()) == 0
            @test MOI.canget(model, MOI.PrimalStatus(1)) == false
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.UnboundedNoResult ||
                MOI.get(model, MOI.TerminationStatus()) == MOI.InfeasibleOrUnbounded
        end
    end
end

# addconstraints
function linear9test(model::MOI.ModelLike, config::TestConfig)
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
    #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64},
    #    [
    #        (MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}),
    #        (MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}),
    #        (MOI.SingleVariable,MOI.GreaterThan{Float64})
    #    ]
    #)

    MOI.empty!(model)
    @test MOI.isempty(model)

    @test MOI.canaddvariable(model)
    x = MOI.addvariable!(model)
    @test MOI.canaddvariable(model)
    y = MOI.addvariable!(model)

    MOI.addconstraints!(model,
        [MOI.SingleVariable(x), MOI.SingleVariable(y)],
        [MOI.GreaterThan(30.0), MOI.GreaterThan(0.0)]
    )

    MOI.addconstraints!(model,
        [MOI.ScalarAffineFunction([x, y], [1.0, -1.5], 0.0)],
        [MOI.GreaterThan(0.0)]
    )

    MOI.addconstraints!(model,
        [
            MOI.ScalarAffineFunction([x, y], [12.0, 8.0], 0.0),
            MOI.ScalarAffineFunction([x, y], [1_000.0, 300.0], 0.0)
        ],
        [
            MOI.LessThan(1_000.0),
            MOI.LessThan(70_000.0)
        ]
    )

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
                      MOI.ScalarAffineFunction([x, y], [1_000.0, 350.0], 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 79e4/11 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 650/11 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 400/11 atol=atol rtol=rtol
    end
end

# ranged constraints
function linear10test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    #   maximize x + y
    #
    #       s.t.  5 <= x + y <= 10
    #                  x,  y >= 0
    #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64},
    #    [
    #        (MOI.ScalarAffineFunction{Float64},MOI.Interval{Float64}),
    #        (MOI.SingleVariable,MOI.GreaterThan{Float64})
    #    ]
    #)

    MOI.empty!(model)
    @test MOI.isempty(model)

    @test MOI.canaddvariable(model)
    x = MOI.addvariable!(model)
    @test MOI.canaddvariable(model)
    y = MOI.addvariable!(model)

    MOI.addconstraints!(model,
        [MOI.SingleVariable(x), MOI.SingleVariable(y)],
        [MOI.GreaterThan(0.0), MOI.GreaterThan(0.0)]
    )

    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64})
    c = MOI.addconstraint!(model, MOI.ScalarAffineFunction([x,y], [1.0, 1.0], 0.0), MOI.Interval(5.0, 10.0))

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([x, y], [1.0, 1.0], 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 10.0 atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c))
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 10 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c))
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol
        end
    end

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([x, y], [1.0, 1.0], 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 5.0 atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c))
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 5 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c))
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ 1 atol=atol rtol=rtol
        end
    end

    @test MOI.canmodifyconstraint(model, c, MOI.Interval{Float64})
    MOI.modifyconstraint!(model, c, MOI.Interval(2.0, 12.0))

    if config.query
        @test MOI.canget(model, MOI.ConstraintSet(), typeof(c))
        @test MOI.get(model, MOI.ConstraintSet(), c) == MOI.Interval(2.0, 12.0)
    end

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2.0 atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c))
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 2 atol=atol rtol=rtol
    end

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([x, y], [1.0, 1.0], 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 12.0 atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c))
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 12 atol=atol rtol=rtol
    end
end

# changing constraint sense
function linear11test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # simple 2 variable, 1 constraint problem
    # min x + y
    # st   x + y >= 1
    #      x + y >= 2
    #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64},
    #    [
    #        (MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}),
    #        (MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64})
    #    ]
    #)

    MOI.empty!(model)
    @test MOI.isempty(model)

    @test MOI.canaddvariable(model)
    v = MOI.addvariables!(model, 2)

    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})
    c1 = MOI.addconstraint!(model, MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0), MOI.GreaterThan(1.0))
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})
    c2 = MOI.addconstraint!(model, MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0), MOI.GreaterThan(2.0))

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2.0 atol=atol rtol=rtol
    end

    @test MOI.cantransformconstraint(model, c2, MOI.LessThan{Float64})
    c3 = MOI.transformconstraint!(model, c2, MOI.LessThan(2.0))

    @test isa(c3, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}})
    @test MOI.isvalid(model, c2) == false
    @test MOI.isvalid(model, c3) == true

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1.0 atol=atol rtol=rtol
    end
end

# infeasible problem with 2 linear constraints
function linear12test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # min x
    # s.t. 2x-3y <= -7
    #      y <= 2
    # x,y >= 0
    #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64})])

    MOI.empty!(model)
    @test MOI.isempty(model)

    @test MOI.canaddvariable(model)
    x = MOI.addvariable!(model)
    @test MOI.canaddvariable(model)
    y = MOI.addvariable!(model)
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    c1 = MOI.addconstraint!(model, MOI.ScalarAffineFunction([x,y], [2.0,-3.0], 0.0), MOI.LessThan(-7.0))
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    c2 = MOI.addconstraint!(model, MOI.ScalarAffineFunction([y], [1.0], 0.0), MOI.LessThan(2.0))
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    bndx = MOI.addconstraint!(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    bndy = MOI.addconstraint!(model, MOI.SingleVariable(y), MOI.GreaterThan(0.0))

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([x], [1.0], 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.ResultCount())
        if config.infeas_certificates
            # solver returned an infeasibility ray
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
            @test MOI.get(model, MOI.DualStatus()) == MOI.InfeasibilityCertificate
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c1))
            cd1 = MOI.get(model, MOI.ConstraintDual(), c1)
            cd2 = MOI.get(model, MOI.ConstraintDual(), c2)
            bndxd = MOI.get(model, MOI.ConstraintDual(), bndx)
            bndyd = MOI.get(model, MOI.ConstraintDual(), bndy)
            @test cd1 < - atol
            @test cd2 < - atol
            @test - 3 * cd1 + cd2 ≈ -bndyd atol=atol rtol=rtol
            @test 2 * cd1 ≈ -bndxd atol=atol rtol=rtol
            @test -7 * cd1 + 2 * cd2 > atol
        else
            # solver returned nothing
            @test MOI.get(model, MOI.ResultCount()) == 0
            @test MOI.canget(model, MOI.PrimalStatus(1)) == false
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.InfeasibleNoResult ||
                MOI.get(model, MOI.TerminationStatus()) == MOI.InfeasibleOrUnbounded
        end
    end
end

# feasibility problem
function linear13test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # find x, y
    # s.t. 2x + 3y >= 1
    #      x - y == 0

    MOI.empty!(model)
    @test MOI.isempty(model)
    @test MOI.canaddvariable(model)
    x = MOI.addvariable!(model)
    @test MOI.canaddvariable(model)
    y = MOI.addvariable!(model)
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})
    c1 = MOI.addconstraint!(model, MOI.ScalarAffineFunction([x,y], [2.0,3.0], 0.0), MOI.GreaterThan(1.0))
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
    c2 = MOI.addconstraint!(model, MOI.ScalarAffineFunction([x,y], [1.0,-1.0], 0.0), MOI.EqualTo(0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.FeasibilitySense)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.FeasibilitySense

    if config.solve
        MOI.optimize!(model)
        @test MOI.canget(model, MOI.ResultCount())
        @test MOI.get(model, MOI.ResultCount()) > 0

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        xsol = MOI.get(model, MOI.VariablePrimal(), x)
        ysol = MOI.get(model, MOI.VariablePrimal(), y)

        c1sol = 2 * xsol + 3 * ysol
        @test c1sol >= 1 || isapprox(c1sol, 1.0, atol=atol, rtol=rtol)
        @test xsol - ysol ≈ 0 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c1))
        c1primval = MOI.get(model, MOI.ConstraintPrimal(), c1)
        @test c1primval >= 1 || isapprox(c1sol, 1.0, atol=atol, rtol=rtol)

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c2))
        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ 0 atol=atol rtol=rtol

        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c1))
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ 0 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ 0 atol=atol rtol=rtol
        end
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
                             "linear12" => linear12test,
                             "linear13" => linear13test)

@moitestset contlinear
