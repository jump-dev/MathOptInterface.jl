# Continuous linear problems

# Basic solver, query, resolve
function linear1test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # simple 2 variable, 1 constraint problem
    # min -x
    # st   x + y <= 1   (x + y - 1 ∈ Nonpositives)
    #       x, y >= 0   (x, y ∈ Nonnegatives)

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{Float64})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})

    #@test MOI.get(model, MOI.SupportsAddConstraintAfterSolve())
    #@test MOI.get(model, MOI.SupportsAddVariableAfterSolve())
    #@test MOI.get(model, MOI.SupportsDeleteConstraint())

    MOI.empty!(model)
    @test MOI.is_empty(model)

    v = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2

    cf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0,1.0], v), 0.0)
    c = MOI.add_constraint(model, cf, MOI.LessThan(1.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

    vc1 = MOI.add_constraint(model, MOI.SingleVariable(v[1]), MOI.GreaterThan(0.0))
    # test fallback
    vc2 = MOI.add_constraint(model, v[2], MOI.GreaterThan(0.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

    # note: adding some redundant zero coefficients to catch solvers that don't handle duplicate coefficients correctly:
    objf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([0.0,0.0,-1.0,0.0,0.0,0.0], [v; v; v]), 0.0)
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MinSense)

    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MinSense

    if config.query
        vrs = MOI.get(model, MOI.ListOfVariableIndices())
        @test vrs == v || vrs == reverse(v)

        @test objf ≈ MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())

        @test cf ≈ MOI.get(model, MOI.ConstraintFunction(), c)

        s = MOI.get(model, MOI.ConstraintSet(), c)
        @test s == MOI.LessThan(1.0)

        s = MOI.get(model, MOI.ConstraintSet(), vc1)
        @test s == MOI.GreaterThan(0.0)

        s = MOI.get(model, MOI.ConstraintSet(), vc2)
        @test s == MOI.GreaterThan(0.0)
    end

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -1 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1, 0] atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 1 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol

            # reduced costs
            @test MOI.get(model, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc2) ≈ 1 atol=atol rtol=rtol
        end
    end

    # change objective to Max +x

    objf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0,0.0], v), 0.0)
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if config.query
        @test objf ≈ MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    end

    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MaxSense

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1, 0] atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol

            @test MOI.get(model, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc2) ≈ 1 atol=atol rtol=rtol
        end
    end

    # add new variable to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x,y,z >= 0

    z = MOI.add_variable(model)
    push!(v, z)
    @test v[3] == z

    if config.query
        # Test that the modification of v has not affected the model
        vars = map(t -> t.variable_index, MOI.get(model, MOI.ConstraintFunction(), c).terms)
        @test vars == [v[1], v[2]] || vars == [v[2], v[1]]
        @test MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, v[1])], 0.0) ≈ MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    end

    vc3 = MOI.add_constraint(model, MOI.SingleVariable(v[3]), MOI.GreaterThan(0.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 3

    if config.modify_lhs
        MOI.modify(model, c, MOI.ScalarCoefficientChange{Float64}(z, 1.0))
    else
        MOI.delete(model, c)
        cf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0,1.0,1.0], v), 0.0)
        c = MOI.add_constraint(model, cf, MOI.LessThan(1.0))
    end

    MOI.modify(model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarCoefficientChange{Float64}(z, 2.0)
    )

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 3

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.get(model, MOI.PrimalStatus(1)) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [0, 0, 1] atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 1 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -2 atol=atol rtol=rtol

            @test MOI.get(model, MOI.ConstraintDual(), vc1) ≈ 1 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc2) ≈ 2 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc3) ≈ 0 atol=atol rtol=rtol
        end
    end

    # setting lb of x to -1 to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x >= -1
    # y,z >= 0
    MOI.set(model, MOI.ConstraintSet(), vc1, MOI.GreaterThan(-1.0))

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [-1, 0, 2] atol=atol rtol=rtol
    end

    # put lb of x back to 0 and fix z to zero to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x, y >= 0, z = 0 (vc3)
    MOI.set(model, MOI.ConstraintSet(), vc1, MOI.GreaterThan(0.0))

    MOI.delete(model, vc3)

    vc3 = MOI.add_constraint(model, MOI.SingleVariable(v[3]), MOI.EqualTo(0.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1, 0, 0] atol=atol rtol=rtol
    end

    # modify affine linear constraint set to be == 2 to get :
    # max x + 2z
    # s.t. x + y + z == 2 (c)
    # x,y >= 0, z = 0
    MOI.delete(model, c)
    # note: adding some redundant zero coefficients to catch solvers that don't handle duplicate coefficients correctly:
    cf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([0.0,0.0,0.0,1.0,1.0,1.0,0.0,0.0,0.0], [v; v; v]), 0.0)
    c = MOI.add_constraint(model, cf, MOI.EqualTo(2.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 0
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64}}()) == 1

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [2, 0, 0] atol=atol rtol=rtol
    end

    # modify objective function to x + 2y to get :
    # max x + 2y
    # s.t. x + y + z == 2 (c)
    # x,y >= 0, z = 0

    objf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0,2.0,0.0], v), 0.0)
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if config.query
        @test objf ≈ MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    end

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 4 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [0, 2, 0] atol=atol rtol=rtol
    end

    # add constraint x - y >= 0 (c2) to get :
    # max x+2y
    # s.t. x + y + z == 2 (c)
    # x - y >= 0 (c2)
    # x,y >= 0 (vc1,vc2), z = 0 (vc3)

    cf2 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, -1.0, 0.0], v), 0.0)
    c2 = MOI.add_constraint(model, cf2, MOI.GreaterThan(0.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 0
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.EqualTo{Float64}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.LessThan{Float64}}()) == 0

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1, 1, 0] atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 2 atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ 0 atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), vc1) ≈ 1 atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), vc2) ≈ 1 atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), vc3) ≈ 0 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.DualStatus(1)) == MOI.FeasiblePoint

            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1.5 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ 0.5 atol=atol rtol=rtol

            @test MOI.get(model, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc2) ≈ 0 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc3) ≈ 1.5 atol=atol rtol=rtol
        end
    end

    if config.query
        @test MOI.get(model, MOI.ConstraintFunction(), c2) ≈ cf2
    end

    # delete variable x to get :
    # max 2y
    # s.t. y + z == 2
    # - y >= 0
    # y >= 0, z = 0

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2
    MOI.delete(model, v[1])
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 1

    if config.query
        @test MOI.get(model, MOI.ConstraintFunction(), c2) ≈ MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-1.0, 0.0], [v[2], z]), 0.0)

        vrs = MOI.get(model, MOI.ListOfVariableIndices())
        @test vrs == [v[2], z] || vrs == [z, v[2]]
        @test MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()) ≈ MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0, 0.0], [v[2], z]), 0.0)
    end
end

# add_variable (one by one)
function linear2test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # Min -x
    # s.t. x + y <= 1
    # x, y >= 0

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)

    @test MOI.get(model, MOI.NumberOfVariables()) == 2

    cf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0,1.0], [x, y]), 0.0)
    c = MOI.add_constraint(model, cf, MOI.LessThan(1.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

    vc1 = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    vc2 = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.GreaterThan(0.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

    objf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-1.0,0.0], [x, y]), 0.0)
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MinSense)

    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MinSense

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -1 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 1 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 0 atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 1 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol

            # reduced costs
            @test MOI.get(model, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
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

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.LessThan{Float64})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1

    MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    cf = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0)
    MOI.add_constraint(model, cf, MOI.GreaterThan(3.0))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}}()) == 1

    objf = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0)
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MinSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 3 atol=atol rtol=rtol
    end

    # max  x
    # s.t. x <= 0
    #      x <= 3

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1

    MOI.add_constraint(model, MOI.SingleVariable(x), MOI.LessThan(0.0))
    cf = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0)
    MOI.add_constraint(model, cf, MOI.LessThan(3.0))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.LessThan{Float64}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

    objf = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0)
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 0 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 0 atol=atol rtol=rtol
    end
end

# Modify GreaterThan and LessThan sets as bounds
function linear4test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.LessThan{Float64})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)

    # Min  x - y
    # s.t. 0.0 <= x          (c1)
    #             y <= 0.0   (c2)

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, -1.0], [x,y]), 0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MinSense)

    c1 = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    c2 = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.LessThan(0.0))

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
    MOI.set(model, MOI.ConstraintSet(), c1, MOI.GreaterThan(100.0))
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
    MOI.set(model, MOI.ConstraintSet(), c2, MOI.LessThan(-100.0))
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

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)

    @test MOI.get(model, MOI.NumberOfVariables()) == 2

    cf1 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0,1.0], [x, y]), 0.0)
    cf2 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0,2.0], [x, y]), 0.0)

    c1 = MOI.add_constraint(model, cf1, MOI.LessThan(4.0))
    c2 = MOI.add_constraint(model, cf2, MOI.LessThan(4.0))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 2

    vc1 = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    vc2 = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.GreaterThan(0.0))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

    objf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0,1.0], [x, y]), 0.0)
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 8/3 atol=atol rtol=rtol

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

    if config.modify_lhs
        MOI.modify(model, c1, MOI.ScalarCoefficientChange(y, 3.0))
    else
        MOI.delete(model, c1)
        cf1 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0,3.0], [x,y]), 0.0)
        c1 = MOI.add_constraint(model, cf1, MOI.LessThan(4.0))
    end

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), [x, y]) ≈ [2.0, 0.0] atol=atol rtol=rtol
    end

    # delconstrs and solve
    #   maximize x + y
    #
    #   s.t. 1 x + 2 y <= 4
    #        x >= 0, y >= 0
    #
    #   solution: x = 4, y = 0, objv = 4
    MOI.delete(model, c1)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 4 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), [x, y]) ≈ [4.0, 0.0] atol=atol rtol=rtol
    end

    # delvars and solve
    #   maximize y
    #
    #   s.t.  2 y <= 4
    #           y >= 0
    #
    #   solution: y = 2, objv = 2
    MOI.delete(model, x)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 2.0 atol=atol rtol=rtol
    end
end

# Modify GreaterThan and LessThan sets as linear constraints
function linear6test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)

    # Min  x - y
    # s.t. 0.0 <= x          (c1)
    #             y <= 0.0   (c2)

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, -1.0], [x,y]), 0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MinSense)

    c1 = MOI.add_constraint(model, MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0), MOI.GreaterThan(0.0))
    c2 = MOI.add_constraint(model, MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, y)], 0.0), MOI.LessThan(0.0))

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
    MOI.set(model, MOI.ConstraintSet(), c1, MOI.GreaterThan(100.0))
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
    MOI.set(model, MOI.ConstraintSet(), c2, MOI.LessThan(-100.0))
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

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonpositives)

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)

    # Min  x - y
    # s.t. 0.0 <= x          (c1)
    #             y <= 0.0   (c2)

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, -1.0], [x,y]), 0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MinSense)

    c1 = MOI.add_constraint(model, MOI.VectorAffineFunction([MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))], [0.0]), MOI.Nonnegatives(1))
    c2 = MOI.add_constraint(model, MOI.VectorAffineFunction([MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, y))], [0.0]), MOI.Nonpositives(1))

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

    if config.modify_lhs
        MOI.modify(model, c1, MOI.VectorConstantChange([-100.0]))
    else
        MOI.delete(model, c1)
        c1 = MOI.add_constraint(model, MOI.VectorAffineFunction([MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))], [-100.0]), MOI.Nonnegatives(1))
    end

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

    if config.modify_lhs
        MOI.modify(model, c2, MOI.VectorConstantChange([100.0]))
    else
        MOI.delete(model, c2)
        c2 = MOI.add_constraint(model, MOI.VectorAffineFunction([MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, y))], [100.0]), MOI.Nonpositives(1))
    end

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

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    c = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0,1.0], [x,y]), 0.0), MOI.LessThan(-1.0))
    bndx = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    bndy = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.GreaterThan(0.0))

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MinSense)

    if config.solve
        MOI.optimize!(model)

        if config.infeas_certificates
            # solver returned an infeasibility ray
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
            @test MOI.get(model, MOI.DualStatus()) == MOI.InfeasibilityCertificate
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

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-1.0,2.0], [x,y]), 0.0), MOI.LessThan(0.0))
    MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    MOI.add_constraint(model, MOI.SingleVariable(y), MOI.GreaterThan(0.0))

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-1.0, -1.0], [x, y]), 0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MinSense)

    if config.solve
        MOI.optimize!(model)

        if config.infeas_certificates
            # solver returned an unbounded ray
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.InfeasibilityCertificate
        else
            # solver returned nothing
            @test MOI.get(model, MOI.ResultCount()) == 0
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

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0,-1.0], [x,y]), 0.0), MOI.EqualTo(0.0))
    MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    MOI.add_constraint(model, MOI.SingleVariable(y), MOI.GreaterThan(0.0))

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-1.0, -1.0], [x, y]), 0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MinSense)

    if config.solve
        MOI.optimize!(model)

        if config.infeas_certificates
            # solver returned an unbounded ray
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.InfeasibilityCertificate
            ray = MOI.get(model, MOI.VariablePrimal(), [x,y])
            @test ray[1] ≈ ray[2] atol=atol rtol=rtol

        else
            # solver returned nothing
            @test MOI.get(model, MOI.ResultCount()) == 0
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.UnboundedNoResult ||
                MOI.get(model, MOI.TerminationStatus()) == MOI.InfeasibleOrUnbounded
        end
    end
end

# add_constraints
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

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)

    MOI.add_constraints(model,
        [MOI.SingleVariable(x), MOI.SingleVariable(y)],
        [MOI.GreaterThan(30.0), MOI.GreaterThan(0.0)]
    )

    MOI.add_constraints(model,
        [MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, -1.5], [x, y]), 0.0)],
        [MOI.GreaterThan(0.0)]
    )

    MOI.add_constraints(model,
        [
            MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([12.0, 8.0], [x, y]), 0.0),
            MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1_000.0, 300.0], [x, y]), 0.0)
        ],
        [
            MOI.LessThan(1_000.0),
            MOI.LessThan(70_000.0)
        ]
    )

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
                      MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1_000.0, 350.0], [x, y]), 0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MaxSense)

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

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)

    MOI.add_constraints(model,
        [MOI.SingleVariable(x), MOI.SingleVariable(y)],
        [MOI.GreaterThan(0.0), MOI.GreaterThan(0.0)]
    )

    c = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 1.0], [x,y]), 0.0), MOI.Interval(5.0, 10.0))

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 1.0], [x, y]), 0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 10.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 10 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol
        end
    end

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 1.0], [x, y]), 0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MinSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 5.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 5 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ 1 atol=atol rtol=rtol
        end
    end

    MOI.set(model, MOI.ConstraintSet(), c, MOI.Interval(2.0, 12.0))

    if config.query
        @test MOI.get(model, MOI.ConstraintSet(), c) == MOI.Interval(2.0, 12.0)
    end

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2.0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 2 atol=atol rtol=rtol
    end

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 1.0], [x, y]), 0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 12.0 atol=atol rtol=rtol
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

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    v = MOI.add_variables(model, 2)

    c1 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0,1.0], v), 0.0), MOI.GreaterThan(1.0))
    c2 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0,1.0], v), 0.0), MOI.GreaterThan(2.0))

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0,1.0], v), 0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MinSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2.0 atol=atol rtol=rtol
    end

    c3 = MOI.transform(model, c2, MOI.LessThan(2.0))

    @test isa(c3, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}})
    @test MOI.is_valid(model, c2) == false
    @test MOI.is_valid(model, c3) == true

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

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    c1 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0,-3.0], [x,y]), 0.0), MOI.LessThan(-7.0))
    c2 = MOI.add_constraint(model, MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, y)], 0.0), MOI.LessThan(2.0))
    bndx = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    bndy = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.GreaterThan(0.0))

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MinSense)

    if config.solve
        MOI.optimize!(model)

        if config.infeas_certificates
            # solver returned an infeasibility ray
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
            @test MOI.get(model, MOI.DualStatus()) == MOI.InfeasibilityCertificate
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

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    c1 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0,3.0], [x,y]), 0.0), MOI.GreaterThan(1.0))
    c2 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0,-1.0], [x,y]), 0.0), MOI.EqualTo(0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.FeasibilitySense)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.FeasibilitySense

    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.ResultCount()) > 0

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        xsol = MOI.get(model, MOI.VariablePrimal(), x)
        ysol = MOI.get(model, MOI.VariablePrimal(), y)

        c1sol = 2 * xsol + 3 * ysol
        @test c1sol >= 1 || isapprox(c1sol, 1.0, atol=atol, rtol=rtol)
        @test xsol - ysol ≈ 0 atol=atol rtol=rtol

        c1primval = MOI.get(model, MOI.ConstraintPrimal(), c1)
        @test c1primval >= 1 || isapprox(c1sol, 1.0, atol=atol, rtol=rtol)

        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ 0 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ 0 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ 0 atol=atol rtol=rtol
        end
    end
end

# Deletion of vector of variables
function linear14test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # max x + 2y + 3z + 4
    # s.t. 3x + 2y + z <= 2
    #      x, y, z >= 0
    #      z <= 1

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.LessThan{Float64})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x, y, z = MOI.add_variables(model, 3)
    c = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([3.0, 2.0, 1.0], [x, y, z]), 0.0), MOI.LessThan(2.0))
    clbx = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    clby = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.GreaterThan(0.0))
    clbz = MOI.add_constraint(model, MOI.SingleVariable(z), MOI.GreaterThan(0.0))
    cubz = MOI.add_constraint(model, MOI.SingleVariable(z), MOI.LessThan(1.0))

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 2.0, 3.0], [x, y, z]), 4.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 8 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 1/2 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), z) ≈ 1 atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 2 atol=atol rtol=rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), clbx) ≈ 0 atol=atol rtol=rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), clby) ≈ 1/2 atol=atol rtol=rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), clbz) ≈ 1 atol=atol rtol=rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cubz) ≈ 1 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol

            # reduced costs
            @test MOI.get(model, MOI.ConstraintDual(), clbx) ≈ 2 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), clby) ≈ 0 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), clbz) ≈ 0 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), cubz) ≈ -2 atol=atol rtol=rtol
        end
    end

    MOI.delete(model, [x, z])

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 6 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 1 atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 2 atol=atol rtol=rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), clby) ≈ 1 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol

            # reduced costs
            @test MOI.get(model, MOI.ConstraintDual(), clby) ≈ 0 atol=atol rtol=rtol
        end
    end
end

# Empty vector affine function rows (LQOI Issue #48)
function linear15test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # minimize 0
    # s.t. 0 == 0
    #      x == 1
    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{Float64}, MOI.Zeros)

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variables(model, 1)
    # Create a VectorAffineFunction with two rows, but only
    # one term, belonging to the second row. The first row,
    # which is empty, is essentially a constraint that 0 == 0.
    c = MOI.add_constraint(model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(2, MOI.ScalarAffineTerm.([1.0], x)),
            zeros(2)
        ),
        MOI.Zeros(2)
    )

    MOI.set(model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([0.0], x), 0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MinSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 0 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), x[1]) ≈ 0 atol=atol rtol=rtol
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
                             "linear13" => linear13test,
                             "linear14" => linear14test,
                             "linear15" => linear15test)

@moitestset contlinear
