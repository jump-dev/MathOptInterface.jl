# Continuous linear problems

# Basic solver, query, resolve
function linear1test(model::MOI.ModelLike, config::TestConfig{T}) where T
    atol = config.atol
    rtol = config.rtol
    # simple 2 variable, 1 constraint problem
    # min -x
    # st   x + y <= 1   (x + y - 1 ∈ Nonpositives)
    #       x, y >= 0   (x, y ∈ Nonnegatives)

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.EqualTo{T})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.LessThan{T})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{T})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{T})

    #@test MOI.get(model, MOI.SupportsAddConstraintAfterSolve())
    #@test MOI.get(model, MOI.SupportsAddVariableAfterSolve())
    #@test MOI.get(model, MOI.SupportsDeleteConstraint())

    MOI.empty!(model)
    @test MOI.is_empty(model)

    v = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2

    cf = MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), one(T)], v), zero(T))
    c = MOI.add_constraint(model, cf, MOI.LessThan(one(T)))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}}()) == 1

    vc1 = MOI.add_constraint(model, MOI.SingleVariable(v[1]), MOI.GreaterThan(zero(T)))
    # We test this after the creation of every `SingleVariable` constraint
    # to ensure a good coverage of corner cases.
    @test vc1.value == v[1].value
    # test fallback
    vc2 = MOI.add_constraint(model, v[2], MOI.GreaterThan(zero(T)))
    @test vc2.value == v[2].value
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{T}}()) == 2

    # note: adding some redundant zero coefficients to catch solvers that don't handle duplicate coefficients correctly:
    objf = MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([zero(T), zero(T), -one(T), zero(T), zero(T), zero(T)], [v; v; v]), zero(T))
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)

    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE

    if config.query
        vrs = MOI.get(model, MOI.ListOfVariableIndices())
        @test vrs == v || vrs == reverse(v)

        @test objf ≈ MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())

        @test cf ≈ MOI.get(model, MOI.ConstraintFunction(), c)

        s = MOI.get(model, MOI.ConstraintSet(), c)
        @test s == MOI.LessThan(one(T))

        s = MOI.get(model, MOI.ConstraintSet(), vc1)
        @test s == MOI.GreaterThan(zero(T))

        s = MOI.get(model, MOI.ConstraintSet(), vc2)
        @test s == MOI.GreaterThan(zero(T))
    end

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED

        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -1 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1, 0] atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 1 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ -1 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol

            # reduced costs
            @test MOI.get(model, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc2) ≈ 1 atol=atol rtol=rtol
        end
    end

    # change objective to Max +x

    objf = MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), zero(T)], v), zero(T))
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)

    if config.query
        @test objf ≈ MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    end

    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1, 0] atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ 1 atol=atol rtol=rtol
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
        @test MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm{T}(one(T), v[1])], zero(T)) ≈ MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    end

    vc3 = MOI.add_constraint(model, MOI.SingleVariable(v[3]), MOI.GreaterThan(zero(T)))
    @test vc3.value == v[3].value
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{T}}()) == 3

    if config.modify_lhs
        MOI.modify(model, c, MOI.ScalarCoefficientChange{T}(z, one(T)))
    else
        MOI.delete(model, c)
        cf = MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), one(T), one(T)], v), zero(T))
        c = MOI.add_constraint(model, cf, MOI.LessThan(one(T)))
    end

    MOI.modify(model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarCoefficientChange{T}(z, T(2))
    )

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{T}}()) == 3

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.get(model, MOI.PrimalStatus(1)) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [0, 0, 1] atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 1 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ 2 atol=atol rtol=rtol
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
    MOI.set(model, MOI.ConstraintSet(), vc1, MOI.GreaterThan(-one(T)))

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [-1, 0, 2] atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ 3 atol=atol rtol=rtol
        end
    end

    # put lb of x back to 0 and fix z to zero to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x, y >= 0, z = 0 (vc3)
    MOI.set(model, MOI.ConstraintSet(), vc1, MOI.GreaterThan(zero(T)))

    MOI.delete(model, vc3)

    vc3 = MOI.add_constraint(model, MOI.SingleVariable(v[3]), MOI.EqualTo(zero(T)))
    @test vc3.value == v[3].value
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{T}}()) == 2

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1, 0, 0] atol=atol rtol=rtol
    end

    # modify affine linear constraint set to be == 2 to get :
    # max x + 2z
    # s.t. x + y + z == 2 (c)
    # x,y >= 0, z = 0
    MOI.delete(model, c)
    # note: adding some redundant zero coefficients to catch solvers that don't handle duplicate coefficients correctly:
    cf = MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([zero(T), zero(T), zero(T), one(T), one(T), one(T), zero(T), zero(T), zero(T)], [v; v; v]), zero(T))
    c = MOI.add_constraint(model, cf, MOI.EqualTo(T(2)))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}}()) == 0
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}()) == 1

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [2, 0, 0] atol=atol rtol=rtol
    end

    # modify objective function to x + 2y to get :
    # max x + 2y
    # s.t. x + y + z == 2 (c)
    # x,y >= 0, z = 0

    objf = MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), T(2), zero(T)], v), zero(T))
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)

    if config.query
        @test objf ≈ MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    end

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 4 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [0, 2, 0] atol=atol rtol=rtol
    end

    # add constraint x - y >= 0 (c2) to get :
    # max x+2y
    # s.t. x + y + z == 2 (c)
    # x - y >= 0 (c2)
    # x,y >= 0 (vc1,vc2), z = 0 (vc3)

    cf2 = MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), -one(T), zero(T)], v), zero(T))
    c2 = MOI.add_constraint(model, cf2, MOI.GreaterThan(zero(T)))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.GreaterThan{T}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}}()) == 0
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.EqualTo{T}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{T}}()) == 2
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.LessThan{T}}()) == 0

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1, 1, 0] atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 2 atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ 0 atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), vc1) ≈ 1 atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), vc2) ≈ 1 atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), vc3) ≈ 0 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.DualStatus(1)) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ 3 atol=atol rtol=rtol

            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -T(3//2) atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ T(1//2) atol=atol rtol=rtol

            @test MOI.get(model, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc2) ≈ 0 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc3) ≈ T(3//2) atol=atol rtol=rtol
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

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{T}}()) == 2
    MOI.delete(model, v[1])
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{T}}()) == 1

    if config.query
        err = MOI.InvalidIndex(vc1)
        # vc1 should have been deleted with `v[1]`.
        @test_throws err MOI.get(model, MOI.ConstraintFunction(), vc1)
        @test_throws err MOI.get(model, MOI.ConstraintSet(), vc1)

        @test MOI.get(model, MOI.ConstraintFunction(), c2) ≈ MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([-one(T), zero(T)], [v[2], z]), zero(T))

        vrs = MOI.get(model, MOI.ListOfVariableIndices())
        @test vrs == [v[2], z] || vrs == [z, v[2]]
        @test MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}()) ≈ MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([T(2), zero(T)], [v[2], z]), zero(T))
    end
end

# add_variable (one by one)
function linear2test(model::MOI.ModelLike, config::TestConfig{T}) where T
    atol = config.atol
    rtol = config.rtol
    # Min -x
    # s.t. x + y <= 1
    # x, y >= 0

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.LessThan{T})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{T})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)

    @test MOI.get(model, MOI.NumberOfVariables()) == 2

    cf = MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), one(T)], [x, y]), zero(T))
    c = MOI.add_constraint(model, cf, MOI.LessThan(one(T)))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}}()) == 1

    vc1 = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(zero(T)))
    @test vc1.value == x.value
    vc2 = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.GreaterThan(zero(T)))
    @test vc2.value == y.value
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{T}}()) == 2

    objf = MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([-one(T), zero(T)], [x, y]), zero(T))
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)

    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED

        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -1 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 1 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 0 atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 1 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ -1 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol

            # reduced costs
            @test MOI.get(model, MOI.ConstraintDual(), vc1) ≈ 0 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc2) ≈ 1 atol=atol rtol=rtol
        end

        if config.basis
            @test MOI.get(model, MOI.ConstraintBasisStatus(), vc1) == MOI.BASIC
            @test MOI.get(model, MOI.ConstraintBasisStatus(), vc2) == MOI.NONBASIC
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c) == MOI.NONBASIC
        end
    end
end

# Issue #40 from Gurobi.jl
function linear3test(model::MOI.ModelLike, config::TestConfig{T}) where T
    atol = config.atol
    rtol = config.rtol
    # min  x
    # s.t. x >= 0
    #      x >= 3

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.LessThan{T})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{T})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.LessThan{T})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1

    vc = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(zero(T)))
    @test vc.value == x.value
    cf = MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm{T}(one(T), x)], zero(T))
    c = MOI.add_constraint(model, cf, MOI.GreaterThan(T(3)))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{T}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.GreaterThan{T}}()) == 1

    objf = MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm{T}(one(T), x)], zero(T))
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 3 atol=atol rtol=rtol

        if config.basis
            @test MOI.get(model, MOI.ConstraintBasisStatus(), vc) == MOI.BASIC
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c) == MOI.NONBASIC
        end
    end

    # max  x
    # s.t. x <= 0
    #      x <= 3

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1

    vc = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.LessThan(zero(T)))
    @test vc.value == x.value
    cf = MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm{T}(one(T), x)], zero(T))
    c = MOI.add_constraint(model, cf, MOI.LessThan(T(3)))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.LessThan{T}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}}()) == 1

    objf = MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm{T}(one(T), x)], zero(T))
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED

        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 0 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 0 atol=atol rtol=rtol

        if config.basis
            @test MOI.get(model, MOI.ConstraintBasisStatus(), vc) == MOI.NONBASIC
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c) == MOI.BASIC
        end
    end
end

# Modify GreaterThan{T} and LessThan{T} sets as bounds
function linear4test(model::MOI.ModelLike, config::TestConfig{T}) where T
    atol = config.atol
    rtol = config.rtol

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{T})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.LessThan{T})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)

    # Min  x - y
    # s.t. zero(T) <= x          (c1)
    #             y <= zero(T)   (c2)

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), -one(T)], [x, y]), zero(T)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)

    c1 = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(zero(T)))
    @test c1.value == x.value
    c2 = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.LessThan(zero(T)))
    @test c2.value == y.value

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED

        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ zero(T) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ zero(T) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ zero(T) atol=atol rtol=rtol
    end

    # Min  x - y
    # s.t. T(100) <= x
    #               y <= zero(T)
    MOI.set(model, MOI.ConstraintSet(), c1, MOI.GreaterThan(T(100)))
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(100) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ T(100) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ zero(T) atol=atol rtol=rtol
    end

    # Min  x - y
    # s.t. T(100) <= x
    #               y <= -T(100)
    MOI.set(model, MOI.ConstraintSet(), c2, MOI.LessThan(-T(100)))
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(200) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ T(100) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ -T(100) atol=atol rtol=rtol
    end
end

# Change coeffs, del constr, del var
function linear5test(model::MOI.ModelLike, config::TestConfig{T}) where T
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
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.LessThan{T})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)

    @test MOI.get(model, MOI.NumberOfVariables()) == 2

    cf1 = MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([T(2), one(T)], [x, y]), zero(T))
    cf2 = MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), T(2)], [x, y]), zero(T))

    c1 = MOI.add_constraint(model, cf1, MOI.LessThan(T(4)))
    c2 = MOI.add_constraint(model, cf2, MOI.LessThan(T(4)))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}}()) == 2

    vc1 = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(zero(T)))
    @test vc1.value == x.value
    vc2 = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.GreaterThan(zero(T)))
    @test vc2.value == y.value

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{T}}()) == 2

    objf = MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), one(T)], [x, y]), zero(T))
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), objf)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED

        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(8//3) atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), [x, y]) ≈ [T(4//3), T(4//3)] atol=atol rtol=rtol
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
        MOI.modify(model, c1, MOI.ScalarCoefficientChange(y, T(3)))
    else
        MOI.delete(model, c1)
        cf1 = MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([T(2), T(3)], [x, y]), zero(T))
        c1 = MOI.add_constraint(model, cf1, MOI.LessThan(T(4)))
    end

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), [x, y]) ≈ [T(2), zero(T)] atol=atol rtol=rtol
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

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 4 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), [x, y]) ≈ [T(4), zero(T)] atol=atol rtol=rtol
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

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ T(2) atol=atol rtol=rtol
    end
end

# Modify GreaterThan{T} and LessThan{T} sets as linear constraints
function linear6test(model::MOI.ModelLike, config::TestConfig{T}) where T
    atol = config.atol
    rtol = config.rtol

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.LessThan{T})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)

    # Min  x - y
    # s.t. zero(T) <= x          (c1)
    #             y <= zero(T)   (c2)

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
            MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), -one(T)], [x, y]),
                                     zero(T)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)

    fx = convert(MOI.ScalarAffineFunction{T},
                 MOI.SingleVariable(x))
    c1 = MOI.add_constraint(model, fx, MOI.GreaterThan(zero(T)))
    fy = convert(MOI.ScalarAffineFunction{T},
                 MOI.SingleVariable(y))
    c2 = MOI.add_constraint(model, fy, MOI.LessThan(zero(T)))

    if config.query
        @test MOI.get(model, MOI.ConstraintFunction(), c1) ≈ fx
        @test MOI.get(model, MOI.ConstraintSet(), c1) == MOI.GreaterThan(zero(T))
        @test MOI.get(model, MOI.ConstraintFunction(), c2) ≈ fy
        @test MOI.get(model, MOI.ConstraintSet(), c2) == MOI.LessThan(zero(T))
    end

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED

        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ zero(T) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ zero(T) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ zero(T) atol=atol rtol=rtol
    end

    # Min  x - y
    # s.t. T(100) <= x
    #               y <= zero(T)
    MOI.set(model, MOI.ConstraintSet(), c1, MOI.GreaterThan(T(100)))

    if config.query
        @test MOI.get(model, MOI.ConstraintFunction(), c1) ≈ fx
        @test MOI.get(model, MOI.ConstraintSet(), c1) == MOI.GreaterThan(T(100))
        @test MOI.get(model, MOI.ConstraintFunction(), c2) ≈ fy
        @test MOI.get(model, MOI.ConstraintSet(), c2) == MOI.LessThan(zero(T))
    end

    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(100) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ T(100) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ zero(T) atol=atol rtol=rtol
    end

    # Min  x - y
    # s.t. T(100) <= x
    #               y <= -T(100)
    MOI.set(model, MOI.ConstraintSet(), c2, MOI.LessThan(-T(100)))

    if config.query
        @test MOI.get(model, MOI.ConstraintFunction(), c1) ≈ fx
        @test MOI.get(model, MOI.ConstraintSet(), c1) == MOI.GreaterThan(T(100))
        @test MOI.get(model, MOI.ConstraintFunction(), c2) ≈ fy
        @test MOI.get(model, MOI.ConstraintSet(), c2) == MOI.LessThan(-T(100))
    end

    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(200) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ T(100) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ -T(100) atol=atol rtol=rtol
    end
end

# Modify constants in Nonnegatives and Nonpositives
function linear7test(model::MOI.ModelLike, config::TestConfig{T}) where T
    atol = config.atol
    rtol = config.rtol

    # Min  x - y
    # s.t. bx <= x          (c1)
    #             y <= by   (c2)
    #
    # or, in more detail,
    #
    # Min    1 x - 1 y
    # s.t. - 1 x       <= - bx  (z)   (c1)
    #              1 y <=   by  (w)   (c2)
    #
    # with generic dual
    #
    # Max  - bx z + by w
    # s.t. -    z        == - 1     (c1)
    #                  w ==   1     (c2)
    # i.e. z == w == 1

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{T}, MOI.Nonnegatives)
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{T}, MOI.Nonpositives)

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)

    # Min  x - y
    # s.t. zero(T) <= x          (c1)
    #             y <= zero(T)   (c2)

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), -one(T)], [x, y]), zero(T)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)

    c1 = MOI.add_constraint(model, MOI.VectorAffineFunction{T}([MOI.VectorAffineTerm{T}(1, MOI.ScalarAffineTerm{T}(one(T), x))], [zero(T)]), MOI.Nonnegatives(1))
    c2 = MOI.add_constraint(model, MOI.VectorAffineFunction{T}([MOI.VectorAffineTerm{T}(1, MOI.ScalarAffineTerm{T}(one(T), y))], [zero(T)]), MOI.Nonpositives(1))

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED

        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ zero(T) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ zero(T) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ zero(T) atol=atol rtol=rtol
    end

    # Min  x - y
    # s.t. T(100) <= x
    #               y <= zero(T)

    if config.modify_lhs
        MOI.modify(model, c1, MOI.VectorConstantChange([-T(100)]))
    else
        MOI.delete(model, c1)
        c1 = MOI.add_constraint(model, MOI.VectorAffineFunction{T}([MOI.VectorAffineTerm{T}(1, MOI.ScalarAffineTerm{T}(one(T), x))], [-T(100)]), MOI.Nonnegatives(1))
    end

    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(100) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ T(100) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ zero(T) atol=atol rtol=rtol
    end

    # Min  x - y
    # s.t. T(100) <= x
    #               y <= -T(100)

    if config.modify_lhs
        MOI.modify(model, c2, MOI.VectorConstantChange([T(100)]))
    else
        MOI.delete(model, c2)
        c2 = MOI.add_constraint(model, MOI.VectorAffineFunction{T}([MOI.VectorAffineTerm{T}(1, MOI.ScalarAffineTerm{T}(one(T), y))], [T(100)]), MOI.Nonpositives(1))
    end

    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(200) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ T(100) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ -T(100) atol=atol rtol=rtol
    end
end

# infeasible problem
function linear8atest(model::MOI.ModelLike, config::TestConfig{T}) where T
    atol = config.atol
    rtol = config.rtol
    # min x
    # s.t. 2x+y <= -1
    # x,y >= 0

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.LessThan{T})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{T})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    c = MOI.add_constraint(model, MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([T(2), one(T)], [x, y]), zero(T)), MOI.LessThan(-one(T)))
    bndx = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(zero(T)))
    @test bndx.value == x.value
    bndy = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.GreaterThan(zero(T)))
    @test bndy.value == y.value

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm{T}(one(T), x)], zero(T)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED

        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE ||
            MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE_OR_UNBOUNDED
        if config.duals && config.infeas_certificates
            # solver returned an infeasibility ray
            @test MOI.get(model, MOI.ResultCount()) >= 1

            @test MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
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
        end
    end
end

# unbounded problem
function linear8btest(model::MOI.ModelLike, config::TestConfig{T}) where T
    atol = config.atol
    rtol = config.rtol
    # min -x-y
    # s.t. -x+2y <= 0
    # x,y >= 0

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.LessThan{T})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{T})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    MOI.add_constraint(model, MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([-one(T), T(2)], [x, y]), zero(T)), MOI.LessThan(zero(T)))
    vc1 = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(zero(T)))
    @test vc1.value == x.value
    vc2 = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.GreaterThan(zero(T)))
    @test vc2.value == y.value

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([-one(T), -one(T)], [x, y]), zero(T)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED

        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.DUAL_INFEASIBLE ||
            MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE_OR_UNBOUNDED
        if config.infeas_certificates
            # solver returned an unbounded ray
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.INFEASIBILITY_CERTIFICATE
        else
            # solver returned nothing
            @test MOI.get(model, MOI.ResultCount()) == 0
        end
    end
end

# unbounded problem with unique ray
function linear8ctest(model::MOI.ModelLike, config::TestConfig{T}) where T
    atol = config.atol
    rtol = config.rtol
    # min -x-y
    # s.t. x-y == 0
    # x,y >= 0

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.EqualTo{T})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{T})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    MOI.add_constraint(model, MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), -one(T)], [x, y]), zero(T)), MOI.EqualTo(zero(T)))
    vc1 = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(zero(T)))
    @test vc1.value == x.value
    vc2 = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.GreaterThan(zero(T)))
    @test vc2.value == y.value

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([-one(T), -one(T)], [x, y]), zero(T)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED

        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.DUAL_INFEASIBLE ||
            MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE_OR_UNBOUNDED
        if config.infeas_certificates
            # solver returned an unbounded ray
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.INFEASIBILITY_CERTIFICATE
            ray = MOI.get(model, MOI.VariablePrimal(), [x, y])
            @test ray[1] ≈ ray[2] atol=atol rtol=rtol
        else
            # solver returned nothing
            @test MOI.get(model, MOI.ResultCount()) == 0
        end
    end
end

# add_constraints
function linear9test(model::MOI.ModelLike, config::TestConfig{T}) where T
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
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.LessThan{T})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{T})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)

    vc12 = MOI.add_constraints(model,
        [MOI.SingleVariable(x), MOI.SingleVariable(y)],
        [MOI.GreaterThan(T(30)), MOI.GreaterThan(zero(T))]
    )
    @test vc12[1].value == x.value
    @test vc12[2].value == y.value

    c1 = MOI.add_constraints(model,
        [MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), -T(3//2)], [x, y]), zero(T))],
        [MOI.GreaterThan(zero(T))]
    )

    c23 = MOI.add_constraints(model,
        [
            MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([T(12), T(8)], [x, y]), zero(T)),
            MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([T(1_000), T(300)], [x, y]), zero(T))
        ],
        [
            MOI.LessThan(T(1_000)),
            MOI.LessThan(T(70_000))
        ]
    )

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
                      MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([T(1_000), T(350)], [x, y]), zero(T)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED

        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 79e4/11 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 650/11 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 400/11 atol=atol rtol=rtol

        if config.basis
            @test MOI.get(model, MOI.ConstraintBasisStatus(), vc12[1]) == MOI.BASIC
            @test MOI.get(model, MOI.ConstraintBasisStatus(), vc12[2]) == MOI.BASIC
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c1[1]) == MOI.BASIC
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c23[1]) == MOI.NONBASIC
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c23[2]) == MOI.NONBASIC
        end
    end
end

# ranged constraints
function linear10test(model::MOI.ModelLike, config::TestConfig{T}) where T
    atol = config.atol
    rtol = config.rtol
    #   maximize x + y
    #
    #       s.t.  5 <= x + y <= 10
    #                  x,  y >= 0

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.Interval{T})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{T})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)

    vc = MOI.add_constraints(model,
        [MOI.SingleVariable(x), MOI.SingleVariable(y)],
        [MOI.GreaterThan(zero(T)), MOI.GreaterThan(zero(T))]
    )
    @test vc[1].value == x.value
    @test vc[2].value == y.value

    c = MOI.add_constraint(model, MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), one(T)], [x, y]), zero(T)), MOI.Interval(T(5), T(10)))

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), one(T)], [x, y]), zero(T)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED

        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(10) atol=atol rtol=rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 10 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ T(10) atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol
        end

        if config.basis
            # There are multiple optimal bases. Either x or y can be in the optimal basis.
            @test (MOI.get(model, MOI.ConstraintBasisStatus(), vc[1]) == MOI.BASIC ||
                   MOI.get(model, MOI.ConstraintBasisStatus(), vc[2])== MOI.BASIC)
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c) == MOI.NONBASIC_AT_UPPER
        end
    end

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), one(T)], [x, y]), zero(T)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(5) atol=atol rtol=rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 5 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ T(5) atol=atol rtol=rtol
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ 1 atol=atol rtol=rtol
        end

        if config.basis
            # There are multiple optimal bases. Either x or y can be in the optimal basis."
            @test (MOI.get(model, MOI.ConstraintBasisStatus(), vc[1]) == MOI.BASIC ||
                   MOI.get(model, MOI.ConstraintBasisStatus(), vc[2])== MOI.BASIC)
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c) == MOI.NONBASIC_AT_LOWER
        end
    end

    MOI.set(model, MOI.ConstraintSet(), c, MOI.Interval(T(2), T(12)))

    if config.query
        @test MOI.get(model, MOI.ConstraintSet(), c) == MOI.Interval(T(2), T(12))
    end

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(2) atol=atol rtol=rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 2 atol=atol rtol=rtol

        if config.basis
            # There are multiple optimal bases. Either x or y can be in the optimal basis.
            @test (MOI.get(model, MOI.ConstraintBasisStatus(), vc[1]) == MOI.BASIC ||
                   MOI.get(model, MOI.ConstraintBasisStatus(), vc[2])== MOI.BASIC)
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c) == MOI.NONBASIC_AT_LOWER
        end

        if config.duals
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ T(2) atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ 1 atol=atol rtol=rtol
        end
    end

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), one(T)], [x, y]), zero(T)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(12) atol=atol rtol=rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 12 atol=atol rtol=rtol

        if config.basis
            # There are multiple optimal bases. Either x or y can be in the optimal basis.
            @test (MOI.get(model, MOI.ConstraintBasisStatus(), vc[1]) == MOI.BASIC ||
                   MOI.get(model, MOI.ConstraintBasisStatus(), vc[2])== MOI.BASIC)
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c) == MOI.NONBASIC_AT_UPPER
        end
    end
end

# inactive ranged constraints
function linear10btest(model::MOI.ModelLike, config::TestConfig{T}) where T
    atol = config.atol
    rtol = config.rtol
    #   minimize x + y
    #
    #       s.t.  -1 <= x + y <= 10
    #                   x,  y >= 0

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.Interval{T})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{T})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)

    vc = MOI.add_constraints(model,
        [MOI.SingleVariable(x), MOI.SingleVariable(y)],
        [MOI.GreaterThan(zero(T)), MOI.GreaterThan(zero(T))]
    )
    @test vc[1].value == x.value
    @test vc[2].value == y.value

    c = MOI.add_constraint(model, MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), one(T)], [x, y]), zero(T)), MOI.Interval(-one(T), T(10)))

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), one(T)], [x, y]), zero(T)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED

        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ zero(T) atol=atol rtol=rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ zero(T) atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ zero(T) atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc[1]) ≈ one(T) atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), vc[2]) ≈ one(T) atol=atol rtol=rtol
        end

        if config.basis
            @test (MOI.get(model, MOI.ConstraintBasisStatus(), vc[1]) == MOI.NONBASIC)
            @test (MOI.get(model, MOI.ConstraintBasisStatus(), vc[1]) == MOI.NONBASIC)
            @test MOI.get(model, MOI.ConstraintBasisStatus(), c) == MOI.BASIC
        end
    end
end

# changing constraint sense
function linear11test(model::MOI.ModelLike, config::TestConfig{T}) where T
    atol = config.atol
    rtol = config.rtol
    # simple 2 variable, 1 constraint problem
    #
    # starts with
    #
    # min x + y
    # st   x + y >= 1
    #      x + y >= 2
    # sol: x+y = 2 (degenerate)
    #
    # with dual
    #
    # max  w + 2z
    # st   w +  z == 1
    #      w +  z == 1
    #      w, z >= 0
    # sol: z = 1, w = 0
    #
    # tranforms problem into:
    #
    # min x + y
    # st   x + y >= 1
    #      x + y <= 2
    # sol: x+y = 1 (degenerate)
    #
    # with dual
    #
    # max  w + 2z
    # st   w +  z == 1
    #      w +  z == 1
    #      w >= 0, z <= 0
    # sol: w = 1, z = 0

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.LessThan{T})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    v = MOI.add_variables(model, 2)

    c1 = MOI.add_constraint(model, MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), one(T)], v), zero(T)), MOI.GreaterThan(one(T)))
    c2 = MOI.add_constraint(model, MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), one(T)], v), zero(T)), MOI.GreaterThan(T(2)))

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), one(T)], v), zero(T)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED

        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(2) atol=atol rtol=rtol
    end

    c3 = MOI.transform(model, c2, MOI.LessThan(T(2)))

    @test isa(c3, MOI.ConstraintIndex{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}})
    @test MOI.is_valid(model, c2) == false
    @test MOI.is_valid(model, c3) == true

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ one(T) atol=atol rtol=rtol
    end
end

# infeasible problem with 2 linear constraints
function linear12test(model::MOI.ModelLike, config::TestConfig{T}) where T
    atol = config.atol
    rtol = config.rtol
    # min x
    # s.t. 2x-3y <= -7
    #      y <= 2
    # x,y >= 0

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.LessThan{T})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{T})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    c1 = MOI.add_constraint(model, MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([T(2), -T(3)], [x, y]), zero(T)), MOI.LessThan(-T(7)))
    c2 = MOI.add_constraint(model, MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm{T}(one(T), y)], zero(T)), MOI.LessThan(T(2)))
    bndx = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(zero(T)))
    @test bndx.value == x.value
    bndy = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.GreaterThan(zero(T)))
    @test bndy.value == y.value

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm{T}(one(T), x)], zero(T)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED

        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE ||
            MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE_OR_UNBOUNDED
        if config.duals && config.infeas_certificates
            # solver returned an infeasibility ray
            @test MOI.get(model, MOI.ResultCount()) >= 1
            @test MOI.get(model, MOI.DualStatus()) == MOI.INFEASIBILITY_CERTIFICATE
            cd1 = MOI.get(model, MOI.ConstraintDual(), c1)
            cd2 = MOI.get(model, MOI.ConstraintDual(), c2)
            bndxd = MOI.get(model, MOI.ConstraintDual(), bndx)
            bndyd = MOI.get(model, MOI.ConstraintDual(), bndy)
            @test cd1 < - atol
            @test cd2 < - atol
            @test - 3 * cd1 + cd2 ≈ -bndyd atol=atol rtol=rtol
            @test 2 * cd1 ≈ -bndxd atol=atol rtol=rtol
            @test -7 * cd1 + 2 * cd2 > atol
        end
    end
end

# feasibility problem
function linear13test(model::MOI.ModelLike, config::TestConfig{T}) where T
    atol = config.atol
    rtol = config.rtol
    # find x, y
    # s.t. 2x + 3y >= 1
    #      x - y == 0

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.EqualTo{T})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    c1 = MOI.add_constraint(model, MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([T(2), T(3)], [x, y]), zero(T)), MOI.GreaterThan(one(T)))
    c2 = MOI.add_constraint(model, MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), -one(T)], [x, y]), zero(T)), MOI.EqualTo(zero(T)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.FEASIBILITY_SENSE

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED

        MOI.optimize!(model)
        @test MOI.get(model, MOI.ResultCount()) > 0

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        xsol = MOI.get(model, MOI.VariablePrimal(), x)
        ysol = MOI.get(model, MOI.VariablePrimal(), y)

        c1sol = 2 * xsol + 3 * ysol
        @test c1sol >= 1 || isapprox(c1sol, one(T), atol=atol, rtol=rtol)
        @test xsol - ysol ≈ 0 atol=atol rtol=rtol

        c1primval = MOI.get(model, MOI.ConstraintPrimal(), c1)
        @test c1primval >= 1 || isapprox(c1sol, one(T), atol=atol, rtol=rtol)

        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ 0 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ 0 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ 0 atol=atol rtol=rtol
        end
    end
end

# Deletion of vector of variables
function linear14test(model::MOI.ModelLike, config::TestConfig{T}) where T
    atol = config.atol
    rtol = config.rtol
    # max x + 2y + 3z + 4
    # s.t. 3x + 2y + z <= 2
    #      x, y, z >= 0
    #      z <= 1

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{T}, MOI.LessThan{T})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{T})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.LessThan{T})

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x, y, z = MOI.add_variables(model, 3)
    c = MOI.add_constraint(model, MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([T(3), T(2), one(T)], [x, y, z]), zero(T)), MOI.LessThan(T(2)))
    clbx = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(zero(T)))
    @test clbx.value == x.value
    clby = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.GreaterThan(zero(T)))
    @test clby.value == y.value
    clbz = MOI.add_constraint(model, MOI.SingleVariable(z), MOI.GreaterThan(zero(T)))
    @test clbz.value == z.value
    cubz = MOI.add_constraint(model, MOI.SingleVariable(z), MOI.LessThan(one(T)))
    @test cubz.value == z.value

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(), MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([one(T), T(2), T(3)], [x, y, z]), T(4)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED

        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

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
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ 8 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol

            # reduced costs
            @test MOI.get(model, MOI.ConstraintDual(), clbx) ≈ 2 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), clby) ≈ 0 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), clbz) ≈ 0 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), cubz) ≈ -2 atol=atol rtol=rtol

            if config.basis
                @test MOI.get(model, MOI.ConstraintBasisStatus(), clbx) == MOI.NONBASIC
                @test MOI.get(model, MOI.ConstraintBasisStatus(), clby) == MOI.BASIC
                @test MOI.get(model, MOI.ConstraintBasisStatus(), clbz) == MOI.BASIC
                @test MOI.get(model, MOI.ConstraintBasisStatus(), cubz) == MOI.NONBASIC
                @test MOI.get(model, MOI.ConstraintBasisStatus(), c) == MOI.NONBASIC
            end
        end
    end

    MOI.delete(model, [x, z])

    if config.solve
        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 6 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 1 atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 2 atol=atol rtol=rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), clby) ≈ 1 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ 6 atol=atol rtol=rtol
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -1 atol=atol rtol=rtol

            # reduced costs
            @test MOI.get(model, MOI.ConstraintDual(), clby) ≈ 0 atol=atol rtol=rtol
        end
    end
end

# Empty vector affine function rows (LQOI Issue #48)
function linear15test(model::MOI.ModelLike, config::TestConfig{T}) where T
    atol = config.atol
    rtol = config.rtol
    # minimize 0
    # s.t. 0 == 0
    #      x == 1
    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{T}, MOI.Zeros)

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variables(model, 1)
    # Create a VectorAffineFunction with two rows, but only
    # one term, belonging to the second row. The first row,
    # which is empty, is essentially a constraint that 0 == 0.
    c = MOI.add_constraint(model,
        MOI.VectorAffineFunction{T}(
            MOI.VectorAffineTerm{T}.(2, MOI.ScalarAffineTerm{T}.([one(T)], x)),
            zeros(2)
        ),
        MOI.Zeros(2)
    )

    MOI.set(model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
        MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([zero(T)], x), zero(T)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED

        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 0 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), x[1]) ≈ 0 atol=atol rtol=rtol

        if config.duals
            @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
            @test MOI.get(model, MOI.DualObjectiveValue()) ≈ 0 atol=atol rtol=rtol
        end
    end
end

# This test can be passed by solvers that don't support VariablePrimalStart
# because copy_to drops start information with a warning.
function partial_start_test(model::MOI.ModelLike, config::TestConfig{T}) where T
    atol = config.atol
    rtol = config.rtol
    # maximize 2x + y
    # s.t. x + y <= 1
    #      x, y >= 0
    #      x starts at one(T). Start point for y is unspecified.
    MOI.empty!(model)
    @test MOI.is_empty(model)

    x = MOI.add_variable(model)
    y = MOI.add_variable(model)

    MOI.set(model, MOI.VariablePrimalStart(), x, one(T))

    MOI.add_constraint(model, x, MOI.GreaterThan(zero(T)))
    MOI.add_constraint(model, y, MOI.GreaterThan(zero(T)))
    obj = MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.([T(2), one(T)], [x, y]),
                                   zero(T))
    MOI.set(model, MOI.ObjectiveFunction{typeof(obj)}(), obj)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    x_plus_y = MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}.(one(T), [x, y]), zero(T))
    MOI.add_constraint(model, x_plus_y, MOI.LessThan(one(T)))

    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ T(2) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ one(T) atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ zero(T) atol=atol rtol=rtol
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
                             "linear10b" => linear10btest,
                             "linear11" => linear11test,
                             "linear12" => linear12test,
                             "linear13" => linear13test,
                             "linear14" => linear14test,
                             "linear15" => linear15test,
                             "partial_start" => partial_start_test)

@moitestset contlinear
