# Continuous linear problems

function contlineartest(solver::AbstractSolver, eps=Base.rtoldefault(Float64))
    @testset "Set up model, variables, constraint, objective" begin
        # simple 2 variable, 1 constraint problem
        # min -x
        # st   x + y <= 1   (x + y - 1 ∈ NonPositive)
        #       x, y >= 0   (x, y ∈ NonNegative)

        m = Model(solver)

        v = addvariables!(m, 2)
        @test getattribute(m, VariableCount()) == 2

        @test cansetattribute(m, VariableLowerBound())
        setattribute!(m, VariableLowerBound(), v, [0, 0])
        @test cangetattribute(m, VariableLowerBound())
        @test isapprox(getattribute(m, VariableLowerBound(), v), [0.0, 0.0], atol=eps)
        @test cangetattribute(m, VariableUpperBound())
        @test all(getattribute(m, VariableUpperBound(), v) .>= 1e20)

        @test getattribute(m, SupportsAffineConstraint{NonPositive}())
        c = addconstraint!(m, -1, v, [1, 1], NonPositive(1))
        @test getattribute(m, ConstraintCount()) == 1

        setattribute!(m, Sense(), MinSense)
        setobjective!(m, 0.0, v, [-1.0, 0.0])

        # TODO query objective
        # (b, a_varref, a_coef, qi, qj, qc) = getobjective(m)
        # @test isapprox(b, 0.0, atol=eps)
        # @test a_varref == v
        # @test isapprox(a_coef, [-1.0, 0.0], atol=eps)
    end

    @testset "Solve model, check results" begin
        optimize!(m)

        @test cangetattribute(m, TerminationStatus())
        @test getattribute(m, TerminationStatus()) == Success

        @test cangetattribute(m, PrimalStatus())
        @test getattribute(m, PrimalStatus()) == FeasiblePoint
        @test cangetattribute(m, DualStatus())
        @test getattribute(m, DualStatus()) == FeasiblePoint

        @test cangetattribute(m, ObjectiveValue())
        @test isapprox(getattribute(m, ObjectiveValue()), -1, atol=eps)

        @test cangetattribute(m, VariablePrimal(), v)
        @test isapprox(getattribute(m, VariablePrimal(), v), [1, 0], atol=eps)
        # TODO variable duals?
        # @test cangetattribute(m, VariableDual(), v)
        # @test isapprox(getattribute(m, VariableDual(), v), [1, 0], atol=eps)

        @test cangetattribute(m, ConstraintPrimal(), c)
        @test isapprox(getattribute(m, ConstraintPrimal(), c), [1], atol=eps)
        @test cangetattribute(m, ConstraintDual(), c)
        @test isapprox(getattribute(m, ConstraintDual(), c), [-1], atol=eps)
    end

    @testset "Modify objective, check results" begin
        # change objective to Max +x

        @test cansetattribute(m, Sense())
        setattribute!(m, Sense(), MaxSense)
        modifyobjective!(m, 1, v[1], 1.0)

        optimize!(m)

        @test cangetattribute(m, TerminationStatus())
        @test getattribute(m, TerminationStatus()) == Success

        @test cangetattribute(m, PrimalStatus())
        @test getattribute(m, PrimalStatus()) == FeasiblePoint
        @test cangetattribute(m, DualStatus())
        @test getattribute(m, DualStatus()) == FeasiblePoint

        @test cangetattribute(m, ObjectiveValue())
        @test isapprox(getattribute(m, ObjectiveValue()), 1, atol=eps)

        @test cangetattribute(m, VariablePrimal(), v)
        @test isapprox(getattribute(m, VariablePrimal(), v), [1, 0], atol=eps)

        # TODO var duals?
        # @test cangetattribute(m, VariableUpperBoundDual(), v)
        # @test isapprox(getattribute(m, VariableUpperBoundDual(), v), [0, -1], atol=eps)
        # @test cangetattribute(m, VariableLowerBoundDual(), v)
        # @test isapprox(getattribute(m, VariableLowerBoundDual(), v), [0, 0], atol=eps)

        @test cangetattribute(m, ConstraintPrimal(), c)
        @test isapprox(getattribute(m, ConstraintPrimal(), c), [1], atol=eps)
        @test cangetattribute(m, ConstraintDual(), c)
        @test isapprox(getattribute(m, ConstraintDual(), c), [1], atol=eps)
    end

    # TODO more test sets here
end
