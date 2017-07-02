# Continuous linear problems

function contlineartest(solver::AbstractSolver, ε=Base.rtoldefault(Float64))
    @testset "Model 1" begin
        # simple 2 variable, 1 constraint problem
        # min -x
        # st   x + y <= 1   (x + y - 1 ∈ NonPositive)
        #       x, y >= 0   (x, y ∈ NonNegative)

        m = Model(solver)

        v = addvariables!(m, 2)
        @test getattribute(m, VariableCount()) == 2

        @test getattribute(m, SupportsAffineConstraint{NonPositive}())
        c = addconstraint!(m, -1, v, [1, 1], NonPositive(1))
        @test getattribute(m, ConstraintCount()) == 1

        @test getattribute(m, SupportsVariablewiseConstraint{GreaterThan}())
        vc1 = addconstraint!(m, v[1], GreaterThan(0))
        vc2 = addconstraint!(m, v[2], GreaterThan(0))
        @test getattribute(m, ConstraintCount()) == 3

        setattribute!(m, Sense(), MinSense)
        setobjective!(m, 0, v, [-1, 0])
        # TODO query objective
        # (b, a_varref, a_coef, qi, qj, qc) = getobjective(m)
        # @test b ≈ 0 atol=ε
        # @test a_varref == v
        # @test a_coef ≈ [-1, 0] atol=ε

        optimize!(m)

        @test cangetattribute(m, TerminationStatus())
        @test getattribute(m, TerminationStatus()) == Success

        @test cangetattribute(m, PrimalStatus())
        @test getattribute(m, PrimalStatus()) == FeasiblePoint
        @test cangetattribute(m, DualStatus())
        @test getattribute(m, DualStatus()) == FeasiblePoint

        @test cangetattribute(m, ObjectiveValue())
        @test getattribute(m, ObjectiveValue()) ≈ -1 atol=ε

        @test cangetattribute(m, VariablePrimal(), v)
        @test getattribute(m, VariablePrimal(), v) ≈ [1, 0] atol=ε
        # TODO var dual?
        
        @test cangetattribute(m, ConstraintPrimal(), c)
        @test getattribute(m, ConstraintPrimal(), c) ≈ [1] atol=ε
        @test cangetattribute(m, ConstraintDual(), c)
        @test getattribute(m, ConstraintDual(), c) ≈ [-1] atol=ε

        # change objective to Max +x

        @test cansetattribute(m, Sense())
        setattribute!(m, Sense(), MaxSense)
        modifyobjective!(m, 1, v[1], 1)

        optimize!(m)

        @test cangetattribute(m, TerminationStatus())
        @test getattribute(m, TerminationStatus()) == Success

        @test cangetattribute(m, PrimalStatus())
        @test getattribute(m, PrimalStatus()) == FeasiblePoint
        @test cangetattribute(m, DualStatus())
        @test getattribute(m, DualStatus()) == FeasiblePoint

        @test cangetattribute(m, ObjectiveValue())
        @test getattribute(m, ObjectiveValue()) ≈ 1 atol=ε

        @test cangetattribute(m, VariablePrimal(), v)
        @test getattribute(m, VariablePrimal(), v) ≈ [1, 0] atol=ε

        @test cangetattribute(m, ConstraintDual(), c)
        @test getattribute(m, ConstraintDual(), c) ≈ [1] atol=ε
    end

    # TODO more test sets here
end
