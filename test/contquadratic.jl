# Continuous quadratic problems

function contquadratictest(solver::MOI.AbstractSolver, ε=Base.rtoldefault(Float64))
    @testset "Testing quadratic interface with $solver" begin
        @testset "QP1 - Quadratic objective" begin
            # simple quadratic objective
            # Min x^2 + 0.5xy + y^2 + 0.5yz + z^2
            # st  x + 2y + 3z >= 4 (c1)
            #     x +  y      >= 1 (c2)
            #     x,y \in R

            @test MOI.supportsproblem(solver, MOI.ScalarQuadraticFunction, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan)])

            m = MOI.SolverInstance(solver)

            v = MOI.addvariables!(m, 3)
            @test MOI.getattribute(m, MOI.VariableCount()) == 3

            c1 = MOI.addconstraint!(m, MOI.ScalarAffineFunction(v, [1.0,2.0,3.0], 0.0), MOI.GreaterThan(4.0))
            @test MOI.getattribute(m, MOI.ConstraintCount()) == 1

            c2 = MOI.addconstraint!(m, MOI.ScalarAffineFunction([v[1],v[2]], [1.0,1.0], 0.0), MOI.GreaterThan(1.0))
            @test MOI.getattribute(m, MOI.ConstraintCount()) == 2

            MOI.setobjective!(m, MOI.MinSense, MOI.ScalarQuadraticFunction(v, [0.0,0.0,0.0],[v[1], v[1], v[2], v[2], v[3]], [v[1], v[2], v[2], v[3], v[3]], [2.0, 1.0, 2.0, 1.0, 2.0], 0.0))
            @test MOI.getattribute(m, MOI.Sense()) == MOI.MinSense

            if MOI.cangetattribute(m, MOI.ObjectiveFunction())
                obj = MPI.getattribute(m, MOI.ObjectiveFunction())
                @test obj.affine_variables == v
                @test obj.affine_coefficients == [0.0,0.0,0.0]
                @test obj.quadratic_rowvariables == [v[1], v[1], v[2], v[2], v[3]]
                @test obj.quadratic_colvariables == [v[1], v[2], v[2], v[3], v[3]]
                @test obj.quadratic_coefficients == [2.0, 1.0, 2.0, 1.0, 2.0]
                @test obj.constant == 0.0
            end

            if MOI.cangetattribute(m, MOI.ConstraintFunction(), c1)
                aff = MOI.getattribute(m, MOI.ConstraintFunction(), c1)
                @test aff.variables == v
                @test aff.coeffcients == [1.0,2.0,3.0]
                @test aff.constant == 0.0
            end

            if MOI.cangetattribute(m, MOI.ConstraintSet(), c1)
                s = MOI.getattribute(m, MOI.ConstraintSet(), c1)
                @test s == MOI.GreaterThan(4.0)
            end

            MOI.optimize!(m)

            @test MOI.cangetattribute(m, MOI.TerminationStatus())
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

            @test MOI.cangetattribute(m, MOI.PrimalStatus())
            @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

            @test MOI.cangetattribute(m, MOI.ObjectiveValue())
            @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 130/70 atol=ε

            @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
            @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [0.5714285714285715,0.4285714285714285,0.8571428571428572] atol=ε
        end
    
        @testset "QP1" begin
            # same as QP0 but with duplicate terms
            # then change the objective and sense
            # simple quadratic objective
            # Min x^2 + xy + y^2 + yz + z^2
            # st  x + 2y + 3z >= 4 (c1)
            #     x +  y      >= 1 (c2)
            #     x,y \in R

            @test MOI.supportsproblem(solver, MOI.ScalarQuadraticFunction, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan)])

            m = MOI.SolverInstance(solver)

            v = MOI.addvariables!(m, 3)
            @test MOI.getattribute(m, MOI.VariableCount()) == 3

            c1 = MOI.addconstraint!(m, MOI.ScalarAffineFunction(v, [1.0,2.0,3.0], 0.0), MOI.GreaterThan(4.0))
            @test MOI.getattribute(m, MOI.ConstraintCount()) == 1

            c2 = MOI.addconstraint!(m, MOI.ScalarAffineFunction([v[1],v[2]], [1.0,1.0], 0.0), MOI.GreaterThan(1.0))
            @test MOI.getattribute(m, MOI.ConstraintCount()) == 2

            MOI.setobjective!(m, MOI.MinSense, MOI.ScalarQuadraticFunction(v, [0.0,0.0,0.0],[v[1], v[1], v[1], v[2], v[2], v[3], v[3]], [v[1], v[2], v[2], v[2], v[3], v[3], v[3]], [2.0, 0.5, 0.5, 2.0, 1.0, 1.0, 1.0], 0.0))
            @test MOI.getattribute(m, MOI.Sense()) == MOI.MinSense

            if MOI.cangetattribute(m, MOI.ObjectiveFunction())
                obj = MPI.getattribute(m, MOI.ObjectiveFunction())
                @test obj.affine_variables == v
                @test obj.affine_coefficients == [0.0,0.0,0.0]
                @test obj.quadratic_rowvariables == [v[1], v[1], v[2], v[2], v[3]]
                @test obj.quadratic_colvariables == [v[1], v[2], v[2], v[3], v[3]]
                @test obj.quadratic_coefficients == [2.0, 2.0, 2.0, 2.0, 2.0]
                @test obj.constant == 0.0
            end

            if MOI.cangetattribute(m, MOI.ConstraintFunction(), c1)
                aff = MOI.getattribute(m, MOI.ConstraintFunction(), c1)
                @test aff.variables == v
                @test aff.coeffcients == [1.0,2.0,3.0]
                @test aff.constant == 0.0
            end

            if MOI.cangetattribute(m, MOI.ConstraintSet(), c1)
                s = MOI.getattribute(m, MOI.ConstraintSet(), c1)
                @test s == MOI.GreaterThan(4.0)
            end

            MOI.optimize!(m)

            @test MOI.cangetattribute(m, MOI.TerminationStatus())
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

            @test MOI.cangetattribute(m, MOI.PrimalStatus())
            @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

            @test MOI.cangetattribute(m, MOI.ObjectiveValue())
            @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 130/70 atol=ε

            @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
            @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [0.5714285714285715,0.4285714285714285,0.8571428571428572] atol=ε

            # change objective to Max -2(x^2 + xy + y^2 + yz + z^2)

            MOI.setobjective!(m, MOI.MaxSense, MOI.ScalarQuadraticFunction(v, [0.0,0.0,0.0],[v[1], v[1], v[1], v[2], v[2], v[3], v[3]], [v[1], v[2], v[2], v[2], v[3], v[3], v[3]], [-4.0, -1.0, -1.0, -4.0, -2.0, -2.0, -2.0], 0.0))
            @test MOI.getattribute(m, MOI.Sense()) == MOI.MaxSense

            if MOI.cangetattribute(m, MOI.ObjectiveFunction())
                obj = MPI.getattribute(m, MOI.ObjectiveFunction())
                @test obj.affine_variables == v
                @test obj.affine_coefficients == [0.0,0.0,0.0]
                @test obj.quadratic_rowvariables == [v[1], v[1], v[2], v[2], v[3]]
                @test obj.quadratic_colvariables == [v[1], v[2], v[2], v[3], v[3]]
                @test obj.quadratic_coefficients == -2*[2.0, 2.0, 2.0, 2.0, 2.0]
                @test obj.constant == 0.0
            end

            MOI.optimize!(m)

            @test MOI.cangetattribute(m, MOI.TerminationStatus())
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

            @test MOI.cangetattribute(m, MOI.PrimalStatus())
            @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

            @test MOI.cangetattribute(m, MOI.ObjectiveValue())
            @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 2*130/70 atol=ε

            @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
            @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [0.5714285714285715,0.4285714285714285,0.8571428571428572] atol=ε
        end
    
        @testset "QP2" begin
            # quadratic constraint
            # Min x + y
            # st  - x + y >= 0 (c1[1])
            #       x + y >= 0 (c1[2])
            #     0.5x^2 + y <= 2 (c2)

            @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction, [(MOI.VectorAffineFunction{Float64},MOI.NonPositive),(MOI.ScalarQuadraticFunction{Float64},MOI.LessThan)])

            m = MOI.SolverInstance(solver)

            x = MOI.addvariable!(m)
            y = MOI.addvariable!(m)
            @test MOI.getattribute(m, MOI.VariableCount()) == 2

            c1 = MOI.addconstraint!(m, MOI.VectorAffineFunction([1,2], [x,y,x,y],[-1.0,1.0,1.0,1.0], [0.0,0.0]), MOI.NonPositive(2))
            @test MOI.getattribute(m, MOI.ConstraintCount()) == 1

            c2 = MOI.addconstraint!(m, MOI.ScalarQuadraticFunction([y],[1.0],[x],[x],[1.0], 0.0), MOI.LessThan(2.0))
            @test MOI.getattribute(m, MOI.ConstraintCount()) == 2

            MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([x,y], [1.0,1.0], 0.0))
            @test MOI.getattribute(m, MOI.Sense()) == MOI.MinSense

            if MOI.cangetattribute(m, MOI.ConstraintFunction(), c2)
                aff = MOI.getattribute(m, MOI.ConstraintFunction(), c2)
                @test obj.affine_variables == [y]
                @test obj.affine_coefficients == [1.0]
                @test obj.quadratic_rowvariables == [x]
                @test obj.quadratic_colvariables == [x]
                @test obj.quadratic_coefficients == [1.0]
                @test obj.constant == 0.0
            end

            MOI.optimize!(m)

            @test MOI.cangetattribute(m, MOI.TerminationStatus())
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

            @test MOI.cangetattribute(m, MOI.PrimalStatus())
            @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

            @test MOI.cangetattribute(m, MOI.ObjectiveValue())
            @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 2.25 atol=ε

            @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
            @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [0.5,1.75] atol=ε
        end
    end
    @testset "Testing QP duals with $solver" begin
        @testset "QP1" begin
            # Max x
            # s.t. x^2 <= 2
        end

        @testset "QP1" begin
            # Min -x
            # s.t. x^2 <= 2
        end
    end
    @testset "Testing SOCP interface with $solver" begin
    end
    # TODO more test sets here
end
