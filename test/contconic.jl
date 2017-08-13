using MathOptInterface
MOI = MathOptInterface

# Continuous conic problems

function lin1test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    if MOI.supportsproblem(solver,
        MOI.ScalarAffineFunction{Float64},
        [
            (MOI.VectorOfVariables,MOI.Nonnegatives),
            (MOI.VectorAffineFunction{Float64},MOI.Nonnegatives),
            (MOI.VectorAffineFunction{Float64},MOI.Zeros)
        ]
    )
        @testset "LIN1" begin
            # linear conic problem
            # min -3x - 2y - 4z
            # st    x +  y +  z == 3
            #            y +  z == 2
            #       x>=0 y>=0 z>=0
            # Opt obj = -11, soln x = 1, y = 0, z = 2

            m = MOI.SolverInstance(solver)

            v = MOI.addvariables!(m, 3)
            @test MOI.getattribute(m, MOI.NumberOfVariables()) == 3

            vc = MOI.addconstraint!(m, MOI.VectorOfVariables(v), MOI.Nonnegatives(3))
            c = MOI.addconstraint!(m, MOI.VectorAffineFunction([1,1,1,2,2], [v;v[2];v[3]], ones(5), [-3.0,-2.0]), MOI.Zeros(2))
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonnegatives}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 1
            loc = MOI.getattribute(m, MOI.ListOfConstraints())
            @test length(loc) == 2
            @test (MOI.VectorOfVariables,MOI.Nonnegatives) in loc
            @test (MOI.VectorAffineFunction{Float64},MOI.Zeros) in loc

            MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction(v, [-3.0, -2.0, -4.0], 0.0))

            MOI.optimize!(m)

            @test MOI.cangetattribute(m, MOI.TerminationStatus())
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

            @test MOI.cangetattribute(m, MOI.PrimalStatus())
            @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint

            @test MOI.cangetattribute(m, MOI.ObjectiveValue())
            @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ -11 atol=atol rtol=rtol

            @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
            @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [1, 0, 2] atol=atol rtol=rtol

            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
            @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ [-3, -1] atol=atol rtol=rtol

            # TODO var dual and con primal
        end
    end
end

function lin1atest(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    if MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64},
        [
            (MOI.VectorAffineFunction{Float64},MOI.Nonnegatives),
            (MOI.VectorAffineFunction{Float64},MOI.Nonnegatives),
            (MOI.VectorAffineFunction{Float64},MOI.Zeros)
        ])
        @testset "LIN1A" begin
            # Same as LIN1 but variable bounds enforced with VectorAffineFunction

            m = MOI.SolverInstance(solver)

            v = MOI.addvariables!(m, 3)
            @test MOI.getattribute(m, MOI.NumberOfVariables()) == 3

            vc = MOI.addconstraint!(m, MOI.VectorAffineFunction([1,2,3], v, ones(3), zeros(3)), MOI.Nonnegatives(3))
            c = MOI.addconstraint!(m, MOI.VectorAffineFunction([1,1,1,2,2], [v;v[2];v[3]], ones(5), [-3.0,-2.0]), MOI.Zeros(2))
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Nonnegatives}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 1

            MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction(v, [-3.0, -2.0, -4.0], 0.0))

            MOI.optimize!(m)

            @test MOI.cangetattribute(m, MOI.TerminationStatus())
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

            @test MOI.cangetattribute(m, MOI.PrimalStatus())
            @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint

            @test MOI.cangetattribute(m, MOI.ObjectiveValue())
            @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ -11 atol=atol rtol=rtol

            @test MOI.cangetattribute(m, MOI.VariablePrimal(), v)
            @test MOI.getattribute(m, MOI.VariablePrimal(), v) ≈ [1, 0, 2] atol=atol rtol=rtol

            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
            @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ [-3, -1] atol=atol rtol=rtol

            # TODO var dual and con primal
        end
    end
end

function lin1tests(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    lin1test(solver, atol=atol, rtol=rtol)
    lin1atest(solver, atol=atol, rtol=rtol)
end

function lin2test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    if MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64},
        [
            (MOI.VectorAffineFunction{Float64},MOI.Zeros),
            (MOI.VectorOfVariables,MOI.Nonnegatives),
            (MOI.VectorOfVariables,MOI.Nonpositives)
        ])
        @testset "LIN2" begin
            # mixed cones
            # min  3x + 2y - 4z + 0s
            # st    x           -  s  == -4    (i.e. x >= -4)
            #            y            == -3
            #       x      +  z       == 12
            #       x free
            #       y <= 0
            #       z >= 0
            #       s zero
            # Opt solution = -82
            # x = -4, y = -3, z = 16, s == 0


            m = MOI.SolverInstance(solver)

            x,y,z,s = MOI.addvariables!(m, 4)
            @test MOI.getattribute(m, MOI.NumberOfVariables()) == 4


            MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([x,y,z], [3.0, 2.0, -4.0], 0.0))


            c = MOI.addconstraint!(m, MOI.VectorAffineFunction([1,1,2,3,3], [x,s,y,x,z], [1.0,-1.0,1.0,1.0,1.0], [4.0,3.0,-12.0]), MOI.Zeros(3))

            vy = MOI.addconstraint!(m, MOI.VectorOfVariables([y]), MOI.Nonpositives(1))
            vz = MOI.addconstraint!(m, MOI.VectorOfVariables([z]), MOI.Nonnegatives(1))
            vz = MOI.addconstraint!(m, MOI.VectorOfVariables([s]), MOI.Zeros(1))

            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonpositives}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonnegatives}()) == 1

            MOI.optimize!(m)

            @test MOI.cangetattribute(m, MOI.TerminationStatus())
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

            @test MOI.cangetattribute(m, MOI.PrimalStatus())
            @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint

            @test MOI.cangetattribute(m, MOI.ObjectiveValue())
            @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ -82 atol=atol rtol=rtol

            @test MOI.cangetattribute(m, MOI.VariablePrimal(), x)
            @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ -4 atol=atol rtol=rtol
            @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ -3 atol=atol rtol=rtol
            @test MOI.getattribute(m, MOI.VariablePrimal(), z) ≈ 16 atol=atol rtol=rtol
            @test MOI.getattribute(m, MOI.VariablePrimal(), s) ≈ 0 atol=atol rtol=rtol

            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
            @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ [7, 2, -4] atol=atol rtol=rtol

            # TODO var dual and con primal
        end
    end
end

function lin2atest(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    if MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.VectorAffineFunction{Float64},MOI.Zeros),(MOI.VectorAffineFunction{Float64},MOI.Nonnegatives),(MOI.VectorAffineFunction{Float64},MOI.Nonpositives)])
        @testset "LIN2A" begin
            # mixed cones
            # same as LIN2 but with variable bounds enforced with VectorAffineFunction
            # min  3x + 2y - 4z + 0s
            # st    x           -  s  == -4    (i.e. x >= -4)
            #            y            == -3
            #       x      +  z       == 12
            #       x free
            #       y <= 0
            #       z >= 0
            #       s zero
            # Opt solution = -82
            # x = -4, y = -3, z = 16, s == 0

            m = MOI.SolverInstance(solver)

            x,y,z,s = MOI.addvariables!(m, 4)
            @test MOI.getattribute(m, MOI.NumberOfVariables()) == 4


            MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([x,y,z], [3.0, 2.0, -4.0], 0.0))


            c = MOI.addconstraint!(m, MOI.VectorAffineFunction([1,1,2,3,3], [x,s,y,x,z], [1.0,-1.0,1.0,1.0,1.0], [4.0,3.0,-12.0]), MOI.Zeros(3))

            vy = MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[y],[1.0],[0.0]), MOI.Nonpositives(1))
            vz = MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[z],[1.0],[0.0]), MOI.Nonnegatives(1))
            vz = MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[s],[1.0],[0.0]), MOI.Zeros(1))

            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 2
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Nonpositives}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Nonnegatives}()) == 1


            MOI.optimize!(m)

            @test MOI.cangetattribute(m, MOI.TerminationStatus())
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

            @test MOI.cangetattribute(m, MOI.PrimalStatus())
            @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint

            @test MOI.cangetattribute(m, MOI.ObjectiveValue())
            @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ -82 atol=atol rtol=rtol

            @test MOI.cangetattribute(m, MOI.VariablePrimal(), x)
            @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ -4 atol=atol rtol=rtol
            @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ -3 atol=atol rtol=rtol
            @test MOI.getattribute(m, MOI.VariablePrimal(), z) ≈ 16 atol=atol rtol=rtol
            @test MOI.getattribute(m, MOI.VariablePrimal(), s) ≈ 0 atol=atol rtol=rtol

            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c)
            @test MOI.getattribute(m, MOI.ConstraintDual(), c) ≈ [7, 2, -4] atol=atol rtol=rtol

            # TODO var dual and con primal
        end
    end
end

function lin2tests(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    lin2test(solver, atol=atol, rtol=rtol)
    lin2atest(solver, atol=atol, rtol=rtol)
end

function lin3test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    if MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.VectorAffineFunction{Float64},MOI.Nonpositives),(MOI.VectorAffineFunction{Float64},MOI.Nonnegatives)])
        @testset "LIN3 - infeasible" begin
            # Problem LIN3 - Infeasible LP
            # min  0
            # s.t. x ≥ 1
            #      x ≤ -1
            # in conic form:
            # min 0
            # s.t. -1 + x ∈ R₊
            #       1 + x ∈ R₋

            m = MOI.SolverInstance(solver)

            x = MOI.addvariable!(m)

            MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[x],[1.0],[-1.0]), MOI.Nonnegatives(1))
            MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[x],[1.0],[1.0]), MOI.Nonpositives(1))

            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Nonnegatives}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Nonpositives}()) == 1

            MOI.optimize!(m)

            @test MOI.cangetattribute(m, MOI.TerminationStatus())
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

            if MOI.cangetattribute(m, MOI.PrimalStatus())
                @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.InfeasiblePoint
            end
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.InfeasibilityCertificate

            # TODO test dual feasibility and objective sign
        end
    end
end

function lin4test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    if MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.VectorAffineFunction{Float64},MOI.Nonnegatives),(MOI.VectorOfVariables,MOI.Nonpositives)])
        @testset "LIN4 - infeasible" begin
            # Problem LIN4 - Infeasible LP
            # min  0
            # s.t. x ≥ 1
            #      x ≤ 0
            # in conic form:
            # min 0
            # s.t. -1 + x ∈ R₊
            #           x ∈ R₋

            m = MOI.SolverInstance(solver)

            x = MOI.addvariable!(m)

            MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[x],[1.0],[-1.0]), MOI.Nonnegatives(1))
            MOI.addconstraint!(m, MOI.VectorOfVariables([x]), MOI.Nonpositives(1))

            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Nonnegatives}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonpositives}()) == 1

            MOI.optimize!(m)

            @test MOI.cangetattribute(m, MOI.TerminationStatus())
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

            if MOI.cangetattribute(m, MOI.PrimalStatus())
                @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.InfeasiblePoint
            end
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.InfeasibilityCertificate

            # TODO test dual feasibility and objective sign
        end
    end
end

function lintests(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    lin1tests(solver, atol=atol, rtol=rtol)
    lin2tests(solver, atol=atol, rtol=rtol)
    lin3test(solver, atol=atol, rtol=rtol)
    lin4test(solver, atol=atol, rtol=rtol)
end


function soc1test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    if MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.VectorAffineFunction{Float64},MOI.Zeros),(MOI.VectorOfVariables,MOI.SecondOrderCone)])
        @testset "SOC1" begin
            # Problem SOC1
            # max 0x + 1y + 1z
            #  st  x            == 1
            #      x >= ||(y,z)||

            m = MOI.SolverInstance(solver)

            x,y,z = MOI.addvariables!(m, 3)

            MOI.setobjective!(m, MOI.MaxSense, MOI.ScalarAffineFunction([y,z],[1.0,1.0],0.0))

            ceq = MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[x],[1.0],[-1.0]), MOI.Zeros(1))
            csoc = MOI.addconstraint!(m, MOI.VectorOfVariables([x,y,z]), MOI.SecondOrderCone(3))

            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SecondOrderCone}()) == 1
            loc = MOI.getattribute(m, MOI.ListOfConstraints())
            @test length(loc) == 2
            @test (MOI.VectorAffineFunction{Float64},MOI.Zeros) in loc
            @test (MOI.VectorOfVariables,MOI.SecondOrderCone) in loc

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
            @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 1 atol=atol rtol=rtol
            @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 1/sqrt(2) atol=atol rtol=rtol
            @test MOI.getattribute(m, MOI.VariablePrimal(), z) ≈ 1/sqrt(2) atol=atol rtol=rtol

            @test MOI.cangetattribute(m, MOI.ConstraintDual(), ceq)
            @test MOI.getattribute(m, MOI.ConstraintDual(), ceq) ≈ [-sqrt(2)] atol=atol rtol=rtol
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), csoc)
            @test MOI.getattribute(m, MOI.ConstraintDual(), csoc) ≈ [sqrt(2), -1.0, -1.0] atol=atol rtol=rtol

            # TODO con primal
        end
    end
end

function soc1atest(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    if MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.VectorAffineFunction{Float64},MOI.Zeros),(MOI.VectorAffineFunction{Float64},MOI.SecondOrderCone)])
        @testset "SOC1A" begin
            # Problem SOC1A
            # max 0x + 1y + 1z
            #  st  x            == 1
            #      x >= ||(y,z)||
            # same as SOC1 but with soc constraint enforced with VectorAffineFunction

            m = MOI.SolverInstance(solver)

            x,y,z = MOI.addvariables!(m, 3)

            MOI.setobjective!(m, MOI.MaxSense, MOI.ScalarAffineFunction([y,z],[1.0,1.0],0.0))

            ceq = MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[x],[1.0],[-1.0]), MOI.Zeros(1))
            csoc = MOI.addconstraint!(m, MOI.VectorAffineFunction([1,2,3],[x,y,z],ones(3),zeros(3)), MOI.SecondOrderCone(3))

            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.SecondOrderCone}()) == 1

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
            @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 1 atol=atol rtol=rtol
            @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 1/sqrt(2) atol=atol rtol=rtol
            @test MOI.getattribute(m, MOI.VariablePrimal(), z) ≈ 1/sqrt(2) atol=atol rtol=rtol

            @test MOI.cangetattribute(m, MOI.ConstraintDual(), ceq)
            @test MOI.getattribute(m, MOI.ConstraintDual(), ceq) ≈ [-sqrt(2)] atol=atol rtol=rtol
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), csoc)
            @test MOI.getattribute(m, MOI.ConstraintDual(), csoc) ≈ [sqrt(2), -1.0, -1.0] atol=atol rtol=rtol

            # TODO con primal
        end
    end
end

function soc1tests(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    soc1test(solver, ɛ)
    soc1atest(solver, ɛ)
end

function soc2test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    if MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.VectorAffineFunction{Float64},MOI.Zeros),(MOI.VectorAffineFunction{Float64},MOI.Nonnegatives),(MOI.VectorAffineFunction{Float64},MOI.SecondOrderCone)])
        @testset "SOC2" begin
            # Problem SOC2
            # min  x
            # s.t. y ≥ 1/√2
            #      x² + y² ≤ 1
            # in conic form:
            # min  x
            # s.t.  -1/√2 + y ∈ R₊
            #        1 - t ∈ {0}
            #      (t,x,y) ∈ SOC₃

            m = MOI.SolverInstance(solver)

            x,y,t = MOI.addvariables!(m, 3)

            MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([x],[1.0],0.0))

            MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[y],[1.0],[-1/sqrt(2)]), MOI.Nonnegatives(1))
            MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[t],[-1.0],[1.0]), MOI.Zeros(1))
            MOI.addconstraint!(m, MOI.VectorAffineFunction([1,2,3],[t,x,y],ones(3),zeros(3)), MOI.SecondOrderCone(3))

            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Nonnegatives}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.SecondOrderCone}()) == 1

            MOI.optimize!(m)

            @test MOI.cangetattribute(m, MOI.TerminationStatus())
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

            @test MOI.cangetattribute(m, MOI.PrimalStatus())
            @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint

            @test MOI.cangetattribute(m, MOI.ObjectiveValue())
            @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ -1/sqrt(2) atol=atol rtol=rtol

            @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ -1/sqrt(2) atol=atol rtol=rtol
            @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 1/sqrt(2) atol=atol rtol=rtol
            @test MOI.getattribute(m, MOI.VariablePrimal(), t) ≈ 1 atol=atol rtol=rtol

            # TODO constraint primal and duals
        end
    end
end

function soc2atest(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    if MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.VectorAffineFunction{Float64},MOI.Zeros),(MOI.VectorAffineFunction{Float64},MOI.Nonpositives),(MOI.VectorAffineFunction{Float64},MOI.SecondOrderCone)])
        @testset "SOC2A" begin
            # Problem SOC2A
            # Same as SOC2 but with nonpositive instead of nonnegative
            # min  x
            # s.t.  1/√2 - y ∈ R₋
            #        1 - t ∈ {0}
            #      (t,x,y) ∈ SOC₃

            m = MOI.SolverInstance(solver)

            x,y,t = MOI.addvariables!(m, 3)

            MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([x],[1.0],0.0))

            MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[y],[-1.0],[1/sqrt(2)]), MOI.Nonpositives(1))
            MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[t],[-1.0],[1.0]), MOI.Zeros(1))
            MOI.addconstraint!(m, MOI.VectorAffineFunction([1,2,3],[t,x,y],ones(3),zeros(3)), MOI.SecondOrderCone(3))

            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Nonpositives}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.SecondOrderCone}()) == 1

            MOI.optimize!(m)

            @test MOI.cangetattribute(m, MOI.TerminationStatus())
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

            @test MOI.cangetattribute(m, MOI.PrimalStatus())
            @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint

            @test MOI.cangetattribute(m, MOI.ObjectiveValue())
            @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ -1/sqrt(2) atol=atol rtol=rtol

            @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ -1/sqrt(2) atol=atol rtol=rtol
            @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 1/sqrt(2) atol=atol rtol=rtol
            @test MOI.getattribute(m, MOI.VariablePrimal(), t) ≈ 1 atol=atol rtol=rtol

            # TODO constraint primal and duals
        end
    end
end

function soc2tests(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    soc2test(solver, atol)
    soc2atest(solver, atol)
end

function soc3test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    if MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.VectorAffineFunction{Float64},MOI.Nonnegatives),(MOI.VectorAffineFunction{Float64},MOI.Nonpositives),(MOI.VectorAffineFunction{Float64},MOI.SecondOrderCone)])
        @testset "SOC3 - infeasible" begin
            # Problem SOC3 - Infeasible
            # min 0
            # s.t. y ≥ 2
            #      x ≤ 1
            #      |y| ≤ x
            # in conic form:
            # min 0
            # s.t. -2 + y ∈ R₊
            #      -1 + x ∈ R₋
            #       (x,y) ∈ SOC₂

            m = MOI.SolverInstance(solver)

            x,y = MOI.addvariables!(m, 2)

            MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[y],[1.0],[-2.0]), MOI.Nonnegatives(1))
            MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[x],[1.0],[-1.0]), MOI.Nonpositives(1))
            MOI.addconstraint!(m, MOI.VectorAffineFunction([1,2],[x,y],ones(2),zeros(2)), MOI.SecondOrderCone(2))

            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Nonnegatives}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Nonpositives}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.SecondOrderCone}()) == 1

            MOI.optimize!(m)

            @test MOI.cangetattribute(m, MOI.TerminationStatus())
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

            @test !MOI.cangetattribute(m, MOI.PrimalStatus())
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.InfeasibilityCertificate

            # TODO test dual feasibility and objective sign
        end
    end
end

function soc4test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    if MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.Zeros),(MOI.VectorOfVariables,MOI.SecondOrderCone)])
        @testset "SOC4" begin
            # Problem SOC4
            # min 0x[1] - 2x[2] - 1x[3]
            #  st  x[1]                                == 1 (c1a)
            #              x[2]         - x[4]         == 0 (c1b)
            #                      x[3]         - x[5] == 0 (c1c)
            #      x[1] >= ||(x[4],x[5])||                  (c2)
            # in conic form:
            # min  c^Tx
            # s.t. Ax + b ∈ {0}₃
            #      (x[1],x[4],x[5]) ∈ SOC₃
            # Like SOCINT1 but with copies of variables and integrality relaxed
            # Tests out-of-order indices in cones

            b = [-1.0, 0.0, 0.0]
            A = [ 1.0  0.0  0.0  0.0  0.0
                  0.0  1.0  0.0 -1.0  0.0
                  0.0  0.0  1.0  0.0 -1.0]
            c = [ 0.0,-2.0,-1.0, 0.0, 0.0]

            m = MOI.SolverInstance(solver)

            x = MOI.addvariables!(m, 5)

            A_cols = x
            A_rows = [1,2,3,2,3]
            A_vals = [1.0,1.0,1.0,-1.0,-1.0]

            c1 = MOI.addconstraint!(m, MOI.VectorAffineFunction(A_rows,A_cols,A_vals,b), MOI.Zeros(3))
            c2 = MOI.addconstraint!(m, MOI.VectorOfVariables([x[1],x[4],x[5]]), MOI.SecondOrderCone(3))

            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SecondOrderCone}()) == 1

            MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction(x,c,0.0))
            MOI.optimize!(m)

            @test MOI.cangetattribute(m, MOI.TerminationStatus())
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

            @test MOI.cangetattribute(m, MOI.PrimalStatus())
            @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint

            @test MOI.cangetattribute(m, MOI.VariablePrimal(), x)
            x_primal = MOI.getattribute(m, MOI.VariablePrimal(), x)
            @test x_primal[1]^2 ≥ x_primal[4]^2 + x_primal[5]^2 - atol

            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c2)
            x_dual = MOI.getattribute(m, MOI.ConstraintDual(), c2)
            @test x_dual[1]^2 ≥ x_dual[2]^2 + x_dual[3]^2 - atol

            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c1)
            c1_dual = MOI.getattribute(m, MOI.ConstraintDual(), c1)

            @test dot(c,x_primal) ≈ -dot(c1_dual,b) atol=atol rtol=rtol
            @test (c-A'c1_dual) ≈ [x_dual[1], 0, 0, x_dual[2], x_dual[3]] atol=atol rtol=rtol
        end
    end
end

function soctests(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    soc1tests(solver, atol=atol, rtol=rtol)
    soc2tests(solver, atol=atol, rtol=rtol)
    soc3test(solver, atol=atol, rtol=rtol)
    soc4test(solver, atol=atol, rtol=rtol)
end

function rotatedsoc1test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    if MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64},
        [(MOI.SingleVariable,MOI.EqualTo{Float64}),
         (MOI.VectorOfVariables,MOI.RotatedSecondOrderCone)])
        @testset "SOCRotated1" begin
            # Problem SOCRotated1
            # min 0a + 0b - 1x - 1y
            #  st  a            == 1/2
            #  st  b            == 1
            #      2a*b >= x^2+y^2
            c = [ 0.0, 0.0, -1.0, -1.0]
            A = [ 1.0  0.0   0.0   0.0
                  0.0  1.0   0.0   0.0]
            b = [ 0.5, 1.0]

            m = MOI.SolverInstance(solver)

            x = MOI.addvariables!(m, 4)

            vc1 = MOI.addconstraint!(m, MOI.SingleVariable(x[1]), MOI.EqualTo(0.5))
            vc2 = MOI.addconstraint!(m, MOI.SingleVariable(x[2]), MOI.EqualTo(1.0))

            rsoc = MOI.addconstraint!(m, MOI.VectorOfVariables(x), MOI.RotatedSecondOrderCone(4))

            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.EqualTo{Float64}}()) == 2
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.RotatedSecondOrderCone}()) == 1

            MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction(x,c,0.0))
            MOI.optimize!(m)

            @test MOI.cangetattribute(m, MOI.TerminationStatus())
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

            @test MOI.cangetattribute(m, MOI.PrimalStatus())
            @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint

            @test MOI.cangetattribute(m, MOI.ObjectiveValue())
            @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ -sqrt(2.0) atol=atol rtol=rtol

            @test MOI.cangetattribute(m, MOI.VariablePrimal(), x)
            @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ [0.5, 1.0, 1.0/sqrt(2.0), 1.0/sqrt(2.0)] atol=atol rtol=rtol

            if MOI.getattribute(solver, MOI.SupportsDuals())
                @test MOI.cangetattribute(m, MOI.DualStatus(1))
                @test MOI.getattribute(m, MOI.DualStatus(1)) == MOI.FeasiblePoint

                @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc1)
                d1 = MOI.getattribute(m, MOI.ConstraintDual(), vc1)
                @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc2)
                d2 = MOI.getattribute(m, MOI.ConstraintDual(), vc2)

                d = [d1, d2]
                dualobj = dot(b, d)
                @test dualobj ≈ -sqrt(2.0) atol=atol rtol=rtol
                @test d1 <= atol
                @test d2 <= atol

                @test MOI.cangetattribute(m, MOI.ConstraintDual(), rsoc)
                vardual = MOI.getattribute(m, MOI.ConstraintDual(), rsoc)

                @test vardual ≈ (c - A'd) atol=atol rtol=rtol
                @test 2*vardual[1]*vardual[2] ≥ vardual[3]^2 + vardual[4]^2 - atol
            end
        end
    end
end

# function rotatedsoc1atest(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
# end

function rotatedsoc2test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    if MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64},
        [(MOI.SingleVariable,MOI.EqualTo{Float64}),
         (MOI.SingleVariable,MOI.LessThan{Float64}),
         (MOI.SingleVariable,MOI.GreaterThan{Float64}),
         (MOI.VectorOfVariables,MOI.RotatedSecondOrderCone)])
        @testset "SOCRotated2 infeasible" begin
            # Problem SOCRotated2 - Infeasible
            # min 0
            # s.t.
            #      x ≤ 1
            #      y = 1/2
            #      z ≥ 2
            #      z^2 ≤ 2x*y
            # in conic form:
            # min 0
            # s.t.
            #      -1 + x ∈ R₋
            #     1/2 - y ∈ {0}
            #      -2 + z ∈ R₊
            #       (x,y,z) ∈ SOCRoated
            b = [-2, -1, 1/2]
            A = [0 0 -1; -1 0 0; 0 1 0]
            c = [0.0,0.0,0.0]

            m = MOI.SolverInstance(solver)

            x = MOI.addvariables!(m, 3)

            vc1 = MOI.addconstraint!(m, MOI.SingleVariable(x[1]), MOI.LessThan(1.0))
            vc2 = MOI.addconstraint!(m, MOI.SingleVariable(x[2]), MOI.EqualTo(0.5))
            vc3 = MOI.addconstraint!(m, MOI.SingleVariable(x[3]), MOI.GreaterThan(2.0))

            rsoc = MOI.addconstraint!(m, MOI.VectorOfVariables(x), MOI.RotatedSecondOrderCone(3))

            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.LessThan{Float64}}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.EqualTo{Float64}}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.RotatedSecondOrderCone}()) == 1

            MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction(x,c,0.0))
            MOI.optimize!(m)

            @test MOI.cangetattribute(m, MOI.TerminationStatus())
            @test MOI.getattribute(m, MOI.TerminationStatus()) in [MOI.Success, MOI.InfeasibleNoResult, MOI.InfeasibleOrUnbounded]

            if MOI.getattribute(m, MOI.TerminationStatus()) == MOI.InfeasibleOrUnbounded && MOI.getattribute(solver, MOI.SupportsDuals())
                @test MOI.cangetattribute(m, MOI.DualStatus())
                @test MOI.getattribute(m, MOI.DualStatus()) in [InfeasibilityCertificate, NearlyInfeasibilityCertificate]

                @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc1)
                y1 = MOI.getattribute(m, MOI.ConstraintDual(), vc1)
                @test y1 > 0

                @test MOI.cangetattribute(m, MOI.ConstraintDual(), vc1)
                y1 = MOI.getattribute(m, MOI.ConstraintDual(), vc1)
                @test y2 < 0

                y = [y1, y2]

                vardual = MOI.getattribute(m, MOI.ConstraintDual(), rsoc)

                @test vardual ≈ (A'y) atol=atol rtol=rtol
                @test 2*vardual[1]*vardual[2] ≥ vardual[3]^2 - atol
                @test -dot(b,y) > 0
            end
        end
    end
end
function soctests(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    rotatedsoc1test(solver, atol=atol, rtol=rtol)
    #rotatedsoc1atest(solver, atol=atol, rtol=rtol)
    rotatedsoc2test(solver, atol=atol, rtol=rtol)
end

function sdp1test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    if MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}), (MOI.VectorOfVariables, MOI.PositiveSemidefiniteConeTriangle), (MOI.VectorOfVariables, MOI.SecondOrderCone)])
        @testset "SDP1" begin
            # Problem SDP1 - sdo1 from MOSEK docs
            # From Mosek.jl/test/mathprogtestextra.jl, under license:
            #   Copyright (c) 2013 Ulf Worsoe, Mosek ApS
            #   Permission is hereby granted, free of charge, to any person obtaining a copy of this
            #   software and associated documentation files (the "Software"), to deal in the Software
            #   without restriction, including without limitation the rights to use, copy, modify, merge,
            #   publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
            #   to whom the Software is furnished to do so, subject to the following conditions:
            #   The above copyright notice and this permission notice shall be included in all copies or
            #   substantial portions of the Software.
            #
            #     | 2 1 0 |
            # min | 1 2 1 | . X + x1
            #     | 0 1 2 |
            #
            #
            # s.t. | 1 0 0 |
            #      | 0 1 0 | . X + x1 = 1
            #      | 0 0 1 |
            #
            #      | 1 1 1 |
            #      | 1 1 1 | . X + x2 + x3 = 1/2
            #      | 1 1 1 |
            #
            #      (x1,x2,x3) in C^3_q
            #      X in C_sdp

            m = MOI.SolverInstance(solver)

            X = MOI.addvariables!(m, 6)
            @test MOI.getattribute(m, MOI.NumberOfVariables()) == 6
            x = MOI.addvariables!(m, 3)
            @test MOI.getattribute(m, MOI.NumberOfVariables()) == 9

            cX = MOI.addconstraint!(m, MOI.VectorOfVariables(X), MOI.PositiveSemidefiniteConeTriangle(3))
            cx = MOI.addconstraint!(m, MOI.VectorOfVariables(x), MOI.SecondOrderCone(3))

            c1 = MOI.addconstraint!(m, MOI.ScalarAffineFunction([X[1], X[4], X[6], x[1]], [1., 1, 1, 1], 0.), MOI.EqualTo(1.))
            c2 = MOI.addconstraint!(m, MOI.ScalarAffineFunction([X; x[2]; x[3]], [1., 2, 2, 1, 2, 1, 1, 1], 0.), MOI.EqualTo(1/2))


            MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([X[1:2]; X[4:6]; x[1]], [2., 2, 2, 2, 2, 1], 0.))


            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}}()) == 2
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorOfVariables, MOI.PositiveSemidefiniteConeTriangle}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorOfVariables, MOI.SecondOrderCone}()) == 1

            MOI.optimize!(m)

            @test MOI.cangetattribute(m, MOI.TerminationStatus())
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

            @test MOI.cangetattribute(m, MOI.PrimalStatus())
            @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint
            @test MOI.cangetattribute(m, MOI.DualStatus())
            @test MOI.getattribute(m, MOI.DualStatus()) == MOI.FeasiblePoint

            @test MOI.cangetattribute(m, MOI.ObjectiveValue())
            @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ 0.705710509 atol=atol rtol=rtol

            @test MOI.cangetattribute(m, MOI.VariablePrimal(), X)
            Xv = MOI.getattribute(m, MOI.VariablePrimal(), X)
            @test MOI.cangetattribute(m, MOI.VariablePrimal(), x)
            xv = MOI.getattribute(m, MOI.VariablePrimal(), x)

            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c1)
            y1 = MOI.getattribute(m, MOI.ConstraintDual(), c1)
            @test MOI.cangetattribute(m, MOI.ConstraintDual(), c2)
            y2 = MOI.getattribute(m, MOI.ConstraintDual(), c2)

            #    X11 X21 X31 X22 X32 X33  x1  x2  x3
            c = [  2,  2,  0,  2,  2,  2,  1,  0,  0]
            b = [1, 1/2]
            # Check primal objective
            comp_pobj = dot(c, [Xv; xv])
            # Check dual objective
            comp_dobj = dot([y1, y2], b)
            @test comp_pobj ≈ comp_dobj atol=atol rtol=rtol

            @test MOI.cangetattribute(m, MOI.ConstraintDual(), cX)
            Xdv = MOI.getattribute(m, MOI.ConstraintDual(), cX)
            Xd = [Xdv[1] Xdv[2] Xdv[3];
                  Xdv[2] Xdv[4] Xdv[5];
                  Xdv[3] Xdv[5] Xdv[6]]

            C = [2 1 0;
                 1 2 1;
                 0 1 2]
            A1 = [1 0 0;
                  0 1 0;
                  0 0 1]
            A2 = [1 1 1;
                  1 1 1;
                  1 1 1]

            @test C ≈ y1 * A1 + y2 * A2 + Xd atol=ɛ

            @test eigmin(Xd) > -ɛ
        end
    end
end

function sdptests(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    sdp1test(solver, atol=atol, rtol=rtol)
end

function contconictest(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    lintests(solver, atol=atol, rtol=rtol)
    soctests(solver, atol=atol, rtol=rtol)
    sdptests(solver, atol=atol, rtol=rtol)
end
