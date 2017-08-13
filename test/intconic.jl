using MathOptInterface
MOI = MathOptInterface

# Integer conic problems

function intsoc1test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    if MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, 
        [(MOI.VectorAffineFunction{Float64},MOI.Zeros),
         (MOI.SingleVariable,MOI.ZeroOne),
         (MOI.VectorOfVariables,MOI.SecondOrderCone)])
        @testset "INTSOC1" begin

            # Problem SINTSOC1
            # min 0x - 2y - 1z
            #  st  x            == 1
            #      x >= ||(y,z)||
            #      (y,z) binary

            m = MOI.SolverInstance(solver)

            x,y,z = MOI.addvariables!(m, 3)

            MOI.setobjective!(m, MOI.MinSense, MOI.ScalarAffineFunction([y,z],[-2.0,-1.0],0.0))

            ceq = MOI.addconstraint!(m, MOI.VectorAffineFunction([1],[x],[1.0],[-1.0]), MOI.Zeros(1))
            csoc = MOI.addconstraint!(m, MOI.VectorOfVariables([x,y,z]), MOI.SecondOrderCone(3))

            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 1
            @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SecondOrderCone}()) == 1
            loc = MOI.getattribute(m, MOI.ListOfConstraints())
            @test length(loc) == 2
            @test (MOI.VectorAffineFunction{Float64},MOI.Zeros) in loc
            @test (MOI.VectorOfVariables,MOI.SecondOrderCone) in loc

            bin1 = MOI.addconstraint!(m, MOI.SingleVariable(y), MOI.ZeroOne)
            bin2 = MOI.addconstraint!(m, MOI.SingleVariable(z), MOI.ZeroOne)

            MOI.optimize!(m)

            @test MOI.cangetattribute(m, MOI.TerminationStatus())
            @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success

            @test MOI.cangetattribute(m, MOI.PrimalStatus())
            @test MOI.getattribute(m, MOI.PrimalStatus()) == MOI.FeasiblePoint

            @test MOI.cangetattribute(m, MOI.ObjectiveValue())
            @test MOI.getattribute(m, MOI.ObjectiveValue()) ≈ -2 atol=atol rtol=rtol

            @test MOI.cangetattribute(m, MOI.VariablePrimal(), x)
            @test MOI.getattribute(m, MOI.VariablePrimal(), x) ≈ 1 atol=atol rtol=rtol
            @test MOI.getattribute(m, MOI.VariablePrimal(), y) ≈ 1 atol=atol rtol=rtol
            @test MOI.getattribute(m, MOI.VariablePrimal(), z) ≈ 0 atol=atol rtol=rtol

        end
    end
end

function intsoctests(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    intsoc1test(solver, atol=atol, rtol=rtol)
end

function intconictests(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    intsoctests(solver, atol=atol, rtol=rtol)
end