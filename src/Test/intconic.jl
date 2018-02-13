# Integer conic problems

function intsoc1test(model::MOI.ModelLike; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64},
    #    [(MOI.VectorAffineFunction{Float64},MOI.Zeros),
    #     (MOI.SingleVariable,MOI.ZeroOne),
    #     (MOI.VectorOfVariables,MOI.SecondOrderCone)])
    @testset "INTSOC1" begin

        # Problem SINTSOC1
        # min 0x - 2y - 1z
        #  st  x            == 1
        #      x >= ||(y,z)||
        #      (y,z) binary

        MOI.empty!(model)
        @test MOI.isempty(model)

        MOI.canaddvariable(model)
        x,y,z = MOI.addvariables!(model, 3)

        @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
        MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([y,z],[-2.0,-1.0],0.0))
        @test MOI.canset(model, MOI.ObjectiveSense())
        MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

        @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Zeros)
        ceq = MOI.addconstraint!(model, MOI.VectorAffineFunction([1],[x],[1.0],[-1.0]), MOI.Zeros(1))
        @test MOI.canaddconstraint(model, MOI.VectorOfVariables, MOI.SecondOrderCone)
        csoc = MOI.addconstraint!(model, MOI.VectorOfVariables([x,y,z]), MOI.SecondOrderCone(3))

        @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 1
        @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SecondOrderCone}()) == 1
        loc = MOI.get(model, MOI.ListOfConstraints())
        @test length(loc) == 2
        @test (MOI.VectorAffineFunction{Float64},MOI.Zeros) in loc
        @test (MOI.VectorOfVariables,MOI.SecondOrderCone) in loc

        @test MOI.canaddconstraint(model, typeof(MOI.SingleVariable(y)), typeof(MOI.ZeroOne))
        bin1 = MOI.addconstraint!(model, MOI.SingleVariable(y), MOI.ZeroOne)
        @test MOI.canaddconstraint(model, typeof(MOI.SingleVariable(z)), typeof(MOI.ZeroOne))
        bin2 = MOI.addconstraint!(model, MOI.SingleVariable(z), MOI.ZeroOne)

        if config.solve
            MOI.optimize!(model)

            @test MOI.canget(model, MOI.TerminationStatus())
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

            @test MOI.canget(model, MOI.PrimalStatus())
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

            @test MOI.canget(model, MOI.ObjectiveValue())
            @test MOI.get(model, MOI.ObjectiveValue()) ≈ -2 atol=atol rtol=rtol

            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 1 atol=atol rtol=rtol
            @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 1 atol=atol rtol=rtol
            @test MOI.get(model, MOI.VariablePrimal(), z) ≈ 0 atol=atol rtol=rtol
        end

    end
end

function intsoctests(model::MOI.ModelLike; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    intsoc1test(model, atol=atol, rtol=rtol)
end

function intconictests(model::MOI.ModelLike; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    intsoctests(model, atol=atol, rtol=rtol)
end
