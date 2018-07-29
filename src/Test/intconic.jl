# Integer conic problems

function intsoc1test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol

    # Problem SINTSOC1
    # min 0x - 2y - 1z
    #  st  x            == 1
    #      x >= ||(y,z)||
    #      (y,z) binary

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Zeros)
    @test MOI.supportsconstraint(model, MOI.SingleVariable, MOI.ZeroOne)
    @test MOI.supportsconstraint(model, MOI.VectorOfVariables, MOI.SecondOrderCone)

    MOI.empty!(model)
    @test MOI.isempty(model)

    x,y,z = MOI.addvariables!(model, 3)

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-2.0,-1.0], [y,z]), 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    ceq = MOI.addconstraint!(model, MOI.VectorAffineFunction([MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))], [-1.0]), MOI.Zeros(1))
    csoc = MOI.addconstraint!(model, MOI.VectorOfVariables([x,y,z]), MOI.SecondOrderCone(3))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SecondOrderCone}()) == 1
    loc = MOI.get(model, MOI.ListOfConstraints())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{Float64},MOI.Zeros) in loc
    @test (MOI.VectorOfVariables,MOI.SecondOrderCone) in loc

    bin1 = MOI.addconstraint!(model, MOI.SingleVariable(y), MOI.ZeroOne())
    bin2 = MOI.addconstraint!(model, MOI.SingleVariable(z), MOI.ZeroOne())

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

const intsoctests = Dict("intsoc1" => intsoc1test)

@moitestset intsoc

const intconictests = Dict("intsoc" => intsoctest)

@moitestset intconic true
