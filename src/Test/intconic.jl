# Integer conic problems

function intsoc1test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol

    # Problem SINTSOC1
    # min 0x - 2y - 1z
    #  st  x            == 1
    #      x >= ||(y,z)||
    #      (y,z) binary

    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{Float64}, MOI.Zeros)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.ZeroOne)
    @test MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.SecondOrderCone)

    MOI.empty!(model)
    @test MOI.is_empty(model)

    x,y,z = MOI.add_variables(model, 3)

    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-2.0,-1.0], [y,z]), 0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MinSense)

    ceq = MOI.add_constraint(model, MOI.VectorAffineFunction([MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))], [-1.0]), MOI.Zeros(1))
    csoc = MOI.add_constraint(model, MOI.VectorOfVariables([x,y,z]), MOI.SecondOrderCone(3))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SecondOrderCone}()) == 1
    loc = MOI.get(model, MOI.ListOfConstraints())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{Float64},MOI.Zeros) in loc
    @test (MOI.VectorOfVariables,MOI.SecondOrderCone) in loc

    bin1 = MOI.add_constraint(model, MOI.SingleVariable(y), MOI.ZeroOne())
    bin2 = MOI.add_constraint(model, MOI.SingleVariable(z), MOI.ZeroOne())

    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OptimizeNotCalled

        MOI.optimize!(model)

        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -2 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 1 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 1 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), z) ≈ 0 atol=atol rtol=rtol
    end
end

const intsoctests = Dict("intsoc1" => intsoc1test)

@moitestset intsoc

const intconictests = Dict("intsoc" => intsoctest)

@moitestset intconic true
