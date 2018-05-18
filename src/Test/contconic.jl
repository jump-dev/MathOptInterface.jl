# Continuous conic problems

function _lin1test(model::MOI.ModelLike, config::TestConfig, vecofvars::Bool)
    atol = config.atol
    rtol = config.rtol
    # linear conic problem
    # min -3x - 2y - 4z
    # st    x +  y +  z == 3
    #            y +  z == 2
    #       x>=0 y>=0 z>=0
    # Opt obj = -11, soln x = 1, y = 0, z = 2

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    if vecofvars
        @test MOI.supportsconstraint(model, MOI.VectorOfVariables, MOI.Nonnegatives)
    else
        @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)
    end
    @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Zeros)

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    v = MOI.addvariables!(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3

    vov = MOI.VectorOfVariables(v)
    if vecofvars
        @test MOI.canaddconstraint(model, MOI.VectorOfVariables, MOI.Nonnegatives)
        vc = MOI.addconstraint!(model, vov, MOI.Nonnegatives(3))
    else
        @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)
        vc = MOI.addconstraint!(model, MOI.VectorAffineFunction{Float64}(vov), MOI.Nonnegatives(3))
    end

    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Zeros)
    c = MOI.addconstraint!(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1,1,1,2,2], MOI.ScalarAffineTerm.(ones(5), [v;v[2];v[3]])), [-3.0,-2.0]), MOI.Zeros(2))
    @test MOI.get(model, MOI.NumberOfConstraints{vecofvars ? MOI.VectorOfVariables : MOI.VectorAffineFunction{Float64},MOI.Nonnegatives}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 1
    loc = MOI.get(model, MOI.ListOfConstraints())
    @test length(loc) == 2
    @test (vecofvars ? MOI.VectorOfVariables : MOI.VectorAffineFunction{Float64},MOI.Nonnegatives) in loc
    @test (MOI.VectorAffineFunction{Float64},MOI.Zeros) in loc

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-3.0, -2.0, -4.0], v), 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
        end

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -11 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1, 0, 2] atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(vc))
        @test MOI.get(model, MOI.ConstraintPrimal(), vc) ≈ [1, 0, 2] atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c))
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ zeros(2) atol=atol rtol=rtol

        if config.duals
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc))
            @test MOI.get(model, MOI.ConstraintDual(), vc) ≈ [0, 2, 0] atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c))
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ [-3, -1] atol=atol rtol=rtol
        end
    end
end

lin1vtest(model::MOI.ModelLike, config::TestConfig) = _lin1test(model, config, true)
lin1ftest(model::MOI.ModelLike, config::TestConfig) = _lin1test(model, config, false)

function _lin2test(model::MOI.ModelLike, config::TestConfig, vecofvars::Bool)
    atol = config.atol
    rtol = config.rtol
    #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64},
    #[
    #    (MOI.VectorAffineFunction{Float64},MOI.Zeros),
    #    (MOI.VectorOfVariables,MOI.Nonnegatives),
    #    (MOI.VectorOfVariables,MOI.Nonpositives)
    #])
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

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    if vecofvars
        @test MOI.supportsconstraint(model, MOI.VectorOfVariables, MOI.Nonnegatives)
        @test MOI.supportsconstraint(model, MOI.VectorOfVariables, MOI.Nonpositives)
        @test MOI.supportsconstraint(model, MOI.VectorOfVariables, MOI.Zeros)
    else
        @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)
        @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonpositives)
    end
    @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Zeros)

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    x,y,z,s = MOI.addvariables!(model, 4)
    @test MOI.get(model, MOI.NumberOfVariables()) == 4

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([3.0, 2.0, -4.0], [x,y,z]), 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Zeros)
    c = MOI.addconstraint!(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1,1,2,3,3], MOI.ScalarAffineTerm.([1.0,-1.0,1.0,1.0,1.0], [x,s,y,x,z])), [4.0,3.0,-12.0]), MOI.Zeros(3))

    vov = MOI.VectorOfVariables([y])
    if vecofvars
        @test MOI.canaddconstraint(model, MOI.VectorOfVariables, MOI.Nonpositives)
        vc = MOI.addconstraint!(model, vov, MOI.Nonpositives(1))
    else
        @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonpositives)
        vc = MOI.addconstraint!(model, MOI.VectorAffineFunction{Float64}(vov), MOI.Nonpositives(1))
    end
    if vecofvars
        # test fallback
        @test MOI.canaddconstraint(model, MOI.VectorOfVariables, MOI.Nonnegatives)
        vz = MOI.addconstraint!(model, [z], MOI.Nonnegatives(1))
    else
        @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)
        vz = MOI.addconstraint!(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1], MOI.ScalarAffineTerm.([1.], [z])), [0.]), MOI.Nonnegatives(1))
    end
    vov = MOI.VectorOfVariables([s])
    if vecofvars
        @test MOI.canaddconstraint(model, MOI.VectorOfVariables, MOI.Zeros)
        vs = MOI.addconstraint!(model, vov, MOI.Zeros(1))
    else
        @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Zeros)
        vs = MOI.addconstraint!(model, MOI.VectorAffineFunction{Float64}(vov), MOI.Zeros(1))
    end

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 2 - vecofvars
    @test MOI.get(model, MOI.NumberOfConstraints{vecofvars ? MOI.VectorOfVariables : MOI.VectorAffineFunction{Float64},MOI.Nonpositives}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{vecofvars ? MOI.VectorOfVariables : MOI.VectorAffineFunction{Float64},MOI.Nonnegatives}()) == 1

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
        end

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -82 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ -4 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ -3 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), z) ≈ 16 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), s) ≈ 0 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c))
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ zeros(3) atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(vc))
        @test MOI.get(model, MOI.ConstraintPrimal(), vc) ≈ [-3] atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(vz))
        @test MOI.get(model, MOI.ConstraintPrimal(), vz) ≈ [16] atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(vs))
        @test MOI.get(model, MOI.ConstraintPrimal(), vs) ≈ [0] atol=atol rtol=rtol

        if config.duals
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c))
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ [7, 2, -4] atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc))
            @test MOI.get(model, MOI.ConstraintDual(), vc) ≈ [0] atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vz))
            @test MOI.get(model, MOI.ConstraintDual(), vz) ≈ [0] atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vs))
            @test MOI.get(model, MOI.ConstraintDual(), vs) ≈ [7] atol=atol rtol=rtol
        end
    end
end

lin2vtest(model::MOI.ModelLike, config::TestConfig) = _lin2test(model, config, true)
lin2ftest(model::MOI.ModelLike, config::TestConfig) = _lin2test(model, config, false)

function lin3test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # Problem LIN3 - Infeasible LP
    # min  0
    # s.t. x ≥ 1
    #      x ≤ -1
    # in conic form:
    # min 0
    # s.t. -1 + x ∈ R₊
    #       1 + x ∈ R₋

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonpositives)
    @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    x = MOI.addvariable!(model)

    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)
    MOI.addconstraint!(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1], MOI.ScalarAffineTerm.([1.0], [x])),[-1.0]), MOI.Nonnegatives(1))
    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonpositives)
    MOI.addconstraint!(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1], MOI.ScalarAffineTerm.([1.0], [x])), [1.0]), MOI.Nonpositives(1))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Nonnegatives}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Nonpositives}()) == 1

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        if config.infeas_certificates
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        else
            @test MOI.get(model, MOI.TerminationStatus()) in [MOI.InfeasibleNoResult, MOI.InfeasibleOrUnbounded]
        end
        if MOI.canget(model, MOI.PrimalStatus())
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.InfeasiblePoint
        end
        if config.duals && config.infeas_certificates
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.InfeasibilityCertificate
        end
        # TODO test dual feasibility and objective sign
    end
end

function lin4test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # Problem LIN4 - Infeasible LP
    # min  0
    # s.t. x ≥ 1
    #      x ≤ 0
    # in conic form:
    # min 0
    # s.t. -1 + x ∈ R₊
    #           x ∈ R₋

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)
    @test MOI.supportsconstraint(model, MOI.VectorOfVariables, MOI.Nonpositives)

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    x = MOI.addvariable!(model)

    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)
    MOI.addconstraint!(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1], MOI.ScalarAffineTerm.([1.0], [x])), [-1.0]), MOI.Nonnegatives(1))
    @test MOI.canaddconstraint(model, MOI.VectorOfVariables, MOI.Nonpositives)
    MOI.addconstraint!(model, MOI.VectorOfVariables([x]), MOI.Nonpositives(1))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Nonnegatives}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonpositives}()) == 1

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        if config.infeas_certificates
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        else
            @test MOI.get(model, MOI.TerminationStatus()) in [MOI.InfeasibleNoResult, MOI.InfeasibleOrUnbounded]
        end

        if MOI.canget(model, MOI.PrimalStatus())
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.InfeasiblePoint
        end
        if config.duals && config.infeas_certificates
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.InfeasibilityCertificate
        end
        # TODO test dual feasibility and objective sign
    end
end

const lintests = Dict("lin1v" => lin1vtest,
                      "lin1f" => lin1ftest,
                      "lin2v" => lin2vtest,
                      "lin2f" => lin2ftest,
                      "lin3"  => lin3test,
                      "lin4"  => lin4test)

@moitestset lin

function _soc1test(model::MOI.ModelLike, config::TestConfig, vecofvars::Bool)
    atol = config.atol
    rtol = config.rtol
    # Problem SOC1
    # max 0x + 1y + 1z
    #  st  x            == 1
    #      x >= ||(y,z)||

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    if vecofvars
        @test MOI.supportsconstraint(model, MOI.VectorOfVariables, MOI.Zeros)
    else
        @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Zeros)
    end
    @test MOI.supportsconstraint(model, MOI.VectorOfVariables,MOI.SecondOrderCone)

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    x,y,z = MOI.addvariables!(model, 3)

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0,1.0], [y,z]), 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)

    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Zeros)
    ceq = MOI.addconstraint!(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1], MOI.ScalarAffineTerm.([1.0], [x])), [-1.0]), MOI.Zeros(1))
    vov = MOI.VectorOfVariables([x,y,z])
    if vecofvars
        @test MOI.canaddconstraint(model, MOI.VectorOfVariables, MOI.SecondOrderCone)
        csoc = MOI.addconstraint!(model, vov, MOI.SecondOrderCone(3))
    else
        @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone)
        csoc = MOI.addconstraint!(model, MOI.VectorAffineFunction{Float64}(vov), MOI.SecondOrderCone(3))
    end

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{vecofvars ? MOI.VectorOfVariables : MOI.VectorAffineFunction{Float64},MOI.SecondOrderCone}()) == 1
    loc = MOI.get(model, MOI.ListOfConstraints())
    @test length(loc) == 2
    @test (MOI.VectorAffineFunction{Float64},MOI.Zeros) in loc
    @test (vecofvars ? MOI.VectorOfVariables : MOI.VectorAffineFunction{Float64},MOI.SecondOrderCone) in loc

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
        end

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ √2 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 1 atol=atol rtol=rtol
        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 1/√2 atol=atol rtol=rtol
        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), z) ≈ 1/√2 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(ceq))
        @test MOI.get(model, MOI.ConstraintPrimal(), ceq) ≈ [0.] atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(csoc))
        @test MOI.get(model, MOI.ConstraintPrimal(), csoc) ≈ [1., 1/√2, 1/√2] atol=atol rtol=rtol

        if config.duals
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(ceq))
            @test MOI.get(model, MOI.ConstraintDual(), ceq) ≈ [-√2] atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(csoc))
            @test MOI.get(model, MOI.ConstraintDual(), csoc) ≈ [√2, -1.0, -1.0] atol=atol rtol=rtol
        end
    end
end

soc1vtest(model::MOI.ModelLike, config::TestConfig) = _soc1test(model, config, true)
soc1ftest(model::MOI.ModelLike, config::TestConfig) = _soc1test(model, config, false)

function _soc2test(model::MOI.ModelLike, config::TestConfig, nonneg::Bool)
    atol = config.atol
    rtol = config.rtol
    # Problem SOC2
    # min  x
    # s.t. y ≥ 1/√2
    #      x² + y² ≤ 1
    # in conic form:
    # min  x
    # s.t.  -1/√2 + y ∈ R₊
    #        1 - t ∈ {0}
    #      (t,x,y) ∈ SOC₃

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Zeros)
    if nonneg
        @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)
    else
        @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonpositives)
    end
    @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64},MOI.SecondOrderCone)

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    x,y,t = MOI.addvariables!(model, 3)

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0], [x]), 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    if nonneg
        @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)
        cnon = MOI.addconstraint!(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1], MOI.ScalarAffineTerm.([1.0], [y])), [-1/√2]), MOI.Nonnegatives(1))
    else
        @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonpositives)
        cnon = MOI.addconstraint!(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1], MOI.ScalarAffineTerm.([-1.0], [y])), [1/√2]), MOI.Nonpositives(1))
    end
    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Zeros)
    ceq = MOI.addconstraint!(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1], MOI.ScalarAffineTerm.([-1.0], [t])), [1.0]), MOI.Zeros(1))
    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone)
    csoc = MOI.addconstraint!(model, MOI.VectorAffineFunction([1,2,3],[t,x,y],ones(3),zeros(3)), MOI.SecondOrderCone(3))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},nonneg ? MOI.Nonnegatives : MOI.Nonpositives}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.SecondOrderCone}()) == 1

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
        end

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -1/√2 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ -1/√2 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 1/√2 atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ 1 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(cnon))
        @test MOI.get(model, MOI.ConstraintPrimal(), cnon) ≈ [0.0] atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(ceq))
        @test MOI.get(model, MOI.ConstraintPrimal(), ceq) ≈ [0.0] atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(csoc))
        @test MOI.get(model, MOI.ConstraintPrimal(), csoc) ≈ [1., -1/√2, 1/√2] atol=atol rtol=rtol

        if config.duals
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(cnon))
            @test MOI.get(model, MOI.ConstraintDual(), cnon) ≈ [nonneg ? 1.0 : -1.0] atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(ceq))
            @test MOI.get(model, MOI.ConstraintDual(), ceq) ≈ [√2] atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(csoc))
            @test MOI.get(model, MOI.ConstraintDual(), csoc) ≈ [√2, 1.0, -1.0] atol=atol rtol=rtol
        end
    end
end

soc2ntest(model::MOI.ModelLike, config::TestConfig) = _soc2test(model, config, true)
soc2ptest(model::MOI.ModelLike, config::TestConfig) = _soc2test(model, config, false)

function soc3test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
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

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64},MOI.Nonnegatives)
    @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64},MOI.Nonpositives)
    @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64},MOI.SecondOrderCone)

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    x,y = MOI.addvariables!(model, 2)

    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)
    MOI.addconstraint!(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1], MOI.ScalarAffineTerm.([1.0], [y])), [-2.0]), MOI.Nonnegatives(1))
    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonpositives)
    MOI.addconstraint!(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1], MOI.ScalarAffineTerm.([1.0], [x])), [-1.0]), MOI.Nonpositives(1))
    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone)
    MOI.addconstraint!(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1,2], MOI.ScalarAffineTerm.(ones(2), [x,y])), zeros(2)), MOI.SecondOrderCone(2))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Nonnegatives}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Nonpositives}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.SecondOrderCone}()) == 1

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        if MOI.canget(model, MOI.PrimalStatus())
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.InfeasiblePoint
        end
        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.InfeasibilityCertificate
        end

        # TODO test dual feasibility and objective sign
    end
end

function soc4test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
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

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Zeros)
    @test MOI.supportsconstraint(model, MOI.VectorOfVariables, MOI.SecondOrderCone)

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    x = MOI.addvariables!(model, 5)

    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Zeros)
    c1 = MOI.addconstraint!(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1,2,3,2,3], MOI.ScalarAffineTerm.([1.0,1.0,1.0,-1.0,-1.0], x)),[-1.0, 0.0, 0.0]), MOI.Zeros(3))
    @test MOI.canaddconstraint(model, MOI.VectorOfVariables, MOI.SecondOrderCone)
    c2 = MOI.addconstraint!(model, MOI.VectorOfVariables([x[1],x[4],x[5]]), MOI.SecondOrderCone(3))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SecondOrderCone}()) == 1

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([0.0,-2.0,-1.0, 0.0, 0.0], x), 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)
    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
        end

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ -√5 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ [1.0, 2/√5, 1/√5, 2/√5, 1/√5] atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c1))
        @test MOI.get(model, MOI.ConstraintPrimal(), c1) ≈ zeros(3) atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c2))
        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ [1.0, 2/√5, 1/√5] atol=atol rtol=rtol

        if config.duals
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c1))
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ [-√5, -2.0, -1.0] atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c2))
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ [√5, -2.0, -1.0] atol=atol rtol=rtol
        end
    end
end

const soctests = Dict("soc1v" => soc1vtest,
                      "soc1f" => soc1ftest,
                      "soc2n" => soc2ntest,
                      "soc2p" => soc2ptest,
                      "soc3"  => soc3test,
                      "soc4"  => soc4test)

@moitestset soc

function _rotatedsoc1test(model::MOI.ModelLike, config::TestConfig, abvars::Bool)
    atol = config.atol
    rtol = config.rtol
    # Problem SOCRotated1v
    # min 0a + 0b - 1x - 1y
    #  st  a            == 1/2
    #  st  b            == 1
    #      2a*b >= x^2+y^2
    # Problem SOCRotated1f - Problem SOCRotated1v with a and b substituted
    # min          -y - z
    #  st [0.5] - [      ] SOCRotated
    #     [1.0] - [      ] SOCRotated
    #     [0.0] - [-y    ] SOCRotated
    #     [0.0] - [    -z] SOCRotated


    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    if abvars
        @test MOI.supportsconstraint(model, MOI.SingleVariable, MOI.EqualTo{Float64})
        @test MOI.supportsconstraint(model, MOI.VectorOfVariables, MOI.RotatedSecondOrderCone)
    else
        @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone)
    end

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    x = MOI.addvariables!(model, 2)
    if abvars
        MOI.canaddvariable(model)
        a = MOI.addvariable!(model)
        MOI.canaddvariable(model)
        b = MOI.addvariable!(model)
        @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.EqualTo{Float64})
        vc1 = MOI.addconstraint!(model, MOI.SingleVariable(a), MOI.EqualTo(0.5))
        @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.EqualTo{Float64})
        vc2 = MOI.addconstraint!(model, MOI.SingleVariable(b), MOI.EqualTo(1.0))
        @test MOI.canaddconstraint(model, MOI.VectorOfVariables, MOI.RotatedSecondOrderCone)
        rsoc = MOI.addconstraint!(model, MOI.VectorOfVariables([a; b; x]), MOI.RotatedSecondOrderCone(4))
    else
        a = 0.5
        b = 1.0
        @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone)
        rsoc = MOI.addconstraint!(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([3, 4], MOI.ScalarAffineTerm.([1., 1.], x)), [a, b, 0., 0.]), MOI.RotatedSecondOrderCone(4))
    end

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.EqualTo{Float64}}()) == (abvars ? 2 : 0)
    @test MOI.get(model, MOI.NumberOfConstraints{abvars ? MOI.VectorOfVariables : MOI.VectorAffineFunction{Float64},MOI.RotatedSecondOrderCone}()) == 1

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(ones(2), x), 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)
    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
        end

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ √2 atol=atol rtol=rtol

        if abvars
            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            @test MOI.get(model, MOI.VariablePrimal(), a) ≈ 0.5 atol=atol rtol=rtol
            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            @test MOI.get(model, MOI.VariablePrimal(), b) ≈ 1.0 atol=atol rtol=rtol
        end
        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ [1/√2, 1/√2] atol=atol rtol=rtol

        if abvars
            @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(vc1))
            @test MOI.get(model, MOI.ConstraintPrimal(), vc1) ≈ 0.5
            @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(vc2))
            @test MOI.get(model, MOI.ConstraintPrimal(), vc2) ≈ 1.0
        end

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(rsoc))
        @test MOI.get(model, MOI.ConstraintPrimal(), rsoc) ≈ [0.5, 1.0, 1/√2, 1/√2] atol=atol rtol=rtol

        if config.duals
            @test MOI.canget(model, MOI.DualStatus(1))
            @test MOI.get(model, MOI.DualStatus(1)) == MOI.FeasiblePoint

            if abvars
                @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc1))
                @test MOI.get(model, MOI.ConstraintDual(), vc1) ≈ -√2 atol=atol rtol=rtol
                @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc2))
                @test MOI.get(model, MOI.ConstraintDual(), vc2) ≈ -1/√2 atol=atol rtol=rtol
            end

            @test MOI.canget(model, MOI.ConstraintDual(), typeof(rsoc))
            @test MOI.get(model, MOI.ConstraintDual(), rsoc) ≈ [√2, 1/√2, -1.0, -1.0] atol=atol rtol=rtol
        end
    end
end

rotatedsoc1vtest(model::MOI.ModelLike, config::TestConfig) = _rotatedsoc1test(model, config, true)
rotatedsoc1ftest(model::MOI.ModelLike, config::TestConfig) = _rotatedsoc1test(model, config, false)

function rotatedsoc2test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
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
    #       (x,y,z) ∈ SOCRotated
    b = [-2, -1, 1/2]
    c = [0.0,0.0,0.0]

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supportsconstraint(model, MOI.SingleVariable,MOI.EqualTo{Float64})
    @test MOI.supportsconstraint(model, MOI.SingleVariable,MOI.LessThan{Float64})
    @test MOI.supportsconstraint(model, MOI.SingleVariable,MOI.GreaterThan{Float64})
    @test MOI.supportsconstraint(model, MOI.VectorOfVariables,MOI.RotatedSecondOrderCone)

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    x = MOI.addvariables!(model, 3)

    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.LessThan{Float64})
    vc1 = MOI.addconstraint!(model, MOI.SingleVariable(x[1]), MOI.LessThan(1.0))
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.EqualTo{Float64})
    vc2 = MOI.addconstraint!(model, MOI.SingleVariable(x[2]), MOI.EqualTo(0.5))
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    vc3 = MOI.addconstraint!(model, MOI.SingleVariable(x[3]), MOI.GreaterThan(2.0))

    @test MOI.canaddconstraint(model, MOI.VectorOfVariables, MOI.RotatedSecondOrderCone)
    rsoc = MOI.addconstraint!(model, MOI.VectorOfVariables(x), MOI.RotatedSecondOrderCone(3))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.LessThan{Float64}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.EqualTo{Float64}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.RotatedSecondOrderCone}()) == 1

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(c, x), 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)
    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) in [MOI.Success, MOI.InfeasibleNoResult, MOI.InfeasibleOrUnbounded]

        if MOI.get(model, MOI.TerminationStatus()) in [MOI.Success, MOI.InfeasibleOrUnbounded] && config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) in [MOI.InfeasibilityCertificate, MOI.NearlyInfeasibilityCertificate]

            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc1))
            y1 = MOI.get(model, MOI.ConstraintDual(), vc1)
            @test y1 < -atol # Should be strictly negative

            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc2))
            y2 = MOI.get(model, MOI.ConstraintDual(), vc2)

            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc3))
            y3 = MOI.get(model, MOI.ConstraintDual(), vc3)
            @test y3 > atol # Should be strictly positive

            y = [y1, y2, y3]

            vardual = MOI.get(model, MOI.ConstraintDual(), rsoc)

            @test vardual ≈ -y atol=atol rtol=rtol
            @test 2*vardual[1]*vardual[2] ≥ vardual[3]^2 - atol
            @test dot(b,y) > atol
        end
    end
end

function rotatedsoc3test(model::MOI.ModelLike, config::TestConfig; n=2, ub=3.0)
    atol = config.atol
    rtol = config.rtol
    # Problem SOCRotated3
    # max v
    # s.t.
    #      x[1:2] ≥ 0
    #      0 ≤ u ≤ 3.0
    #      v
    #      t1 == 1
    #      t2 == 1
    # [t1/√2, t2/√2, x] in SOC4
    # [x1/√2, u/√2,  v] in SOC3

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supportsconstraint(model, MOI.SingleVariable, MOI.EqualTo{Float64})
    @test MOI.supportsconstraint(model, MOI.VectorOfVariables, MOI.Nonnegatives)
    @test MOI.supportsconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    @test MOI.supportsconstraint(model, MOI.SingleVariable, MOI.LessThan{Float64})
    @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone)

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    x = MOI.addvariables!(model, n)
    MOI.canaddvariable(model)
    u = MOI.addvariable!(model)
    MOI.canaddvariable(model)
    v = MOI.addvariable!(model)
    MOI.canaddvariable(model)
    t = MOI.addvariables!(model, 2)

    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.EqualTo{Float64})
    ct1 = MOI.addconstraint!(model, MOI.SingleVariable(t[1]), MOI.EqualTo(1.0))
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.EqualTo{Float64})
    ct2 = MOI.addconstraint!(model, MOI.SingleVariable(t[2]), MOI.EqualTo(1.0))
    @test MOI.canaddconstraint(model, MOI.VectorOfVariables, MOI.Nonnegatives)
    cx  = MOI.addconstraint!(model, MOI.VectorOfVariables(x), MOI.Nonnegatives(n))
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    cu1 = MOI.addconstraint!(model, MOI.SingleVariable(u), MOI.GreaterThan(0.0))
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.LessThan{Float64})
    cu2 = MOI.addconstraint!(model, MOI.SingleVariable(u), MOI.LessThan(ub))

    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone)
    c1 = MOI.addconstraint!(model, MOI.VectorAffineFunction(collect(1:(2+n)), [t; x], [1/√2; 1/√2; ones(n)], zeros(2+n)), MOI.RotatedSecondOrderCone(2+n))
    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone)
    c2 = MOI.addconstraint!(model, MOI.VectorAffineFunction([1, 2, 3], [x[1], u, v], [1/√2; 1/√2; 1.0], zeros(3)), MOI.RotatedSecondOrderCone(3))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.EqualTo{Float64}}()) == 2
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonnegatives}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.LessThan{Float64}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.RotatedSecondOrderCone}()) == 2

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([v],[1.0],0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
        end

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ √ub atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ [1.0; zeros(n-1)] atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), u) ≈ ub atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ √ub atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ ones(2) atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(cx))
        @test MOI.get(model, MOI.ConstraintPrimal(), cx) ≈ [1.0; zeros(n-1)] atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(cu1))
        @test MOI.get(model, MOI.ConstraintPrimal(), cu1) ≈ ub atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(cu2))
        @test MOI.get(model, MOI.ConstraintPrimal(), cu2) ≈ ub atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(ct1))
        @test MOI.get(model, MOI.ConstraintPrimal(), ct1) ≈ 1.0 atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(ct2))
        @test MOI.get(model, MOI.ConstraintPrimal(), ct2) ≈ 1.0 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c1))
        @test MOI.get(model, MOI.ConstraintPrimal(), c1) ≈ [1/√2; 1/√2; 1.0; zeros(n-1)] atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c2))
        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ [1/√2, ub/√2, √ub] atol=atol rtol=rtol

        if config.duals
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(cx))
            @test MOI.get(model, MOI.ConstraintDual(), cx) ≈ zeros(n) atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(cu1))
            @test MOI.get(model, MOI.ConstraintDual(), cu1) ≈ 0.0 atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(cu2))
            @test MOI.get(model, MOI.ConstraintDual(), cu2) ≈ -1/(2*√ub) atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(ct1))
            @test MOI.get(model, MOI.ConstraintDual(), ct1) ≈ -√ub/4 atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(ct2))
            @test MOI.get(model, MOI.ConstraintDual(), ct2) ≈ -√ub/4 atol=atol rtol=rtol

            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c1))
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ [√ub/(2*√2); √ub/(2*√2); -√ub/2; zeros(n-1)] atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c2))
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ [√ub/√2, 1/√(2*ub), -1.0] atol=atol rtol=rtol
        end
    end
end


const rsoctests = Dict("rotatedsoc1v" => rotatedsoc1vtest,
                       "rotatedsoc1f" => rotatedsoc1ftest,
                       "rotatedsoc2"  => rotatedsoc2test,
                       "rotatedsoc3"  => rotatedsoc3test)

@moitestset rsoc

function _geomean1test(model::MOI.ModelLike, config::TestConfig, vecofvars, n=3)
    atol = config.atol
    rtol = config.rtol
    # Problem GeoMean1
    # max (xyz)^(1/3)
    # s.t.
    #      x + y + z ≤ 3
    # in conic form:
    # max t
    # s.t.
    #   (t,x,y,z) ∈ GeometricMeanCone(4)
    #     x+y+z-3 ∈ LessThan(0.)
    # By the arithmetic-geometric mean inequality,
    # (xyz)^(1/3) ≤ (x+y+z)/3 = 1
    # Therefore xyz ≤ 1
    # This can be attained using x = y = z = 1 so it is optimal.

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    if vecofvars
        @test MOI.supportsconstraint(model, MOI.VectorOfVariables, MOI.GeometricMeanCone)
    else
        @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone)
    end
    @test MOI.supportsconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    t = MOI.addvariable!(model)
    MOI.canaddvariable(model)
    x = MOI.addvariables!(model, n)

    vov = MOI.VectorOfVariables([t; x])
    if vecofvars
        @test MOI.canaddconstraint(model, typeof(vov), MOI.GeometricMeanCone)
        gmc = MOI.addconstraint!(model, vov, MOI.GeometricMeanCone(n+1))
    else
        @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone)
        gmc = MOI.addconstraint!(model, MOI.VectorAffineFunction{Float64}(vov), MOI.GeometricMeanCone(n+1))
    end
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    c = MOI.addconstraint!(model, MOI.ScalarAffineFunction(x, ones(n), 0.), MOI.LessThan(Float64(n)))

    @test MOI.get(model, MOI.NumberOfConstraints{vecofvars ? MOI.VectorOfVariables : MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}}()) == 1

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0], [t]), 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)
    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ 1 atol=atol rtol=rtol
        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ ones(n) atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(gmc))
        @test MOI.get(model, MOI.ConstraintPrimal(), gmc) ≈ ones(n+1) atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c))
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ n atol=atol rtol=rtol

    #    if config.duals
    #        @test MOI.canget(model, MOI.DualStatus())
    #        @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
    #
    #        @test MOI.canget(model, MOI.ConstraintDual(), typeof(gmc))
    #        @show MOI.get(model, MOI.ConstraintDual(), gmc)
    #
    #        @test MOI.canget(model, MOI.ConstraintDual(), typeof(c))
    #        @show MOI.get(model, MOI.ConstraintDual(), c)
    #    end
    end
end

geomean1vtest(model::MOI.ModelLike, config::TestConfig) = _geomean1test(model, config, true)
geomean1ftest(model::MOI.ModelLike, config::TestConfig) = _geomean1test(model, config, false)

geomeantests = Dict("geomean1v" => geomean1vtest,
                    "geomean1f" => geomean1ftest)

@moitestset geomean

function _exp1test(model::MOI.ModelLike, config::TestConfig, vecofvars::Bool)
    atol = config.atol
    rtol = config.rtol
    # Problem EXP1 - ExpPrimal
    # min x + y + z
    #  st  y e^(x/y) <= z, y > 0 (i.e (x, y, z) are in the exponential primal cone)
    #      x == 1
    #      y == 2

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    if vecofvars
        @test MOI.supportsconstraint(model, MOI.VectorOfVariables, MOI.ExponentialCone)
    else
        @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone)
    end
    @test MOI.supportsconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    v = MOI.addvariables!(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3

    vov = MOI.VectorOfVariables(v)
    if vecofvars
        @test MOI.canaddconstraint(model, typeof(vov), MOI.ExponentialCone)
        vc = MOI.addconstraint!(model, vov, MOI.ExponentialCone())
    else
        @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone)
        vc = MOI.addconstraint!(model, MOI.VectorAffineFunction{Float64}(vov), MOI.ExponentialCone())
    end

    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
    cx = MOI.addconstraint!(model, MOI.ScalarAffineFunction([v[1]], [1.], 0.), MOI.EqualTo(1.))
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
    cy = MOI.addconstraint!(model, MOI.ScalarAffineFunction([v[2]], [1.], 0.), MOI.EqualTo(2.))

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(v, ones(3), 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
        end

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3 + 2exp(1/2) atol=atol rtol=rtol
        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1., 2., 2exp(1/2)] atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(vc))
        @test MOI.get(model, MOI.ConstraintPrimal(), vc) ≈ [1., 2., 2exp(1/2)] atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(cx))
        @test MOI.get(model, MOI.ConstraintPrimal(), cx) ≈ 1 atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(cy))
        @test MOI.get(model, MOI.ConstraintPrimal(), cy) ≈ 2 atol=atol rtol=rtol

        if config.duals
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(vc))
            u, v, w = MOI.get(model, MOI.ConstraintDual(), vc)
            @test u ≈ -exp(1/2) atol=atol rtol=rtol
            @test v ≈ -exp(1/2)/2 atol=atol rtol=rtol
            @test w ≈ 1 atol=atol rtol=rtol

            @test MOI.canget(model, MOI.ConstraintDual(), typeof(cx))
            @test MOI.get(model, MOI.ConstraintDual(), cx) ≈ 1 + exp(1/2) atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(cy))
            @test MOI.get(model, MOI.ConstraintDual(), cy) ≈ 1 + exp(1/2)/2 atol=atol rtol=rtol
        end
    end
end

exp1vtest(model::MOI.ModelLike, config::TestConfig) = _exp1test(model, config, true)
exp1ftest(model::MOI.ModelLike, config::TestConfig) = _exp1test(model, config, false)

function exp2test(model::MOI.ModelLike, config::TestConfig)
    # Problem EXP2
    # A problem where ECOS was failing
    atol = config.atol
    rtol = config.rtol

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone)
    @test MOI.supportsconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
    @test MOI.supportsconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    v = MOI.addvariables!(model, 9)
    @test MOI.get(model, MOI.NumberOfVariables()) == 9

    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone)
    ec1 = MOI.addconstraint!(model, MOI.VectorAffineFunction([1, 1, 3], [v[2], v[3], v[4]], ones(3), [0., 1., 0.]), MOI.ExponentialCone())
    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone)
    ec2 = MOI.addconstraint!(model, MOI.VectorAffineFunction([1, 1, 3], [v[2], v[3], v[5]], [1., -1., 1.], [0., 1., 0.]), MOI.ExponentialCone())
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
    c1 = MOI.addconstraint!(model, MOI.ScalarAffineFunction([v[4], v[5], v[6]], [.5, .5, -1.], 0.), MOI.EqualTo(0.))
    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)
    c2 = MOI.addconstraint!(model, MOI.VectorAffineFunction([1, 2, 3, 1, 2, 3], [v[1], v[2], v[3], v[7], v[8], v[9]], [ 1.,  1.,  1., 0.3, 0.3, 0.3], zeros(3)), MOI.Nonnegatives(3))
    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)
    c3 = MOI.addconstraint!(model, MOI.VectorAffineFunction([1, 2, 3, 1, 2, 3], [v[1], v[2], v[3], v[7], v[8], v[9]], [-1., -1., -1., 0.3, 0.3, 0.3], zeros(3)), MOI.Nonnegatives(3))
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    c4 = MOI.addconstraint!(model, MOI.ScalarAffineFunction([v[7], v[8], v[9]], ones(3), 0.), MOI.LessThan(1.))
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
    c5 = MOI.addconstraint!(model, MOI.ScalarAffineFunction([v[7]], [1.], 0.), MOI.EqualTo(0.))

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([v[6]], [1.], 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
        end

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ exp(-0.3) atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [0., -0.3, 0., exp(-0.3), exp(-0.3), exp(-0.3), 0., 1.0, 0.] atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(ec1))
        @test MOI.get(model, MOI.ConstraintPrimal(), ec1) ≈ [-0.3, 1.0, exp(-0.3)] atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(ec2))
        @test MOI.get(model, MOI.ConstraintPrimal(), ec2) ≈ [-0.3, 1.0, exp(-0.3)] atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c1))
        @test MOI.get(model, MOI.ConstraintPrimal(), c1) ≈ 0. atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c2))
        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ zeros(3) atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c3))
        @test MOI.get(model, MOI.ConstraintPrimal(), c3) ≈ [0., 0.6, 0.] atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c4))
        @test MOI.get(model, MOI.ConstraintPrimal(), c4) ≈ 1. atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c5))
        @test MOI.get(model, MOI.ConstraintPrimal(), c5) ≈ 0. atol=atol rtol=rtol

        if config.duals
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(ec1))
            @test MOI.get(model, MOI.ConstraintDual(), ec1) ≈ [-exp(-0.3)/2, -1.3exp(-0.3)/2, 0.5] atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(ec2))
            @test MOI.get(model, MOI.ConstraintDual(), ec2) ≈ [-exp(-0.3)/2, -1.3exp(-0.3)/2, 0.5] atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c1))
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ -1 atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c5))
            d5 = MOI.get(model, MOI.ConstraintDual(), c5) # degree of freedom
            d23 = (exp(-0.3)*0.3 - d5) / 0.6 # dual constraint corresponding to v[7]
            @test d23 >= -atol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c2))
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ [d23, exp(-0.3), exp(-0.3)/2] atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c3))
            @test MOI.get(model, MOI.ConstraintDual(), c3) ≈ [d23, 0.0, exp(-0.3)/2] atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c4))
            @test MOI.get(model, MOI.ConstraintDual(), c4) ≈ -exp(-0.3)*0.3 atol=atol rtol=rtol
        end
    end
end

function exp3test(model::MOI.ModelLike, config::TestConfig)
    # Problem EXP3
    # A problem where ECOS was failing
    atol = config.atol
    rtol = config.rtol

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supportsconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    @test MOI.supportsconstraint(model, MOI.SingleVariable,                MOI.LessThan{Float64})
    @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone)

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    x = MOI.addvariable!(model)
    MOI.canaddvariable(model)
    y = MOI.addvariable!(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2

    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    xc = MOI.addconstraint!(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0], [x]), 0.0), MOI.LessThan(4.0))
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.LessThan{Float64})
    yc = MOI.addconstraint!(model, MOI.SingleVariable(y), MOI.LessThan(5.0))
    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone)
    ec = MOI.addconstraint!(model, MOI.VectorAffineFunction([1, 3], [x, y], ones(2), [0.0, 1.0, 0.0]), MOI.ExponentialCone())

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0], [x]), 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
        end

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ log(5) atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ log(5) atol=atol rtol=rtol
        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), y) ≈ 5. atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(xc))
        @test MOI.get(model, MOI.ConstraintPrimal(), xc) ≈ 2log(5) atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(yc))
        @test MOI.get(model, MOI.ConstraintPrimal(), yc) ≈ 5 atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(ec))
        @test MOI.get(model, MOI.ConstraintPrimal(), ec) ≈ [log(5), 1., 5.] atol=atol rtol=rtol

        if config.duals
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(xc))
            @test MOI.get(model, MOI.ConstraintDual(), xc) ≈ 0. atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(yc))
            @test MOI.get(model, MOI.ConstraintDual(), yc) ≈ -1/5 atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(ec))
            @test MOI.get(model, MOI.ConstraintDual(), ec) ≈ [-1., log(5)-1, 1/5] atol=atol rtol=rtol
        end
    end
end

exptests = Dict("exp1v" => exp1vtest,
                "exp1f" => exp1ftest,
                "exp2"  => exp2test,
                "exp3"  => exp3test)

@moitestset exp

function _psd0test(model::MOI.ModelLike, vecofvars::Bool, psdcone, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    square = psdcone == MOI.PositiveSemidefiniteConeSquare
    # min X[1,1] + X[2,2]    max y
    #     X[2,1] = 1         [0   y/2     [ 1  0
    #                         y/2 0    <=   0  1]
    #     X >= 0              y free
    # Optimal solution:
    #     ⎛ 1   1 ⎞
    # X = ⎜       ⎟           y = 2
    #     ⎝ 1   1 ⎠

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    if vecofvars
        @test MOI.supportsconstraint(model, MOI.VectorOfVariables, psdcone)
    else
        @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, psdcone)
    end
    @test MOI.supportsconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    X = MOI.addvariables!(model, square ? 4 : 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == (square ? 4 : 3)

    vov = MOI.VectorOfVariables(X)
    if vecofvars
        @test MOI.canaddconstraint(model, typeof(vov), psdcone)
        cX = MOI.addconstraint!(model, vov, psdcone(2))
    else
        @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, psdcone)
        cX = MOI.addconstraint!(model, MOI.VectorAffineFunction{Float64}(vov), psdcone(2))
    end

    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
    c = MOI.addconstraint!(model, MOI.ScalarAffineFunction([X[2]], [1.], 0.), MOI.EqualTo(1.))

    @test MOI.get(model, MOI.NumberOfConstraints{vecofvars ? MOI.VectorOfVariables : MOI.VectorAffineFunction{Float64}, psdcone}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}}()) == 1

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([X[1], X[end]], ones(2), 0.))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)
    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
        end

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2 atol=atol rtol=rtol

        Xv = square ? ones(4) : ones(3)
        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), X) ≈ Xv atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(cX))
        @test MOI.get(model, MOI.ConstraintPrimal(), cX) ≈ Xv atol=atol rtol=rtol

        if config.duals
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c))
            @test MOI.get(model, MOI.ConstraintDual(), c) ≈ 2 atol=atol rtol=rtol

            cXv = square ? [1, -1, -1, 1] : [1, -1, 1]
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(cX))
            @test MOI.get(model, MOI.ConstraintDual(), cX) ≈ cXv atol=atol rtol=rtol
        end
    end
end

psdt0vtest(model::MOI.ModelLike, config::TestConfig) = _psd0test(model, true, MOI.PositiveSemidefiniteConeTriangle, config)
psdt0ftest(model::MOI.ModelLike, config::TestConfig) = _psd0test(model, false, MOI.PositiveSemidefiniteConeTriangle, config)
psds0vtest(model::MOI.ModelLike, config::TestConfig) = _psd0test(model, true, MOI.PositiveSemidefiniteConeSquare, config)
psds0ftest(model::MOI.ModelLike, config::TestConfig) = _psd0test(model, false, MOI.PositiveSemidefiniteConeSquare, config)

function _psd1test(model::MOI.ModelLike, vecofvars::Bool, psdcone, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    square = psdcone == MOI.PositiveSemidefiniteConeSquare
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
    #      X in C_psd
    #
    # The dual is
    # max y1 + y2/2
    #
    # s.t. | y1+y2    y2    y2 |
    #      |    y2 y1+y2    y2 | in C_psd
    #      |    y2    y2 y1+y2 |
    #
    #      (1-y1, -y2, -y2) in C^3_q
    #
    # The dual of the SDP constraint is rank two of the form
    # [γ, 0, -γ] * [γ, 0, γ'] + [δ, ε, δ] * [δ, ε, δ]'
    # and the dual of the SOC constraint is of the form (√2*y2, -y2, -y2)
    #
    # The feasible set of the constraint dual contains only four points.
    # Eliminating, y1, y2 and γ from the dual constraints gives
    # -ε^2 + -εδ + 2δ^2 + 1
    # (√2-2)ε^2 + (-2√2+2)δ^2 + 1
    # Eliminating ε from this set of equation give
    # (-6√2+4)δ^4 + (3√2-2)δ^2 + (2√2-3)
    # from which we find the solution
    δ = √(1 + (3*√2+2)*√(-116*√2+166) / 14) / 2
    # which is optimal
    ε = √((1 - 2*(√2-1)*δ^2) / (2-√2))
    y2 = 1 - ε*δ
    y1 = 1 - √2*y2
    obj = y1 + y2/2
    # The primal solution is rank one of the form
    # X = [α, β, α] * [α, β, α]'
    # and by complementary slackness, x is of the form (√2*x2, x2, x2)
    # The primal reduces to
    #      4α^2+4αβ+2β^2+√2*x2= obj
    #      2α^2    + β^2+√2*x2 = 1 (1)
    #      8α^2+8αβ+2β^2+ 4 x2 = 1
    # Eliminating β, we get
    # 4α^2 + 4x2 = 3 - 2obj (2)
    # By complementary slackness, we have β = kα where
    k = -2*δ/ε
    # Replacing β by kα in (1) allows to eliminate α^2 in (2) to get
    x2 = ((3-2obj)*(2+k^2)-4) / (4*(2+k^2)-4*√2)
    # With (2) we get
    α = √(3-2obj-4x2)/2
    β = k*α

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supportsconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
    if vecofvars
        @test MOI.supportsconstraint(model, MOI.VectorOfVariables, psdcone)
    else
        @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, psdcone)
    end
    @test MOI.supportsconstraint(model, MOI.VectorOfVariables, MOI.SecondOrderCone)

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    X = MOI.addvariables!(model, square ? 9 : 6)
    @test MOI.get(model, MOI.NumberOfVariables()) == (square ? 9 : 6)
    MOI.canaddvariable(model)
    x = MOI.addvariables!(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == (square ? 12 : 9)

    vov = MOI.VectorOfVariables(X)
    if vecofvars
        @test MOI.canaddconstraint(model, typeof(vov), psdcone)
        cX = MOI.addconstraint!(model, vov, psdcone(3))
    else
        @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, psdcone)
        cX = MOI.addconstraint!(model, MOI.VectorAffineFunction{Float64}(vov), psdcone(3))
    end
    @test MOI.canaddconstraint(model, MOI.VectorOfVariables, MOI.SecondOrderCone)
    cx = MOI.addconstraint!(model, MOI.VectorOfVariables(x), MOI.SecondOrderCone(3))

    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
    c1 = MOI.addconstraint!(model, MOI.ScalarAffineFunction([X[1], X[square ? 5 : 3], X[end], x[1]], [1., 1, 1, 1], 0.), MOI.EqualTo(1.))
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
    c2 = MOI.addconstraint!(model, MOI.ScalarAffineFunction([X; x[2]; x[3]], square ? ones(11) : [1., 2, 1, 2, 2, 1, 1, 1], 0.), MOI.EqualTo(1/2))

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    objXidx = square ? [1:2; 4:6; 8:9] : [1:3; 5:6]
    objXcoefs = square ? [2., 1., 1., 2., 1., 1., 2.] : 2*ones(5)
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([X[objXidx]; x[1]], [objXcoefs; 1.], 0.))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    @test MOI.get(model, MOI.NumberOfConstraints{vecofvars ? MOI.VectorOfVariables : MOI.VectorAffineFunction{Float64}, psdcone}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}}()) == 2
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorOfVariables, MOI.SecondOrderCone}()) == 1

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
        end

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ obj atol=atol rtol=rtol

        Xv = square ? [α^2, α*β, α^2, α*β, β^2, α*β, α^2, α*β, α^2] : [α^2, α*β, β^2, α^2, α*β, α^2]
        xv = [√2*x2, x2, x2]
        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), X) ≈ Xv atol=atol rtol=rtol
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ xv atol=atol rtol=rtol

        @test MOI.get(model, MOI.ConstraintPrimal(), cX) ≈ Xv atol=atol rtol=rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), cx) ≈ xv atol=atol rtol=rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c1) ≈ 1. atol=atol rtol=rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ .5 atol=atol rtol=rtol

        if config.duals
            cX0 = 1+(√2-1)*y2
            cX1 = 1-y2
            cX2 = -y2
            cXv = square ? [cX0, cX1, cX2, cX1, cX0, cX1, cX2, cX1, cX0] : [cX0, cX1, cX0, cX2, cX1, cX0]
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(cX))
            @test MOI.get(model, MOI.ConstraintDual(), cX) ≈ cXv atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(cx))
            @test MOI.get(model, MOI.ConstraintDual(), cx) ≈ [1-y1, -y2, -y2] atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c1))
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ y1 atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c2))
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ y2 atol=atol rtol=rtol
        end
    end
end

psdt1vtest(model::MOI.ModelLike, config::TestConfig) = _psd1test(model, true, MOI.PositiveSemidefiniteConeTriangle, config)
psdt1ftest(model::MOI.ModelLike, config::TestConfig) = _psd1test(model, false, MOI.PositiveSemidefiniteConeTriangle, config)
psds1vtest(model::MOI.ModelLike, config::TestConfig) = _psd1test(model, true, MOI.PositiveSemidefiniteConeSquare, config)
psds1ftest(model::MOI.ModelLike, config::TestConfig) = _psd1test(model, false, MOI.PositiveSemidefiniteConeSquare, config)

function psdt2test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # Caused getdual to fail on SCS and Mosek

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supportsconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})
    @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonpositives)
    @test MOI.supportsconstraint(model, MOI.VectorOfVariables, MOI.PositiveSemidefiniteConeTriangle)
    @test MOI.supportsconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    x = MOI.addvariables!(model, 7)
    @test MOI.get(model, MOI.NumberOfVariables()) == 7

    η = 10.0
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})
    c1 = MOI.addconstraint!(model, MOI.ScalarAffineFunction(x[1:6], -ones(6), η), MOI.GreaterThan(0.0))
    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonpositives)
    c2 = MOI.addconstraint!(model, MOI.VectorAffineFunction(collect(1:6), x[1:6], -ones(6), zeros(6)), MOI.Nonpositives(6))
    α = 0.8
    δ = 0.9
    c3 = MOI.addconstraint!(model, MOI.VectorAffineFunction([fill(1, 7); fill(2, 5);     fill(3, 6)],
                                                               [x[1:7];     x[1:3]; x[5:6]; x[1:3]; x[5:7]],
                                                               [ δ/2,       α,   δ, δ/4, δ/8,      0.0, -1.0,
                                                                -δ/(2*√2), -δ/4, 0,     -δ/(8*√2), 0.0,
                                                                 δ/2,     δ-α,   0,      δ/8,      δ/4, -1.0],
                                                               zeros(3)), MOI.PositiveSemidefiniteConeTriangle(2))
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
    c4 = MOI.addconstraint!(model, MOI.ScalarAffineFunction([x[1:3]; x[5:6]], zeros(5), 0.0), MOI.EqualTo(0.))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.Nonpositives}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}}()) == 1

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([x[7]], [1.], 0.))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)
    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        if config.duals
            @test MOI.canget(model, MOI.DualStatus())
            @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
        end

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ [2η/3, 0, η/3, 0, 0, 0, η*δ*(1 - 1/√3)/2] atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c1))
        @test MOI.get(model, MOI.ConstraintPrimal(), c1) ≈ 0.0 atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c2))
        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ [-2η/3, 0, -η/3, 0, 0, 0] atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c3))
        @test MOI.get(model, MOI.ConstraintPrimal(), c3) ≈ [η*δ*(1/√3+1/3)/2, -η*δ/(3*√2), η*δ*(1/√3-1/3)/2] atol=atol rtol=rtol
        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c4))
        @test MOI.get(model, MOI.ConstraintPrimal(), c4) ≈ 0.0 atol=atol rtol=rtol

        if config.duals
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c1))
            @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ δ*(1-1/√3)/2 atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c2))
            @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ [0, -α/√3+δ/(2*√6)*(2*√2-1), 0, -3δ*(1-1/√3)/8, -3δ*(1-1/√3)/8, -δ*(3 - 2*√3 + 1/√3)/8] atol=atol rtol=rtol
            @test MOI.canget(model, MOI.ConstraintDual(), typeof(c3))
            @test MOI.get(model, MOI.ConstraintDual(), c3) ≈ [(1-1/√3)/2, 1/√6, (1+1/√3)/2] atol=atol rtol=rtol
            # Dual of c4 could be anything
        end
    end
end

# PSDConeTriangle
const psdttests = Dict("psdt0v" => psdt0vtest,
                       "psdt0f" => psdt0ftest,
                       "psdt1v" => psdt1vtest,
                       "psdt1f" => psdt1ftest,
                       "psdt2"  => psdt2test)

# PSDConeSquare
@moitestset psdt

const psdstests = Dict("psds0v" => psdt0vtest,
                       "psds0v" => psdt0vtest,
                       "psds1v" => psds1vtest,
                       "psds1f" => psds1ftest)

@moitestset psds

const sdptests = Dict("psdt" => psdttest,
                      "psds" => psdstest)

@moitestset sdp true

function _det1test(model::MOI.ModelLike, config::TestConfig, vecofvars::Bool, detcone)
    atol = config.atol
    rtol = config.rtol
    square = detcone == MOI.LogDetConeSquare || detcone == MOI.RootDetConeSquare
    logdet = detcone == MOI.LogDetConeTriangle || detcone == MOI.LogDetConeSquare
    # We look for an ellipsoid x^T P x ≤ 1 contained in the square.
    # Let Q = inv(P) (x^T Q x ≤ 1 is its polar ellipsoid), we have
    # max t
    #     t <= log det Q (or t <= (det Q)^(1/n))
    #             Q22 ≤ 1
    #            _________
    #           |         |
    #           |         |
    # -Q11 ≥ -1 |    +    | Q11 ≤ 1
    #           |         |
    #           |_________|
    #            -Q22 ≥ -1

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    if vecofvars
        @test MOI.supportsconstraint(model, MOI.VectorOfVariables, detcone)
    else
        @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, detcone)
    end
    @test MOI.supportsconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    t = MOI.addvariable!(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    MOI.canaddvariable(model)
    Q = MOI.addvariables!(model, square ? 4 : 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == (square ? 5 : 4)

    vov = MOI.VectorOfVariables([t; Q])
    if vecofvars
        @test MOI.canaddconstraint(model, typeof(vov), detcone)
        cX = MOI.addconstraint!(model, vov, detcone(2))
    else
        @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, detcone)
        cX = MOI.addconstraint!(model, MOI.VectorAffineFunction{Float64}(vov), detcone(2))
    end

    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)
    c = MOI.addconstraint!(model, MOI.VectorAffineFunction(collect(1:2), [Q[1], Q[end]], [-1., -1.], ones(2)), MOI.Nonnegatives(2))

    @test MOI.get(model, MOI.NumberOfConstraints{vecofvars ? MOI.VectorOfVariables : MOI.VectorAffineFunction{Float64}, detcone}()) == 1
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives}()) == 1

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([t], ones(1), 0.))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)
    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        expectedobjval = logdet ? 0. : 1.
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ expectedobjval atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), t) ≈ expectedobjval atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        Qv = MOI.get(model, MOI.VariablePrimal(), Q)
        @test Qv[1] ≈ 1. atol=atol rtol=rtol
        @test Qv[2] ≈ 0. atol=atol rtol=rtol
        if square
            @test Qv[3] ≈ 0. atol=atol rtol=rtol
        end
        @test Qv[end] ≈ 1. atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(cX))
        tQv = MOI.get(model, MOI.ConstraintPrimal(), cX)
        @test tQv[1] ≈ expectedobjval atol=atol rtol=rtol
        @test tQv[2:end] ≈ Qv atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c))
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ [0., 0.] atol=atol rtol=rtol
    end
end

logdett1vtest(model::MOI.ModelLike, config::TestConfig) = _det1test(model, config, true, MOI.LogDetConeTriangle)
logdett1ftest(model::MOI.ModelLike, config::TestConfig) = _det1test(model, config, false, MOI.LogDetConeTriangle)
logdets1vtest(model::MOI.ModelLike, config::TestConfig) = _det1test(model, config, true, MOI.LogDetConeSquare)
logdets1ftest(model::MOI.ModelLike, config::TestConfig) = _det1test(model, config, false, MOI.LogDetConeSquare)

const logdetttests = Dict("logdett1v" => logdett1vtest,
                          "logdett1f" => logdett1ftest)

@moitestset logdett

const logdetstests = Dict("logdets1v" => logdets1vtest,
                          "logdets1f" => logdets1ftest)

@moitestset logdets

const logdettests = Dict("logdett" => logdetttest,
                         "logdets" => logdetstest)

@moitestset logdet true

rootdett1vtest(model::MOI.ModelLike, config::TestConfig) = _det1test(model, config, true, MOI.RootDetConeTriangle)
rootdett1ftest(model::MOI.ModelLike, config::TestConfig) = _det1test(model, config, false, MOI.RootDetConeTriangle)
rootdets1vtest(model::MOI.ModelLike, config::TestConfig) = _det1test(model, config, true, MOI.RootDetConeSquare)
rootdets1ftest(model::MOI.ModelLike, config::TestConfig) = _det1test(model, config, false, MOI.RootDetConeSquare)

const rootdetttests = Dict("rootdett1v" => rootdett1vtest,
                           "rootdett1f" => rootdett1ftest)

@moitestset rootdett

const rootdetstests = Dict("rootdets1v" => rootdets1vtest,
                           "rootdets1f" => rootdets1ftest)

@moitestset rootdets

const rootdettests = Dict("rootdett" => rootdetttest,
                          "rootdets" => rootdetstest)

@moitestset rootdet true

const contconictests = Dict("lin" => lintest,
                            "soc" => soctest,
                            "rsoc" => rsoctest,
                            "geomean" => geomeantest,
                            "exp" => exptest,
                            "sdp" => sdptest,
                            "logdet" => logdettest,
                            "rootdet" => rootdettest)

@moitestset contconic true
