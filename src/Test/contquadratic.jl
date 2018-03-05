# Continuous quadratic problems

function qp1test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    @testset "QP1 - Quadratic objective" begin
        # simple quadratic objective
        # Min x^2 + xy + y^2 + yz + z^2
        # st  x + 2y + 3z >= 4 (c1)
        #     x +  y      >= 1 (c2)
        #     x,y \in R

        #@test MOI.supportsproblem(model, MOI.ScalarQuadraticFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64})])

        MOI.empty!(model)
        @test MOI.isempty(model)

        MOI.canaddvariable(model)
        v = MOI.addvariables!(model, 3)
        @test MOI.get(model, MOI.NumberOfVariables()) == 3

        cf1 = MOI.ScalarAffineFunction(v, [1.0,2.0,3.0], 0.0)
        @test MOI.canaddconstraint(model, typeof(cf1), MOI.GreaterThan{Float64})
        c1 = MOI.addconstraint!(model, cf1, MOI.GreaterThan(4.0))
        @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}()) == 1

        @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})
        c2 = MOI.addconstraint!(model, MOI.ScalarAffineFunction([v[1],v[2]], [1.0,1.0], 0.0), MOI.GreaterThan(1.0))
        @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}()) == 2

        obj = MOI.ScalarQuadraticFunction(MOI.VariableIndex[], Float64[], v[[1,1,2,2,3]], v[[1,2,2,3,3]], [2.0, 1.0, 2.0, 1.0, 2.0], 0.0)
        @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}())
        MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(), obj)
        @test MOI.canset(model, MOI.ObjectiveSense())
        MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)
        @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MinSense

        if config.query
            @test MOI.canget(model, MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}())
            @test obj ≈ MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}())

            @test MOI.canget(model, MOI.ConstraintFunction(), typeof(c1))
            @test cf1 ≈ MOI.get(model, MOI.ConstraintFunction(), c1)

            @test MOI.canget(model, MOI.ConstraintSet(), typeof(c1))
            @test MOI.GreaterThan(4.0) == MOI.get(model, MOI.ConstraintSet(), c1)
        end

        if config.solve
            MOI.optimize!(model)

            @test MOI.canget(model, MOI.TerminationStatus())
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

            @test MOI.canget(model, MOI.PrimalStatus())
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

            @test MOI.canget(model, MOI.ObjectiveValue())
            @test MOI.get(model, MOI.ObjectiveValue()) ≈ 13/7 atol=atol rtol=rtol

            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [4/7,3/7,6/7] atol=atol rtol=rtol
        end
    end
end

function qp2test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    @testset "QP2" begin
        # same as QP0 but with duplicate terms
        # then change the objective and sense
        # simple quadratic objective
        # Min x^2 + xy + y^2 + yz + z^2
        # st  x + 2y + 3z >= 4 (c1)
        #     x +  y      >= 1 (c2)
        #     x,y \in R

        #@test MOI.supportsproblem(model, MOI.ScalarQuadraticFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64})])

        MOI.empty!(model)
        @test MOI.isempty(model)

        MOI.canaddvariable(model)
        v = MOI.addvariables!(model, 3)
        @test MOI.get(model, MOI.NumberOfVariables()) == 3

        c1f = MOI.ScalarAffineFunction(v, [1.0,2.0,3.0], 0.0)
        @test MOI.canaddconstraint(model, typeof(c1f), MOI.GreaterThan{Float64})
        c1 = MOI.addconstraint!(model, c1f, MOI.GreaterThan(4.0))
        @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}()) == 1

        @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})
        c2 = MOI.addconstraint!(model, MOI.ScalarAffineFunction([v[1],v[2]], [1.0,1.0], 0.0), MOI.GreaterThan(1.0))
        @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}()) == 2

        obj = MOI.ScalarQuadraticFunction(v, [0.0,0.0,0.0],[v[1], v[1], v[1], v[2], v[2], v[3], v[3]], [v[1], v[2], v[2], v[2], v[3], v[3], v[3]], [2.0, 0.5, 0.5, 2.0, 1.0, 1.0, 1.0], 0.0)
        @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}())
        MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(), obj)
        @test MOI.canset(model, MOI.ObjectiveSense())
        MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)
        @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MinSense

        if config.query
            @test MOI.canget(model, MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}())
            @test obj ≈ MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}())

            @test MOI.canget(model, MOI.ConstraintFunction(), typeof(c1))
            @test c1f ≈ MOI.get(model, MOI.ConstraintFunction(), c1)

            @test MOI.canget(model, MOI.ConstraintSet(), typeof(c1))
            @test MOI.GreaterThan(4.0) == MOI.get(model, MOI.ConstraintSet(), c1)
        end

        if config.solve
            MOI.optimize!(model)

            @test MOI.canget(model, MOI.TerminationStatus())
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

            @test MOI.canget(model, MOI.PrimalStatus())
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

            @test MOI.canget(model, MOI.ObjectiveValue())
            @test MOI.get(model, MOI.ObjectiveValue()) ≈ 13/7 atol=atol rtol=rtol

            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [4/7,3/7,6/7] atol=atol rtol=rtol
        end

        # change objective to Max -2(x^2 + xy + y^2 + yz + z^2)
        obj2 = MOI.ScalarQuadraticFunction(v, [0.0,0.0,0.0],[v[1], v[1], v[1], v[2], v[2], v[3], v[3]], [v[1], v[2], v[2], v[2], v[3], v[3], v[3]], [-4.0, -1.0, -1.0, -4.0, -2.0, -2.0, -2.0], 0.0)
        @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}())
        MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(), obj2)
        @test MOI.canset(model, MOI.ObjectiveSense())
        MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)
        @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MaxSense

        if config.query
            @test MOI.canget(model, MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}())
            @test obj2 ≈ MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}())
        end

        if config.solve
            MOI.optimize!(model)

            @test MOI.canget(model, MOI.TerminationStatus())
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

            @test MOI.canget(model, MOI.PrimalStatus())
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

            @test MOI.canget(model, MOI.ObjectiveValue())
            @test MOI.get(model, MOI.ObjectiveValue()) ≈ -2*13/7 atol=atol rtol=rtol

            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [4/7,3/7,6/7] atol=atol rtol=rtol
        end
    end
end

function qp3test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    @testset "qp3test - Linear Quadratic objective" begin
        # simple quadratic objective
        #    minimize 2 x^2 + y^2 + xy + x + y + 1
        #       s.t.  x, y >= 0
        #             x + y = 1

        #@test MOI.supportsproblem(model, MOI.ScalarQuadraticFunction{Float64},
        #    [
        #        (MOI.SingleVariable,MOI.GreaterThan{Float64}),
        #        (MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64})
        #    ]
        #)

        MOI.empty!(model)
        @test MOI.isempty(model)

        MOI.canaddvariable(model)
        x = MOI.addvariable!(model)
        MOI.canaddvariable(model)
        y = MOI.addvariable!(model)

        MOI.addconstraint!(model,
            MOI.ScalarAffineFunction([x,y], [1.0,1.0], 0.0),
            MOI.EqualTo(1.0)
        )

        @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
        MOI.addconstraint!(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
        @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
        MOI.addconstraint!(model, MOI.SingleVariable(y), MOI.GreaterThan(0.0))

        obj = MOI.ScalarQuadraticFunction(
                [x,y], [1.0,1.0],
                [x,y,x], [x,y,y], [4.0, 2.0, 1.0],
                1.0
              )
        @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}())
        MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(), obj)
        @test MOI.canset(model, MOI.ObjectiveSense())
        MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

        if config.solve
            MOI.optimize!(model)

            @test MOI.canget(model, MOI.TerminationStatus())
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

            @test MOI.canget(model, MOI.PrimalStatus())
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

            @test MOI.canget(model, MOI.ObjectiveValue())
            @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2.875 atol=atol rtol=rtol
            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            @test MOI.get(model, MOI.VariablePrimal(), [x,y]) ≈ [0.25, 0.75] atol=atol rtol=rtol
        end

        # change back to linear
        #        max 2x + y + 1
        #       s.t.  x, y >= 0
        #             x + y = 1
        # (x,y) = (1,0), obj = 3
        objf = MOI.ScalarAffineFunction([x,y], [2.0,1.0], 1.0)
        @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
        MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
        @test MOI.canset(model, MOI.ObjectiveSense())
        MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)

        if config.solve
            MOI.optimize!(model)

            @test MOI.canget(model, MOI.TerminationStatus())
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

            @test MOI.canget(model, MOI.PrimalStatus())
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

            @test MOI.canget(model, MOI.ObjectiveValue())
            @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3.0 atol=atol rtol=rtol
            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            @test MOI.get(model, MOI.VariablePrimal(), [x,y]) ≈ [1.0, 0.0] atol=atol rtol=rtol
        end

    end
end

const qptests = Dict("qp1" => qp1test,
                     "qp2" => qp2test,
                     "qp3" => qp3test)

@moitestset qp

#=
    Quadratically constrained programs
=#

function qcp1test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    @testset "qcp1" begin
        # quadratic constraint
        # Max x + y
        # st  - x + y >= 0 (c1[1])
        #       x + y >= 0 (c1[2])
        #     0.5x^2 + y <= 2 (c2)

        #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64}, [(MOI.VectorAffineFunction{Float64},MOI.Nonnegatives),(MOI.ScalarQuadraticFunction{Float64},MOI.LessThan{Float64})])

        MOI.empty!(model)
        @test MOI.isempty(model)

        MOI.canaddvariable(model)
        x = MOI.addvariable!(model)
        MOI.canaddvariable(model)
        y = MOI.addvariable!(model)
        @test MOI.get(model, MOI.NumberOfVariables()) == 2

        @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)
        c1 = MOI.addconstraint!(model, MOI.VectorAffineFunction([1,1,2,2], [x,y,x,y], [-1.0,1.0,1.0,1.0], [0.0,0.0]), MOI.Nonnegatives(2))
        @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives}()) == 1

        c2f = MOI.ScalarQuadraticFunction([y],[1.0],[x],[x],[1.0], 0.0)
        @test MOI.canaddconstraint(model, typeof(c2f), MOI.LessThan{Float64})
        c2 = MOI.addconstraint!(model, c2f, MOI.LessThan(2.0))
        @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}}()) == 1

        @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
        MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([x,y], [1.0,1.0], 0.0))
        @test MOI.canset(model, MOI.ObjectiveSense())
        MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)
        @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MaxSense

        if config.query
            @test MOI.canget(model, MOI.ConstraintFunction(), typeof(c2))
            @test c2f ≈ MOI.get(model, MOI.ConstraintFunction(), c2)
        end

        if config.solve
            MOI.optimize!(model)

            @test MOI.canget(model, MOI.TerminationStatus())
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

            @test MOI.canget(model, MOI.PrimalStatus())
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

            @test MOI.canget(model, MOI.ObjectiveValue())
            @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2.25 atol=atol rtol=rtol

            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            @test MOI.get(model, MOI.VariablePrimal(), [x,y]) ≈ [0.5,1.75] atol=atol rtol=rtol
        end

        # try delete quadratic constraint and go back to linear

        # MOI.delete!(model, c2)
        #
        # MOI.optimize!(model)
        #
        # @test MOI.canget(model, MOI.TerminationStatus())
        # @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success
        #
        # @test MOI.canget(model, MOI.PrimalStatus())
        # @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
        #
        # @test MOI.canget(model, MOI.ObjectiveValue())
        # @test MOI.get(model, MOI.ObjectiveValue()) ≈ 0.0 atol=atol rtol=rtol
    end
end


function qcp2test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    @testset "qcp2" begin
        # Max x
        # s.t. x^2 <= 2 (c)

        #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarQuadraticFunction{Float64},MOI.LessThan{Float64})])

        MOI.empty!(model)
        @test MOI.isempty(model)

        MOI.canaddvariable(model)
        x = MOI.addvariable!(model)
        @test MOI.get(model, MOI.NumberOfVariables()) == 1

        cf = MOI.ScalarQuadraticFunction(MOI.VariableIndex[x],Float64[0.0],[x],[x],[1.0], 0.0)
        @test MOI.canaddconstraint(model, typeof(cf), MOI.LessThan{Float64})
        c = MOI.addconstraint!(model, cf, MOI.LessThan(2.0))
        @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}}()) == 1

        @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
        MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([x], [1.0], 0.0))
        @test MOI.canset(model, MOI.ObjectiveSense())
        MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)
        @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MaxSense

        if config.query
            @test MOI.canget(model, MOI.ConstraintFunction(), typeof(c))
            @test cf ≈ MOI.get(model, MOI.ConstraintFunction(), c)
        end

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
            @test MOI.get(model, MOI.ObjectiveValue()) ≈ sqrt(2) atol=atol rtol=rtol

            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            @test MOI.get(model, MOI.VariablePrimal(), x) ≈ sqrt(2) atol=atol rtol=rtol

            # TODO - duals
            # @test MOI.canget(model, MOI.ConstraintDual(), typeof(c))
            # @test MOI.get(model, MOI.ConstraintDual(), c) ≈ 0.5/sqrt(2) atol=atol rtol=rtol
        end
    end
end

function qcp3test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    @testset "qcp3" begin
        # Min -x
        # s.t. x^2 <= 2

        #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarQuadraticFunction{Float64},MOI.LessThan{Float64})])

        MOI.empty!(model)
        @test MOI.isempty(model)

        MOI.canaddvariable(model)
        x = MOI.addvariable!(model)
        @test MOI.get(model, MOI.NumberOfVariables()) == 1

        cf = MOI.ScalarQuadraticFunction(MOI.VariableIndex[],Float64[],[x],[x],[1.0], 0.0)
        @test MOI.canaddconstraint(model, typeof(cf), MOI.LessThan{Float64})
        c = MOI.addconstraint!(model, cf, MOI.LessThan(2.0))
        @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}}()) == 1

        @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
        MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([x], [-1.0], 0.0))
        @test MOI.canset(model, MOI.ObjectiveSense())
        MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)
        @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MinSense

        if config.query
            @test MOI.canget(model, MOI.ConstraintFunction(), typeof(c))
            @test cf ≈ MOI.get(model, MOI.ConstraintFunction(), c)
        end

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
            @test MOI.get(model, MOI.ObjectiveValue()) ≈ -sqrt(2) atol=atol rtol=rtol

            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            @test MOI.get(model, MOI.VariablePrimal(), x) ≈ sqrt(2) atol=atol rtol=rtol

            # TODO - duals
            # @test MOI.canget(model, MOI.ConstraintDual(), typeof(c))
            # @test MOI.get(model, MOI.ConstraintDual(), c) ≈ -0.5/sqrt(2) atol=atol rtol=rtol
        end
    end
end

const qcptests = Dict("qcp1" => qcp1test,
                      "qcp2" => qcp2test,
                      "qcp3" => qcp3test)

@moitestset qcp

#=
    SOCP
=#

function socp1test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    @testset "socp1" begin
        # min t
        # s.t. x + y >= 1 (c1)
        #      x^2 + y^2 <= t^2 (c2)
        #      t >= 0 (bound)

        #@test MOI.supportsproblem(model, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarQuadraticFunction{Float64},MOI.LessThan{Float64}), (MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}), (MOI.SingleVariable,MOI.GreaterThan{Float64})])

        MOI.empty!(model)
        @test MOI.isempty(model)

        MOI.canaddvariable(model)
        x = MOI.addvariable!(model)
        MOI.canaddvariable(model)
        y = MOI.addvariable!(model)
        MOI.canaddvariable(model)
        t = MOI.addvariable!(model)
        @test MOI.get(model, MOI.NumberOfVariables()) == 3

        c1f = MOI.ScalarAffineFunction([x,y],[1.0, 1.0], 0.0)
        @test MOI.canaddconstraint(model, typeof(c1f), MOI.GreaterThan{Float64})
        c1 = MOI.addconstraint!(model, c1f, MOI.GreaterThan(1.0))
        @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}()) == 1

        c2f = MOI.ScalarQuadraticFunction(MOI.VariableIndex[],Float64[],[x,y,t],[x,y,t],[1.0,1.0,-1.0], 0.0)
        @test MOI.canaddconstraint(model, typeof(c2f), MOI.LessThan{Float64})
        c2 = MOI.addconstraint!(model, c2f, MOI.LessThan(0.0))
        @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}}()) == 1

        @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
        bound = MOI.addconstraint!(model, MOI.SingleVariable(t), MOI.GreaterThan(0.0))
        @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable, MOI.GreaterThan{Float64}}()) == 1

        @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
        MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction([t], [1.0], 0.0))
        @test MOI.canset(model, MOI.ObjectiveSense())
        MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)
        @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MinSense

        if config.query
            @test MOI.canget(model, MOI.ConstraintFunction(), typeof(c1))
            @test c1f ≈ MOI.get(model, MOI.ConstraintFunction(), c1)

            @test MOI.canget(model, MOI.ConstraintFunction(), typeof(c2))
            @test c2f ≈ MOI.get(model, MOI.ConstraintFunction(), c2)
        end

        if config.solve
            MOI.optimize!(model)

            @test MOI.canget(model, MOI.TerminationStatus())
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

            @test MOI.canget(model, MOI.PrimalStatus())
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

            @test MOI.canget(model, MOI.ObjectiveValue())
            @test MOI.get(model, MOI.ObjectiveValue()) ≈ sqrt(1/2) atol=atol rtol=rtol

            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            @test MOI.get(model, MOI.VariablePrimal(), [x,y,t]) ≈ [0.5,0.5,sqrt(1/2)] atol=atol rtol=rtol

            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            @test MOI.get(model, MOI.VariablePrimal(), [t,x,y,t]) ≈ [sqrt(1/2),0.5,0.5,sqrt(1/2)] atol=atol rtol=rtol
        end
    end
end

const socptests = Dict("socp1" => socp1test)

@moitestset socp

const contquadratictests = Dict("qp" => qptest,
                                "qcp" => qcptest,
                                "socp" => socptest)

@moitestset contquadratic true
