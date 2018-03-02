# MIP01 from CPLEX.jl
function int1test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # an example on mixed integer programming
    #
    #   maximize 1.1x + 2 y + 5 z
    #
    #   s.t.  x + y + z <= 10
    #         x + 2 y + z <= 15
    #
    #         x is continuous: 0 <= x <= 5
    #         y is integer: 0 <= y <= 10
    #         z is binary

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supportsconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    @test MOI.supportsconstraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    @test MOI.supportsconstraint(model, MOI.SingleVariable, MOI.ZeroOne)
    @test MOI.supportsconstraint(model, MOI.SingleVariable, MOI.Integer)

    MOI.empty!(model)
    @test MOI.isempty(model)

    MOI.canaddvariable(model)
    v = MOI.addvariables!(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3

    cf = MOI.ScalarAffineFunction(v, [1.0,1.0,1.0], 0.0)
    @test MOI.canaddconstraint(model, typeof(cf), MOI.LessThan{Float64})
    c = MOI.addconstraint!(model, cf, MOI.LessThan(10.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

    cf2 = MOI.ScalarAffineFunction(v, [1.0,2.0,1.0], 0.0)
    @test MOI.canaddconstraint(model, typeof(cf2), MOI.LessThan{Float64})
    c2 = MOI.addconstraint!(model, cf2, MOI.LessThan(15.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 2


    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.Interval{Float64})
    MOI.addconstraint!(model, MOI.SingleVariable(v[1]), MOI.Interval(0.0, 5.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.Interval{Float64}}()) == 1

    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.Interval{Float64})
    MOI.addconstraint!(model, MOI.SingleVariable(v[2]), MOI.Interval(0.0, 10.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.Interval{Float64}}()) == 2
    @test MOI.canaddconstraint(model, typeof(MOI.SingleVariable(v[2])), MOI.Integer)
    MOI.addconstraint!(model, MOI.SingleVariable(v[2]), MOI.Integer())
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.Integer}()) == 1

    @test MOI.canaddconstraint(model, typeof(MOI.SingleVariable(v[3])), MOI.ZeroOne)
    MOI.addconstraint!(model, MOI.SingleVariable(v[3]), MOI.ZeroOne())
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.ZeroOne}()) == 1

    objf = MOI.ScalarAffineFunction(v, [1.1, 2.0, 5.0], 0.0)
    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)

    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MaxSense

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.ResultCount())
        @test MOI.get(model, MOI.ResultCount()) >= 1

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) in [ MOI.FeasiblePoint, MOI.NearlyFeasiblePoint ]

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 19.4 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [4,5,1] atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c))
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 10 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.ConstraintPrimal(), typeof(c2))
        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ 15 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.DualStatus()) == false

        if MOI.canget(model, MOI.ObjectiveBound())
            @test MOI.get(model, MOI.ObjectiveBound()) >= 19.4
        end
        if MOI.canget(model, MOI.RelativeGap())
            @test MOI.get(model, MOI.RelativeGap()) >= 0.0
        end
        if MOI.canget(model, MOI.SolveTime())
            @test MOI.get(model, MOI.SolveTime()) >= 0.0
        end
        if MOI.canget(model, MOI.SimplexIterations())
            @test MOI.get(model, MOI.SimplexIterations()) >= 0
        end
        if MOI.canget(model, MOI.BarrierIterations())
            @test MOI.get(model, MOI.BarrierIterations()) >= 0
        end
        if MOI.canget(model, MOI.NodeCount())
            @test MOI.get(model, MOI.NodeCount()) >= 0
        end
    end
end

Base.isapprox(a::T, b::T; kwargs...) where T <: Union{MOI.SOS1, MOI.SOS2} = isapprox(a.weights, b.weights; kwargs...)

# sos from CPLEX.jl" begin
function int2test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    @testset "SOSI" begin
        @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
        @test MOI.supportsconstraint(model, MOI.VectorOfVariables, MOI.SOS1{Float64})
        @test MOI.supportsconstraint(model, MOI.SingleVariable, MOI.LessThan{Float64})

        MOI.empty!(model)
        @test MOI.isempty(model)

        MOI.canaddvariable(model)
        v = MOI.addvariables!(model, 3)
        @test MOI.get(model, MOI.NumberOfVariables()) == 3
        @test MOI.canaddconstraint(model, typeof(MOI.SingleVariable(v[1])), MOI.LessThan{Float64})
        MOI.addconstraint!(model, MOI.SingleVariable(v[1]), MOI.LessThan(1.0))
        @test MOI.canaddconstraint(model, typeof(MOI.SingleVariable(v[2])), MOI.LessThan{Float64})
        MOI.addconstraint!(model, MOI.SingleVariable(v[2]), MOI.LessThan(1.0))
        @test MOI.canaddconstraint(model, typeof(MOI.SingleVariable(v[3])), MOI.LessThan{Float64})
        MOI.addconstraint!(model, MOI.SingleVariable(v[3]), MOI.LessThan(2.0))

        @test MOI.canaddconstraint(model, MOI.VectorOfVariables, MOI.SOS1{Float64})
        c1 = MOI.addconstraint!(model, MOI.VectorOfVariables([v[1], v[2]]), MOI.SOS1([1.0, 2.0]))
        @test MOI.canaddconstraint(model, MOI.VectorOfVariables, MOI.SOS1{Float64})
        c2 = MOI.addconstraint!(model, MOI.VectorOfVariables([v[1], v[3]]), MOI.SOS1([1.0, 2.0]))
        @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SOS1{Float64}}()) == 2


        @test MOI.canget(model, MOI.ConstraintSet(), typeof(c2))
        @test MOI.canget(model, MOI.ConstraintFunction(), typeof(c2))
        #=
            To allow for permutations in the sets and variable vectors
            we're going to sort according to the weights
        =#
        cs_sos = MOI.get(model, MOI.ConstraintSet(), c2)
        cf_sos = MOI.get(model, MOI.ConstraintFunction(), c2)
        p = sortperm(cs_sos.weights)
        @test cs_sos.weights[p] ≈ [1.0, 2.0] atol=atol rtol=rtol
        @test cf_sos.variables[p] == v[[1,3]]

        objf = MOI.ScalarAffineFunction(v, [2.0, 1.0, 1.0], 0.0)
        @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
        MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
        @test MOI.canset(model, MOI.ObjectiveSense())
        MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)
        @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MaxSense

        if config.solve
            MOI.optimize!(model)

            @test MOI.canget(model, MOI.TerminationStatus())
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

            @test MOI.canget(model, MOI.ResultCount())
            @test MOI.get(model, MOI.ResultCount()) >= 1

            @test MOI.canget(model, MOI.PrimalStatus())
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

            @test MOI.canget(model, MOI.ObjectiveValue())
            @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3 atol=atol rtol=rtol

            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [0,1,2] atol=atol rtol=rtol

            @test MOI.canget(model, MOI.DualStatus()) == false
        end

        @test MOI.candelete(model, c1)
        MOI.delete!(model, c1)
        @test MOI.candelete(model, c2)
        MOI.delete!(model, c2)

        if config.solve
            MOI.optimize!(model)

            @test MOI.canget(model, MOI.TerminationStatus())
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

            @test MOI.canget(model, MOI.ResultCount())
            @test MOI.get(model, MOI.ResultCount()) >= 1

            @test MOI.canget(model, MOI.PrimalStatus())
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

            @test MOI.canget(model, MOI.ObjectiveValue())
            @test MOI.get(model, MOI.ObjectiveValue()) ≈ 5 atol=atol rtol=rtol

            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1,1,2] atol=atol rtol=rtol
        end
    end
    @testset "SOSII" begin
        @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
        @test MOI.supportsconstraint(model, MOI.VectorOfVariables, MOI.SOS1{Float64})
        @test MOI.supportsconstraint(model, MOI.VectorOfVariables, MOI.SOS2{Float64})
        @test MOI.supportsconstraint(model, MOI.SingleVariable, MOI.ZeroOne)
        @test MOI.supportsconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})

        MOI.empty!(model)
        @test MOI.isempty(model)

        MOI.canaddvariable(model)
        v = MOI.addvariables!(model, 10)
        @test MOI.get(model, MOI.NumberOfVariables()) == 10

        bin_constraints = []
        for i in 1:8
            @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.Interval{Float64})
            MOI.addconstraint!(model, MOI.SingleVariable(v[i]), MOI.Interval(0.0, 2.0))
            @test MOI.canaddconstraint(model, typeof(MOI.SingleVariable(v[i])), MOI.ZeroOne)
            push!(bin_constraints, MOI.addconstraint!(model, MOI.SingleVariable(v[i]), MOI.ZeroOne()))
        end

        MOI.addconstraint!(model,
            MOI.ScalarAffineFunction(v[[1,2,3,9]], [1.0,2.0,3.0,-1.0], 0.0),
            MOI.EqualTo(0.0)
        )

        MOI.addconstraint!(model,
            MOI.ScalarAffineFunction(v[[4,5,6,7,8,10]], [5.0,4.0,7.0,2.0,1.0,-1.0], 0.0),
            MOI.EqualTo(0.0)
        )

        MOI.addconstraint!(model,
            MOI.VectorOfVariables(v[[1, 2, 3]]),
            MOI.SOS1([1.0, 2.0, 3.0])
        )

        vv   = MOI.VectorOfVariables(v[[4,5,6,7,8]])
        sos2 = MOI.SOS2([5.0, 4.0, 7.0, 2.0, 1.0])
        @test MOI.canaddconstraint(model, typeof(vv), MOI.SOS2{Float64})
        c = MOI.addconstraint!(model, vv, sos2)

        @test MOI.canget(model, MOI.ConstraintSet(), typeof(c))
        @test MOI.canget(model, MOI.ConstraintFunction(), typeof(c))
        #=
            To allow for permutations in the sets and variable vectors
            we're going to sort according to the weights
        =#
        cs_sos = MOI.get(model, MOI.ConstraintSet(), c)
        cf_sos = MOI.get(model, MOI.ConstraintFunction(), c)
        p = sortperm(cs_sos.weights)
        @test cs_sos.weights[p] ≈ [1.0, 2.0, 4.0, 5.0, 7.0] atol=atol rtol=rtol
        @test cf_sos.variables[p] == v[[8,7,5,4,6]]

        objf = MOI.ScalarAffineFunction([v[9], v[10]], [1.0, 1.0], 0.0)
        @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
        MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
        @test MOI.canset(model, MOI.ObjectiveSense())
        MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)
        @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MaxSense

        if config.solve
            MOI.optimize!(model)

            @test MOI.canget(model, MOI.TerminationStatus())
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

            @test MOI.canget(model, MOI.ResultCount())
            @test MOI.get(model, MOI.ResultCount()) >= 1

            @test MOI.canget(model, MOI.PrimalStatus())
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

            @test MOI.canget(model, MOI.ObjectiveValue())
            @test MOI.get(model, MOI.ObjectiveValue()) ≈ 15.0 atol=atol rtol=rtol

            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 3.0, 12.0] atol=atol rtol=rtol

            @test MOI.canget(model, MOI.DualStatus()) == false
        end

        for cref in bin_constraints
            @test MOI.candelete(model, cref)
            MOI.delete!(model, cref)
        end

        if config.solve
            MOI.optimize!(model)

            @test MOI.canget(model, MOI.TerminationStatus())
            @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

            @test MOI.canget(model, MOI.ResultCount())
            @test MOI.get(model, MOI.ResultCount()) >= 1

            @test MOI.canget(model, MOI.PrimalStatus())
            @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

            @test MOI.canget(model, MOI.ObjectiveValue())
            @test MOI.get(model, MOI.ObjectiveValue()) ≈ 30.0 atol=atol rtol=rtol

            @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
            @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [0.0, 0.0, 2.0, 2.0, 0.0, 2.0, 0.0, 0.0, 6.0, 24.0] atol=atol rtol=rtol

            @test MOI.canget(model, MOI.DualStatus()) == false
        end
    end
end

# CPLEX #76
function int3test(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # integer knapsack problem
    # max   z - 0.5 ( b1 + b2 + b3) / 40
    # s.t.  0 <= z - 0.5 eᵀ b / 40 <= 0.999
    #       b1, b2, ... b10 ∈ {0, 1}
    #       z in {0, 1, 2, ..., 100}

    MOI.empty!(model)
    @test MOI.isempty(model)

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supportsconstraint(model, MOI.SingleVariable, MOI.ZeroOne)
    @test MOI.supportsconstraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supportsconstraint(model, MOI.SingleVariable, MOI.Interval{Float64})
    @test MOI.supportsconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64})

    MOI.canaddvariable(model)
    z = MOI.addvariable!(model)
    @test MOI.canaddconstraint(model, typeof(MOI.SingleVariable(z)), MOI.Integer)
    MOI.addconstraint!(model, MOI.SingleVariable(z), MOI.Integer())
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.Integer)
    MOI.addconstraint!(model, MOI.SingleVariable(z), MOI.Interval(0.0, 100.0))

    MOI.canaddvariable(model)
    b = MOI.addvariables!(model, 10)

    for bi in b
        @test MOI.canaddconstraint(model, typeof(MOI.SingleVariable(bi)), MOI.ZeroOne)
        MOI.addconstraint!(model, MOI.SingleVariable(bi), MOI.ZeroOne())
    end

    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64})
    c = MOI.addconstraint!(model, MOI.ScalarAffineFunction(vcat(z, b), vcat(1.0, fill(-0.5 / 40, 10)), 0.0), MOI.Interval(0.0, 0.999))

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(vcat(z, b[1:3]), vcat(1.0, fill(-0.5 / 40, 3)), 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1 atol=atol rtol=rtol

        # test for CPLEX.jl #76
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1 atol=atol rtol=rtol
    end
end


# Mixed-integer linear problems

function knapsacktest(model::MOI.ModelLike, config::TestConfig)
    atol = config.atol
    rtol = config.rtol
    # integer knapsack problem
    # max 5a + 3b + 2c + 7d + 4e
    # st  2a + 8b + 4c + 2d + 5e <= 10
    #                  a,b,c,d,e ∈ binary

    MOI.empty!(model)
    @test MOI.isempty(model)

    @test MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supportsconstraint(model, MOI.SingleVariable, MOI.ZeroOne)
    @test MOI.supportsconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})

    MOI.canaddvariable(model)
    v = MOI.addvariables!(model, 5)
    @test MOI.get(model, MOI.NumberOfVariables()) == 5

    for vi in v
        @test MOI.canaddconstraint(model, typeof(MOI.SingleVariable(vi)), MOI.ZeroOne)
        MOI.addconstraint!(model, MOI.SingleVariable(vi), MOI.ZeroOne())
    end
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.ZeroOne}()) == 5
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    c = MOI.addconstraint!(model, MOI.ScalarAffineFunction(v, [2.0, 8.0, 4.0, 2.0, 5.0], 0.0), MOI.LessThan(10.0))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(v, [5.0, 3.0, 2.0, 7.0, 4.0], 0.0))
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if MOI.canset(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
        MOI.set!(model, MOI.VariablePrimalStart(), v, [0.0, 0.0, 0.0, 0.0, 0.0])
    end

    if config.solve
        MOI.optimize!(model)

        @test MOI.canget(model, MOI.TerminationStatus())
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(model, MOI.PrimalStatus())
        @test MOI.get(model, MOI.PrimalStatus()) in [ MOI.FeasiblePoint, MOI.NearlyFeasiblePoint ]

        @test MOI.canget(model, MOI.ObjectiveValue())
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 16 atol=atol rtol=rtol

        @test MOI.canget(model, MOI.VariablePrimal(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1, 0, 0, 1, 1] atol=atol rtol=rtol
    end
end

const intlineartests = Dict("knapsack" => knapsacktest,
                            "int1"     => int1test,
                            "int2"     => int2test,
                            "int3"     => int3test)

@moitestset intlinear
