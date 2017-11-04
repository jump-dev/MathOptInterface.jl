using MathOptInterface
MOI = MathOptInterface

using MathOptInterfaceUtilities

function int1test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "MIP01 from CPLEX.jl" begin
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

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}),(MOI.SingleVariable,MOI.GreaterThan{Float64}), (MOI.SingleVariable, MOI.ZeroOne), (MOI.SingleVariable, MOI.Integer)])

        instance = MOI.SolverInstance(solver)

        v = MOI.addvariables!(instance, 3)
        @test MOI.get(instance, MOI.NumberOfVariables()) == 3

        cf = MOI.ScalarAffineFunction(v, [1.0,1.0,1.0], 0.0)
        c = MOI.addconstraint!(instance, cf, MOI.LessThan(10.0))
        @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

        cf2 = MOI.ScalarAffineFunction(v, [1.0,2.0,1.0], 0.0)
        c2 = MOI.addconstraint!(instance, cf2, MOI.LessThan(15.0))
        @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 2


        MOI.addconstraint!(instance, MOI.SingleVariable(v[1]), MOI.Interval(0.0, 5.0))
        @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.Interval{Float64}}()) == 1

        MOI.addconstraint!(instance, MOI.SingleVariable(v[2]), MOI.Interval(0.0, 10.0))
        @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.Interval{Float64}}()) == 2
        MOI.addconstraint!(instance, MOI.SingleVariable(v[2]), MOI.Integer())
        @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.Integer}()) == 1

        MOI.addconstraint!(instance, MOI.SingleVariable(v[3]), MOI.ZeroOne())
        @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.ZeroOne}()) == 1

        objf = MOI.ScalarAffineFunction(v, [1.1, 2.0, 5.0], 0.0)
        MOI.set!(instance, MOI.ObjectiveFunction(), objf)
        MOI.set!(instance, MOI.ObjectiveSense(), MOI.MaxSense)

        @test MOI.get(instance, MOI.ObjectiveSense()) == MOI.MaxSense

        MOI.optimize!(instance)

        @test MOI.canget(instance, MOI.TerminationStatus())
        @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(instance, MOI.ResultCount())
        @test MOI.get(instance, MOI.ResultCount()) >= 1

        @test MOI.canget(instance, MOI.PrimalStatus())
        @test MOI.get(instance, MOI.PrimalStatus()) in [ MOI.FeasiblePoint, MOI.NearlyFeasiblePoint ]

        @test MOI.canget(instance, MOI.ObjectiveValue())
        @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 19.4 atol=atol rtol=rtol

        @test MOI.canget(instance, MOI.VariablePrimal(), v)
        @test MOI.get(instance, MOI.VariablePrimal(), v) ≈ [4,5,1] atol=atol rtol=rtol

        @test MOI.canget(instance, MOI.ConstraintPrimal(), c)
        @test MOI.get(instance, MOI.ConstraintPrimal(), c) ≈ 10 atol=atol rtol=rtol

        @test MOI.canget(instance, MOI.ConstraintPrimal(), c2)
        @test MOI.get(instance, MOI.ConstraintPrimal(), c2) ≈ 15 atol=atol rtol=rtol

        @test MOI.canget(instance, MOI.DualStatus()) == false

        if MOI.canget(instance, MOI.ObjectiveBound())
            @test MOI.get(instance, MOI.ObjectiveBound()) >= 19.4
        end
        if MOI.canget(instance, MOI.RelativeGap())
            @test MOI.get(instance, MOI.RelativeGap()) >= 0.0
        end
        if MOI.canget(instance, MOI.SolveTime())
            @test MOI.get(instance, MOI.SolveTime()) >= 0.0
        end
        if MOI.canget(instance, MOI.SimplexIterations())
            @test MOI.get(instance, MOI.SimplexIterations()) >= 0
        end
        if MOI.canget(instance, MOI.BarrierIterations())
            @test MOI.get(instance, MOI.BarrierIterations()) >= 0
        end
        if MOI.canget(instance, MOI.NodeCount())
            @test MOI.get(instance, MOI.NodeCount()) >= 0
        end

    end
end

Base.isapprox(a::T, b::T; kwargs...) where T <: Union{MOI.SOS1, MOI.SOS2} = isapprox(a.weights, b.weights; kwargs...)
Base.:(==)(a::MOI.VectorOfVariables, b::MOI.VectorOfVariables) = (a.variables == b.variables)

function int2test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    if MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [ (MOI.VectorOfVariables, MOI.SOS1),
                                                                        (MOI.VectorOfVariables, MOI.SOS2) ])
        @testset "sos from CPLEX.jl" begin
            @testset "SOSI" begin
                @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.VectorOfVariables,MOI.SOS1), (MOI.SingleVariable,MOI.LessThan{Float64})])

                instance = MOI.SolverInstance(solver)

                v = MOI.addvariables!(instance, 3)
                @test MOI.get(instance, MOI.NumberOfVariables()) == 3
                MOI.addconstraint!(instance, MOI.SingleVariable(v[1]), MOI.LessThan(1.0))
                MOI.addconstraint!(instance, MOI.SingleVariable(v[2]), MOI.LessThan(1.0))
                MOI.addconstraint!(instance, MOI.SingleVariable(v[3]), MOI.LessThan(2.0))

                c1 = MOI.addconstraint!(instance, MOI.VectorOfVariables([v[1], v[2]]), MOI.SOS1([1.0, 2.0]))
                c2 = MOI.addconstraint!(instance, MOI.VectorOfVariables([v[1], v[3]]), MOI.SOS1([1.0, 2.0]))
                @test MOI.get(instance, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SOS1}()) == 2


                @test MOI.canget(instance, MOI.ConstraintSet(), c2)
                @test MOI.canget(instance, MOI.ConstraintFunction(), c2)
                #=
                    To allow for permutations in the sets and variable vectors
                    we're going to sort according to the weights
                =#
                cs_sos = MOI.get(instance, MOI.ConstraintSet(), c2)
                cf_sos = MOI.get(instance, MOI.ConstraintFunction(), c2)
                p = sortperm(cs_sos.weights)
                @test cs_sos.weights[p] ≈ [1.0, 2.0] atol=atol rtol=rtol
                @test cf_sos.variables[p] == v[[1,3]]

                objf = MOI.ScalarAffineFunction(v, [2.0, 1.0, 1.0], 0.0)
                MOI.set!(instance, MOI.ObjectiveFunction(), objf)
                MOI.set!(instance, MOI.ObjectiveSense(), MOI.MaxSense)
                @test MOI.get(instance, MOI.ObjectiveSense()) == MOI.MaxSense

                MOI.optimize!(instance)

                @test MOI.canget(instance, MOI.TerminationStatus())
                @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

                @test MOI.canget(instance, MOI.ResultCount())
                @test MOI.get(instance, MOI.ResultCount()) >= 1

                @test MOI.canget(instance, MOI.PrimalStatus())
                @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

                @test MOI.canget(instance, MOI.ObjectiveValue())
                @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 3 atol=atol rtol=rtol

                @test MOI.canget(instance, MOI.VariablePrimal(), v)
                @test MOI.get(instance, MOI.VariablePrimal(), v) ≈ [0,1,2] atol=atol rtol=rtol

                @test MOI.canget(instance, MOI.DualStatus()) == false

                @test MOI.candelete(instance, c1)
                MOI.delete!(instance, c1)
                @test MOI.candelete(instance, c2)
                MOI.delete!(instance, c2)

                MOI.optimize!(instance)

                @test MOI.canget(instance, MOI.TerminationStatus())
                @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

                @test MOI.canget(instance, MOI.ResultCount())
                @test MOI.get(instance, MOI.ResultCount()) >= 1

                @test MOI.canget(instance, MOI.PrimalStatus())
                @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

                @test MOI.canget(instance, MOI.ObjectiveValue())
                @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 5 atol=atol rtol=rtol

                @test MOI.canget(instance, MOI.VariablePrimal(), v)
                @test MOI.get(instance, MOI.VariablePrimal(), v) ≈ [1,1,2] atol=atol rtol=rtol
            end
            @testset "SOSII" begin
                @test MOI.supportsproblem(solver,
                    MOI.ScalarAffineFunction{Float64},
                    [
                        (MOI.VectorOfVariables,MOI.SOS1),
                        (MOI.VectorOfVariables,MOI.SOS2),
                        (MOI.SingleVariable, MOI.ZeroOne),
                        (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
                        ]
                )

                instance = MOI.SolverInstance(solver)

                v = MOI.addvariables!(instance, 10)
                @test MOI.get(instance, MOI.NumberOfVariables()) == 10

                bin_constraints = []
                for i in 1:8
                    MOI.addconstraint!(instance, MOI.SingleVariable(v[i]), MOI.Interval(0.0, 2.0))
                    push!(bin_constraints, MOI.addconstraint!(instance, MOI.SingleVariable(v[i]), MOI.ZeroOne()))
                end

                MOI.addconstraint!(instance,
                    MOI.ScalarAffineFunction(v[[1,2,3,9]], [1.0,2.0,3.0,-1.0], 0.0),
                    MOI.EqualTo(0.0)
                )

                MOI.addconstraint!(instance,
                    MOI.ScalarAffineFunction(v[[4,5,6,7,8,10]], [5.0,4.0,7.0,2.0,1.0,-1.0], 0.0),
                    MOI.EqualTo(0.0)
                )

                MOI.addconstraint!(instance,
                    MOI.VectorOfVariables(v[[1, 2, 3]]),
                    MOI.SOS1([1.0, 2.0, 3.0])
                )

                vv   = MOI.VectorOfVariables(v[[4,5,6,7,8]])
                sos2 = MOI.SOS2([5.0, 4.0, 7.0, 2.0, 1.0])
                c = MOI.addconstraint!(instance, vv, sos2)

                @test MOI.canget(instance, MOI.ConstraintSet(), c)
                @test MOI.canget(instance, MOI.ConstraintFunction(), c)
                #=
                    To allow for permutations in the sets and variable vectors
                    we're going to sort according to the weights
                =#
                cs_sos = MOI.get(instance, MOI.ConstraintSet(), c)
                cf_sos = MOI.get(instance, MOI.ConstraintFunction(), c)
                p = sortperm(cs_sos.weights)
                @test cs_sos.weights[p] ≈ [1.0, 2.0, 4.0, 5.0, 7.0] atol=atol rtol=rtol
                @test cf_sos.variables[p] == v[[8,7,5,4,6]]

                objf = MOI.ScalarAffineFunction([v[9], v[10]], [1.0, 1.0], 0.0)
                MOI.set!(instance, MOI.ObjectiveFunction(), objf)
                MOI.set!(instance, MOI.ObjectiveSense(), MOI.MaxSense)
                @test MOI.get(instance, MOI.ObjectiveSense()) == MOI.MaxSense

                MOI.optimize!(instance)

                @test MOI.canget(instance, MOI.TerminationStatus())
                @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

                @test MOI.canget(instance, MOI.ResultCount())
                @test MOI.get(instance, MOI.ResultCount()) >= 1

                @test MOI.canget(instance, MOI.PrimalStatus())
                @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

                @test MOI.canget(instance, MOI.ObjectiveValue())
                @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 15.0 atol=atol rtol=rtol

                @test MOI.canget(instance, MOI.VariablePrimal(), v)
                @test MOI.get(instance, MOI.VariablePrimal(), v) ≈ [0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 3.0, 12.0] atol=atol rtol=rtol

                @test MOI.canget(instance, MOI.DualStatus()) == false

                for cref in bin_constraints
                    @test MOI.candelete(instance, cref)
                    MOI.delete!(instance, cref)
                end

                MOI.optimize!(instance)

                @test MOI.canget(instance, MOI.TerminationStatus())
                @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

                @test MOI.canget(instance, MOI.ResultCount())
                @test MOI.get(instance, MOI.ResultCount()) >= 1

                @test MOI.canget(instance, MOI.PrimalStatus())
                @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

                @test MOI.canget(instance, MOI.ObjectiveValue())
                @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 30.0 atol=atol rtol=rtol

                @test MOI.canget(instance, MOI.VariablePrimal(), v)
                @test MOI.get(instance, MOI.VariablePrimal(), v) ≈ [0.0, 0.0, 2.0, 2.0, 0.0, 2.0, 0.0, 0.0, 6.0, 24.0] atol=atol rtol=rtol

                @test MOI.canget(instance, MOI.DualStatus()) == false
            end
        end
    end
end

function int3test(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "CPLEX #76" begin
        # integer knapsack problem
        # max   z - 0.5 ( b1 + b2 + b3) / 40
        # s.t.  0 <= z - 0.5 eᵀ b / 40 <= 0.999
        #       b1, b2, ... b10 ∈ {0, 1}
        #       z in {0, 1, 2, ..., 100}

        instance = MOI.SolverInstance(solver)

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64},
            [
                (MOI.SingleVariable,MOI.ZeroOne),
                (MOI.SingleVariable,MOI.Integer),
                (MOI.SingleVariable,MOI.Interval{Float64}),
                (MOI.ScalarAffineFunction{Float64},MOI.Interval{Float64})
            ]
        )

        z = MOI.addvariable!(instance)
        MOI.addconstraint!(instance, MOI.SingleVariable(z), MOI.Integer())
        MOI.addconstraint!(instance, MOI.SingleVariable(z), MOI.Interval(0.0, 100.0))

        b = MOI.addvariables!(instance, 10)

        for bi in b
            MOI.addconstraint!(instance, MOI.SingleVariable(bi), MOI.ZeroOne())
        end

        c = MOI.addconstraint!(instance, MOI.ScalarAffineFunction(vcat(z, b), vcat(1.0, fill(-0.5 / 40, 10)), 0.0), MOI.Interval(0.0, 0.999))

        MOI.set!(instance, MOI.ObjectiveFunction(), MOI.ScalarAffineFunction(vcat(z, b[1:3]), vcat(1.0, fill(-0.5 / 40, 3)), 0.0))
        MOI.set!(instance, MOI.ObjectiveSense(), MOI.MaxSense)

        MOI.optimize!(instance)

        @test MOI.canget(instance, MOI.TerminationStatus())
        @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(instance, MOI.PrimalStatus())
        @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(instance, MOI.ObjectiveValue())
        @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 1 atol=atol rtol=rtol

        # test for CPLEX.jl #76
        MOI.optimize!(instance)

        @test MOI.canget(instance, MOI.TerminationStatus())
        @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(instance, MOI.PrimalStatus())
        @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint

        @test MOI.canget(instance, MOI.ObjectiveValue())
        @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 1 atol=atol rtol=rtol
    end
end


# Mixed-integer linear problems

function knapsacktest(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    @testset "Knapsack model" begin
        # integer knapsack problem
        # max 5a + 3b + 2c + 7d + 4e
        # st  2a + 8b + 4c + 2d + 5e <= 10
        #                  a,b,c,d,e ∈ binary

        instance = MOI.SolverInstance(solver)

        @test MOI.supportsproblem(solver, MOI.ScalarAffineFunction{Float64}, [(MOI.SingleVariable,MOI.ZeroOne),(MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64})])

        v = MOI.addvariables!(instance, 5)
        @test MOI.get(instance, MOI.NumberOfVariables()) == 5

        for vi in v
            MOI.addconstraint!(instance, MOI.SingleVariable(vi), MOI.ZeroOne())
        end
        @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.ZeroOne}()) == 5
        c = MOI.addconstraint!(instance, MOI.ScalarAffineFunction(v, [2.0, 8.0, 4.0, 2.0, 5.0], 0.0), MOI.LessThan(10.0))
        @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

        MOI.set!(instance, MOI.ObjectiveFunction(), MOI.ScalarAffineFunction(v, [5.0, 3.0, 2.0, 7.0, 4.0], 0.0))
        MOI.set!(instance, MOI.ObjectiveSense(), MOI.MaxSense)

        if MOI.canset(instance, MOI.VariablePrimalStart(), v)
            MOI.set!(instance, MOI.VariablePrimalStart(), v, [0.0, 0.0, 0.0, 0.0, 0.0])
        end

        MOI.optimize!(instance)

        @test MOI.canget(instance, MOI.TerminationStatus())
        @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success

        @test MOI.canget(instance, MOI.PrimalStatus())
        @test MOI.get(instance, MOI.PrimalStatus()) in [ MOI.FeasiblePoint, MOI.NearlyFeasiblePoint ]

        @test MOI.canget(instance, MOI.ObjectiveValue())
        @test MOI.get(instance, MOI.ObjectiveValue()) ≈ 16 atol=atol rtol=rtol

        @test MOI.canget(instance, MOI.VariablePrimal(), v)
        @test MOI.get(instance, MOI.VariablePrimal(), v) ≈ [1, 0, 0, 1, 1] atol=atol rtol=rtol
    end
end

function intlineartest(solver::MOI.AbstractSolver; atol=Base.rtoldefault(Float64), rtol=Base.rtoldefault(Float64))
    knapsacktest(solver, atol=atol, rtol=rtol)
    int1test(solver, atol=atol, rtol=rtol)
    int2test(solver, atol=atol, rtol=rtol)
    int3test(solver, atol=atol, rtol=rtol)
end
