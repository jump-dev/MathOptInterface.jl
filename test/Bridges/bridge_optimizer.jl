using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("utilities.jl")

struct DummyModelAttribute <: MOI.AbstractModelAttribute end
struct DummyEvaluator <: MOI.AbstractNLPEvaluator end
struct DummyVariableAttribute <: MOI.AbstractVariableAttribute end
struct DummyConstraintAttribute <: MOI.AbstractConstraintAttribute end

@testset "Substitution of variables" begin
    mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
    bridged = MOIB.Variable.Vectorize{Float64}(mock)
    x, cx = MOI.add_constrained_variable(bridged, MOI.GreaterThan(1.0))
    fx = MOI.SingleVariable(x)
    y = MOI.get(mock, MOI.ListOfVariableIndices())[1]
    fy = MOI.SingleVariable(y)

    c2fx = MOI.add_constraint(bridged, 2.0fx, MOI.GreaterThan(1.0))
    @test MOI.get(bridged, MOI.ConstraintFunction(), c2fx) ≈ 2.0fx
    @test MOI.get(bridged, MOI.ConstraintSet(), c2fx) == MOI.GreaterThan(1.0)
    # The constant was moved to the set
    @test MOI.get(mock, MOI.ConstraintFunction(), c2fx) ≈ 2.0fy
    @test MOI.get(mock, MOI.ConstraintSet(), c2fx) == MOI.GreaterThan(-1.0)

    MOI.set(bridged, MOI.ConstraintSet(), c2fx, MOI.GreaterThan(2.0))
    @test MOI.get(bridged, MOI.ConstraintFunction(), c2fx) ≈ 2.0fx
    @test MOI.get(bridged, MOI.ConstraintSet(), c2fx) == MOI.GreaterThan(2.0)
    @test MOI.get(mock, MOI.ConstraintFunction(), c2fx) ≈ 2.0fy
    @test MOI.get(mock, MOI.ConstraintSet(), c2fx) == MOI.GreaterThan(0.0)

    MOI.set(bridged, MOI.ConstraintFunction(), c2fx, 3.0fx)
    @test MOI.get(bridged, MOI.ConstraintFunction(), c2fx) ≈ 3.0fx
    @test MOI.get(bridged, MOI.ConstraintSet(), c2fx) == MOI.GreaterThan(2.0)
    @test MOI.get(mock, MOI.ConstraintFunction(), c2fx) ≈ 3.0fy
    @test MOI.get(mock, MOI.ConstraintSet(), c2fx) == MOI.GreaterThan(-1.0)

    MOI.set(bridged, MOI.ConstraintSet(), c2fx, MOI.GreaterThan(4.0))
    @test MOI.get(bridged, MOI.ConstraintFunction(), c2fx) ≈ 3.0fx
    @test MOI.get(bridged, MOI.ConstraintSet(), c2fx) == MOI.GreaterThan(4.0)
    @test MOI.get(mock, MOI.ConstraintFunction(), c2fx) ≈ 3.0fy
    @test MOI.get(mock, MOI.ConstraintSet(), c2fx) == MOI.GreaterThan(1.0)

    MOI.set(bridged, MOI.ObjectiveFunction{typeof(2.0fx)}(), 2.0fx)
    @test MOI.get(mock, MOI.ObjectiveFunction{typeof(2.0fy)}()) ≈ 2.0fy + 2.0

    MOI.set(bridged, DummyModelAttribute(), 2.0fx + 1.0)
    @test MOI.get(bridged, DummyModelAttribute()) ≈ 2.0fx + 1.0
    @test MOI.get(mock, DummyModelAttribute()) ≈ 2.0fy + 3.0

    z = MOI.add_variable(bridged)
    fz = MOI.SingleVariable(z)
    for (attr, index) in [
        (DummyVariableAttribute(), z),
        (DummyConstraintAttribute(), c2fx)]
        MOI.set(bridged, attr, index, 1.0fx)
        @test MOI.get(bridged, attr, index) ≈ 1.0fx
        @test MOI.get(mock, attr, index) ≈ 1.0fy + 1.0

        MOI.set(bridged, attr, [index], [3.0fx + 1.0fz])
        @test MOI.get(bridged, attr, [index])[1] ≈ 3.0fx + 1.0fz
        @test MOI.get(mock, attr, [index])[1] ≈ 3.0fy + 1.0fz + 3.0
    end

    @testset "RawSolver" begin
        MOI.get(bridged, MOI.RawSolver()) === mock
    end

    @testset "HeuristicCallback" begin
        attr = MOI.HeuristicCallback()
        f(callback_data) = nothing
        MOI.set(bridged, attr, f)
        @test MOI.get(bridged, attr) === f
    end

    @testset "CallbackVariablePrimal" begin
        attr = MOI.CallbackVariablePrimal(nothing)
        err = ErrorException("No mock callback primal is set for variable `$z`.")
        @test_throws err MOI.get(bridged, attr, z)
        MOI.set(mock, attr, y, 1.0)
        MOI.set(mock, attr, z, 2.0)
        @test MOI.get(bridged, attr, z) == 2.0
        err = ArgumentError("Variable bridge of type `$(typeof(MOIB.bridge(bridged, x)))` does not support accessing the attribute `$attr`.")
        @test_throws err MOI.get(bridged, attr, x)
    end

    @testset "LazyConstraint" begin
        sub = MOI.LazyConstraint(nothing)
        @test MOI.supports(bridged, sub)
        MOI.submit(bridged, sub, 2.0fx, MOI.GreaterThan(1.0))
        mock.submitted[sub][1][1] ≈ 2.0fy + 2.0
        mock.submitted[sub][1][2] == MOI.GreaterThan(1.0)
    end
    @testset "HeuristicSolution" begin
        sub = MOI.HeuristicSolution(nothing)
        @test MOI.supports(bridged, sub)
        @testset "Non-bridged variable" begin
            MOI.submit(bridged, sub, [z], [1.0])
            mock.submitted[sub][1][1] == [z]
            mock.submitted[sub][1][2] == [1.0]
        end
        @testset "Bridged variable" begin
            err = ErrorException("Cannot substitute `$x` as it is bridged into `$(1.0fy + 1.0)`.")
            @test_throws err MOI.submit(bridged, sub, [x], [1.0])
        end
    end

    nlp_data = MOI.NLPBlockData(
        [MOI.NLPBoundsPair(1.0, 2.0)],
        DummyEvaluator(), false)
    MOI.set(bridged, MOI.NLPBlock(), nlp_data)
    for nlp_data in [MOI.get(bridged, MOI.NLPBlock()), MOI.get(mock, MOI.NLPBlock())]
        @test nlp_data.constraint_bounds == [MOI.NLPBoundsPair(1.0, 2.0)]
        @test nlp_data.evaluator isa DummyEvaluator
        @test nlp_data.has_objective == false
    end
end

# Model not supporting Interval
MOIU.@model(NoIntervalModel,
            (),
            (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval),
            (MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives,
             MOI.NormInfinityCone, MOI.NormOneCone, MOI.SecondOrderCone,
             MOI.RotatedSecondOrderCone, MOI.GeometricMeanCone,
             MOI.RelativeEntropyCone, MOI.NormSpectralCone, MOI.NormNuclearCone,
             MOI.PositiveSemidefiniteConeTriangle, MOI.ExponentialCone),
            (MOI.PowerCone, MOI.DualPowerCone),
            (),
            (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
            (MOI.VectorOfVariables,),
            (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction))

mock = MOIU.MockOptimizer(NoIntervalModel{Float64}())
bridged_mock = MOIB.Constraint.LessToGreater{Float64}(MOIB.Constraint.SplitInterval{Float64}(mock))

@testset "Unsupported constraint attribute" begin
    attr = MOIT.UnknownConstraintAttribute()
    err = ArgumentError(
        "Bridge of type `$(MOI.Bridges.Constraint.SplitIntervalBridge{Float64,MOI.SingleVariable,MOI.Interval{Float64},MOI.GreaterThan{Float64},MOI.LessThan{Float64}})` " *
        "does not support accessing the attribute `$attr`.")
    x = MOI.add_variable(bridged_mock)
    ci = MOI.add_constraint(bridged_mock, MOI.SingleVariable(x),
                            MOI.Interval(0.0, 1.0))
    @test !MOI.Bridges.is_bridged(bridged_mock, ci)
    @test MOI.Bridges.is_bridged(bridged_mock.model, ci)
    @test !MOI.supports(bridged_mock, attr, typeof(ci))
    @test_throws err MOI.get(bridged_mock, attr, ci)
end

@testset "Issue #453" begin
    MOI.empty!(bridged_mock)
    MOIU.loadfromstring!(bridged_mock, """
        variables: x
        maxobjective: 3.0x
        c: 2.0x in Interval(1.0, 4.0)
        d: x in LessThan(1.5)
    """)
    x = MOI.get(bridged_mock, MOI.VariableIndex, "x")
    @test isa(x, MOI.VariableIndex)
    c1 = MOI.get(bridged_mock, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}}, "c")
    @test isa(c1, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}})
    c2 = MOI.get(bridged_mock, MOI.ConstraintIndex, "c")
    @test c1 == c2
    d1 = MOI.get(bridged_mock, MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{Float64}}, "d")
    @test isa(d1, MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{Float64}})
    d2 = MOI.get(bridged_mock, MOI.ConstraintIndex, "d")
    @test d1 == d2
end

MOI.empty!(bridged_mock)

@testset "Name test" begin
    MOIT.nametest(bridged_mock)
    # Test that names are deleted in `MOI.empty!`
    MOIT.nametest(bridged_mock)
end

@testset "Copy test" begin
    MOIT.failcopytestc(bridged_mock)
    MOIT.failcopytestia(bridged_mock)
    MOIT.failcopytestva(bridged_mock)
    MOIT.failcopytestca(bridged_mock)
    MOIT.copytest(bridged_mock, MOIU.Model{Float64}())
end

@testset "Custom test" begin
    model = MOIB.Constraint.SplitInterval{Int}(NoIntervalModel{Int}())
    @test !MOIB.supports_bridging_constraint(model, MOI.VectorAffineFunction{Float64}, MOI.Interval{Float64})

    x, y = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2

    f1 = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(3, x)], 7)
    c1 = MOI.add_constraint(model, f1, MOI.Interval(-1, 1))

    @test MOI.get(model, MOI.ListOfConstraints()) == [(MOI.ScalarAffineFunction{Int},MOI.Interval{Int})]
    test_num_constraints(model, MOI.ScalarAffineFunction{Int}, MOI.GreaterThan{Int}, 0)
    test_num_constraints(model, MOI.ScalarAffineFunction{Int}, MOI.Interval{Int}, 1)
    @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())) == [c1]

    f2 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2, -1], [x, y]), 2)
    c2 = MOI.add_constraint(model, f1, MOI.GreaterThan(-2))

    @test MOI.get(model, MOI.ListOfConstraints()) == [(MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}), (MOI.ScalarAffineFunction{Int},MOI.Interval{Int})]
    test_num_constraints(model, MOI.ScalarAffineFunction{Int}, MOI.GreaterThan{Int}, 1)
    test_num_constraints(model, MOI.ScalarAffineFunction{Int}, MOI.Interval{Int}, 1)
    @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())) == [c1]
    @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}}())) == [c2]

    n = 4
    z = MOI.add_variables(model, n)
    scon_indices = MOI.ConstraintIndex{MOI.SingleVariable, MOI.Interval{Int}}[]
    for (i, v) in enumerate([x; y; z])
        f = MOI.SingleVariable(v)
        c = MOI.add_constraint(model, f, MOI.Interval(i, 2i))
        push!(scon_indices, c)

        @test Set(MOI.get(model, MOI.ListOfConstraints())) == Set([(MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}), (MOI.ScalarAffineFunction{Int},MOI.Interval{Int}), (MOI.SingleVariable, MOI.Interval{Int})])
        test_num_constraints(model, MOI.ScalarAffineFunction{Int}, MOI.GreaterThan{Int}, 1)
        test_num_constraints(model, MOI.ScalarAffineFunction{Int}, MOI.Interval{Int}, 1)
        test_num_constraints(model, MOI.SingleVariable, MOI.Interval{Int}, i)
        @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())) == [c1]
        @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}}())) == [c2]
        # The indices should be returned in order of creation
        @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.SingleVariable,MOI.Interval{Int}}())) == scon_indices
    end

    vcon_indices = MOI.ConstraintIndex{MOI.VectorOfVariables, MOI.Nonnegatives}[]
    for (i, v) in enumerate(z)
        f = MOI.VectorOfVariables([v])
        c = MOI.add_constraint(model, f, MOI.Nonnegatives(1))
        push!(vcon_indices, c)

        @test Set(MOI.get(model, MOI.ListOfConstraints())) == Set([(MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}), (MOI.ScalarAffineFunction{Int},MOI.Interval{Int}), (MOI.SingleVariable, MOI.Interval{Int}), (MOI.VectorOfVariables, MOI.Nonnegatives)])
        test_num_constraints(model, MOI.ScalarAffineFunction{Int}, MOI.GreaterThan{Int}, 1)
        test_num_constraints(model, MOI.ScalarAffineFunction{Int}, MOI.Interval{Int}, 1)
        test_num_constraints(model, MOI.SingleVariable, MOI.Interval{Int}, n + 2)
        test_num_constraints(model, MOI.VectorOfVariables, MOI.Nonnegatives, i)
        @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())) == [c1]
        @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}}())) == [c2]
        # The indices should be returned in order of creation
        @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.SingleVariable,MOI.Interval{Int}}())) == scon_indices
        @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Nonnegatives}())) == vcon_indices
    end

    @test MOI.is_valid(model, c2)
    MOI.delete(model, c2)

    @test Set(MOI.get(model, MOI.ListOfConstraints())) == Set([(MOI.ScalarAffineFunction{Int},MOI.Interval{Int}), (MOI.SingleVariable, MOI.Interval{Int}), (MOI.VectorOfVariables, MOI.Nonnegatives)])
    test_num_constraints(model, MOI.ScalarAffineFunction{Int}, MOI.GreaterThan{Int}, 0)
    test_num_constraints(model, MOI.ScalarAffineFunction{Int}, MOI.Interval{Int}, 1)
    test_num_constraints(model, MOI.SingleVariable, MOI.Interval{Int}, n + 2)
    test_num_constraints(model, MOI.VectorOfVariables, MOI.Nonnegatives, n)
    @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())) == [c1]
    # The indices should be returned in order of creation
    @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.SingleVariable,MOI.Interval{Int}}())) == scon_indices
    @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Nonnegatives}())) == vcon_indices
end

@testset "Continuous Linear" begin
    exclude = ["partial_start"] # VariablePrimalStart not supported.
    MOIT.contlineartest(bridged_mock, MOIT.TestConfig(solve=false), exclude)
end

@testset "Show" begin
    @test sprint(show, bridged_mock) == MOI.Utilities.replace_acronym("""
    $(MOIB.Constraint.SingleBridgeOptimizer{MOIB.Constraint.LessToGreaterBridge{Float64,F,G} where G<:MOI.AbstractScalarFunction where F<:MOI.AbstractScalarFunction,MOIB.Constraint.SingleBridgeOptimizer{MOIB.Constraint.SplitIntervalBridge{Float64,F,S,LS,US} where US<:MOI.AbstractSet where LS<:MOI.AbstractSet where S<:MOI.AbstractSet where F<:MOI.AbstractFunction,MOIU.MockOptimizer{NoIntervalModel{Float64}}}})
    with 1 constraint bridge
    with inner model $(MOIB.Constraint.SingleBridgeOptimizer{MOIB.Constraint.SplitIntervalBridge{Float64,F,S,LS,US} where US<:MOI.AbstractSet where LS<:MOI.AbstractSet where S<:MOI.AbstractSet where F<:MOI.AbstractFunction,MOIU.MockOptimizer{NoIntervalModel{Float64}}})
      with 0 constraint bridges
      with inner model $(MOIU.MockOptimizer{NoIntervalModel{Float64}})""")
end
