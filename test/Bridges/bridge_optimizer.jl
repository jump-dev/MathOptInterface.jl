using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("simple_model.jl")
include("utilities.jl")

struct UnknownConstraintAttribute <: MOI.AbstractConstraintAttribute end
MOI.is_set_by_optimize(::UnknownConstraintAttribute) = true

mock = MOIU.MockOptimizer(SimpleModel{Float64}())
bridged_mock = MOIB.Constraint.LessToGreater{Float64}(MOIB.Constraint.SplitInterval{Float64}(mock))

@testset "Unsupported constraint attribute" begin
    attr = UnknownConstraintAttribute()
    err = ArgumentError(
        "Constraint bridge of type `MathOptInterface.Bridges.Constraint.SplitIntervalBridge{Float64,MathOptInterface.SingleVariable}` " *
        "does not support accessing the attribute `$attr`.")
    x = MOI.add_variable(bridged_mock)
    ci = MOI.add_constraint(bridged_mock, MOI.SingleVariable(x),
                            MOI.Interval(0.0, 1.0))
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
    MOIT.copytest(bridged_mock, SimpleModel{Float64}())
end

@testset "Custom test" begin
    model = MOIB.Constraint.SplitInterval{Int}(SimpleModel{Int}())
    @test !MOIB.supports_bridging_constraint(model, MOI.VectorAffineFunction{Float64}, MOI.Interval{Float64})

    x, y = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2

    f1 = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(3, x)], 7)
    c1 = MOI.add_constraint(model, f1, MOI.Interval(-1, 1))

    @test MOI.get(model, MOI.ListOfConstraints()) == [(MOI.ScalarAffineFunction{Int},MOI.Interval{Int})]
    test_noc(model, MOI.ScalarAffineFunction{Int}, MOI.GreaterThan{Int}, 0)
    test_noc(model, MOI.ScalarAffineFunction{Int}, MOI.Interval{Int}, 1)
    @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())) == [c1]

    f2 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2, -1], [x, y]), 2)
    c2 = MOI.add_constraint(model, f1, MOI.GreaterThan(-2))

    @test MOI.get(model, MOI.ListOfConstraints()) == [(MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}), (MOI.ScalarAffineFunction{Int},MOI.Interval{Int})]
    test_noc(model, MOI.ScalarAffineFunction{Int}, MOI.GreaterThan{Int}, 1)
    test_noc(model, MOI.ScalarAffineFunction{Int}, MOI.Interval{Int}, 1)
    @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())) == [c1]
    @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}}())) == [c2]

    @test MOI.is_valid(model, c2)
    MOI.delete(model, c2)

    @test MOI.get(model, MOI.ListOfConstraints()) == [(MOI.ScalarAffineFunction{Int},MOI.Interval{Int})]
    test_noc(model, MOI.ScalarAffineFunction{Int}, MOI.GreaterThan{Int}, 0)
    test_noc(model, MOI.ScalarAffineFunction{Int}, MOI.Interval{Int}, 1)
    @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())) == [c1]
end

@testset "Continuous Linear" begin
    exclude = ["partial_start"] # VariablePrimalStart not supported.
    MOIT.contlineartest(bridged_mock, MOIT.TestConfig(solve=false), exclude)
end
