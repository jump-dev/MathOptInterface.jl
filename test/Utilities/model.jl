# TODO: It's hard to find where Model is defined!

# We need to test this in a module at the top level because it can't be defined
# in a testset. If it runs without error, then we're okay.
module TestExternalModel
    using MathOptInterface
    struct NewSet <: MathOptInterface.AbstractScalarSet end
    struct NewFunction <: MathOptInterface.AbstractScalarFunction end
    Base.copy(::NewFunction) = NewFunction()
    Base.copy(::NewSet) = NewSet()
    MathOptInterface.Utilities.@model(ExternalModel,
        (MathOptInterface.ZeroOne, NewSet,),
        (),
        (),
        (),
        (NewFunction,),
        (),
        (),
        ()
    )
end

@testset "External @model" begin
    model = TestExternalModel.ExternalModel{Float64}()
    c = MOI.add_constraint(
        model, TestExternalModel.NewFunction(), TestExternalModel.NewSet())
    @test typeof(c) == MOI.ConstraintIndex{TestExternalModel.NewFunction,
        TestExternalModel.NewSet}
    c2 = MOI.add_constraint(
        model, TestExternalModel.NewFunction(), MOI.ZeroOne())
    @test typeof(c2) ==
        MOI.ConstraintIndex{TestExternalModel.NewFunction, MOI.ZeroOne}
end

@testset "Name test" begin
    MOIT.nametest(Model{Float64}())
end

@testset "Valid test" begin
    MOIT.validtest(Model{Float64}())
end

@testset "Empty test" begin
    MOIT.emptytest(Model{Float64}())
end

@testset "supports_constraint test" begin
    MOIT.supports_constrainttest(Model{Float64}(), Float64, Int)
    MOIT.supports_constrainttest(Model{Int}(), Int, Float64)
end

@testset "OrderedIndices" begin
    MOIT.orderedindicestest(Model{Float64}())
end

@testset "Continuous Linear tests" begin
    config = MOIT.TestConfig(solve=false)
    MOIT.contlineartest(Model{Float64}(), config)
end

@testset "Continuous Conic tests" begin
    config = MOIT.TestConfig(solve=false)
    MOIT.contconictest(Model{Float64}(), config)
end

@testset "Quadratic functions" begin

    model = Model{Int}()

    x, y = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2

    f1 = MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm.([3], [x]), MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]), 7)
    c1 = MOI.add_constraint(model, f1, MOI.Interval(-1, 1))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarQuadraticFunction{Int},MOI.Interval{Int}}()) == 1
    @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarQuadraticFunction{Int},MOI.Interval{Int}}())) == [c1]

    f2 = MOI.VectorQuadraticFunction(MOI.VectorAffineTerm.([1, 2, 2], MOI.ScalarAffineTerm.([3, 1, 2], [x, x, y])), MOI.VectorQuadraticTerm.([1, 1, 2], MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y])), [7, 3, 4])
    c2 = MOI.add_constraint(model, f2, MOI.PositiveSemidefiniteConeTriangle(3))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle}()) == 1
    @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle}())) == [c2]

    loc = MOI.get(model, MOI.ListOfConstraints())
    @test length(loc) == 2
    @test (MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle) in loc
    @test (MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle) in loc

    f3 = MOIU.modifyfunction(f1, MOI.ScalarConstantChange(9))
    f3 = MOIU.modifyfunction(f3, MOI.ScalarCoefficientChange(y, 2))

    @test !(MOI.get(model, MOI.ConstraintFunction(), c1) ≈ f3)
    MOI.set(model, MOI.ConstraintFunction(), c1, f3)
    @test MOI.get(model, MOI.ConstraintFunction(), c1) ≈ f3

    f4 = MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 1, 2], MOI.ScalarAffineTerm.([2, 4, 3], [x, y, y])), [5, 7])
    c4 = MOI.add_constraint(model, f4, MOI.SecondOrderCone(2))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Int},MOI.SecondOrderCone}()) == 1

    f5 = MOI.VectorOfVariables([x])
    c5 = MOI.add_constraint(model, f5, MOI.RotatedSecondOrderCone(1))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.RotatedSecondOrderCone}()) == 1

    f6 = MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.([2, 9], [x, y])), [6, 8])
    c6 = MOI.add_constraint(model, f6, MOI.SecondOrderCone(2))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Int},MOI.SecondOrderCone}()) == 2

    f7 = MOI.VectorOfVariables([x, y])
    c7 = MOI.add_constraint(model, f7, MOI.Nonpositives(2))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonpositives}()) == 1

    loc1 = MOI.get(model, MOI.ListOfConstraints())
    loc2 = Vector{Tuple{DataType, DataType}}()
    function _pushloc(constrs::Vector{MOIU.ConstraintEntry{F, S}}) where {F, S}
        if !isempty(constrs)
            push!(loc2, (F, S))
        end
    end
    MOIU.broadcastcall(_pushloc, model)
    for loc in (loc1, loc2)
        @test length(loc) == 5
        @test (MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle) in loc
        @test (MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle) in loc
        @test (MOI.VectorOfVariables,MOI.RotatedSecondOrderCone) in loc
        @test (MOI.VectorAffineFunction{Int},MOI.SecondOrderCone) in loc
        @test (MOI.VectorOfVariables,MOI.Nonpositives) in loc
    end

    @test MOI.is_valid(model, c4)
    MOI.delete(model, c4)
    @test !MOI.is_valid(model, c4)

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Int},MOI.SecondOrderCone}()) == 1
    @test MOI.get(model, MOI.ConstraintFunction(), c6).constants == f6.constants

    MOI.delete(model, y)

    f = MOI.get(model, MOI.ConstraintFunction(), c2)
    @test f.affine_terms == MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.([3, 1], [x, x]))
    @test f.quadratic_terms == MOI.VectorQuadraticTerm.([1], MOI.ScalarQuadraticTerm.([1], [x], [x]))
    @test f.constants == [7, 3, 4]

    f = MOI.get(model, MOI.ConstraintFunction(), c6)
    @test f.terms == MOI.VectorAffineTerm.([1], MOI.ScalarAffineTerm.([2], [x]))
    @test f.constants == [6, 8]

    f =  MOI.get(model, MOI.ConstraintFunction(), c7)
    @test f.variables == [x]

    s =  MOI.get(model, MOI.ConstraintSet(), c7)
    @test MOI.dimension(s) == 1

end

# We create a new function and set to test catching errors if users create their
# own sets and functions
struct FunctionNotSupportedBySolvers <: MOI.AbstractScalarFunction
    x::MOI.VariableIndex
end
struct SetNotSupportedBySolvers <: MOI.AbstractSet end
@testset "Default fallbacks" begin
    @testset "set" begin
        m = Model{Float64}()
        x = MOI.add_variable(m)
        c = MOI.add_constraint(m, MOI.SingleVariable(x), MOI.LessThan(0.0))
        @test !MOI.supports_constraint(m, FunctionNotSupportedBySolvers, SetNotSupportedBySolvers)

        # set of different type
        @test_throws Exception MOI.set(m, MOI.ConstraintSet(), c, MOI.GreaterThan(0.0))
        # set not implemented
        @test_throws Exception MOI.set(m, MOI.ConstraintSet(), c, SetNotSupportedBySolvers())

        # function of different type
        @test_throws Exception MOI.set(m, MOI.ConstraintFunction(), c, MOI.VectorOfVariables([x]))
        # function not implemented
        @test_throws Exception MOI.set(m, MOI.ConstraintFunction(), c, FunctionNotSupportedBySolvers(x))
    end
end
