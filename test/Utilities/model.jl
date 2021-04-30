using Test
import MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.Test
const MOIU = MOI.Utilities

@testset "Basic constraint tests" begin
    model = MOIU.Model{Float64}()
    MOIT.basic_constraint_tests(model, MOIT.TestConfig())
end

# We need to test this in a module at the top level because it can't be defined
# in a testset. If it runs without error, then we're okay.
module TestExternalModel
using MathOptInterface
struct NewSet <: MathOptInterface.AbstractScalarSet end
struct NewFunction <: MathOptInterface.AbstractScalarFunction end
MathOptInterface.Utilities.canonicalize!(f::NewFunction) = f
Base.copy(::NewFunction) = NewFunction()
Base.copy(::NewSet) = NewSet()
MathOptInterface.Utilities.@model(
    ExternalModel,
    (MathOptInterface.ZeroOne, NewSet),
    (),
    (),
    (),
    (NewFunction,),
    (MathOptInterface.ScalarAffineFunction,),
    (),
    ()
)
MathOptInterface.Utilities.@model(
    ExternalOptimizer,
    (MathOptInterface.ZeroOne, NewSet),
    (),
    (),
    (),
    (NewFunction,),
    (),
    (),
    (),
    true
)
end
@test isdefined(TestExternalModel, :ExternalModelScalarConstraints)
@test !isdefined(TestExternalModel, :ExternalModelVectorConstraints)
@test isdefined(TestExternalModel, :ExternalModelFunctionConstraints)
@test isdefined(TestExternalModel, :ExternalOptimizerScalarConstraints)
@test !isdefined(TestExternalModel, :ExternalOptimizerVectorConstraints)
@test !isdefined(TestExternalModel, :ExternalOptimizerFunctionConstraints)
@test TestExternalModel.ExternalModel{Int} == MOIU.GenericModel{
    Int,
    TestExternalModel.ExternalModelFunctionConstraints{Int},
}
@test TestExternalModel.ExternalOptimizer{Int} == MOIU.GenericOptimizer{
    Int,
    TestExternalModel.ExternalOptimizerScalarConstraints{
        Int,
        MOIU.VectorOfConstraints{TestExternalModel.NewFunction,MOI.ZeroOne},
        MOIU.VectorOfConstraints{
            TestExternalModel.NewFunction,
            TestExternalModel.NewSet,
        },
    },
}

@testset "Super-types" begin
    model = TestExternalModel.ExternalModel{Float64}()
    optimizer = TestExternalModel.ExternalOptimizer{Float64}()
    @test isa(model, MOIU.AbstractModelLike{Float64})
    @test !isa(model, MOIU.AbstractOptimizer{Float64})
    @test !isa(model, MOI.AbstractOptimizer)
    @test !isa(optimizer, MOIU.AbstractModelLike{Float64})
    @test isa(optimizer, MOIU.AbstractOptimizer{Float64})
    @test isa(optimizer, MOI.AbstractOptimizer)
end

@testset "External @model" begin
    model = TestExternalModel.ExternalModel{Float64}()
    c = MOI.add_constraint(
        model,
        TestExternalModel.NewFunction(),
        TestExternalModel.NewSet(),
    )
    @test typeof(c) == MOI.ConstraintIndex{
        TestExternalModel.NewFunction,
        TestExternalModel.NewSet,
    }
    c2 = MOI.add_constraint(
        model,
        TestExternalModel.NewFunction(),
        MOI.ZeroOne(),
    )
    @test typeof(c2) ==
          MOI.ConstraintIndex{TestExternalModel.NewFunction,MOI.ZeroOne}
end

struct DummyFunction <: MOI.AbstractScalarFunction end
struct DummySet <: MOI.AbstractScalarSet end

@testset "Supports with $T" for T in [Float64, Int]
    model = MOIU.Model{T}()
    @testset "ObjectiveFunction" begin
        @test !MOI.supports(model, MOI.ObjectiveFunction{DummyFunction}())
        for F in [
            MOI.SingleVariable,
            MOI.ScalarAffineFunction{T},
            MOI.ScalarQuadraticFunction{T},
        ]
            @test MOI.supports(model, MOI.ObjectiveFunction{F}())
        end
        U = Float32
        for F in [MOI.ScalarAffineFunction{U}, MOI.ScalarQuadraticFunction{U}]
            @test !MOI.supports(model, MOI.ObjectiveFunction{F}())
        end
    end
    @testset "`SingleVariable`-in-`S`" begin
        @test !MOI.supports_constraint(model, DummyFunction, DummySet)
        @test !MOI.supports_constraint(model, MOI.SingleVariable, DummySet)
        @test !MOI.supports_add_constrained_variable(model, DummySet)
        for S in [
            MOI.EqualTo{T},
            MOI.GreaterThan{T},
            MOI.LessThan{T},
            MOI.Interval{T},
            MOI.Integer,
            MOI.ZeroOne,
            MOI.Semicontinuous{T},
            MOI.Semiinteger{T},
        ]
            @test MOI.supports_constraint(model, MOI.SingleVariable, S)
            @test MOI.supports_add_constrained_variable(model, S)
        end
        U = Float32
        for S in [
            MOI.EqualTo{U},
            MOI.GreaterThan{U},
            MOI.LessThan{U},
            MOI.Interval{U},
            MOI.Semicontinuous{U},
            MOI.Semiinteger{U},
        ]
            @test !MOI.supports_constraint(model, MOI.SingleVariable, S)
            @test !MOI.supports_add_constrained_variable(model, S)
        end
    end
end

@testset "Setting lower/upper bound twice" begin
    @testset "flag_to_set_type" begin
        T = Int
        @test_throws AssertionError MOIU.flag_to_set_type(0x11, T)
        @test MOIU.flag_to_set_type(0x10, T) == MOI.Integer
        @test MOIU.flag_to_set_type(0x20, T) == MOI.ZeroOne
    end
    @testset "$T" for T in [Int, Float64]
        model = MOIU.Model{T}()
        MOIT.set_lower_bound_twice(model, T)
        MOIT.set_upper_bound_twice(model, T)
    end
end

@testset "Name test" begin
    MOIT.nametest(MOIU.Model{Float64}())
end

@testset "Valid test" begin
    MOIT.validtest(MOIU.Model{Float64}())
end

@testset "Delete test" begin
    MOIT.delete_test(MOIU.Model{Float64}())
end

@testset "Empty test" begin
    MOIT.emptytest(MOIU.Model{Float64}())
end

@testset "supports_constraint test" begin
    MOIT.supports_constrainttest(MOIU.Model{Float64}(), Float64, Int)
    MOIT.supports_constrainttest(MOIU.Model{Int}(), Int, Float64)
end

@testset "OrderedIndices" begin
    MOIT.orderedindicestest(MOIU.Model{Float64}())
end

@testset "Continuous Linear tests" begin
    config = MOIT.TestConfig(solve = false)
    exclude = ["partial_start"] # Model doesn't support VariablePrimalStart.
    MOIT.contlineartest(MOIU.Model{Float64}(), config, exclude)
end

@testset "Continuous Conic tests" begin
    config = MOIT.TestConfig(solve = false)
    MOIT.contconictest(MOIU.Model{Float64}(), config)
end

@testset "Quadratic functions" begin
    model = MOIU.Model{Int}()

    x, y = MOI.add_variables(model, 2)
    @test 2 == @inferred MOI.get(model, MOI.NumberOfVariables())

    f1 = MOI.ScalarQuadraticFunction(
        MOI.ScalarAffineTerm.([3], [x]),
        MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]),
        7,
    )
    c1 = MOI.add_constraint(model, f1, MOI.Interval(-1, 1))

    @test 1 == @inferred MOI.get(
        model,
        MOI.NumberOfConstraints{
            MOI.ScalarQuadraticFunction{Int},
            MOI.Interval{Int},
        }(),
    )
    @test (@inferred MOI.get(
        model,
        MOI.ListOfConstraintIndices{
            MOI.ScalarQuadraticFunction{Int},
            MOI.Interval{Int},
        }(),
    )) == [c1]

    f2 = MOI.VectorQuadraticFunction(
        MOI.VectorAffineTerm.(
            [1, 2, 2],
            MOI.ScalarAffineTerm.([3, 1, 2], [x, x, y]),
        ),
        MOI.VectorQuadraticTerm.(
            [1, 1, 2],
            MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]),
        ),
        [7, 3, 4],
    )
    c2 = MOI.add_constraint(model, f2, MOI.PositiveSemidefiniteConeTriangle(3))

    @test 1 == @inferred MOI.get(
        model,
        MOI.NumberOfConstraints{
            MOI.VectorQuadraticFunction{Int},
            MOI.PositiveSemidefiniteConeTriangle,
        }(),
    )
    @test (@inferred MOI.get(
        model,
        MOI.ListOfConstraintIndices{
            MOI.VectorQuadraticFunction{Int},
            MOI.PositiveSemidefiniteConeTriangle,
        }(),
    )) == [c2]

    loc = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test (
        MOI.VectorQuadraticFunction{Int},
        MOI.PositiveSemidefiniteConeTriangle,
    ) in loc
    @test (
        MOI.VectorQuadraticFunction{Int},
        MOI.PositiveSemidefiniteConeTriangle,
    ) in loc

    c3 = MOI.add_constraint(model, f1, MOI.Interval(-1, 1))

    change_1 = MOI.ScalarConstantChange(9)
    f3 = MOIU.modify_function(f1, change_1)
    change_2 = MOI.ScalarCoefficientChange(y, 2)
    f3 = MOIU.modify_function(f3, change_2)

    @test !(MOI.get(model, MOI.ConstraintFunction(), c1) ≈ f3)
    @test !(MOI.get(model, MOI.ConstraintFunction(), c3) ≈ f3)
    MOI.set(model, MOI.ConstraintFunction(), c1, f3)
    MOI.modify(model, c3, change_1)
    MOI.modify(model, c3, change_2)
    F1 = MOI.get(model, MOI.CanonicalConstraintFunction(), c1)
    @test F1 ≈ f3
    @test F1 !== MOI.get(model, MOI.ConstraintFunction(), c1)
    @test MOI.Utilities.is_canonical(F1)
    F3 = MOI.get(model, MOI.CanonicalConstraintFunction(), c3)
    @test F3 ≈ f3
    @test F3 === MOI.get(model, MOI.ConstraintFunction(), c3)
    @test MOI.Utilities.is_canonical(F3)
    @test MOI.get(model, MOI.CanonicalConstraintFunction(), c1) ≈ f3
    @test MOI.Utilities.is_canonical(
        MOI.get(model, MOI.CanonicalConstraintFunction(), c1),
    )

    f4 = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(
            [1, 1, 2],
            MOI.ScalarAffineTerm.([2, 4, 3], [x, y, y]),
        ),
        [5, 7],
    )
    c4 = MOI.add_constraint(model, f4, MOI.SecondOrderCone(2))
    @test 1 == @inferred MOI.get(
        model,
        MOI.NumberOfConstraints{
            MOI.VectorAffineFunction{Int},
            MOI.SecondOrderCone,
        }(),
    )

    f5 = MOI.VectorOfVariables([x])
    c5 = MOI.add_constraint(model, f5, MOI.RotatedSecondOrderCone(1))
    @test 1 == @inferred MOI.get(
        model,
        MOI.NumberOfConstraints{
            MOI.VectorOfVariables,
            MOI.RotatedSecondOrderCone,
        }(),
    )

    f6 = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.([2, 9], [x, y])),
        [6, 8],
    )
    c6 = MOI.add_constraint(model, f6, MOI.SecondOrderCone(2))
    @test 2 == @inferred MOI.get(
        model,
        MOI.NumberOfConstraints{
            MOI.VectorAffineFunction{Int},
            MOI.SecondOrderCone,
        }(),
    )

    f7 = MOI.VectorOfVariables([x, y])
    c7 = MOI.add_constraint(model, f7, MOI.Nonpositives(2))
    @test 1 == @inferred MOI.get(
        model,
        MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonpositives}(),
    )

    f8 = MOI.VectorOfVariables([x, y])
    c8 = MOI.add_constraint(model, f8, MOI.SecondOrderCone(2))
    @test 1 == @inferred MOI.get(
        model,
        MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SecondOrderCone}(),
    )

    loc1 = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    loc2 = Vector{Tuple{DataType,DataType}}()
    function _pushloc(v::MOI.Utilities.VectorOfConstraints{F,S}) where {F,S}
        if !MOI.is_empty(v)
            push!(loc2, (F, S))
        end
    end
    function _pushloc(model::MOI.Utilities.StructOfConstraints)
        return MOIU.broadcastcall(_pushloc, model)
    end
    MOIU.broadcastcall(_pushloc, model.constraints)
    for loc in (loc1, loc2)
        @test length(loc) == 6
        @test (
            MOI.VectorQuadraticFunction{Int},
            MOI.PositiveSemidefiniteConeTriangle,
        ) in loc
        @test (
            MOI.VectorQuadraticFunction{Int},
            MOI.PositiveSemidefiniteConeTriangle,
        ) in loc
        @test (MOI.VectorOfVariables, MOI.RotatedSecondOrderCone) in loc
        @test (MOI.VectorAffineFunction{Int}, MOI.SecondOrderCone) in loc
        @test (MOI.VectorOfVariables, MOI.Nonpositives) in loc
    end

    @test MOI.is_valid(model, c4)
    MOI.delete(model, c4)
    @test !MOI.is_valid(model, c4)

    @test 1 == @inferred MOI.get(
        model,
        MOI.NumberOfConstraints{
            MOI.VectorAffineFunction{Int},
            MOI.SecondOrderCone,
        }(),
    )
    @test MOI.get(model, MOI.ConstraintFunction(), c6).constants == f6.constants

    fx = MOI.SingleVariable(x)
    fy = MOI.SingleVariable(y)
    obj = 1fx + 2fy
    MOI.set(model, MOI.ObjectiveFunction{typeof(obj)}(), obj)
    message = string(
        "Cannot delete variable as it is constrained with other",
        " variables in a `MOI.VectorOfVariables`.",
    )
    err = MOI.DeleteNotAllowed(y, message)
    @test_throws err MOI.delete(model, y)
    @test MOI.get(model, MOI.ObjectiveFunction{typeof(obj)}()) ≈ 1fx + 2fy

    @test MOI.is_valid(model, c8)
    MOI.delete(model, c8)
    @test !MOI.is_valid(model, c8)
    @test 0 == @inferred MOI.get(
        model,
        MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SecondOrderCone}(),
    )

    MOI.delete(model, y)
    @test MOI.get(model, MOI.ObjectiveFunction{typeof(obj)}()) ≈ 1fx

    f = MOI.get(model, MOI.ConstraintFunction(), c2)
    @test f.affine_terms ==
          MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.([3, 1], [x, x]))
    @test f.quadratic_terms ==
          MOI.VectorQuadraticTerm.([1], MOI.ScalarQuadraticTerm.([1], [x], [x]))
    @test f.constants == [7, 3, 4]

    f = MOI.get(model, MOI.ConstraintFunction(), c6)
    @test f.terms == MOI.VectorAffineTerm.([1], MOI.ScalarAffineTerm.([2], [x]))
    @test f.constants == [6, 8]

    @test 1 == @inferred MOI.get(
        model,
        MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonpositives}(),
    )
    @test [c7] == @inferred MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Nonpositives}(),
    )

    f = MOI.get(model, MOI.ConstraintFunction(), c7)
    @test f.variables == [x]

    s = MOI.get(model, MOI.ConstraintSet(), c7)
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
        model = MOIU.Model{Float64}()
        x = MOI.add_variable(model)
        func = convert(MOI.ScalarAffineFunction{Float64}, MOI.SingleVariable(x))
        c = MOI.add_constraint(model, func, MOI.LessThan(0.0))
        @test !MOI.supports_constraint(
            model,
            FunctionNotSupportedBySolvers,
            SetNotSupportedBySolvers,
        )

        # set of different type
        @test_throws Exception MOI.set(
            model,
            MOI.ConstraintSet(),
            c,
            MOI.GreaterThan(0.0),
        )
        # set not implemented
        @test_throws Exception MOI.set(
            model,
            MOI.ConstraintSet(),
            c,
            SetNotSupportedBySolvers(),
        )

        # function of different type
        @test_throws Exception MOI.set(
            model,
            MOI.ConstraintFunction(),
            c,
            MOI.VectorOfVariables([x]),
        )
        # function not implemented
        @test_throws Exception MOI.set(
            model,
            MOI.ConstraintFunction(),
            c,
            FunctionNotSupportedBySolvers(x),
        )
    end
end

@testset "ListOfSupportedConstraints" begin
    @testset "$set" for set in (
        MOI.EqualTo(1.0),
        MOI.GreaterThan(1.0),
        MOI.LessThan(1.0),
        MOI.Interval(1.0, 2.0),
        MOI.Semicontinuous(1.0, 2.0),
        MOI.Semiinteger(1.0, 2.0),
        MOI.Integer(),
        MOI.ZeroOne(),
    )
        model = MOIU.Model{Float64}()
        x = MOI.add_variable(model)
        MOI.add_constraint(model, MOI.SingleVariable(x), set)
        @test MOI.get(model, MOI.ListOfConstraintTypesPresent()) ==
              [(MOI.SingleVariable, typeof(set))]
    end
end

@testset "Extension dictionary" begin
    model = MOIU.Model{Float64}()
    model.ext[:my_store] = 1
    @test model.ext[:my_store] == 1
    MOI.empty!(model)
    @test model.ext[:my_store] == 1
    model.ext[:my_store] = 2
    dest = MOIU.Model{Float64}()
    MOI.copy_to(dest, model)
    @test !haskey(dest.ext, :my_store)
    @test model.ext[:my_store] == 2
end

@testset "Objective set via modify" begin
    model = MOIU.Model{Float64}()
    x = MOI.add_variable(model)
    attr = MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()
    MOI.modify(model, attr, MOI.ScalarCoefficientChange(x, 1.0))
    @test attr in MOI.get(model, MOI.ListOfModelAttributesSet())
end

@testset "Incorrect modifications" begin
    model = MOIU.Model{Float64}()
    x = MOI.add_variable(model)
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
        MOI.EqualTo(1.0),
    )
    @test_throws(
        ArgumentError,
        MOI.set(model, MOI.ConstraintSet(), c, MOI.LessThan(1.0)),
    )
    @test_throws(
        ArgumentError,
        MOI.set(model, MOI.ConstraintFunction(), c, MOI.SingleVariable(x)),
    )
end
