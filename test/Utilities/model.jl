# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestModel

using Test

import MathOptInterface as MOI

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

###
### Test helpers
###

"""
    TestExternalModel

We need to define this in a module at the top level because it can't be
defined in a testset. If it runs without error, then we're okay.
"""
module TestExternalModel

import MathOptInterface as MOI

struct NewSet <: MOI.AbstractScalarSet end

struct NewFunction <: MOI.AbstractScalarFunction end

MOI.Utilities.canonicalize!(f::NewFunction) = f

Base.copy(::NewFunction) = NewFunction()

Base.copy(::NewSet) = NewSet()

MOI.Utilities.@model(
    ExternalModel,
    (MOI.ZeroOne, NewSet),
    (),
    (),
    (),
    (NewFunction,),
    (MOI.ScalarAffineFunction,),
    (),
    (),
)

MOI.Utilities.@model(
    ExternalOptimizer,
    (MOI.ZeroOne, NewSet),
    (),
    (),
    (),
    (NewFunction,),
    (),
    (),
    (),
    true,
)

end

struct DummyFunction <: MOI.AbstractScalarFunction end

struct DummySet <: MOI.AbstractScalarSet end

# We create a new function and set to test catching errors if users create their
# own sets and functions
struct FunctionNotSupportedBySolvers <: MOI.AbstractScalarFunction
    x::MOI.VariableIndex
end

struct SetNotSupportedBySolvers <: MOI.AbstractSet end

###
### The tests start here.
###

function test_MOI_Test()
    MOI.Test.runtests(
        MOI.Utilities.Model{Float64}(),
        MOI.Test.Config(exclude = Any[MOI.optimize!,]),
    )
    return
end

function test_TestExternalModel_fields()
    @test isdefined(TestExternalModel, :ExternalModelScalarConstraints)
    @test !isdefined(TestExternalModel, :ExternalModelVectorConstraints)
    @test isdefined(TestExternalModel, :ExternalModelFunctionConstraints)
    @test isdefined(TestExternalModel, :ExternalOptimizerScalarConstraints)
    @test !isdefined(TestExternalModel, :ExternalOptimizerVectorConstraints)
    @test !isdefined(TestExternalModel, :ExternalOptimizerFunctionConstraints)
    @test TestExternalModel.ExternalModel{Int} == MOI.Utilities.GenericModel{
        Int,
        MOI.Utilities.ObjectiveContainer{Int},
        MOI.Utilities.VariablesContainer{Int},
        TestExternalModel.ExternalModelFunctionConstraints{Int},
    }
    model = MOI.Utilities.GenericOptimizer{
        Int,
        MOI.Utilities.ObjectiveContainer{Int},
        MOI.Utilities.VariablesContainer{Int},
        TestExternalModel.ExternalOptimizerScalarConstraints{
            Int,
            MOI.Utilities.VectorOfConstraints{
                TestExternalModel.NewFunction,
                MOI.ZeroOne,
            },
            MOI.Utilities.VectorOfConstraints{
                TestExternalModel.NewFunction,
                TestExternalModel.NewSet,
            },
        },
    }
    @test TestExternalModel.ExternalOptimizer{Int} == model

    return
end

function test_TestExternalModel_supertypes()
    model = TestExternalModel.ExternalModel{Float64}()
    optimizer = TestExternalModel.ExternalOptimizer{Float64}()
    @test isa(model, MOI.Utilities.AbstractModelLike{Float64})
    @test !isa(model, MOI.Utilities.AbstractOptimizer{Float64})
    @test !isa(model, MOI.AbstractOptimizer)
    @test !isa(optimizer, MOI.Utilities.AbstractModelLike{Float64})
    @test isa(optimizer, MOI.Utilities.AbstractOptimizer{Float64})
    @test isa(optimizer, MOI.AbstractOptimizer)
    return
end

function test_TestExternalModel()
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
    return
end

function _test_ObjectiveFunction(T)
    model = MOI.Utilities.Model{T}()
    @test !MOI.supports(model, MOI.ObjectiveFunction{DummyFunction}())
    for F in [
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
    ]
        @test MOI.supports(model, MOI.ObjectiveFunction{F}())
    end
    U = Float32
    for F in [MOI.ScalarAffineFunction{U}, MOI.ScalarQuadraticFunction{U}]
        @test !MOI.supports(model, MOI.ObjectiveFunction{F}())
    end
    return
end

function test_ObjectiveFunction()
    _test_ObjectiveFunction(Float64)
    _test_ObjectiveFunction(Int)
    return
end

function _test_VariableIndex(T)
    model = MOI.Utilities.Model{T}()
    @test !MOI.supports_constraint(model, DummyFunction, DummySet)
    @test !MOI.supports_constraint(model, MOI.VariableIndex, DummySet)
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
        @test MOI.supports_constraint(model, MOI.VariableIndex, S)
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
        @test !MOI.supports_constraint(model, MOI.VariableIndex, S)
        @test !MOI.supports_add_constrained_variable(model, S)
    end
    return
end

function test_VariableIndex()
    _test_VariableIndex(Float64)
    _test_VariableIndex(Int)
    return
end

function test_quadratic_functions()
    model = MOI.Utilities.Model{Int}()
    x, y = MOI.add_variables(model, 2)
    @test 2 == @inferred MOI.get(model, MOI.NumberOfVariables())
    f1 = MOI.ScalarQuadraticFunction(
        MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]),
        MOI.ScalarAffineTerm.([3], [x]),
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
        MOI.VectorQuadraticTerm.(
            [1, 1, 2],
            MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]),
        ),
        MOI.VectorAffineTerm.(
            [1, 2, 2],
            MOI.ScalarAffineTerm.([3, 1, 2], [x, x, y]),
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
    f3 = MOI.Utilities.modify_function(f1, change_1)
    change_2 = MOI.ScalarCoefficientChange(y, 2)
    f3 = MOI.Utilities.modify_function(f3, change_2)
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
    @test F3 ≈ MOI.get(model, MOI.ConstraintFunction(), c3)
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
    f5 = MOI.VectorOfVariables([x, x])
    c5 = MOI.add_constraint(model, f5, MOI.RotatedSecondOrderCone(2))
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
    loc2 = Vector{Tuple{Type,Type}}()
    function _pushloc(v::MOI.Utilities.VectorOfConstraints{F,S}) where {F,S}
        if !MOI.is_empty(v)
            push!(loc2, (F, S))
        end
    end
    _pushloc(::Nothing) = nothing
    function _pushloc(model::MOI.Utilities.StructOfConstraints)
        return MOI.Utilities.broadcastcall(_pushloc, model)
    end
    MOI.Utilities.broadcastcall(_pushloc, model.constraints)
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
    obj = 1x + 2y
    MOI.set(model, MOI.ObjectiveFunction{typeof(obj)}(), obj)
    message = string(
        "Cannot delete variable as it is constrained with other",
        " variables in a `MOI.VectorOfVariables`.",
    )
    err = MOI.DeleteNotAllowed(y, message)
    @test_throws err MOI.delete(model, y)
    @test MOI.get(model, MOI.ObjectiveFunction{typeof(obj)}()) ≈ 1x + 2y
    @test MOI.is_valid(model, c8)
    MOI.delete(model, c8)
    @test !MOI.is_valid(model, c8)
    @test 0 == @inferred MOI.get(
        model,
        MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SecondOrderCone}(),
    )
    MOI.delete(model, y)
    @test MOI.get(model, MOI.ObjectiveFunction{typeof(obj)}()) ≈ 1x
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

function test_default_fallbacks()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    func = convert(MOI.ScalarAffineFunction{Float64}, x)
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

function test_extension_dictionary()
    model = MOI.Utilities.Model{Float64}()
    model.ext[:my_store] = 1
    @test model.ext[:my_store] == 1
    MOI.empty!(model)
    @test model.ext[:my_store] == 1
    model.ext[:my_store] = 2
    dest = MOI.Utilities.Model{Float64}()
    MOI.copy_to(dest, model)
    @test !haskey(dest.ext, :my_store)
    @test model.ext[:my_store] == 2
    return
end

struct _UnsupportedSetIssue2005 <: MOI.AbstractScalarSet end

function test_get_unsupported_constraint()
    model = MOI.Utilities.Model{Float64}()
    S = _UnsupportedSetIssue2005
    for F in (MOI.VariableIndex, MOI.ScalarAffineFunction{Float64})
        @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 0
        @test MOI.get(model, MOI.ListOfConstraintIndices{F,S}()) ==
              MOI.ConstraintIndex{F,S}[]
    end
    return
end

function test_vector_of_constraints_list_of_constraint_attributes_set()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    MOI.add_constraint(model, 1.0 * x, MOI.EqualTo(1.0))
    inner = model.constraints.moi_scalaraffinefunction.moi_equalto
    F, S = MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}
    @test MOI.get(inner, MOI.ListOfConstraintAttributesSet{F,S}()) ==
          MOI.AbstractConstraintAttribute[]
    return
end

function test_copy_to_unsupported_scalar_variable()
    F, S = MOI.VariableIndex, MOI.GreaterThan{Float64}
    model = MOI.Utilities.Model{Float64}()
    x, _ = MOI.add_constrained_variable(model, MOI.GreaterThan(1.0))
    dest = MOI.FileFormats.CBF.Model()
    @test_throws(MOI.UnsupportedConstraint{F,S}, MOI.copy_to(dest, model))
    return
end

function test_copy_to_unsupported_vector_variables()
    F, S = MOI.VectorOfVariables, MOI.AllDifferent
    model = MOI.Utilities.Model{Float64}()
    x, _ = MOI.add_constrained_variables(model, MOI.AllDifferent(3))
    dest = MOI.FileFormats.CBF.Model()
    @test_throws(MOI.UnsupportedConstraint{F,S}, MOI.copy_to(dest, model))
    return
end

function test_copy_to_unsupported_scalar_function()
    F, S = MOI.ScalarNonlinearFunction, MOI.EqualTo{Float64}
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    f = MOI.ScalarNonlinearFunction(:log, Any[x])
    MOI.add_constraint(model, f, MOI.EqualTo(0.0))
    dest = MOI.FileFormats.CBF.Model()
    @test_throws(MOI.UnsupportedConstraint{F,S}, MOI.copy_to(dest, model))
    return
end

function test_indicator_constraints()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    z, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
    f = MOI.Utilities.operate(vcat, Float64, 1.0 * x, z)
    for si in (MOI.LessThan(1.0), MOI.GreaterThan(1.0), MOI.EqualTo(1.0))
        for a in (MOI.ACTIVATE_ON_ONE, MOI.ACTIVATE_ON_ZERO)
            s = MOI.Indicator{a}(si)
            MOI.supports_constraint(model, typeof(f), typeof(s))
            ci = MOI.add_constraint(model, f, s)
            @test MOI.is_valid(model, ci)
            @test MOI.get(model, MOI.ConstraintFunction(), ci) ≈ f
            @test MOI.get(model, MOI.ConstraintSet(), ci) == s
        end
    end
    return
end

function test_set_unsupported_objective()
    model = MOI.Utilities.Model{Float64}()
    f = FunctionNotSupportedBySolvers(MOI.VariableIndex(1))
    attr = MOI.ObjectiveFunction{typeof(f)}()
    @test_throws(MOI.UnsupportedAttribute(attr), MOI.set(model, attr, f))
    return
end

function test_variable_coefficient_VectorQuadraticFunction()
    x = MOI.VariableIndex.(1:2)
    fs = [
        1.0 * x[1] * x[1] + 2.0 * x[1] * x[2] + 3.0 * x[2] + 0.5,
        1.0 * x[2] * x[2],
    ]
    f = MOI.Utilities.vectorize(fs)
    for xi in x
        @test MOI.Utilities._variable_coefficient(f, xi, y -> sin(y.value)) ≈
              MOI.Utilities._variable_coefficient.(fs, xi, y -> sin(y.value))
    end
    return
end

function test_constraints_invalid_index()
    model = MOI.Utilities.Model{Float64}()
    F, S = FunctionNotSupportedBySolvers, MOI.ZeroOne
    ci = MOI.ConstraintIndex{F,S}(1)
    @test_throws(
        MOI.InvalidIndex(ci),
        MOI.Utilities.constraints(model.constraints, ci),
    )
    return
end

MOI.Utilities.@struct_of_constraints_by_set_types(
    Set4802,
    Union{MOI.LessThan{T},MOI.GreaterThan{T}},
    MOI.EqualTo{T},
    MOI.ZeroOne,
    Int,
)

function test_struct_of_constraints_by_set_types()
    set = Set4802{Float64,Vector{Float64},Vector{Float64},Vector{Bool},Int}()
    @test set.num_variables == 0
    @test set.int === nothing
    set.int = 1
    @test set.moi_zeroone === nothing
    set.moi_zeroone = Bool[]
    @test set.moi_equalto === nothing
    set.moi_equalto = Float64[]
    @test set.moi_lessthan === nothing
    set.moi_lessthan = Float64[]
    return
end

end  # module

TestModel.runtests()
