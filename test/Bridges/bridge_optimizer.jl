# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestBridgeOptimizer

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

include("utilities.jl")

struct DummyModelAttribute <: MOI.AbstractModelAttribute end

struct DummyEvaluator <: MOI.AbstractNLPEvaluator end

struct DummyVariableAttribute <: MOI.AbstractVariableAttribute end

struct DummyConstraintAttribute <: MOI.AbstractConstraintAttribute end

function test_subsitution_of_variables_constant()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged = MOI.Bridges.Variable.Vectorize{Float64}(mock)
    x, _ = MOI.add_constrained_variable(bridged, MOI.GreaterThan(1.0))
    f, s = 2.0x + 1.0, MOI.Integer()
    @test_throws(
        MOI.ScalarFunctionConstantNotZero{Float64,typeof(f),typeof(s)}(1.0),
        MOI.add_constraint(bridged, f, s),
    )
    return
end

function test_subsitution_of_variables()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged = MOI.Bridges.Variable.Vectorize{Float64}(mock)
    x, cx = MOI.add_constrained_variable(bridged, MOI.GreaterThan(1.0))
    y = MOI.get(mock, MOI.ListOfVariableIndices())[1]
    c2x = MOI.add_constraint(bridged, 2.0x, MOI.GreaterThan(1.0))
    @test MOI.get(bridged, MOI.ConstraintFunction(), c2x) ≈ 2.0x
    @test MOI.get(bridged, MOI.ConstraintSet(), c2x) == MOI.GreaterThan(1.0)
    # The constant was moved to the set
    @test MOI.get(mock, MOI.ConstraintFunction(), c2x) ≈ 2.0y
    @test MOI.get(mock, MOI.ConstraintSet(), c2x) == MOI.GreaterThan(-1.0)

    MOI.set(bridged, MOI.ConstraintSet(), c2x, MOI.GreaterThan(2.0))
    @test MOI.get(bridged, MOI.ConstraintFunction(), c2x) ≈ 2.0x
    @test MOI.get(bridged, MOI.ConstraintSet(), c2x) == MOI.GreaterThan(2.0)
    @test MOI.get(mock, MOI.ConstraintFunction(), c2x) ≈ 2.0y
    @test MOI.get(mock, MOI.ConstraintSet(), c2x) == MOI.GreaterThan(0.0)

    MOI.set(bridged, MOI.ConstraintFunction(), c2x, 3.0x)
    @test MOI.get(bridged, MOI.ConstraintFunction(), c2x) ≈ 3.0x
    @test MOI.get(bridged, MOI.ConstraintSet(), c2x) == MOI.GreaterThan(2.0)
    @test MOI.get(mock, MOI.ConstraintFunction(), c2x) ≈ 3.0y
    @test MOI.get(mock, MOI.ConstraintSet(), c2x) == MOI.GreaterThan(-1.0)

    MOI.set(bridged, MOI.ConstraintSet(), c2x, MOI.GreaterThan(4.0))
    @test MOI.get(bridged, MOI.ConstraintFunction(), c2x) ≈ 3.0x
    @test MOI.get(bridged, MOI.ConstraintSet(), c2x) == MOI.GreaterThan(4.0)
    @test MOI.get(mock, MOI.ConstraintFunction(), c2x) ≈ 3.0y
    @test MOI.get(mock, MOI.ConstraintSet(), c2x) == MOI.GreaterThan(1.0)

    MOI.set(bridged, MOI.ObjectiveFunction{typeof(2.0x)}(), 2.0x)
    @test MOI.get(mock, MOI.ObjectiveFunction{typeof(2.0y)}()) ≈ 2.0y + 2.0

    MOI.set(bridged, DummyModelAttribute(), 2.0x + 1.0)
    @test MOI.get(bridged, DummyModelAttribute()) ≈ 2.0x + 1.0
    @test MOI.get(mock, DummyModelAttribute()) ≈ 2.0y + 3.0

    z = MOI.add_variable(bridged)
    for (attr, index) in
        [(DummyVariableAttribute(), z), (DummyConstraintAttribute(), c2x)]
        MOI.set(bridged, attr, index, 1.0x)
        @test MOI.get(bridged, attr, index) ≈ 1.0x
        @test MOI.get(mock, attr, index) ≈ 1.0y + 1.0

        MOI.set(bridged, attr, [index], [3.0x + 1.0z])
        @test MOI.get(bridged, attr, [index])[1] ≈ 3.0x + 1.0z
        @test MOI.get(mock, attr, [index])[1] ≈ 3.0y + 1.0z + 3.0
    end
    return
end

function test_raw_solver()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged = MOI.Bridges.Variable.Vectorize{Float64}(mock)
    return MOI.get(bridged, MOI.RawSolver()) === mock
end

function test_HeuristicCallback()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged = MOI.Bridges.Variable.Vectorize{Float64}(mock)
    attr = MOI.HeuristicCallback()
    f(::Any) = nothing
    MOI.set(bridged, attr, f)
    @test MOI.get(bridged, attr) === f
    return
end

function test_CallbackVariablePrimal()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged = MOI.Bridges.Variable.Vectorize{Float64}(mock)
    x, _ = MOI.add_constrained_variable(bridged, MOI.GreaterThan(1.0))
    y = MOI.get(mock, MOI.ListOfVariableIndices())[1]
    z = MOI.add_variable(bridged)
    attr = MOI.CallbackVariablePrimal(nothing)
    @test_throws(
        ErrorException("No mock callback primal is set for variable `$z`."),
        MOI.get(bridged, attr, z),
    )
    MOI.set(mock, attr, y, 1.0)
    MOI.set(mock, attr, z, 2.0)
    @test MOI.get(bridged, attr, z) == 2.0
    err = ArgumentError(
        "Variable bridge of type `$(typeof(MOI.Bridges.bridge(bridged, x)))` " *
        "does not support accessing the attribute `$attr`.",
    )
    @test_throws err MOI.get(bridged, attr, x)
    return
end

function test_LazyConstraint()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged = MOI.Bridges.Variable.Vectorize{Float64}(mock)
    x, _ = MOI.add_constrained_variable(bridged, MOI.GreaterThan(1.0))
    y = MOI.get(mock, MOI.ListOfVariableIndices())[1]
    sub = MOI.LazyConstraint(nothing)
    @test MOI.supports(bridged, sub)
    MOI.submit(bridged, sub, 2.0x, MOI.GreaterThan(1.0))
    mock.submitted[sub][1][1] ≈ 2.0y + 2.0
    mock.submitted[sub][1][2] == MOI.GreaterThan(1.0)
    return
end

function test_HeuristicSolution()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged = MOI.Bridges.Variable.Vectorize{Float64}(mock)
    x, _ = MOI.add_constrained_variable(bridged, MOI.GreaterThan(1.0))
    y = MOI.get(mock, MOI.ListOfVariableIndices())[1]
    z = MOI.add_variable(bridged)
    sub = MOI.HeuristicSolution(nothing)
    @test MOI.supports(bridged, sub)
    MOI.submit(bridged, sub, [z], [1.0])
    mock.submitted[sub][1][1] == [z]
    mock.submitted[sub][1][2] == [1.0]
    err = ErrorException(
        "Cannot substitute `$x` as it is bridged into `$(1.0y + 1.0)`.",
    )
    @test_throws err MOI.submit(bridged, sub, [x], [1.0])
    return
end

function test_NLPBlock()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged = MOI.Bridges.Variable.Vectorize{Float64}(mock)
    nlp_data =
        MOI.NLPBlockData([MOI.NLPBoundsPair(1.0, 2.0)], DummyEvaluator(), false)
    MOI.set(bridged, MOI.NLPBlock(), nlp_data)
    for nlp_data in
        [MOI.get(bridged, MOI.NLPBlock()), MOI.get(mock, MOI.NLPBlock())]
        @test nlp_data.constraint_bounds == [MOI.NLPBoundsPair(1.0, 2.0)]
        @test nlp_data.evaluator isa DummyEvaluator
        @test nlp_data.has_objective == false
    end
    return
end

# Model not supporting Interval
MOI.Utilities.@model(
    NoIntervalModel,
    (),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval),
    (
        MOI.Zeros,
        MOI.Nonnegatives,
        MOI.Nonpositives,
        MOI.NormInfinityCone,
        MOI.NormOneCone,
        MOI.SecondOrderCone,
        MOI.RotatedSecondOrderCone,
        MOI.GeometricMeanCone,
        MOI.RelativeEntropyCone,
        MOI.NormSpectralCone,
        MOI.NormNuclearCone,
        MOI.PositiveSemidefiniteConeTriangle,
        MOI.ExponentialCone,
    ),
    (MOI.PowerCone, MOI.DualPowerCone),
    (),
    (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
    (MOI.VectorOfVariables,),
    (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction)
)

struct AttributeNotAllowed <: MOI.AbstractConstraintAttribute end

function MOI.supports(
    ::MOI.ModelLike,
    ::AttributeNotAllowed,
    ::Type{<:MOI.Bridges.Constraint.SplitIntervalBridge},
)
    return true
end

function test_unsupported_constraint_attribute()
    mock = MOI.Utilities.MockOptimizer(NoIntervalModel{Float64}())
    bridged_mock = MOI.Bridges.Constraint.LessToGreater{Float64}(
        MOI.Bridges.Constraint.SplitInterval{Float64}(mock),
    )
    bridge = MOI.Bridges.Constraint.SplitIntervalBridge{
        Float64,
        MOI.VariableIndex,
        MOI.Interval{Float64},
        MOI.GreaterThan{Float64},
        MOI.LessThan{Float64},
    }
    attr = MOI.Test.UnknownConstraintAttribute()
    message(action) = MOI.Bridges._attribute_error_message(attr, bridge, action)
    x = MOI.add_variable(bridged_mock)
    ci = MOI.add_constraint(bridged_mock, x, MOI.Interval(0.0, 1.0))
    @test !MOI.Bridges.is_bridged(bridged_mock, ci)
    @test MOI.Bridges.is_bridged(bridged_mock.model, ci)
    @test !MOI.supports(bridged_mock, attr, typeof(ci))
    err = ArgumentError(message("accessing"))
    @test_throws err MOI.get(bridged_mock, attr, ci)
    err = MOI.UnsupportedAttribute(attr, message("setting a value for"))
    @test_throws err MOI.set(bridged_mock, attr, ci, 1)
    attr = AttributeNotAllowed()
    err = MOI.SetAttributeNotAllowed(attr, message("setting a value for"))
    @test_throws err MOI.set(bridged_mock, attr, ci, 1)
    return
end

function test_issue_453()
    mock = MOI.Utilities.MockOptimizer(NoIntervalModel{Float64}())
    bridged_mock = MOI.Bridges.Constraint.LessToGreater{Float64}(
        MOI.Bridges.Constraint.SplitInterval{Float64}(mock),
    )
    MOI.Utilities.loadfromstring!(
        bridged_mock,
        """
    variables: x
    maxobjective: 3.0x
    c: 2.0x in Interval(1.0, 4.0)
    x in LessThan(1.5)
""",
    )
    x = MOI.get(bridged_mock, MOI.VariableIndex, "x")
    @test isa(x, MOI.VariableIndex)
    c1 = MOI.get(
        bridged_mock,
        MOI.ConstraintIndex{
            MOI.ScalarAffineFunction{Float64},
            MOI.Interval{Float64},
        },
        "c",
    )
    @test isa(
        c1,
        MOI.ConstraintIndex{
            MOI.ScalarAffineFunction{Float64},
            MOI.Interval{Float64},
        },
    )
    c2 = MOI.get(bridged_mock, MOI.ConstraintIndex, "c")
    @test c1 == c2
    @test MOI.is_valid(
        bridged_mock,
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.LessThan{Float64}}(x.value),
    )
    return
end

function test_custom_test()
    model = MOI.Bridges.Constraint.SplitInterval{Int}(NoIntervalModel{Int}())
    @test !MOI.Bridges.supports_bridging_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Interval{Float64},
    )

    x, y = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2

    f1 = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(3, x)], 7)
    c1 = MOI.add_constraint(model, f1, MOI.Interval(-1, 1))

    @test MOI.get(model, MOI.ListOfConstraintTypesPresent()) ==
          [(MOI.ScalarAffineFunction{Int}, MOI.Interval{Int})]
    _test_num_constraints(
        model,
        MOI.ScalarAffineFunction{Int},
        MOI.GreaterThan{Int},
        0,
    )
    _test_num_constraints(
        model,
        MOI.ScalarAffineFunction{Int},
        MOI.Interval{Int},
        1,
    )
    @test (@inferred MOI.get(
        model,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Int},
            MOI.Interval{Int},
        }(),
    )) == [c1]

    f2 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2, -1], [x, y]), 2)
    c2 = MOI.add_constraint(model, f1, MOI.GreaterThan(-2))

    @test MOI.get(model, MOI.ListOfConstraintTypesPresent()) == [
        (MOI.ScalarAffineFunction{Int}, MOI.GreaterThan{Int}),
        (MOI.ScalarAffineFunction{Int}, MOI.Interval{Int}),
    ]
    _test_num_constraints(
        model,
        MOI.ScalarAffineFunction{Int},
        MOI.GreaterThan{Int},
        1,
    )
    _test_num_constraints(
        model,
        MOI.ScalarAffineFunction{Int},
        MOI.Interval{Int},
        1,
    )
    @test (@inferred MOI.get(
        model,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Int},
            MOI.Interval{Int},
        }(),
    )) == [c1]
    @test (@inferred MOI.get(
        model,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Int},
            MOI.GreaterThan{Int},
        }(),
    )) == [c2]

    n = 4
    z = MOI.add_variables(model, n)
    scon_indices = MOI.ConstraintIndex{MOI.VariableIndex,MOI.Interval{Int}}[]
    for (i, v) in enumerate([x; y; z])
        f = v
        c = MOI.add_constraint(model, f, MOI.Interval(i, 2i))
        push!(scon_indices, c)

        @test Set(MOI.get(model, MOI.ListOfConstraintTypesPresent())) == Set([
            (MOI.ScalarAffineFunction{Int}, MOI.GreaterThan{Int}),
            (MOI.ScalarAffineFunction{Int}, MOI.Interval{Int}),
            (MOI.VariableIndex, MOI.Interval{Int}),
        ])
        _test_num_constraints(
            model,
            MOI.ScalarAffineFunction{Int},
            MOI.GreaterThan{Int},
            1,
        )
        _test_num_constraints(
            model,
            MOI.ScalarAffineFunction{Int},
            MOI.Interval{Int},
            1,
        )
        _test_num_constraints(model, MOI.VariableIndex, MOI.Interval{Int}, i)
        @test (@inferred MOI.get(
            model,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{Int},
                MOI.Interval{Int},
            }(),
        )) == [c1]
        @test (@inferred MOI.get(
            model,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{Int},
                MOI.GreaterThan{Int},
            }(),
        )) == [c2]
        # The indices should be returned in order of creation
        @test (@inferred MOI.get(
            model,
            MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.Interval{Int}}(),
        )) == scon_indices
    end

    vcon_indices = MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Nonnegatives}[]
    for (i, v) in enumerate(z)
        f = MOI.VectorOfVariables([v])
        c = MOI.add_constraint(model, f, MOI.Nonnegatives(1))
        push!(vcon_indices, c)

        @test Set(MOI.get(model, MOI.ListOfConstraintTypesPresent())) == Set([
            (MOI.ScalarAffineFunction{Int}, MOI.GreaterThan{Int}),
            (MOI.ScalarAffineFunction{Int}, MOI.Interval{Int}),
            (MOI.VariableIndex, MOI.Interval{Int}),
            (MOI.VectorOfVariables, MOI.Nonnegatives),
        ])
        _test_num_constraints(
            model,
            MOI.ScalarAffineFunction{Int},
            MOI.GreaterThan{Int},
            1,
        )
        _test_num_constraints(
            model,
            MOI.ScalarAffineFunction{Int},
            MOI.Interval{Int},
            1,
        )
        _test_num_constraints(
            model,
            MOI.VariableIndex,
            MOI.Interval{Int},
            n + 2,
        )
        _test_num_constraints(model, MOI.VectorOfVariables, MOI.Nonnegatives, i)
        @test (@inferred MOI.get(
            model,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{Int},
                MOI.Interval{Int},
            }(),
        )) == [c1]
        @test (@inferred MOI.get(
            model,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{Int},
                MOI.GreaterThan{Int},
            }(),
        )) == [c2]
        # The indices should be returned in order of creation
        @test (@inferred MOI.get(
            model,
            MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.Interval{Int}}(),
        )) == scon_indices
        @test (@inferred MOI.get(
            model,
            MOI.ListOfConstraintIndices{
                MOI.VectorOfVariables,
                MOI.Nonnegatives,
            }(),
        )) == vcon_indices
    end

    @test MOI.is_valid(model, c2)
    MOI.delete(model, c2)

    @test Set(MOI.get(model, MOI.ListOfConstraintTypesPresent())) == Set([
        (MOI.ScalarAffineFunction{Int}, MOI.Interval{Int}),
        (MOI.VariableIndex, MOI.Interval{Int}),
        (MOI.VectorOfVariables, MOI.Nonnegatives),
    ])
    _test_num_constraints(
        model,
        MOI.ScalarAffineFunction{Int},
        MOI.GreaterThan{Int},
        0,
    )
    _test_num_constraints(
        model,
        MOI.ScalarAffineFunction{Int},
        MOI.Interval{Int},
        1,
    )
    _test_num_constraints(model, MOI.VariableIndex, MOI.Interval{Int}, n + 2)
    _test_num_constraints(model, MOI.VectorOfVariables, MOI.Nonnegatives, n)
    @test (@inferred MOI.get(
        model,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Int},
            MOI.Interval{Int},
        }(),
    )) == [c1]
    # The indices should be returned in order of creation
    @test (@inferred MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.Interval{Int}}(),
    )) == scon_indices
    @test (@inferred MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Nonnegatives}(),
    )) == vcon_indices
    return
end

function test_MOI_Test()
    mock = MOI.Utilities.MockOptimizer(NoIntervalModel{Float64}())
    bridged_mock = MOI.Bridges.Constraint.LessToGreater{Float64}(
        MOI.Bridges.Constraint.SplitInterval{Float64}(mock),
    )
    MOI.Test.runtests(
        bridged_mock,
        MOI.Test.Config(exclude = Any[MOI.optimize!]),
        include = ["test_linear_", "test_model_"],
        exclude = [
            "test_model_LowerBoundAlreadySet",
            "test_model_UpperBoundAlreadySet",
            "test_model_ListOfConstraintAttributesSet",
        ],
    )
    return
end

MOI.Utilities.@model(
    AffineOnlyModel,
    (),
    (MOI.Interval,),
    (MOI.PositiveSemidefiniteConeTriangle,),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (),
    (MOI.VectorAffineFunction,),
    true
)

function MOI.supports_constraint(
    ::AffineOnlyModel{T},
    ::Type{MOI.VariableIndex},
    ::Type{MOI.LessThan{T}},
) where {T}
    return false
end

function MOI.supports_constraint(
    ::AffineOnlyModel{T},
    ::Type{MOI.VariableIndex},
    ::Type{MOI.Interval{T}},
) where {T}
    return false
end

function test_double_deletion_scalar()
    # The variable is bridged to `VariableIndex`-in-`Interval` and then `ScalarAffineFunction`-in-`Interval`.
    # Hence there is two bridged `VariableIndex` constraints on the same variables and we need to be
    # careful not to delete the second one twice, see https://github.com/jump-dev/MathOptInterface.jl/issues/1231
    model =
        MOI.instantiate(AffineOnlyModel{Float64}, with_bridge_type = Float64)
    # If LessToGreaterBridge exists, this goes from:
    #   VariableIndex(x) in LessThan(1.0)
    #   ScalarAffineFunction(-1.0x) in GreaterThan(-1.0)     (LessToGreater)
    #   ScalarAffineFunction(-1.0x) in Interval(-1.0, Inf)   (GreaterToInterval)
    # we want instead
    #   VariableIndex(x) in LessThan(1.0)
    #   VariableIndex(x) in Interval(-Inf, 1.0)             (LessToInterval)
    #   ScalarAffineFunction(1.0x) in Interval(-Inf, 1.0)   (FunctionConversion)
    # To check that we handle the LessThan and Interval sets on the same
    # VariableIndex correctly.
    MOI.Bridges.remove_bridge(
        model,
        MOI.Bridges.Constraint.LessToGreaterBridge{Float64},
    )
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, x, MOI.LessThan(1.0))
    # Need to test the bridging to make sure it's not functionized first as otherwise,
    # this test would not cover the case we want to test
    b1 = MOI.Bridges.bridge(model, c)
    @test b1 isa MOI.Bridges.Constraint.LessToIntervalBridge
    b2 = MOI.Bridges.bridge(model, b1.constraint)
    @test b2 isa MOI.Bridges.Constraint.ScalarFunctionizeBridge
    @test !MOI.Bridges.is_bridged(model, b2.constraint)
    MOI.delete(model, x)
    @test !MOI.is_valid(model, x)
    @test MOI.is_empty(model)
    return
end

function test_double_deletion_vector()
    model =
        MOI.instantiate(AffineOnlyModel{Float64}, with_bridge_type = Float64)
    x = MOI.add_variables(model, 4)
    c = MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.PositiveSemidefiniteConeSquare(2),
    )
    b1 = MOI.Bridges.bridge(model, c)
    @test b1 isa MOI.Bridges.Constraint.SquareBridge
    b2 = MOI.Bridges.bridge(model, b1.triangle)
    @test b2 isa MOI.Bridges.Constraint.VectorFunctionizeBridge
    @test !MOI.Bridges.is_bridged(model, b2.constraint)
    MOI.delete(model, x)
    @test all(vi -> !MOI.is_valid(model, vi), x)
    @test MOI.is_empty(model)
    return
end

function test_nesting_SingleBridgeOptimizer()
    T = Float64
    model = MOI.Utilities.Model{T}()
    b0 = MOI.Bridges.Variable.Free{T}(model)
    b1 = MOI.Bridges.Constraint.ScalarFunctionize{T}(b0)
    b2 = MOI.Bridges.Constraint.VectorFunctionize{T}(b1)
    b = MOI.Bridges.Objective.Functionize{T}(b2)
    x = MOI.add_variable(b)
    @test MOI.is_valid(b, x)
    @test !MOI.Bridges.is_bridged(b, x)
    @test MOI.is_valid(b2, x)
    @test !MOI.Bridges.is_bridged(b2, x)
    @test MOI.is_valid(b1, x)
    @test !MOI.Bridges.is_bridged(b1, x)
    @test MOI.is_valid(b0, x)
    @test MOI.Bridges.is_bridged(b0, x)
    @test !MOI.is_valid(model, x)
    clt = MOI.add_constraint(b, x, MOI.LessThan(one(T)))
    @test MOI.is_valid(b, clt)
    @test !MOI.Bridges.is_bridged(b, clt)
    @test MOI.is_valid(b2, clt)
    @test !MOI.Bridges.is_bridged(b2, clt)
    @test MOI.is_valid(b1, clt)
    @test MOI.Bridges.is_bridged(b1, clt)
    @test !MOI.is_valid(b0, clt)
    @test !MOI.is_valid(model, clt)
    cnn =
        MOI.add_constraint(b, MOI.Utilities.vectorize([x]), MOI.Nonnegatives(1))
    @test MOI.is_valid(b, cnn)
    @test !MOI.Bridges.is_bridged(b, cnn)
    @test MOI.is_valid(b2, cnn)
    @test MOI.Bridges.is_bridged(b2, cnn)
    @test !MOI.is_valid(b1, cnn)
    @test !MOI.is_valid(b0, cnn)
    @test !MOI.is_valid(model, cnn)
    obj = x
    attr = MOI.ObjectiveFunction{typeof(obj)}()
    MOI.set(b, attr, obj)
    @test MOI.Bridges.is_bridged(b, attr)
    obj = one(T) * x
    attr = MOI.ObjectiveFunction{typeof(obj)}()
    MOI.set(b, attr, obj)
    @test !MOI.Bridges.is_bridged(b, attr)
    @test !MOI.Bridges.is_bridged(b2, attr)
    @test !MOI.Bridges.is_bridged(b1, attr)
    @test !MOI.Bridges.is_bridged(b0, attr)
    return
end

struct UnsupportedSet <: MOI.AbstractScalarSet end

function test_supports_bridging_constrained_variable()
    model = MOI.Bridges.Constraint.SplitInterval{Int}(NoIntervalModel{Int}())
    @test !MOI.Bridges.supports_bridging_constrained_variable(
        model,
        UnsupportedSet,
    )
    return
end

function test_supports_add_constrained_variable()
    model = MOI.Bridges.Constraint.SplitInterval{Int}(NoIntervalModel{Int}())
    @test !MOI.supports_add_constrained_variable(model, UnsupportedSet)
    return
end

function test_supports_bridging_constraint()
    model = MOI.Bridges.Constraint.SplitInterval{Int}(NoIntervalModel{Int}())
    @test !MOI.Bridges.supports_bridging_constraint(
        model,
        MOI.VariableIndex,
        UnsupportedSet,
    )
    return
end

function test_supports_bridging_objective_function()
    model = MOI.Bridges.Constraint.SplitInterval{Int}(NoIntervalModel{Int}())
    @test !MOI.Bridges.supports_bridging_objective_function(
        model,
        MOI.VariableIndex,
    )
    return
end

function test_get_ObjectiveFunctionType()
    model = MOI.Bridges.Constraint.SplitInterval{Int}(NoIntervalModel{Int}())
    x = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) == MOI.VariableIndex
    return
end

include("identity_bridge.jl")

function test_recursive_model_variable(::Type{T} = Int) where {T}
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}())
    BT = IdentityBridges.VariableBridge{T}
    b = MOI.Bridges.Variable.SingleBridgeOptimizer{BT}(model)
    x, cx = MOI.add_constrained_variable(b, MOI.EqualTo(one(T)))
    @test MOI.Bridges.is_bridged(b, x)
    @test MOI.Bridges.is_bridged(b, cx)
    @test MOI.get(b, MOI.ConstraintFunction(), cx) == x
    @test MOI.get(b, MOI.ConstraintSet(), cx) == MOI.EqualTo(one(T))
    MOI.set(b, MOI.ConstraintSet(), cx, MOI.EqualTo(zero(T)))
    @test MOI.get(b, MOI.ConstraintSet(), cx) == MOI.EqualTo(zero(T))
    @test MOI.is_valid(b, x)
    MOI.delete(b, x)
    @test !MOI.is_valid(b, x)
    MOI.Bridges.runtests(
        IdentityBridges.VariableBridge,
        """
        constrainedvariable: x in EqualTo(1.0)
        minobjective: 1.0 * x + 2.0
        """,
        """
        constrainedvariable: x in EqualTo(1.0)
        minobjective: 1.0 * x + 2.0
        """,
    )
    return
end

function test_recursive_model_constraint(::Type{T} = Int) where {T}
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}())
    BT = IdentityBridges.ConstraintBridge{T}
    b = MOI.Bridges.Constraint.SingleBridgeOptimizer{BT}(model)
    x = MOI.add_variable(b)
    func = one(T) * x
    set = MOI.EqualTo(zero(T))
    c = MOI.add_constraint(b, func, set)
    @test MOI.Bridges.is_bridged(b, c)
    @test MOI.get(b, MOI.ConstraintFunction(), c) ≈ func
    new_func = T(2) * x
    MOI.set(b, MOI.ConstraintFunction(), c, new_func)
    @test MOI.get(b, MOI.ConstraintFunction(), c) ≈ new_func
    MOI.modify(b, c, MOI.ScalarCoefficientChange(x, T(3)))
    @test MOI.get(b, MOI.ConstraintFunction(), c) ≈ T(3) * x
    MOI.modify(b, c, MOI.ScalarConstantChange(T(-1)))
    @test MOI.get(b, MOI.ConstraintFunction(), c) ≈ T(3) * x + T(-1)
    @test MOI.get(b, MOI.ConstraintSet(), c) == set
    new_set = MOI.EqualTo(one(T))
    MOI.set(b, MOI.ConstraintSet(), c, new_set)
    @test MOI.get(b, MOI.ConstraintSet(), c) == new_set
    MOI.set(b, MOI.ConstraintDualStart(), c, one(T))
    @test MOI.get(b, MOI.ConstraintDualStart(), c) == one(T)
    @test MOI.is_valid(b, c)
    MOI.delete(b, c)
    @test !MOI.is_valid(b, c)
    return
end

function test_recursive_model_objective(::Type{T} = Int) where {T}
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}())
    BT = IdentityBridges.ObjectiveBridge{T}
    b = MOI.Bridges.Objective.SingleBridgeOptimizer{BT}(model)
    x = MOI.add_variable(b)
    func = one(T) * x
    MOI.set(b, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    @test !MOI.Bridges.is_objective_bridged(b)
    attr = MOI.ObjectiveFunction{typeof(func)}()
    MOI.set(b, attr, func)
    @test MOI.Bridges.is_objective_bridged(b)
    @test MOI.get(b, attr) ≈ func
    attr = MOI.ObjectiveFunction{typeof(x)}()
    MOI.set(b, attr, x)
    @test !MOI.Bridges.is_objective_bridged(b)
    @test MOI.get(b, attr) ≈ x
end

function test_invalid_modifications()
    model = MOI.Bridges.full_bridge_optimizer(
        MOI.Utilities.Model{Float64}(),
        Float64,
    )
    config = MOI.Test.Config()
    MOI.Test.test_modification_set_function_single_variable(model, config)
    MOI.empty!(model)
    MOI.Test.test_modification_incorrect(model, config)
    MOI.empty!(model)
    MOI.Test.test_modification_incorrect_VariableIndex(model, config)
    return
end

function test_ListOfConstraintAttributesSet()
    model = MOI.Bridges.Constraint.SplitInterval{Float64}(
        MOI.Utilities.Model{Float64}(),
    )
    x = MOI.add_variable(model)
    f = 1.0 * x
    s = MOI.Interval(0.0, 1.0)
    c = MOI.add_constraint(model, f, s)
    MOI.set(model, MOI.ConstraintName(), c, "c")
    @test MOI.get(
        model,
        MOI.ListOfConstraintAttributesSet{typeof(f),typeof(s)}(),
    ) == [MOI.ConstraintName()]
    return
end

function test_constant_modification_with_affine_variable_bridge()
    model = MOI.Utilities.Model{Float64}()
    b = MOI.Bridges.Variable.Vectorize{Float64}(model)
    x, cx = MOI.add_constrained_variable(b, MOI.GreaterThan(1.0))
    func = MOI.Utilities.vectorize([3.0x + 2.0])
    ci = MOI.add_constraint(b, func, MOI.Zeros(1))
    obj_func = 4.0x - 1.0
    obj = MOI.ObjectiveFunction{typeof(obj_func)}()
    MOI.set(b, obj, obj_func)
    y = MOI.Bridges.bridged_variable_function(b, x)
    attr = MOI.ConstraintFunction()
    MOI.modify(b, ci, MOI.VectorConstantChange([-1.0]))
    new_func = MOI.Utilities.vectorize([3.0x - 1.0])
    new_inner_func = MOI.Utilities.vectorize([3.0y - 1.0])
    @test MOI.get(b, MOI.ConstraintFunction(), ci) ≈ new_func
    @test MOI.get(model, MOI.ConstraintFunction(), ci) ≈ new_inner_func
    MOI.modify(b, obj, MOI.ScalarConstantChange(1.0))
    new_obj = 4.0x + 1.0
    new_inner_obj = 4.0y + 1.0
    @test MOI.get(b, obj) ≈ new_obj
    @test MOI.get(model, obj) ≈ new_inner_obj
end

struct _IssueIpopt333 <: MOI.AbstractOptimizer end

function MOI.supports(
    ::_IssueIpopt333,
    ::MOI.ConstraintDualStart,
    ::Type{MOI.ConstraintIndex{MOI.VariableIndex,MOI.GreaterThan{Float64}}},
)
    return true
end

function MOI.supports_constraint(
    ::_IssueIpopt333,
    ::Type{MOI.VariableIndex},
    ::Type{MOI.GreaterThan{Float64}},
)
    return true
end

function test_IssueIpopt333_supports_ConstraintDualStart_VariableIndex()
    model = MOI.Bridges.full_bridge_optimizer(_IssueIpopt333(), Float64)
    attr = MOI.ConstraintDualStart()
    IndexType = MOI.ConstraintIndex{MOI.VariableIndex,MOI.GreaterThan{Float64}}
    @test MOI.supports(model, attr, IndexType)
    return
end

mutable struct _Issue1992 <: MOI.AbstractOptimizer
    supports::Bool
    variables::Int
    constraints::Int
    _Issue1992(flag) = new(flag, 0, 0)
end

function MOI.supports_add_constrained_variables(
    ::_Issue1992,
    ::Type{<:Union{MOI.Nonpositives,MOI.Nonnegatives}},
)
    return true
end

function MOI.add_constrained_variables(
    model::_Issue1992,
    set::S,
) where {S<:Union{MOI.Nonpositives,MOI.Nonnegatives}}
    model.variables += 1
    ci = MOI.ConstraintIndex{MOI.VectorOfVariables,S}(model.variables)
    return MOI.VariableIndex.(1:set.dimension), ci
end

function MOI.add_constraint(
    model::_Issue1992,
    ::F,
    ::S,
) where {F<:MOI.VectorAffineFunction{Float64},S<:MOI.Nonnegatives}
    model.constraints += 1
    return MOI.ConstraintIndex{F,S}(model.constraints)
end

function MOI.supports_constraint(
    ::_Issue1992,
    ::Type{MOI.VectorAffineFunction{Float64}},
    ::Type{MOI.Nonnegatives},
)
    return true
end

function MOI.supports(
    model::_Issue1992,
    ::MOI.ConstraintDualStart,
    ::Type{MOI.ConstraintIndex{F,MOI.Nonnegatives}},
) where {F<:MOI.VectorAffineFunction{Float64}}
    return model.supports
end

function test_Issue1992_supports_ConstraintDualStart_VariableIndex()
    # supports should be false
    model = MOI.Bridges.full_bridge_optimizer(_Issue1992(false), Float64)
    x, _ = MOI.add_constrained_variables(model, MOI.Nonpositives(1))
    c = MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.Nonnegatives(1))
    @test !MOI.supports(model, MOI.ConstraintDualStart(), typeof(c))
    # supports should be true
    model = MOI.Bridges.full_bridge_optimizer(_Issue1992(true), Float64)
    x, _ = MOI.add_constrained_variables(model, MOI.Nonpositives(1))
    c = MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.Nonnegatives(1))
    # !!! warning
    #     This test is broken with a false negative. See the discussion in
    #     PR#1992.
    @test_broken MOI.supports(model, MOI.ConstraintDualStart(), typeof(c))
    return
end

function test_bridge_supports_issue_1992()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Variable.NonposToNonneg{Float64}(inner)
    x = MOI.add_variable(model)
    c = MOI.add_constraint(
        model,
        MOI.VectorOfVariables([x]),
        MOI.Nonpositives(1),
    )
    @test MOI.supports(model, MOI.ConstraintDualStart(), typeof(c))
    @test MOI.get(model, MOI.ConstraintDualStart(), c) === nothing
    MOI.set(model, MOI.ConstraintDualStart(), c, [1.0])
    @test MOI.get(model, MOI.ConstraintDualStart(), c) == [1.0]
    return
end

function test_delete_constraint_vector()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Constraint.GreaterToLess{Float64}(inner)
    x = MOI.add_variables(model, 3)
    c = MOI.add_constraint.(model, 1.0 .* x, MOI.GreaterThan.(1.0:3.0))
    c_eq = MOI.add_constraint.(model, 1.0 .* x, MOI.EqualTo.(1.0:3.0))
    F, S = MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}
    @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 3
    MOI.delete(model, c)
    @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 0
    c = MOI.add_constraint.(model, 1.0 .* x, MOI.GreaterThan.(1.0:3.0))
    MOI.delete(model, [c[1], c[3]])
    @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 1
    @test MOI.get(model, MOI.ConstraintSet(), c[2]) == MOI.GreaterThan(2.0)
    F, S = MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}
    @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 3
    MOI.delete(model, c_eq)
    @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 0
    c_eq = MOI.add_constraint.(model, 1.0 .* x, MOI.EqualTo.(1.0:3.0))
    MOI.delete(model, [c_eq[1], c_eq[3]])
    @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 1
    @test MOI.get(model, MOI.ConstraintSet(), c_eq[2]) == MOI.EqualTo(2.0)
    return
end

function test_deleting_variable_in_bridged_objective()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Objective.VectorFunctionize{Float64}(inner)
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    attr = MOI.ObjectiveFunction{MOI.VectorOfVariables}()
    MOI.set(model, attr, MOI.VectorOfVariables(x))
    MOI.delete(model, x[1])
    @test MOI.get(model, attr) == MOI.VectorOfVariables([x[2]])
    return
end

function test_deleting_variables_in_bridged_objective()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Objective.VectorFunctionize{Float64}(inner)
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    attr = MOI.ObjectiveFunction{MOI.VectorOfVariables}()
    MOI.set(model, attr, MOI.VectorOfVariables(x))
    MOI.delete(model, [x[1]])
    @test MOI.get(model, attr) == MOI.VectorOfVariables([x[2]])
    return
end

function test_deleting_all_variables_in_bridged_objective()
    # We need to make sure that the bridges have the same functionality as the
    # base Utilities.Model.
    model_1 = MOI.Utilities.Model{Float64}()
    model_2 = MOI.Bridges.Objective.VectorFunctionize{Float64}(
        MOI.Utilities.Model{Float64}(),
    )
    for model in (model_1, model_2)
        x = MOI.add_variable(model)
        f = MOI.VectorOfVariables([x])
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
        MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
        MOI.delete(model, x)
        attrs = MOI.get(model, MOI.ListOfModelAttributesSet())
        @test length(attrs) == 1
        @test attrs[1] == MOI.ObjectiveSense()
    end
    return
end

function test_deleting_all_vector_variables_in_bridged_objective()
    # We need to make sure that the bridges have the same functionality as the
    # base Utilities.Model.
    model_1 = MOI.Utilities.Model{Float64}()
    model_2 = MOI.Bridges.Objective.VectorFunctionize{Float64}(
        MOI.Utilities.Model{Float64}(),
    )
    for model in (model_1, model_2)
        x = MOI.add_variable(model)
        f = MOI.VectorOfVariables([x])
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
        MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
        MOI.delete(model, [x])
        attrs = MOI.get(model, MOI.ListOfModelAttributesSet())
        @test length(attrs) == 1
        @test attrs[1] == MOI.ObjectiveSense()
    end
    return
end

function test_deleting_all_variables_in_bridged_functionize_objective()
    # We need to make sure that the bridges have the same functionality as the
    # base Utilities.Model.
    model_1 = MOI.Utilities.Model{Float64}()
    model_2 = MOI.Bridges.Objective.Functionize{Float64}(
        MOI.Utilities.Model{Float64}(),
    )
    for model in (model_1, model_2)
        x = MOI.add_variable(model)
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
        MOI.set(model, MOI.ObjectiveFunction{typeof(x)}(), x)
        MOI.delete(model, x)
        attrs = MOI.get(model, MOI.ListOfModelAttributesSet())
        @test length(attrs) == 1
        @test MOI.ObjectiveSense() in attrs
    end
    return
end

function test_modify_objective_scalar_quadratic_coefficient_change()
    T = Float64
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}())
    model = MOI.Bridges.Objective.Slack{T}(inner)
    x = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    attr = MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}}()
    MOI.set(model, attr, T(1) * x * x + T(2) * x + T(3))
    change = MOI.ScalarQuadraticCoefficientChange(x, x, T(4))
    @test_throws(
        MOI.Bridges.ModifyBridgeNotAllowed,
        MOI.modify(model, attr, change),
    )
    return
end

function test_modify_variable_scalar_quadratic_coefficient_change()
    T = Float64
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}())
    model = MOI.Bridges.Variable.Free{T}(inner)
    x = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    attr = MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}}()
    MOI.set(model, attr, T(1) * x * x + T(2) * x + T(3))
    change = MOI.ScalarQuadraticCoefficientChange(x, x, T(4))
    @test_throws MOI.ModifyObjectiveNotAllowed MOI.modify(model, attr, change)
    return
end

function test_modify_constraint_scalar_quadratic_coefficient_change()
    T = Float64
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}())
    model = MOI.Bridges.Constraint.QuadtoSOC{T}(inner)
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, T(1) * x * x + T(2) * x, MOI.LessThan(T(1)))
    change = MOI.ScalarQuadraticCoefficientChange(x, x, T(4))
    @test_throws(
        MOI.Bridges.ModifyBridgeNotAllowed,
        MOI.modify(model, c, change),
    )
    return
end

function test_show_modify_bridge_not_allowed()
    x = MOI.VariableIndex(1)
    change = MOI.ScalarQuadraticCoefficientChange(x, x, 2.0)
    err = MOI.Bridges.ModifyBridgeNotAllowed(change)
    @test occursin("cannot be performed", sprint(showerror, err))
    return
end

function test_list_of_constraints_with_attribute_set()
    config = MOI.Test.Config()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.full_bridge_optimizer(inner, Float64)
    MOI.Test.test_model_ListOfConstraintsWithAttributeSet(model, config)
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Constraint.Vectorize{Float64}(inner)
    MOI.Test.test_model_ListOfConstraintsWithAttributeSet(model, config)
    return
end

function test_first_bridge()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    bridge = MOI.Bridges.Constraint.ZeroOne{Float64}(inner)
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
        bridge,
    )
    x = MOI.add_variable(model)
    ci = MOI.add_constraint(model, x, MOI.ZeroOne())
    MOI.Utilities.attach_optimizer(model)
    b = MOI.get(model, MOI.Bridges.FirstBridge(), ci)
    @test b isa MOI.Bridges.Constraint.ZeroOneBridge
    y = MOI.add_variable(model)
    ci = MOI.add_constraint(model, y, MOI.Integer())
    @test_throws(
        MOI.GetAttributeNotAllowed{MOI.Bridges.FirstBridge},
        MOI.get(model, MOI.Bridges.FirstBridge(), ci),
    )
    return
end

function test_variable_bridge_constraint_attribute()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    model = MOI.Bridges.Variable.NonposToNonneg{Float64}(mock)
    x, cx = MOI.add_constrained_variables(model, MOI.Nonpositives(2))
    y, cy = MOI.add_constrained_variables(model, MOI.Nonpositives(2))
    @test MOI.Bridges.bridge(model, x[1]) === MOI.Bridges.bridge(model, cx)
    @test MOI.Bridges.bridge(model, x[2]) === MOI.Bridges.bridge(model, cx)
    @test MOI.Bridges.bridge(model, y[1]) === MOI.Bridges.bridge(model, cy)
    @test MOI.Bridges.bridge(model, y[2]) === MOI.Bridges.bridge(model, cy)
    var = [x; y]
    val = float.(collect(1:4))
    mock_var = MOI.get(mock, MOI.ListOfVariableIndices())
    MOI.set(mock, MOI.VariablePrimal(), mock_var, -val)
    @test MOI.get(model, MOI.VariablePrimal(), var) == val
    @test MOI.get(model, MOI.ConstraintPrimal(), cx) == val[1:2]
    @test MOI.get(model, MOI.ConstraintPrimal(), cy) == val[3:4]
    for v in var
        @test MOI.is_valid(model, v)
    end
    @test MOI.is_valid(model, cx)
    @test MOI.is_valid(model, cy)
    MOI.delete(model, x)
    @test !MOI.is_valid(model, cx)
    @test MOI.is_valid(model, cy)
    return
end

function test_ListOfVariablesWithAttributeSet(T = Float64)
    uf = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}())
    model = MOI.Bridges.full_bridge_optimizer(uf, T)
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.VariableName(), x[1], "x")
    # Passed through to Model with no special support
    attr = MOI.ListOfVariablesWithAttributeSet(MOI.VariableName())
    @test MOI.get(model, attr) == x
    # Handled by UniversalFallback
    attr = MOI.ListOfVariablesWithAttributeSet(MOI.VariablePrimalStart())
    # no attributes set
    @test MOI.get(model, attr) == MOI.VariableIndex[]
    # one attribute set
    MOI.set(model, MOI.VariablePrimalStart(), x[2], 1.0)
    @test MOI.get(model, attr) == [x[2]]
    return
end

function test_cannot_unbridge_variable_function()
    model = MOI.Bridges.Variable.Zeros{Float64}(MOI.Utilities.Model{Float64}())
    x, c = MOI.add_constrained_variables(model, MOI.Zeros(1))
    c2 = MOI.add_constraint(model, 1.0 * x[1], MOI.EqualTo(1.0))
    @test_throws(
        MOI.GetAttributeNotAllowed{MOI.ConstraintFunction},
        MOI.get(model, MOI.ConstraintFunction(), c2),
    )
    return
end

MOI.Utilities.@model(
    Model2452,
    (),
    (),
    (MOI.Nonnegatives, MOI.Zeros),
    (),
    (),
    (),
    (MOI.VectorOfVariables,),
    (MOI.VectorAffineFunction,)
)

function MOI.supports_constraint(
    ::Model2452{T},
    ::Type{MOI.VariableIndex},
    ::Type{
        <:Union{
            MOI.GreaterThan{T},
            MOI.LessThan{T},
            MOI.EqualTo{T},
            MOI.Interval{T},
            MOI.ZeroOne,
        },
    },
) where {T}
    return false
end

function MOI.supports_constraint(
    ::Model2452{T},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.Reals},
) where {T}
    return false
end

function test_issue_2452_multiple_variable_bridges()
    src = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x = MOI.add_variable(src)
    MOI.add_constraint(src, x, MOI.LessThan(1.0))
    c = MOI.add_constraint(src, 2.0 * x, MOI.EqualTo(3.0))
    dest = MOI.instantiate(Model2452{Float64}; with_bridge_type = Float64)
    index_map = MOI.copy_to(dest, src)
    set = MOI.get(dest, MOI.ConstraintSet(), index_map[c])
    @test set == MOI.EqualTo(3.0)
    @test_throws(
        MOI.SetAttributeNotAllowed,
        MOI.set(dest, MOI.ConstraintSet(), index_map[c], set),
    )
    return
end

function test_2452()
    F, S = MOI.VectorAffineFunction{Float64}, MOI.Zeros
    src = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x = MOI.add_variable(src)
    MOI.add_constraint(src, x, MOI.GreaterThan(1.0))
    set = MOI.EqualTo(3.0)
    c = MOI.add_constraint(src, 2.0 * x, set)
    dest = MOI.instantiate(Model2452{Float64}; with_bridge_type = Float64)
    index_map = MOI.copy_to(dest, src)
    y = only(MOI.get(dest.model, MOI.ListOfVariableIndices()))
    ci = only(MOI.get(dest.model, MOI.ListOfConstraintIndices{F,S}()))
    @test MOI.get(dest, MOI.ConstraintSet(), index_map[c]) == set
    @test ≈(
        MOI.get(dest.model, MOI.ConstraintFunction(), ci),
        MOI.Utilities.operate(vcat, Float64, -1.0 + 2.0 * y),
    )
    @test MOI.get(dest.model, MOI.ConstraintSet(), ci) == MOI.Zeros(1)
    @test_throws(
        MOI.SetAttributeNotAllowed,
        MOI.set(dest, MOI.ConstraintSet(), index_map[c], set),
    )
    return
end

function test_issue_2452_with_constant()
    src = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x = MOI.add_variable(src)
    MOI.add_constraint(src, x, MOI.GreaterThan(1.0))
    MOI.add_constraint(src, 2.0 * x + 1.0, MOI.EqualTo(3.0))
    dest = MOI.instantiate(Model2452{Float64}; with_bridge_type = Float64)
    @test_throws MOI.ScalarFunctionConstantNotZero MOI.copy_to(dest, src)
    return
end

function test_issue_2452_integer()
    src = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x = MOI.add_variable(src)
    MOI.add_constraint(src, x, MOI.GreaterThan(1.0))
    y = MOI.add_variable(src)
    c = MOI.add_constraint(src, 1.0 * y, MOI.Integer())
    dest = MOI.instantiate(Model2452{Float64}; with_bridge_type = Float64)
    index_map = MOI.copy_to(dest, src)
    @test MOI.get(dest, MOI.ConstraintSet(), index_map[c]) == MOI.Integer()
    return
end

struct EmptyBridgeOptimizer <: MOI.Bridges.AbstractBridgeOptimizer end

function test_supports_bridging_constrained_variable_fallback()
    @test !MOI.Bridges.supports_bridging_constrained_variable(
        EmptyBridgeOptimizer(),
        MOI.ZeroOne,
    )
    return
end

function test_supports_bridging_constraint_fallback()
    @test !MOI.Bridges.supports_bridging_constraint(
        EmptyBridgeOptimizer(),
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )
    return
end

function test_delete_variable_from_bridged_objective()
    model = MOI.Utilities.Model{Float64}()
    b = MOI.Bridges.Objective.VectorFunctionize{Float64}(model)
    x = MOI.add_variables(b, 2)
    y = MOI.add_variable(b)
    f = MOI.VectorOfVariables(x)
    MOI.set(b, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(b, MOI.ObjectiveFunction{typeof(f)}(), f)
    G = MOI.VectorAffineFunction{Float64}
    g = MOI.get(model, MOI.ObjectiveFunction{G}())
    MOI.delete(b, y)
    @test g ≈ MOI.get(model, MOI.ObjectiveFunction{G}())
    MOI.delete(b, x[1])
    @test ≈(
        MOI.Utilities.vectorize([MOI.Utilities.scalarize(g)[2]]),
        MOI.get(model, MOI.ObjectiveFunction{G}()),
    )
    MOI.delete(b, x[2])
    @test ≈(
        MOI.Utilities.vectorize([zero(MOI.ScalarAffineFunction{Float64})]),
        MOI.get(model, MOI.ObjectiveFunction{G}()),
    )
    return
end

function test_get_ObjectiveFunctionValue()
    model = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    b = MOI.Bridges.full_bridge_optimizer(model, Float64)
    x = MOI.add_variable(b)
    MOI.set(b, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(b, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    y = only(MOI.get(model, MOI.ListOfVariableIndices()))
    MOI.set(model, MOI.VariablePrimal(1), y, 1.23)
    @test MOI.get(b, MOI.Bridges.ObjectiveFunctionValue{typeof(x)}(1)) == 1.23
    return
end

function test_bridged_function()
    model = MOI.Utilities.Model{Float64}()
    b = MOI.Bridges.Variable.Free{Float64}(model)
    x = MOI.add_variable(b)
    y, _ = MOI.add_constrained_variable(b, MOI.GreaterThan(1.0))
    @test_throws(
        ErrorException("Using bridged variable in `VariableIndex` function."),
        MOI.Bridges.bridged_function(b, x),
    )
    @test MOI.Bridges.bridged_function(b, y) == y
    return
end

function test_bridged_constraint_function_no_variable_bridges()
    model = MOI.Utilities.Model{Float64}()
    bridged = MOI.Bridges.full_bridge_optimizer(model, Float64)
    x = MOI.add_variable(bridged)
    set = MOI.ZeroOne()
    @test MOI.Bridges.bridged_constraint_function(bridged, x, set) == (x, set)
    return
end

function test_bridged_constraint_function_with_variable_bridges()
    model = MOI.Utilities.Model{Float64}()
    b = MOI.Bridges.Variable.Free{Float64}(model)
    x = MOI.add_variable(b)
    set = MOI.ZeroOne()
    f, set = MOI.Bridges.bridged_constraint_function(b, 1.0 * x, set)
    @test set == MOI.ZeroOne()
    y = MOI.get(model, MOI.ListOfVariableIndices())
    @test f ≈ 1.0 * y[1] - 1.0 * y[2]
    return
end

struct EmptyObjectiveBridge <: MOI.Bridges.Objective.AbstractBridge end

function test_fallback_bridge_objective()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    @test_throws(
        MOI.UnsupportedAttribute(MOI.ObjectiveFunction{MOI.VariableIndex}()),
        MOI.Bridges.Objective.bridge_objective(EmptyObjectiveBridge, model, x),
    )
    return
end

function test_fallback_set_objective_sense()
    model = MOI.Utilities.Model{Float64}()
    bridge = EmptyObjectiveBridge()
    @test_throws(
        MOI.SetAttributeNotAllowed{MOI.ObjectiveSense},
        MOI.set(model, MOI.ObjectiveSense(), bridge, MOI.MIN_SENSE),
    )
    return
end

function test_fallback_get_objective_function()
    model = MOI.Utilities.Model{Float64}()
    attr = MOI.ObjectiveFunction{MOI.VariableIndex}()
    @test_throws(
        MOI.GetAttributeNotAllowed{typeof(attr)},
        MOI.get(model, attr, EmptyObjectiveBridge()),
    )
    return
end

function test_fallback_delete_objective_bridge()
    model = MOI.Utilities.Model{Float64}()
    @test_throws ArgumentError MOI.delete(model, EmptyObjectiveBridge())
    return
end

struct Model2714 <: MOI.ModelLike
    dimensions::Vector{Int}
    Model2714() = new(Int[])
end

MOI.is_empty(model::Model2714) = isempty(model.dimensions)

function MOI.supports_add_constrained_variables(
    ::Model2714,
    ::Type{MOI.Nonnegatives},
)
    return true
end

function MOI.add_constrained_variables(model::Model2714, set::MOI.Nonnegatives)
    n = MOI.dimension(set)
    x = MOI.VariableIndex(sum(model.dimensions) .+ (1:n))
    push!(model.dimensions, n)
    F, S = MOI.VectorOfVariables, MOI.Nonnegatives
    return x, MOI.ConstraintIndex{F,S}(length(model.dimensions))
end

function test_issue_2714()
    model = MOI.instantiate(Model2714; with_bridge_type = Float64)
    t, _ = MOI.add_constrained_variable(model, MOI.LessThan(10.0))
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    @test MOI.get(model, MOI.ListOfVariableIndices()) == [t]
    return
end

function test_BridgeRequiresFiniteDomainError()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.SOS1ToMILP{Int}(inner)
    x = MOI.add_variables(model, 3)
    c = MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.SOS1([1, 2, 3]))
    err = MOI.Bridges.BridgeRequiresFiniteDomainError(model.map[c], x[1])
    contents = sprint(showerror, err)
    @test occursin("To fix this error, add a lower and upper bound", contents)
    return
end

MOI.Utilities.@model(
    Model2817a,
    (),
    (),
    (MOI.Nonnegatives,),
    (),
    (),
    (),
    (),
    (MOI.VectorAffineFunction,)
);

function MOI.supports_constraint(
    ::Model2817a,
    ::Type{MOI.VariableIndex},
    ::Type{S},
) where {S<:MOI.AbstractScalarSet}
    return false
end

function test_issue_2817a()
    inner = Model2817a{Float64}()
    model = MOI.Bridges.full_bridge_optimizer(inner, Float64);
    x, c = MOI.add_constrained_variable(model, MOI.Interval(0.0, 1.0));
    @test isa(
        model.constraint_map[c],
        MOI.Bridges.Constraint.IntervalToHyperRectangleBridge,
    )
    MOI.delete(model, x)
    @test !MOI.is_valid(model, x)
    @test MOI.is_empty(model)
    @test MOI.is_empty(inner)
    @test isempty(model.constraint_map)
    return
end

MOI.Utilities.@model(
    Model2817b,
    (),
    (MOI.LessThan,),
    (),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (),
    ()
);

function MOI.supports_constraint(
    ::Model2817b,
    ::Type{MOI.VariableIndex},
    ::Type{S},
) where {S<:MOI.AbstractScalarSet}
    return false
end

function test_issue_2817b()
    inner = Model2817b{Float64}()
    model = MOI.Bridges.full_bridge_optimizer(inner, Float64);
    x, c = MOI.add_constrained_variables(model, MOI.Nonpositives(1))
    @test isa(model.constraint_map[c], MOI.Bridges.Constraint.ScalarizeBridge)
    MOI.delete(model, x)
    @test !MOI.is_valid(model, x[1])
    @test MOI.is_empty(model)
    @test MOI.is_empty(inner)
    @test isempty(model.constraint_map)
    return
end

MOI.Utilities.@model(
    Model2817c,
    (),
    (MOI.GreaterThan, MOI.LessThan),
    (),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (),
    ()
);

function MOI.supports_constraint(
    ::Model2817c,
    ::Type{MOI.VariableIndex},
    ::Type{S},
) where {S<:Union{MOI.ZeroOne,MOI.Semiinteger,MOI.Semicontinuous}}
    return false
end

function test_issue_2817c()
    inner = Model2817c{Float64}()
    model = MOI.Bridges.full_bridge_optimizer(inner, Float64);
    x, c = MOI.add_constrained_variable(model, MOI.Semiinteger(2.0, 3.0))
    @test isa(
        model.constraint_map[c],
        MOI.Bridges.Constraint.SemiToBinaryBridge,
    )
    MOI.delete(model, x)
    @test !MOI.is_valid(model, x)
    @test MOI.is_empty(model)
    @test MOI.is_empty(inner)
    @test isempty(model.constraint_map)
    return
end

function test_issue_2762_success()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Variable.ParameterToEqualTo{Float64}(inner)
    p, _ = MOI.add_constrained_variable(model, MOI.Parameter(1.0))
    MOI.set(model, MOI.VariableName(), p, "p")
    @test MOI.get(model, MOI.VariableName(), p) == "p"
    x = only(MOI.get(inner, MOI.ListOfVariableIndices()))
    @test MOI.get(inner, MOI.VariableName(), x) == "p"
    return
end

function test_issue_2762_fail()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Variable.NonposToNonneg{Float64}(inner)
    y, _ = MOI.add_constrained_variables(model, MOI.Nonpositives(2))
    MOI.set(model, MOI.VariableName(), y[1], "y1")
    @test MOI.get(model, MOI.VariableName(), y[1]) == "y1"
    x = MOI.get(inner, MOI.ListOfVariableIndices())
    @test all(isempty, MOI.get(inner, MOI.VariableName(), x))
    return
end

end  # module

TestBridgeOptimizer.runtests()
