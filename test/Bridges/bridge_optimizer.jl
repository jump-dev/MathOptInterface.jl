module TestBridgeOptimizer

using Test

using MathOptInterface

const MOI = MathOptInterface

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

function unsupported_constraint_attribute()
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
    err = ArgumentError(
        "Bridge of type `$(bridge)` does not support accessing " *
        "the attribute `$attr`. If you encountered this error " *
        "unexpectedly, it probably means your model has been " *
        "reformulated using the bridge, and you are attempting to query " *
        "an attribute that we haven't implemented yet for this bridge. " *
        "Please open an issue at https://github.com/jump-dev/MathOptInterface.jl/issues/new " *
        "and provide a reproducible example explaining what you were " *
        "trying to do.",
    )
    x = MOI.add_variable(bridged_mock)
    ci = MOI.add_constraint(bridged_mock, x, MOI.Interval(0.0, 1.0))
    @test !MOI.Bridges.is_bridged(bridged_mock, ci)
    @test MOI.Bridges.is_bridged(bridged_mock.model, ci)
    @test !MOI.supports(bridged_mock, attr, typeof(ci))
    @test_throws err MOI.get(bridged_mock, attr, ci)
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

function test_show()
    mock = MOI.Utilities.MockOptimizer(NoIntervalModel{Float64}())
    bridged_mock = MOI.Bridges.Constraint.LessToGreater{Float64}(
        MOI.Bridges.Constraint.SplitInterval{Float64}(mock),
    )
    @test_broken sprint(show, bridged_mock) == MOI.Utilities.replace_acronym(
        """
$(MOI.Bridges.Constraint.SingleBridgeOptimizer{MOI.Bridges.Constraint.LessToGreaterBridge{Float64,F,G} where G<:MOI.AbstractScalarFunction where F<:MOI.AbstractScalarFunction,MOI.Bridges.Constraint.SingleBridgeOptimizer{MOI.Bridges.Constraint.SplitIntervalBridge{Float64,F,S,LS,US} where US<:MOI.AbstractSet where LS<:MOI.AbstractSet where S<:MOI.AbstractSet where F<:MOI.AbstractFunction,MOI.Utilities.MockOptimizer{NoIntervalModel{Float64}}}})
with 1 constraint bridge
with inner model $(MOI.Bridges.Constraint.SingleBridgeOptimizer{MOI.Bridges.Constraint.SplitIntervalBridge{Float64,F,S,LS,US} where US<:MOI.AbstractSet where LS<:MOI.AbstractSet where S<:MOI.AbstractSet where F<:MOI.AbstractFunction,MOI.Utilities.MockOptimizer{NoIntervalModel{Float64}}})
  with 0 constraint bridges
  with inner model $(MOI.Utilities.MockOptimizer{NoIntervalModel{Float64}})""",
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
    @test MOI.get(b, MOI.ConstraintFunction(), c) == new_func
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

end  # module

TestBridgeOptimizer.runtests()
