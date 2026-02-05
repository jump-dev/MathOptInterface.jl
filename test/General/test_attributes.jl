# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestAttributes

using Test
import MathOptInterface as MOI

include("dummy.jl")

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$name", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
end

function test_attributes_is_set_by_optimize()
    @test MOI.is_set_by_optimize(MOI.TerminationStatus())
    @test !MOI.is_set_by_optimize(MOI.ConstraintSet())
    @test !MOI.is_set_by_optimize(MOI.ObjectiveSense())
    @test MOI.is_set_by_optimize(MOI.CallbackNodeStatus(1))
end

function test_attributes_is_copyable()
    @test !MOI.is_copyable(MOI.TerminationStatus())
    @test !MOI.is_copyable(MOI.ConstraintSet())
    @test MOI.is_copyable(MOI.ObjectiveSense())
end

function test_attributes_supports()
    model = DummyModel()
    @test_throws ArgumentError MOI.supports(model, MOI.TerminationStatus())
    @test_throws ArgumentError begin
        MOI.supports(
            model,
            MOI.ConstraintSet(),
            MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}},
        )
    end
    @test MOI.supports(model, MOI.ObjectiveSense())
end

function test_attributes_set_vector()
    attr = MOI.VariablePrimalStart()
    err = DimensionMismatch(
        "Number of indices (1) does not match the " *
        "number of values (2) set to `$attr`.",
    )
    model = DummyModel()
    x = MOI.VariableIndex(1)
    @test_throws err MOI.set(model, MOI.VariablePrimalStart(), [x], ones(2))
end

function test_attributes_integration_compute_conflict_1()
    optimizer = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.Model{Float64}(),
        MOI.Bridges.full_bridge_optimizer(optimizer, Float64),
    )
    x = MOI.add_variable(model)
    c1 = MOI.add_constraint(model, x, MOI.LessThan(0.0))
    c2 = MOI.add_constraint(model, x, MOI.GreaterThan(1.0))
    MOI.optimize!(model)
    @test MOI.get(optimizer, MOI.ConflictStatus()) ==
          MOI.COMPUTE_CONFLICT_NOT_CALLED
    MOI.set(optimizer, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
    MOI.set(optimizer, MOI.ConflictCount(), 1)
    MOI.set(
        optimizer,
        MOI.ConstraintConflictStatus(),
        MOI.get(
            optimizer,
            MOI.ListOfConstraintIndices{
                MOI.VariableIndex,
                MOI.LessThan{Float64},
            }(),
        )[1],
        MOI.NOT_IN_CONFLICT,
    )
    MOI.set(
        optimizer,
        MOI.ConstraintConflictStatus(),
        MOI.get(
            optimizer,
            MOI.ListOfConstraintIndices{
                MOI.VariableIndex,
                MOI.GreaterThan{Float64},
            }(),
        )[1],
        MOI.IN_CONFLICT,
    )
    MOI.compute_conflict!(model)
    @test MOI.get(model, MOI.ConflictStatus()) == MOI.CONFLICT_FOUND
    @test MOI.get(model, MOI.ConflictCount()) == 1
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c1) ==
          MOI.NOT_IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c2) == MOI.IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(1), c1) ==
          MOI.NOT_IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(1), c2) == MOI.IN_CONFLICT
    @test_throws MOI.ConflictIndexBoundsError MOI.get(
        model,
        MOI.ConstraintConflictStatus(2),
        c1,
    )
    @test_throws MOI.ConflictIndexBoundsError MOI.get(
        model,
        MOI.ConstraintConflictStatus(2),
        c2,
    )
end

MOI.Utilities.@model(
    OnlyScalarConstraints,
    (),
    (MOI.GreaterThan, MOI.LessThan),
    (),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (),
    ()
)

function test_attributes_integration_compute_conflict_2()
    optimizer = MOI.Utilities.MockOptimizer(OnlyScalarConstraints{Float64}())
    model = MOI.Bridges.full_bridge_optimizer(optimizer, Float64)
    x = MOI.add_variable(model)
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
        MOI.Interval(0.0, 1.0),
    )
    MOI.optimize!(model)
    @test MOI.get(optimizer, MOI.ConflictStatus()) ==
          MOI.COMPUTE_CONFLICT_NOT_CALLED
    MOI.set(optimizer, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
    MOI.set(optimizer, MOI.ConflictCount(), 1)
    MOI.compute_conflict!(model)
    @test MOI.get(model, MOI.ConflictStatus()) == MOI.CONFLICT_FOUND
    @test MOI.get(model, MOI.ConflictCount()) == 1
    return
end

struct _NoConstraintName <: MOI.AbstractOptimizer end

function test_no_constraint_name()
    model = _NoConstraintName()
    @test_throws(
        MOI.VariableIndexConstraintNameError(),
        MOI.supports(
            model,
            MOI.ConstraintName(),
            MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne},
        ),
    )
    @test_throws(
        MOI.VariableIndexConstraintNameError(),
        MOI.set(
            model,
            MOI.ConstraintName(),
            MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(1),
            "name",
        ),
    )
end

function test_get_fallback()
    model = DummyModelWithAdd()
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, x, MOI.EqualTo(0.0))
    @test_throws(
        MOI.GetAttributeNotAllowed(
            MOI.SolveTimeSec(),
            "$(typeof(model)) does not support getting the attribute " *
            "$(MOI.SolveTimeSec()).",
        ),
        MOI.get(model, MOI.SolveTimeSec()),
    )
    output = Ref{Cdouble}()
    @test_throws(
        MOI.GetAttributeNotAllowed(
            MOI.SolveTimeSec(),
            "$(typeof(model)) does not support getting the attribute " *
            "$(MOI.SolveTimeSec()).",
        ),
        MOI.get!(output, model, MOI.SolveTimeSec()),
    )
    @test_throws(
        MOI.GetAttributeNotAllowed(
            MOI.VariablePrimal(),
            "$(typeof(model)) does not support getting the attribute " *
            "$(MOI.VariablePrimal()).",
        ),
        MOI.get(model, MOI.VariablePrimal(), x),
    )
    @test_throws(
        MOI.GetAttributeNotAllowed(
            MOI.ConstraintPrimal(),
            "$(typeof(model)) does not support getting the attribute " *
            "$(MOI.ConstraintPrimal()).",
        ),
        MOI.get(model, MOI.ConstraintPrimal(), c),
    )
    @test_throws(
        MOI.GetAttributeNotAllowed(
            MOI.VariablePrimal(),
            "Unable to get attribute $(MOI.VariablePrimal()): invalid " *
            "arguments $((c,)).",
        ),
        MOI.get(model, MOI.VariablePrimal(), c),
    )
    return
end

function test_ConstraintBasisStatus_fallback()
    model = DummyModelWithAdd()
    c = MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}}(1)
    @test_throws(
        ErrorException(
            "Querying the basis status of a `VariableIndex` constraint is " *
            "not supported. Use [`VariableBasisStatus`](@ref) instead.",
        ),
        MOI.get(model, MOI.ConstraintBasisStatus(), c),
    )
end

function test_UnsupportedSubmittable()
    model = DummyModelWithAdd()
    sub = MOI.LazyConstraint{Int}(1)
    @test_throws(
        MOI.UnsupportedSubmittable(
            sub,
            "submit(::$(typeof(model)), ::$(typeof(sub))) is not supported.",
        ),
        MOI.submit(model, sub, 1),
    )
end

function test_attribute_value_type()
    @test MOI.attribute_value_type(MOI.CallbackNodeStatus(1)) ==
          MOI.CallbackNodeStatusCode
    @test MOI.attribute_value_type(MOI.LazyConstraintCallback()) == Function
    @test MOI.attribute_value_type(MOI.RelativeGap()) == Float64
    @test MOI.attribute_value_type(MOI.SimplexIterations()) == Int64
    @test MOI.attribute_value_type(MOI.BarrierIterations()) == Int64
    @test MOI.attribute_value_type(MOI.NodeCount()) == Int64
    @test MOI.attribute_value_type(
        MOI.ConstraintBridgingCost{MOI.VariableIndex,MOI.ZeroOne}(),
    ) == Float64
    @test MOI.attribute_value_type(MOI.VariableBridgingCost{MOI.ZeroOne}()) ==
          Float64
end

MOI.Utilities.@model(
    _Model1777,
    (),
    (MOI.LessThan,),
    (MOI.Nonnegatives,),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (MOI.VectorOfVariables,),
    ()
)

function MOI.supports_constraint(
    ::_Model1777,
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.Reals},
)
    return false
end

function MOI.supports_add_constrained_variables(
    ::_Model1777,
    ::Type{MOI.Nonnegatives},
)
    return true
end

MOI.supports_add_constrained_variables(::_Model1777, ::Type{MOI.Reals}) = false

function MOI.get(
    model::_Model1777,
    attr::MOI.ConstraintFunction,
    ci::MOI.ConstraintIndex,
)
    return MOI.get_fallback(model, attr, ci)
end

function test_issue_1777()
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
        MOI.Bridges.full_bridge_optimizer(_Model1777{Float64}(), Float64),
    )
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, 1.0 * x, MOI.LessThan(1.0))
    MOI.Utilities.attach_optimizer(model)
    MOI.set(model, MOI.ConstraintSet(), c, MOI.LessThan(2.0))
    @test MOI.get(model, MOI.ConstraintSet(), c) == MOI.LessThan(2.0)
    @test MOI.Utilities.state(model) == MOI.Utilities.EMPTY_OPTIMIZER
    return
end

function test_scalar_nonlinear_function_ConstraintName()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    f = MOI.ScalarNonlinearFunction(
        :+,
        Any[x, MOI.ScalarNonlinearFunction(:sin, Any[x])],
    )
    c = MOI.add_constraint(model, f, MOI.EqualTo(0.0))
    MOI.set(model, MOI.ConstraintName(), c, "c")
    @test MOI.get(model, MOI.ConstraintName(), c) == "c"
    return
end

function test_scalar_nonlinear_function_set_objective()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    f = MOI.ScalarNonlinearFunction(
        :+,
        Any[x, MOI.ScalarNonlinearFunction(:sin, Any[x])],
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    attr = MOI.ObjectiveFunction{typeof(f)}()
    MOI.set(model, attr, f)
    @test isapprox(MOI.get(model, attr), f)
    return
end

function test_attributes_AutomaticDifferentiationBackend()
    @test MOI.attribute_value_type(MOI.AutomaticDifferentiationBackend()) ==
          MOI.Nonlinear.AbstractAutomaticDifferentiation
    return
end

function test_empty_vector_attribute()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.get(model, MOI.ListOfVariableIndices())
    @test typeof(x) == Vector{MOI.VariableIndex}
    ret = MOI.get(model, MOI.VariablePrimalStart(), x)
    @test typeof(ret) == Vector{Any}
    ret = MOI.get(model, MOI.VariableName(), x)
    @test typeof(ret) == Vector{String}
    F, S = MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}
    c = MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
    ret = MOI.get(model, MOI.ConstraintPrimalStart(), c)
    @test typeof(ret) == Vector{Any}
    ret = MOI.get(model, MOI.ConstraintName(), c)
    @test typeof(ret) == Vector{String}
    return
end

function test_broadcastable_submittable()
    submit = MOI.LazyConstraint(1)
    b = Base.broadcastable(submit)
    @test b isa Base.RefValue
    @test b[] == submit
    return
end

function test_submit_not_allowed()
    submit = MOI.LazyConstraint(1)
    @test MOI.SubmitNotAllowed(submit) == MOI.SubmitNotAllowed(submit, "")
    err = MOI.SubmitNotAllowed(submit, "msg")
    contents = sprint(showerror, err)
    @test occursin("Submitting $submit cannot be performed", contents)
    @test occursin("msg", contents)
    return
end

struct ModelWithSupportedSubmittable <: MOI.ModelLike end

MOI.supports(::ModelWithSupportedSubmittable, ::MOI.LazyConstraint) = true

function test_submit_argument_error()
    model = ModelWithSupportedSubmittable()
    submit = MOI.LazyConstraint(1)
    @test MOI.supports(model, submit)
    @test_throws ArgumentError MOI.submit(model, submit, false)
    return
end

function test_showerror_OptimizeInProgress()
    err = MOI.OptimizeInProgress(MOI.VariablePrimal())
    @test sprint(showerror, err) ==
          "$(typeof(err)): Cannot get result as the `MOI.optimize!` has not finished."
    return
end

function test_showerror_FunctionTypeMismatch()
    F, G = MOI.VariableIndex, MOI.VectorOfVariables
    contents = sprint(showerror, MOI.FunctionTypeMismatch{F,G}())
    @test occursin("Cannot modify functions of different types", contents)
    return
end

function test_showerror_SetTypeMismatch()
    F, G = MOI.ZeroOne, MOI.Integer
    contents = sprint(showerror, MOI.SetTypeMismatch{F,G}())
    @test occursin("Cannot modify sets of different types", contents)
    return
end

function test_NLPBlockDual_is_set_by_optimize()
    @test MOI.is_set_by_optimize(MOI.NLPBlockDual())
    return
end

function test_CallbackVariablePrimal_is_set_by_optimize()
    @test MOI.is_set_by_optimize(MOI.CallbackVariablePrimal(nothing))
    return
end

function test_get_bang()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variables(model, 2)
    MOI.set.(model, MOI.VariableName(), x, ["x", "y"])
    output = ["", ""]
    @test MOI.get!(output, model, MOI.VariableName(), x) === nothing
    @test output == ["x", "y"]
    return
end

end  # module

TestAttributes.runtests()
