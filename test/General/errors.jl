# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestErrors

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

function test_errors_fallback_AddVariableNotAllowed()
    model = DummyModel()
    @test_throws MOI.AddVariableNotAllowed MOI.add_variable(model)
    try
        MOI.add_variable(model)
    catch err
        contents = sprint(showerror, err)
        @test occursin("$(MOI.AddVariableNotAllowed)", contents)
        @test occursin("Adding variables cannot be performed", contents)
        @test occursin("## Fixing this error", contents)
    end
    @test_throws MOI.AddVariableNotAllowed MOI.add_variables(model, 2)
    return
end

function test_errors_inconsistent_vectorscalar()
    model = DummyModel()
    vi = MOI.VariableIndex(1)
    @test_throws(
        MOI.ErrorException,
        MOI.add_constraint(
            model,
            MOI.VectorOfVariables([vi, vi]),
            MOI.EqualTo(0),
        )
    )
    @test_throws(
        MOI.ErrorException,
        MOI.add_constraint(model, vi, MOI.Nonnegatives(2))
    )
    return
end

function test_errors_UnsupportedConstraint()
    model = DummyModel()
    vi = MOI.VariableIndex(1)
    @test_throws(
        MOI.UnsupportedConstraint,
        MOI.add_constraint(model, vi, MOI.EqualTo(0)),
    )
    msg = """
    UnsupportedConstraint: `$(MOI.VariableIndex)`-in-`$(MOI.EqualTo{Int})` constraints are not supported by the
    solver you have chosen, and we could not reformulate your model into a
    form that is supported.

    To fix this error you must choose a different solver.


    """
    try
        MOI.add_constraint(model, vi, MOI.EqualTo(0))
    catch err
        @test sprint(showerror, err) == msg
    end
    return
end

function test_errors_UnsupportedConstraint_shortcut()
    model = DummyModel()
    vi = MOI.VariableIndex(1)
    @test_throws MOI.UnsupportedConstraint begin
        MOI.add_constraint(model, vi, MOI.EqualTo(0))
    end
    @test_throws MOI.UnsupportedConstraint begin
        MOI.add_constraint(model, [vi, vi], MOI.Nonnegatives(2))
    end
    @test_throws MOI.UnsupportedConstraint begin
        MOI.add_constraints(model, [vi, vi], [MOI.EqualTo(0), MOI.EqualTo(0)])
    end
end

function test_errors_add_constraint()
    model = DummyModel()
    vi = MOI.VariableIndex(1)
    @test_throws(
        MOI.AddConstraintNotAllowed,
        MOI.add_constraint(model, vi, MOI.EqualTo(0.0)),
    )
    try
        MOI.add_constraint(model, vi, MOI.EqualTo(0.0))
    catch err
        contents = sprint(showerror, err)
        F, S = MOI.VariableIndex, MOI.EqualTo{Float64}
        @test occursin("$(MOI.AddConstraintNotAllowed{F,S})", contents)
        @test occursin(
            "Adding `$F`-in-`$S` constraints cannot be performed",
            contents,
        )
        @test occursin("## Fixing this error", contents)
    end
    @test_throws(
        MOI.AddConstraintNotAllowed,
        MOI.add_constraint(model, vi, MOI.EqualTo(0.0)),
    )
    @test_throws(
        MOI.AddConstraintNotAllowed,
        MOI.add_constraint(model, [vi, vi], MOI.Zeros(2)),
    )
    @test_throws(
        MOI.AddConstraintNotAllowed,
        MOI.add_constraints(
            model,
            [vi, vi],
            [MOI.EqualTo(0.0), MOI.EqualTo(0.0)],
        ),
    )
    return
end

function test_errors_DeleteNotAllowed()
    model = DummyModel()
    vi = MOI.VariableIndex(1)
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}}(1)
    @test_throws MOI.DeleteNotAllowed{typeof(vi)} MOI.delete(model, vi)
    try
        MOI.delete(model, vi)
    catch err
        contents = sprint(showerror, err)
        @test occursin("$(MOI.DeleteNotAllowed{typeof(vi)})", contents)
        @test occursin("Deleting the index $vi cannot be performed", contents)
        @test occursin("## Fixing this error", contents)
    end
    @test_throws MOI.DeleteNotAllowed{typeof(ci)} MOI.delete(model, ci)
    try
        MOI.delete(model, ci)
    catch err
        contents = sprint(showerror, err)
        @test occursin("$(MOI.DeleteNotAllowed{typeof(ci)})", contents)
        @test occursin("Deleting the index $ci cannot be performed", contents)
        @test occursin("## Fixing this error", contents)
    end
    return
end

function _test_unsupported_attribute(f)
    model = DummyModel()
    vi = MOI.VariableIndex(1)
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}}(1)
    @test_throws(
        MOI.UnsupportedAttribute,
        f(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), vi),
    )
    @test_throws(
        MOI.UnsupportedAttribute,
        f(model, MOI.ConstraintDualStart(), ci, 0.0),
    )
    return
end

function test_errors_unsupported_attribute()
    _test_unsupported_attribute(MOI.set)
    return
end

function test_errors_SetAttributeNotAllowed()
    model = DummyModel()
    vi = MOI.VariableIndex(1)
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}}(1)
    model = DummyModel()
    @test_throws(
        MOI.SetAttributeNotAllowed,
        MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE),
    )
    @test_throws(
        MOI.SetAttributeNotAllowed,
        MOI.set(model, MOI.ConstraintPrimalStart(), ci, 0.0),
    )
    return
end

function test_errors_ConstraintFunction_NotAllowed()
    model = DummyModel()
    vi = MOI.VariableIndex(1)
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}}(1)
    @test_throws(
        MOI.SetAttributeNotAllowed,
        MOI.set(model, MOI.ConstraintFunction(), ci, vi)
    )
    @test_throws(
        MOI.FunctionTypeMismatch{
            MOI.VariableIndex,
            MOI.ScalarAffineFunction{Float64},
        },
        MOI.set(
            model,
            MOI.ConstraintFunction(),
            ci,
            MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, vi)], 0.0),
        ),
    )
    return
end

function test_errors_ConstraintSet_NotAllowed()
    model = DummyModel()
    vi = MOI.VariableIndex(1)
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}}(1)
    @test_throws(
        MOI.SetAttributeNotAllowed,
        MOI.set(model, MOI.ConstraintSet(), ci, MOI.EqualTo(1.0))
    )
    @test_throws(
        MOI.SetTypeMismatch{MOI.EqualTo{Float64},MOI.EqualTo{Int}},
        MOI.set(model, MOI.ConstraintSet(), ci, MOI.EqualTo(1))
    )
    @test_throws(
        MOI.SetTypeMismatch{MOI.EqualTo{Float64},MOI.GreaterThan{Float64}},
        MOI.set(model, MOI.ConstraintSet(), ci, MOI.GreaterThan(1.0))
    )
end

function test_errors_ModifyNotAllowed_constraint()
    model = DummyModel()
    vi = MOI.VariableIndex(1)
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}}(1)
    change = MOI.ScalarConstantChange(1.0)
    err = MOI.ModifyConstraintNotAllowed(ci, change)
    @test_throws err MOI.modify(model, ci, change)
    contents = sprint(showerror, err)
    @test occursin("$(typeof(err)):", contents)
    @test occursin(
        "Modifying the constraints $ci with $change cannot be performed",
        contents,
    )
    @test occursin("## Fixing this error", contents)
    return
end

function test_errors_ModifyNotAllowed_objective()
    model = DummyModel()
    change = MOI.ScalarConstantChange(1.0)
    attr = MOI.ObjectiveFunction{MOI.VariableIndex}()
    err = MOI.ModifyObjectiveNotAllowed(change)
    @test_throws err MOI.modify(model, attr, change)
    contents = sprint(showerror, err)
    @test occursin("$(typeof(err)):", contents)
    @test occursin(
        "Modifying the objective function with $change cannot be performed",
        contents,
    )
    @test occursin("## Fixing this error", contents)
    return
end

function test_errors_show_SetAttributeNotAllowed()
    @test sprint(showerror, MOI.UnsupportedAttribute(MOI.Name())) ==
          "$MOI.UnsupportedAttribute{$MOI.Name}:" *
          " Attribute $MOI.Name() is not supported by the model."
    @test sprint(showerror, MOI.UnsupportedAttribute(MOI.Name(), "Message")) ==
          "$MOI.UnsupportedAttribute{$MOI.Name}:" *
          " Attribute $MOI.Name() is not supported by the model: Message"
    contents = sprint(showerror, MOI.SetAttributeNotAllowed(MOI.Name()))
    @test occursin("$MOI.SetAttributeNotAllowed{$MOI.Name}:", contents)
    @test occursin(
        "Setting attribute $(MOI.Name()) cannot be performed",
        contents,
    )
    @test occursin("## Fixing this error", contents)
    err = MOI.SetAttributeNotAllowed(MOI.Name(), "Message")
    contents = sprint(showerror, err)
    @test occursin("$(typeof(err))", contents)
    @test occursin("Message", contents)
    @test occursin(
        "Setting attribute $(MOI.Name()) cannot be performed",
        contents,
    )
    @test occursin("## Fixing this error", contents)
    return
end

function test_errors_ResultIndexBoundsError()
    @test sprint(
        showerror,
        MOI.ResultIndexBoundsError(MOI.VariablePrimal(1), 0),
    ) ==
          "Result index of attribute MathOptInterface.VariablePrimal(1) out of" *
          " bounds. There are currently 0 solution(s) in the model."
end

function test_errors_ConflictIndexBoundsError()
    @test sprint(
        showerror,
        MOI.ConflictIndexBoundsError(MOI.ConstraintConflictStatus(1), 0),
    ) ==
          "Conflict index of attribute " *
          "MathOptInterface.ConstraintConflictStatus(1) out of bounds. " *
          "There are currently 0 conflict(s) in the model."
end

function test_errors_InvalidCalbackUsage()
    @test sprint(
        showerror,
        MOI.InvalidCallbackUsage(MOI.LazyConstraintCallback(), MOI.UserCut(1)),
    ) ==
          "InvalidCallbackUsage: Cannot submit $(MOI.UserCut(1)) inside a MathOptInterface.LazyConstraintCallback()."
end

struct TestCopyToFallback <: MOI.AbstractOptimizer end

function test_errors_copy_to_fallback()
    dest = TestCopyToFallback()
    @test_throws(
        ErrorException(
            "`copy_to` is not supported by the solver `$(typeof(dest))`. Did " *
            "you mean to call " *
            "`optimize!(dest::AbstractOptimizer, src::ModelLike)` instead?",
        ),
        MOI.copy_to(dest, MOI.Utilities.Model{Float64}()),
    )
    return
end

struct Optimizer1697 <: MOI.AbstractOptimizer end

function test_compute_conflict_fallback()
    model = Optimizer1697()
    @test_throws(
        ArgumentError(
            "The optimizer $(typeof(model)) does not support " *
            "`compute_conflict!`",
        ),
        MOI.compute_conflict!(model),
    )
    return
end

function test_get_fallback_error()
    model = MOI.Utilities.Model{Float64}()
    @test_throws(
        MOI.GetAttributeNotAllowed,
        MOI.get(model, MOI.SolveTimeSec()),
    )
    err = MOI.GetAttributeNotAllowed(MOI.SolveTimeSec(), "")
    contents = sprint(showerror, err)
    @test occursin("$(typeof(err)):", contents)
    @test occursin(
        "Getting attribute $(MOI.SolveTimeSec()) cannot be performed",
        contents,
    )
    @test occursin("## Fixing this error", contents)
    return
end

function test_unsupported_nonlinear_operator()
    @test MOI.element_name(MOI.UnsupportedNonlinearOperator(:f)) ==
          "The nonlinear operator `:f`"
    return
end

function test_ScalarFunctionConstantNotZero_equality()
    for T in (Int, Float64, BigFloat)
        F, S = MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}
        w = MOI.ScalarFunctionConstantNotZero{T,F,MOI.LessThan{T}}(zero(T))
        x = MOI.ScalarFunctionConstantNotZero{T,F,S}(zero(T))
        y = MOI.ScalarFunctionConstantNotZero{T,F,S}(zero(T))
        z = MOI.ScalarFunctionConstantNotZero{T,F,S}(one(T))
        @test x == y
        @test x != z
        @test w != x
    end
    return
end

function test_showerror_InvalidIndex()
    x = MOI.VariableIndex(1)
    @test sprint(showerror, MOI.InvalidIndex(x)) ==
          "The index $x is invalid. Note that an index becomes invalid after it has been deleted."
    return
end

struct ModelWithNoIsValid <: MOI.ModelLike end

function test_isvalid_fallback()
    model = ModelWithNoIsValid()
    x = MOI.VariableIndex(1)
    @test !MOI.is_valid(model, x)
    return
end

function test_logs_precompile()
    model = MOI.Utilities.Model{Float64}()
    F, S = MOI.VariableIndex, MOI.ZeroOne
    @test_logs (:warn,) MOI.precompile_constraint(model, F, S)
    @test_logs (:warn,) MOI.precompile_variables(model)
    @test_logs (:warn,) MOI.precompile_model(model, [(F, S)])
    return
end

end  # module

TestErrors.runtests()
