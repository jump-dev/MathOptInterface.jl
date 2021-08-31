module TestErrors

using Test
using MathOptInterface
const MOI = MathOptInterface

include("dummy.jl")

function test_errors_fallback_AddVariableNotAllowed()
    model = DummyModel()
    @test_throws MOI.AddVariableNotAllowed MOI.add_variable(model)
    try
        MOI.add_variable(model)
    catch err
        @test sprint(showerror, err) ==
              "MathOptInterface.AddVariableNotAllowed:" *
              " Adding variables cannot be performed. You may want to use a" *
              " `CachingOptimizer` in `AUTOMATIC` mode or you may need to call" *
              " `reset_optimizer` before doing this operation if the" *
              " `CachingOptimizer` is in `MANUAL` mode."
    end
    @test_throws MOI.AddVariableNotAllowed MOI.add_variables(model, 2)
    return
end

function test_errors_inconsistent_vectorscalar()
    model = DummyModel()
    vi = MOI.VariableIndex(1)
    func = vi
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
        MOI.add_constraint(model, func, MOI.Nonnegatives(2))
    )
    return
end

function _test_errors_UnsupportedConstraint(f)
    model = DummyModel()
    vi = MOI.VariableIndex(1)
    func = vi
    @test_throws(MOI.UnsupportedConstraint, f(model, func, MOI.EqualTo(0)),)
    try
        f(model, func, MOI.EqualTo(0))
    catch err
        @test sprint(showerror, err) ==
              "$(MOI.UnsupportedConstraint{MOI.VariableIndex,MOI.EqualTo{Int}}):" *
              " `$MOI.VariableIndex`-in-`$MOI.EqualTo{$Int}` constraint is" *
              " not supported by the model."
    end
    return
end

function test_errors_UnsupportedConstraint()
    _test_errors_UnsupportedConstraint(MOI.add_constraint)
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
    func = vi
    @test_throws(
        MOI.AddConstraintNotAllowed,
        MOI.add_constraint(model, func, MOI.EqualTo(0.0)),
    )
    try
        MOI.add_constraint(model, func, MOI.EqualTo(0.0))
    catch err
        @test sprint(showerror, err) ==
              "$(MOI.AddConstraintNotAllowed{MOI.VariableIndex,MOI.EqualTo{Float64}}):" *
              " Adding `$MOI.VariableIndex`-in-`$MOI.EqualTo{Float64}`" *
              " constraints cannot be performed. You may want to use a" *
              " `CachingOptimizer` in `AUTOMATIC` mode or you may need to call" *
              " `reset_optimizer` before doing this operation if the" *
              " `CachingOptimizer` is in `MANUAL` mode."
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
    func = vi
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}}(1)
    @test_throws MOI.DeleteNotAllowed{typeof(vi)} MOI.delete(model, vi)
    try
        MOI.delete(model, vi)
    catch err
        @test sprint(showerror, err) ==
              "MathOptInterface.DeleteNotAllowed{MathOptInterface.VariableIndex}:" *
              " Deleting the index MathOptInterface.VariableIndex(1) cannot be" *
              " performed. You may want to use a `CachingOptimizer` in" *
              " `AUTOMATIC` mode or you may need to call `reset_optimizer`" *
              " before doing this operation if the `CachingOptimizer` is in" *
              " `MANUAL` mode."
    end
    @test_throws MOI.DeleteNotAllowed{typeof(ci)} MOI.delete(model, ci)
    try
        MOI.delete(model, ci)
    catch err
        @test sprint(showerror, err) ==
              "$(MathOptInterface.DeleteNotAllowed{MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex,MathOptInterface.EqualTo{Float64}}}):" *
              " Deleting the index $(MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex,MathOptInterface.EqualTo{Float64}}(1))" *
              " cannot be performed. You may want to use a `CachingOptimizer`" *
              " in `AUTOMATIC` mode or you may need to call `reset_optimizer`" *
              " before doing this operation if the `CachingOptimizer` is in" *
              " `MANUAL` mode."
    end
    return
end

function _test_unsupported_attribute(f)
    model = DummyModel()
    vi = MOI.VariableIndex(1)
    func = vi
    ci = MOI.ConstraintIndex{typeof(func),MOI.EqualTo{Float64}}(1)
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
    func = vi
    ci = MOI.ConstraintIndex{typeof(func),MOI.EqualTo{Float64}}(1)
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
    func = vi
    ci = MOI.ConstraintIndex{typeof(func),MOI.EqualTo{Float64}}(1)
    @test_throws(
        MOI.SetAttributeNotAllowed,
        MOI.set(model, MOI.ConstraintFunction(), ci, func)
    )
    @test_throws(
        ArgumentError,
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
    func = vi
    ci = MOI.ConstraintIndex{typeof(func),MOI.EqualTo{Float64}}(1)
    @test_throws(
        MOI.SetAttributeNotAllowed,
        MOI.set(model, MOI.ConstraintSet(), ci, MOI.EqualTo(1.0))
    )
    @test_throws(
        ArgumentError,
        MOI.set(model, MOI.ConstraintSet(), ci, MOI.EqualTo(1))
    )
    @test_throws(
        ArgumentError,
        MOI.set(model, MOI.ConstraintSet(), ci, MOI.GreaterThan(1.0))
    )
end

function test_errors_ModifyNotAllowed_constraint()
    model = DummyModel()
    vi = MOI.VariableIndex(1)
    func = vi
    ci = MOI.ConstraintIndex{typeof(func),MOI.EqualTo{Float64}}(1)
    change = MOI.ScalarConstantChange(1.0)
    err = MOI.ModifyConstraintNotAllowed(ci, change)
    @test_throws err MOI.modify(model, ci, change)
    @test sprint(showerror, err) ==
          "$(MathOptInterface.ModifyConstraintNotAllowed{MathOptInterface.VariableIndex,MathOptInterface.EqualTo{Float64},MathOptInterface.ScalarConstantChange{Float64}}):" *
          " Modifying the constraints $(MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex,MathOptInterface.EqualTo{Float64}}(1))" *
          " with MathOptInterface.ScalarConstantChange{Float64}(1.0) cannot" *
          " be performed. You may want to use a `CachingOptimizer` in" *
          " `AUTOMATIC` mode or you may need to call `reset_optimizer`" *
          " before doing this operation if the `CachingOptimizer` is in" *
          " `MANUAL` mode."
end

function test_errors_ModifyNotAllowed_objective()
    model = DummyModel()
    change = MOI.ScalarConstantChange(1.0)
    attr = MOI.ObjectiveFunction{MOI.VariableIndex}()
    err = MOI.ModifyObjectiveNotAllowed(change)
    @test_throws err MOI.modify(model, attr, change)
    @test sprint(showerror, err) ==
          "$(MathOptInterface.ModifyObjectiveNotAllowed{MathOptInterface.ScalarConstantChange{Float64}}):" *
          " Modifying the objective function with $(MathOptInterface.ScalarConstantChange{Float64}(1.0))" *
          " cannot be performed. You may want to use a `CachingOptimizer`" *
          " in `AUTOMATIC` mode or you may need to call `reset_optimizer`" *
          " before doing this operation if the `CachingOptimizer` is in" *
          " `MANUAL` mode."
end

function test_errors_show_SetAttributeNotAllowed()
    @test sprint(showerror, MOI.UnsupportedAttribute(MOI.Name())) ==
          "$MOI.UnsupportedAttribute{$MOI.Name}:" *
          " Attribute $MOI.Name() is not supported by the model."
    @test sprint(showerror, MOI.UnsupportedAttribute(MOI.Name(), "Message")) ==
          "$MOI.UnsupportedAttribute{$MOI.Name}:" *
          " Attribute $MOI.Name() is not supported by the model: Message"
    @test sprint(showerror, MOI.SetAttributeNotAllowed(MOI.Name())) ==
          "$MOI.SetAttributeNotAllowed{$MOI.Name}:" *
          " Setting attribute $MOI.Name() cannot be performed. You may want to use" *
          " a `CachingOptimizer` in `AUTOMATIC` mode or you may need to call" *
          " `reset_optimizer` before doing this operation if the" *
          " `CachingOptimizer` is in `MANUAL` mode."
    @test sprint(
              showerror,
              MOI.SetAttributeNotAllowed(MOI.Name(), "Message"),
          ) ==
          "$MOI.SetAttributeNotAllowed{$MOI.Name}:" *
          " Setting attribute $MOI.Name() cannot be performed: Message You may want" *
          " to use a `CachingOptimizer` in `AUTOMATIC` mode or you may need to call" *
          " `reset_optimizer` before doing this operation if the `CachingOptimizer`" *
          " is in `MANUAL` mode." ==
          "$MOI.SetAttributeNotAllowed{$MOI.Name}:" *
          " Setting attribute $MOI.Name() cannot be performed: Message You may want" *
          " to use a `CachingOptimizer` in `AUTOMATIC` mode or you may need to call" *
          " `reset_optimizer` before doing this operation if the `CachingOptimizer`" *
          " is in `MANUAL` mode."
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

function test_errors_InvalidCalbackUsage()
    @test sprint(
        showerror,
        MOI.InvalidCallbackUsage(MOI.LazyConstraintCallback(), MOI.UserCut(1)),
    ) ==
          "InvalidCallbackUsage: Cannot submit $(MOI.UserCut(1)) inside a MathOptInterface.LazyConstraintCallback()."
end

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$name", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
end

end

TestErrors.runtests()
