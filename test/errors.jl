using Test
using MathOptInterface
const MOI = MathOptInterface

include("dummy.jl")

@testset "Fallbacks for `set` methods" begin
    model = DummyModel()

    @testset "AddVariableNotAllowed" begin
        @test_throws MOI.AddVariableNotAllowed MOI.add_variable(model)
        try
            MOI.add_variable(model)
        catch err
            @test sprint(showerror, err) == "MathOptInterface.AddVariableNotAllowed:" *
            " Adding variables cannot be performed. You may want to use a" *
            " `CachingOptimizer` in `AUTOMATIC` mode or you may need to call" *
            " `reset_optimizer` before doing this operation if the" *
            " `CachingOptimizer` is in `MANUAL` mode."
        end
        @test_throws MOI.AddVariableNotAllowed MOI.add_variables(model, 2)
    end

    vi = MOI.VariableIndex(1)
    func = MOI.SingleVariable(vi)

    @testset "Inconsistent Vector/Scalar errors" begin
        @test_throws MOI.ErrorException begin
            MOI.add_constraint(model, MOI.VectorOfVariables([vi, vi]), MOI.EqualTo(0))
        end
        @test_throws MOI.ErrorException begin
            MOI.add_constraint(model, func, MOI.Nonnegatives(2))
        end
    end
    @testset "Unsupported constraint with $f" for f in (
        MOI.add_constraint, MOIU.allocate_constraint,
        (model, func, set) -> MOIU.load_constraint(model, MOI.ConstraintIndex{typeof(func), typeof(set)}(1), func, set))
        @test_throws MOI.UnsupportedConstraint begin
            f(model, func, MOI.EqualTo(0))
        end
        try
            f(model, func, MOI.EqualTo(0))
        catch err
            @test sprint(showerror, err) == "$MOI.UnsupportedConstraint{$MOI.SingleVariable,$MOI.EqualTo{$Int}}:" *
            " `$MOI.SingleVariable`-in-`$MOI.EqualTo{$Int}` constraint is" *
            " not supported by the model."
        end
    end
    @testset "Unsupported constraint for shortcuts" begin
        @test_throws MOI.UnsupportedConstraint begin
            MOI.add_constraint(model, vi, MOI.EqualTo(0))
        end
        @test_throws MOI.UnsupportedConstraint begin
            MOI.add_constraint(model, [vi, vi], MOI.Nonnegatives(2))
        end
        @test_throws MOI.UnsupportedConstraint begin
            MOI.add_constraints(model, [vi, vi], [MOI.EqualTo(0),
                                                  MOI.EqualTo(0)])
        end
    end
    @testset "add_constraint errors" begin
        @test_throws MOI.AddConstraintNotAllowed begin
            MOI.add_constraint(model, func, MOI.EqualTo(0.0))
        end
        try
            MOI.add_constraint(model, func, MOI.EqualTo(0.0))
        catch err
            @test sprint(showerror, err) == "$MOI.AddConstraintNotAllowed{$MOI.SingleVariable,$MOI.EqualTo{Float64}}:" *
            " Adding `$MOI.SingleVariable`-in-`$MOI.EqualTo{Float64}`" *
            " constraints cannot be performed. You may want to use a" *
            " `CachingOptimizer` in `AUTOMATIC` mode or you may need to call" *
            " `reset_optimizer` before doing this operation if the" *
            " `CachingOptimizer` is in `MANUAL` mode."
        end
        @test_throws MOI.AddConstraintNotAllowed begin
            MOI.add_constraint(model, vi, MOI.EqualTo(0.0))
        end
        @test_throws MOI.AddConstraintNotAllowed begin
            MOI.add_constraint(model, [vi, vi], MOI.Zeros(2))
        end
        @test_throws MOI.AddConstraintNotAllowed begin
            MOI.add_constraints(model, [vi, vi], [MOI.EqualTo(0.0),
                                                  MOI.EqualTo(0.0)])
        end
    end

    @testset "allocate_constraint errors" begin
        @test_throws MOI.ErrorException begin
            MOIU.allocate_constraint(model, func, MOI.EqualTo(0.0))
        end
        try
            MOIU.allocate_constraint(model, func, MOI.EqualTo(0.0))
        catch err
            @test err === MOIU.ALLOCATE_LOAD_NOT_IMPLEMENTED
        end
    end
    @testset "load_constraint errors" begin
        ci = MOI.ConstraintIndex{typeof(func), MOI.EqualTo{Float64}}(1)
        @test_throws MOI.ErrorException begin
            MOIU.load_constraint(model, ci, func, MOI.EqualTo(0.0))
        end
        try
            MOIU.load_constraint(model, ci, func, MOI.EqualTo(0.0))
        catch err
            @test err === MOIU.ALLOCATE_LOAD_NOT_IMPLEMENTED
        end
    end

    ci = MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{Float64}}(1)

    @testset "DeleteNotAllowed" begin
        @test_throws MOI.DeleteNotAllowed{typeof(vi)} MOI.delete(model, vi)
        try
            MOI.delete(model, vi)
        catch err
            @test sprint(showerror, err) == "MathOptInterface.DeleteNotAllowed{MathOptInterface.VariableIndex}:" *
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
            @test sprint(showerror, err) == "MathOptInterface.DeleteNotAllowed{MathOptInterface.ConstraintIndex{MathOptInterface.SingleVariable,MathOptInterface.EqualTo{Float64}}}:" *
            " Deleting the index MathOptInterface.ConstraintIndex{MathOptInterface.SingleVariable,MathOptInterface.EqualTo{Float64}}(1)" *
            " cannot be performed. You may want to use a `CachingOptimizer`" *
            " in `AUTOMATIC` mode or you may need to call `reset_optimizer`" *
            " before doing this operation if the `CachingOptimizer` is in" *
            " `MANUAL` mode."
        end
    end

    @testset "Unsupported attribute with $f" for f in (MOI.set, MOIU.load,
                                                       MOIU.allocate)
        @test_throws MOI.UnsupportedAttribute begin
            f(model, MOI.ObjectiveFunction{MOI.SingleVariable}(),
                     MOI.SingleVariable(vi))
        end
        @test_throws MOI.UnsupportedAttribute begin
            f(model, MOI.ConstraintDualStart(), ci, 0.0)
        end
    end

    @testset "SetAttributeNotAllowed" begin
        @test_throws MOI.SetAttributeNotAllowed begin
            MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
        end
        @test_throws MOI.SetAttributeNotAllowed begin
            MOI.set(model, MOI.ConstraintPrimalStart(), ci, 0.0)
        end
    end

    @testset "$f errors" for f in (MOIU.load, MOIU.allocate)
        @test_throws MOI.ErrorException begin
            f(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
        end
        try
            f(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
        catch err
            @test err === MOIU.ALLOCATE_LOAD_NOT_IMPLEMENTED
        end
        @test_throws MOI.ErrorException begin
            f(model, MOI.ConstraintPrimalStart(), ci, 0.0)
        end
    end

    @testset "ConstraintFunction" begin
        @test_throws MOI.SetAttributeNotAllowed begin
            MOI.set(model, MOI.ConstraintFunction(), ci, func)
        end
        @test_throws ArgumentError begin
            MOI.set(model, MOI.ConstraintFunction(), ci,
                     MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, vi)],
                                              0.0))
        end
    end
    @testset "ConstraintSet" begin
        @test_throws MOI.SetAttributeNotAllowed begin
            MOI.set(model, MOI.ConstraintSet(), ci, MOI.EqualTo(1.0))
        end
        @test_throws ArgumentError begin
            MOI.set(model, MOI.ConstraintSet(), ci, MOI.EqualTo(1))
        end
        @test_throws ArgumentError begin
            MOI.set(model, MOI.ConstraintSet(), ci, MOI.GreaterThan(1.0))
        end
    end

    @testset "ModifyNotAllowed" begin
        change = MOI.ScalarConstantChange(1.0)
        @testset "Constraint" begin
            err = MOI.ModifyConstraintNotAllowed(ci, change)
            @test_throws err MOI.modify(model, ci, change)
            @test sprint(showerror, err) == "MathOptInterface.ModifyConstraintNotAllowed{MathOptInterface.SingleVariable,MathOptInterface.EqualTo{Float64},MathOptInterface.ScalarConstantChange{Float64}}:" *
            " Modifying the constraints MathOptInterface.ConstraintIndex{MathOptInterface.SingleVariable,MathOptInterface.EqualTo{Float64}}(1)" *
            " with MathOptInterface.ScalarConstantChange{Float64}(1.0) cannot" *
            " be performed. You may want to use a `CachingOptimizer` in" *
            " `AUTOMATIC` mode or you may need to call `reset_optimizer`" *
            " before doing this operation if the `CachingOptimizer` is in" *
            " `MANUAL` mode."
        end
        @testset "Objective" begin
            attr = MOI.ObjectiveFunction{MOI.SingleVariable}()
            err = MOI.ModifyObjectiveNotAllowed(change)
            @test_throws err MOI.modify(model, attr, change)
            @test sprint(showerror, err) == "MathOptInterface.ModifyObjectiveNotAllowed{MathOptInterface.ScalarConstantChange{Float64}}:" *
            " Modifying the objective function with MathOptInterface.ScalarConstantChange{Float64}(1.0)" *
            " cannot be performed. You may want to use a `CachingOptimizer`" *
            " in `AUTOMATIC` mode or you may need to call `reset_optimizer`" *
            " before doing this operation if the `CachingOptimizer` is in" *
            " `MANUAL` mode."
        end
    end
end

@testset "Error messages" begin
    @test sprint(showerror, MOI.UnsupportedAttribute(MOI.Name())) == "$MOI.UnsupportedAttribute{$MOI.Name}:" *
    " Attribute $MOI.Name() is not supported by the model."
    @test sprint(showerror, MOI.UnsupportedAttribute(MOI.Name(), "Message")) == "$MOI.UnsupportedAttribute{$MOI.Name}:" *
    " Attribute $MOI.Name() is not supported by the model: Message"
    @test sprint(showerror, MOI.SetAttributeNotAllowed(MOI.Name())) == "$MOI.SetAttributeNotAllowed{$MOI.Name}:" *
    " Setting attribute $MOI.Name() cannot be performed. You may want to use" *
    " a `CachingOptimizer` in `AUTOMATIC` mode or you may need to call" *
    " `reset_optimizer` before doing this operation if the" *
    " `CachingOptimizer` is in `MANUAL` mode."
    @test sprint(showerror, MOI.SetAttributeNotAllowed(MOI.Name(), "Message")) == "$MOI.SetAttributeNotAllowed{$MOI.Name}:" *
    " Setting attribute $MOI.Name() cannot be performed: Message You may want" *
    " to use a `CachingOptimizer` in `AUTOMATIC` mode or you may need to call" *
    " `reset_optimizer` before doing this operation if the `CachingOptimizer`" *
    " is in `MANUAL` mode." == "$MOI.SetAttributeNotAllowed{$MOI.Name}:" *
    " Setting attribute $MOI.Name() cannot be performed: Message You may want" *
    " to use a `CachingOptimizer` in `AUTOMATIC` mode or you may need to call" *
    " `reset_optimizer` before doing this operation if the `CachingOptimizer`" *
    " is in `MANUAL` mode."

    @test sprint(
        showerror,
        MOI.ResultIndexBoundsError(MOI.VariablePrimal(1), 0)
    ) == "Result index of attribute MathOptInterface.VariablePrimal(1) out of" *
         " bounds. There are currently 0 solution(s) in the model."
end
