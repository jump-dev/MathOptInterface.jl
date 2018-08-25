struct DummyModel <: MOI.ModelLike
end
MOI.supports(::DummyModel, ::MOI.ObjectiveSense) = true
MOI.supports(::DummyModel, ::MOI.ConstraintPrimalStart,
             ::Type{<:MOI.ConstraintIndex}) = true
MOI.supports_constraint(::DummyModel, ::Type{MOI.SingleVariable},
                       ::Type{MOI.EqualTo{Float64}}) = true
MOI.supports_constraint(::DummyModel, ::Type{MOI.VectorOfVariables},
                       ::Type{MOI.Zeros}) = true

@testset "Fallbacks for `set` methods" begin
    model = DummyModel()

    @testset "AddVariableNotAllowed" begin
        @test_throws MOI.AddVariableNotAllowed MOI.add_variable(model)
        try
            MOI.add_variable(model)
        catch err
            @test sprint(showerror, err) == "MathOptInterface.AddVariableNotAllowed:" *
            " Adding variables cannot be performed. You may want to use a" *
            " `CachingOptimizer` in `Automatic` mode or you may need to call" *
            " `resetoptimizer!` before doing this operation if the" *
            " `CachingOptimizer` is in `Manual` mode."
        end
        @test_throws MOI.AddVariableNotAllowed MOI.add_variables(model, 2)
    end

    vi = MOI.VariableIndex(1)
    func = MOI.SingleVariable(vi)

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
            " `CachingOptimizer` in `Automatic` mode or you may need to call" *
            " `resetoptimizer!` before doing this operation if the" *
            " `CachingOptimizer` is in `Manual` mode."
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
        @test_throws MOI.UnsupportedConstraint begin
            MOI.add_constraint(model, func, MOI.EqualTo(0))
        end
        try
            MOI.add_constraint(model, func, MOI.EqualTo(0))
        catch err
            @test sprint(showerror, err) == "$MOI.UnsupportedConstraint{$MOI.SingleVariable,$MOI.EqualTo{$Int}}:" *
            " `$MOI.SingleVariable`-in-`$MOI.EqualTo{$Int}` constraints is" *
            " not supported by the the model."
        end
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

    ci = MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{Float64}}(1)

    @testset "DeleteNotAllowed" begin
        @test_throws MOI.DeleteNotAllowed{typeof(vi)} MOI.delete(model, vi)
        try
            MOI.delete(model, vi)
        catch err
            @test sprint(showerror, err) == "MathOptInterface.DeleteNotAllowed{MathOptInterface.VariableIndex}:" *
            " Deleting the index MathOptInterface.VariableIndex(1) cannot be" *
            " performed. You may want to use a `CachingOptimizer` in" *
            " `Automatic` mode or you may need to call `resetoptimizer!`" *
            " before doing this operation if the `CachingOptimizer` is in" *
            " `Manual` mode."
        end
        @test_throws MOI.DeleteNotAllowed{typeof(ci)} MOI.delete(model, ci)
        try
            MOI.delete(model, ci)
        catch err
            @test sprint(showerror, err) == "MathOptInterface.DeleteNotAllowed{MathOptInterface.ConstraintIndex{MathOptInterface.SingleVariable,MathOptInterface.EqualTo{Float64}}}:" *
            " Deleting the index MathOptInterface.ConstraintIndex{MathOptInterface.SingleVariable,MathOptInterface.EqualTo{Float64}}(1)" *
            " cannot be performed. You may want to use a `CachingOptimizer`" *
            " in `Automatic` mode or you may need to call `resetoptimizer!`" *
            " before doing this operation if the `CachingOptimizer` is in" *
            " `Manual` mode."
        end
    end

    @testset "UnsupportedAttribute" begin
        @test_throws MOI.UnsupportedAttribute begin
            MOI.set(model, MOI.ObjectiveFunction{MOI.SingleVariable}(),
                     MOI.SingleVariable(vi))
        end
        @test_throws MOI.UnsupportedAttribute begin
            MOI.set(model, MOI.ConstraintDualStart(), ci, 0.0)
        end
    end

    @testset "SetAttributeNotAllowed" begin
        @test_throws MOI.SetAttributeNotAllowed begin
            MOI.set(model, MOI.ObjectiveSense(), MOI.MaxSense)
        end
        @test_throws MOI.SetAttributeNotAllowed begin
            MOI.set(model, MOI.ConstraintPrimalStart(), ci, 0.0)
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
end

@testset "Error messages" begin
    @test sprint(showerror, MOI.UnsupportedAttribute(MOI.Name())) == "$MOI.UnsupportedAttribute{$MOI.Name}:" *
    " Attribute $MOI.Name() is not supported by the the model."
    @test sprint(showerror, MOI.UnsupportedAttribute(MOI.Name(), "Message")) == "$MOI.UnsupportedAttribute{$MOI.Name}:" *
    " Attribute $MOI.Name() is not supported by the the model: Message"
    @test sprint(showerror, MOI.SetAttributeNotAllowed(MOI.Name())) == "$MOI.SetAttributeNotAllowed{$MOI.Name}:" *
    " Setting attribute $MOI.Name() cannot be performed. You may want to use" *
    " a `CachingOptimizer` in `Automatic` mode or you may need to call" *
    " `resetoptimizer!` before doing this operation if the" *
    " `CachingOptimizer` is in `Manual` mode."
    @test sprint(showerror, MOI.SetAttributeNotAllowed(MOI.Name(), "Message")) == "$MOI.SetAttributeNotAllowed{$MOI.Name}:" *
    " Setting attribute $MOI.Name() cannot be performed: Message You may want" *
    " to use a `CachingOptimizer` in `Automatic` mode or you may need to call" *
    " `resetoptimizer!` before doing this operation if the `CachingOptimizer`" *
    " is in `Manual` mode." == "$MOI.SetAttributeNotAllowed{$MOI.Name}:" *
    " Setting attribute $MOI.Name() cannot be performed: Message You may want" *
    " to use a `CachingOptimizer` in `Automatic` mode or you may need to call" *
    " `resetoptimizer!` before doing this operation if the `CachingOptimizer`" *
    " is in `Manual` mode."
end
