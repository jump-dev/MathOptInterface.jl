struct DummyModel <: MOI.ModelLike
end
MOI.supports(::DummyModel, ::MOI.ObjectiveSense) = true
MOI.supports(::DummyModel, ::MOI.ConstraintPrimalStart,
             ::Type{<:MOI.ConstraintIndex}) = true

@testset "Fallbacks for `set!` methods" begin
    model = DummyModel()
    ci = MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{Float64}}(1)
    vi = MOI.VariableIndex(1)

    @test_throws MOI.CannotDelete{typeof(ci)} MOI.delete!(model, ci)

    @testset "UnsupportedAttribute" begin
        @test_throws MOI.UnsupportedAttribute begin
            MOI.set!(model, MOI.ObjectiveFunction{MOI.SingleVariable}(),
                     MOI.SingleVariable(vi))
        end
        @test_throws MOI.UnsupportedAttribute begin
            MOI.set!(model, MOI.ConstraintDualStart(), ci, 0.0)
        end
    end

    @testset "CannotSetAttribute" begin
        @test_throws MOI.CannotSetAttribute begin
            MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)
        end
        @test_throws MOI.CannotSetAttribute begin
            MOI.set!(model, MOI.ConstraintPrimalStart(), ci, 0.0)
        end
    end

    @testset "ConstraintFunction" begin
        @test_throws MOI.CannotSetAttribute begin
            MOI.set!(model, MOI.ConstraintFunction(), ci, MOI.SingleVariable(vi))
        end
        @test_throws ArgumentError begin
            MOI.set!(model, MOI.ConstraintFunction(), ci,
                     MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, vi)],
                                              0.0))
        end
    end
    @testset "ConstraintSet" begin
        @test_throws MOI.CannotSetAttribute begin
            MOI.set!(model, MOI.ConstraintSet(), ci, MOI.EqualTo(1.0))
        end
        @test_throws ArgumentError begin
            MOI.set!(model, MOI.ConstraintSet(), ci, MOI.GreaterThan(1.0))
        end
    end
end

@testset "Error messages" begin
    @test sprint(showerror, MOI.UnsupportedAttribute(MOI.Name())) == "MathOptInterface.UnsupportedAttribute{MathOptInterface.Name}: Attribute MathOptInterface.Name() is not supported by the the model."
    @test sprint(showerror, MOI.UnsupportedAttribute(MOI.Name(), "Message")) == "MathOptInterface.UnsupportedAttribute{MathOptInterface.Name}: Attribute MathOptInterface.Name() is not supported by the the model: Message"
    @test sprint(showerror, MOI.CannotSetAttribute(MOI.Name())) == "MathOptInterface.CannotSetAttribute{MathOptInterface.Name}: Setting attribute MathOptInterface.Name() cannot be performed. You may want to use a `CachingOptimizer` in `Automatic` mode or you may need to call `resetoptimizer!` before doing this operation if the `CachingOptimizer` is in `Manual` mode."
    @test sprint(showerror, MOI.CannotSetAttribute(MOI.Name(), "Message")) == "MathOptInterface.CannotSetAttribute{MathOptInterface.Name}: Setting attribute MathOptInterface.Name() cannot be performed: Message You may want to use a `CachingOptimizer` in `Automatic` mode or you may need to call `resetoptimizer!` before doing this operation if the `CachingOptimizer` is in `Manual` mode." == "MathOptInterface.CannotSetAttribute{MathOptInterface.Name}: Setting attribute MathOptInterface.Name() cannot be performed: Message You may want to use a `CachingOptimizer` in `Automatic` mode or you may need to call `resetoptimizer!` before doing this operation if the `CachingOptimizer` is in `Manual` mode."
end
