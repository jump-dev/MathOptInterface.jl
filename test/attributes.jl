struct DummyModel <: MOI.ModelLike
end

@testset "Fallbacks for `set!` methods" begin
    model = DummyModel()
    ci = MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{Float64}}(1)
    @testset "ConstraintFunction" begin
        vi = MOI.VariableIndex(1)
        @test_throws MOI.UnsupportedAttribute begin
            MOI.set!(model, MOI.ConstraintFunction(), ci, MOI.SingleVariable(vi))
        end
        @test_throws ArgumentError begin
            MOI.set!(model, MOI.ConstraintFunction(), ci,
                     MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, vi)],
                                              0.0))
        end
    end
    @testset "ConstraintSet" begin
        @test_throws MOI.UnsupportedAttribute begin
            MOI.set!(model, MOI.ConstraintSet(), ci, MOI.EqualTo(1.0))
        end
        @test_throws ArgumentError begin
            MOI.set!(model, MOI.ConstraintSet(), ci, MOI.GreaterThan(1.0))
        end
    end
end
