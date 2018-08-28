@testset "Attributes" begin
    @testset "is_set_by_optimize" begin
        @test MOI.is_set_by_optimize(MOI.TerminationStatus())
        @test !MOI.is_set_by_optimize(MOI.ConstraintSet())
        @test !MOI.is_set_by_optimize(MOI.ObjectiveSense())
    end
    @testset "is_copyable" begin
        @test !MOI.is_copyable(MOI.TerminationStatus())
        @test !MOI.is_copyable(MOI.ConstraintSet())
        @test MOI.is_copyable(MOI.ObjectiveSense())
    end
    @testset "supports" begin
        model = DummyModel()
        @test_throws ArgumentError MOI.supports(model, MOI.TerminationStatus())
        @test_throws ArgumentError begin
            MOI.supports(model, MOI.ConstraintSet(),
                         MOI.ConstraintIndex{MOI.SingleVariable,
                                             MOI.EqualTo{Float64}})
        end
        @test MOI.supports(model, MOI.ObjectiveSense())
    end
end
