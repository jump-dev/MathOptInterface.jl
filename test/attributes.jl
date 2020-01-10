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
    @testset "set vector" begin
        attr = MOI.VariablePrimalStart()
        err = DimensionMismatch("Number of indices (1) does not match the " *
                                "number of values (2) set to `$attr`.")
        model = DummyModel()
        x = MOI.VariableIndex(1)
        @test_throws err MOI.set(model, MOI.VariablePrimalStart(), [x],
                                 ones(2))
    end
    attr = MOI.VariablePrimal()
    @test_deprecated MOI._result_index_field(attr)
end
