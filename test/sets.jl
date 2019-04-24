@testset "Sets" begin
    @testset "Copy" begin
        @testset "for $S" for S in [MOI.SOS1, MOI.SOS2]
            s = S([1.0])
            s_copy = copy(s)
            s_copy.weights[1] = 2.0
            @test s.weights[1] == 1.0
        end
        @testset "IndicatorSet" begin
            s1 = MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(4.0))
            s2 = MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO}(MOI.GreaterThan(4.0))
            s1_copy = copy(s1)
            s2_copy = copy(s2)
            @test s1_copy isa MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}
            @test s1 == s1_copy
            @test s2_copy isa MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO}
            @test s2 == s2_copy
        end
    end
end
