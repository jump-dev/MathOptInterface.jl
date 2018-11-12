@testset "Sets" begin
    @testset "Copy" begin
        @testset "for $S" for S in [MOI.SOS1, MOI.SOS2]
            s = S([1.0])
            s_copy = copy(s)
            s_copy.weights[1] = 2.0
            @test s.weights[1] == 1.0
        end
    end
end
