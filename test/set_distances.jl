using Test

using MathOptInterface
const MOI = MathOptInterface
import LinearAlgebra

@testset "Set distances" begin
    @testset "$n-dimensional orthants" for n in 1:3:15
        v = rand(n)
        @test MOI.distance_to_set(v, MOI.Reals(n)) ≈ 0 atol=eps(Float64)
        @test MOI.distance_to_set(v, MOI.Zeros(n)) > 0
        @test MOI.distance_to_set(v, MOI.Nonnegatives(n)) ≈ 0 atol=eps(Float64)
        @test MOI.distance_to_set(-v, MOI.Nonpositives(n)) ≈ 0 atol=eps(Float64)
        @test MOI.distance_to_set(v, MOI.Nonpositives(n)) ≈ MOI.distance_to_set(-v, MOI.Nonnegatives(n)) > 0
    end

    @testset "Scalar comparisons" begin
        values = rand(10)
        for v in values
            @test MOI.distance_to_set(v, MOI.EqualTo(v)) ≈ 0 atol=eps(Float64)
            @test MOI.distance_to_set(-v, MOI.EqualTo(v)) ≈ MOI.distance_to_set(v, MOI.EqualTo(-v)) ≈ 2v
            @test MOI.distance_to_set(v, MOI.LessThan(v)) ≈ MOI.distance_to_set(v, MOI.LessThan(v+1)) ≈ 0
            @test MOI.distance_to_set(v, MOI.LessThan(0)) ≈ MOI.distance_to_set(-v, MOI.GreaterThan(0)) ≈ v
            @test MOI.distance_to_set(v, MOI.GreaterThan(v)) ≈ MOI.distance_to_set(v+1, MOI.GreaterThan(v+1)) ≈ 0
            @test MOI.distance_to_set(v, MOI.Interval(v,v)) ≈ MOI.distance_to_set(v, MOI.Interval(-v,v)) ≈ 0
            @test MOI.distance_to_set(v, MOI.Interval(-v, 0.0)) ≈ MOI.distance_to_set(-v, MOI.Interval(0.0, v)) ≈ v
        end
    end
    @testset "$n-dimensional norm cones" for n in 2:5:15
        x = rand(n)
        tsum = sum(x)
        vsum = vcat(tsum, x)
        @test MOI.distance_to_set(vsum, MOI.NormOneCone(n+1)) ≈ MOI.distance_to_set(vsum, MOI.NormInfinityCone(n+1)) ≈ 0
        tmax = maximum(x)
        vmax = vcat(tmax, x)
        @test MOI.distance_to_set(vmax, MOI.NormOneCone(n+1)) > 0
        @test MOI.distance_to_set(vmax, MOI.NormInfinityCone(n+1)) ≈ 0 atol=eps(Float64)
        tmin = 0
        vmin = vcat(tmin, x)
        @test MOI.distance_to_set(vmin, MOI.NormInfinityCone(n+1)) ≈ tmax
        @test MOI.distance_to_set(vmin, MOI.NormOneCone(n+1)) ≈ tsum

        tvalid = sqrt(n) # upper bound on the norm2
        vok_soc = vcat(tvalid, x)
        @test MOI.distance_to_set(vok_soc, MOI.SecondOrderCone(n+1) ) ≈ 0 atol=eps(Float64)
        vko_soc = vcat(-2, x)
        @test MOI.distance_to_set(vko_soc, MOI.SecondOrderCone(n+1) ) ≈ 2 + LinearAlgebra.norm2(x)

        vko_soc = vcat(-2, x)
        @test MOI.distance_to_set(vko_soc, MOI.SecondOrderCone(n+1) ) ≈ 2 + LinearAlgebra.norm2(x)

        t_ko_rot = u_ko_rot = LinearAlgebra.norm2(x) / 2
        vko_roc = vcat(t_ko_rot, u_ko_rot, x)
        @test MOI.distance_to_set(vko_roc, MOI.RotatedSecondOrderCone(n+2)) ≈ LinearAlgebra.dot(x,x) / 2
        vok_roc = vcat(t_ko_rot * 2, u_ko_rot, x)
        @test MOI.distance_to_set(vok_roc, MOI.RotatedSecondOrderCone(n+2)) ≈ 0 atol=eps(Float64)
    end

    @testset "Other vector cones" for n in 2:5:15
        x = rand(n)
        t = 0.5 * prod(x)^(inv(n))
        vok = vcat(t, x)
        @test MOI.distance_to_set(vok, MOI.GeometricMeanCone(n+1)) ≈ 0 atol=eps(Float64)
        @test MOI.distance_to_set(vcat(t / 2, x), MOI.GeometricMeanCone(n+1)) ≈ 0 atol=eps(Float64)
        # negative x always means positive distance
        @test MOI.distance_to_set(vcat(t / 2, vcat(x, -1)), MOI.GeometricMeanCone(n+2)) > 0
        @test MOI.distance_to_set(vcat(t / 2, -x), MOI.GeometricMeanCone(n+1)) > 0        
    end
end
