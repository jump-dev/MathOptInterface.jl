using Test

using MathOptInterface
const MOI = MathOptInterface
import LinearAlgebra

@testset "Set distances" begin
    @testset "$n-dimensional orthants" for n in 1:3:15
        v = rand(n)
        @test MOI.distance_to_set(MOI.DefaultDistance(), v, MOI.Reals(n)) ≈ 0 atol=eps(Float64)
        @test MOI.distance_to_set(MOI.DefaultDistance(), v, MOI.Zeros(n)) > 0
        @test MOI.distance_to_set(MOI.DefaultDistance(), v, MOI.Nonnegatives(n)) ≈ 0 atol=eps(Float64)
        @test MOI.distance_to_set(MOI.DefaultDistance(), -v, MOI.Nonpositives(n)) ≈ 0 atol=eps(Float64)
        @test MOI.distance_to_set(MOI.DefaultDistance(), v, MOI.Nonpositives(n)) ≈ MOI.distance_to_set(MOI.DefaultDistance(), -v, MOI.Nonnegatives(n)) > 0
    end

    @testset "Scalar comparisons" begin
        values = rand(10)
        for v in values
            @test MOI.distance_to_set(MOI.DefaultDistance(), v, MOI.EqualTo(v)) ≈ 0 atol=eps(Float64)
            @test MOI.distance_to_set(MOI.DefaultDistance(), -v, MOI.EqualTo(v)) ≈ MOI.distance_to_set(MOI.DefaultDistance(), v, MOI.EqualTo(-v)) ≈ 2v
            @test MOI.distance_to_set(MOI.DefaultDistance(), v, MOI.LessThan(v)) ≈ MOI.distance_to_set(MOI.DefaultDistance(), v, MOI.LessThan(v+1)) ≈ 0
            @test MOI.distance_to_set(MOI.DefaultDistance(), v, MOI.LessThan(0)) ≈ MOI.distance_to_set(MOI.DefaultDistance(), -v, MOI.GreaterThan(0)) ≈ v
            @test MOI.distance_to_set(MOI.DefaultDistance(), v, MOI.GreaterThan(v)) ≈ MOI.distance_to_set(MOI.DefaultDistance(), v+1, MOI.GreaterThan(v+1)) ≈ 0
            @test MOI.distance_to_set(MOI.DefaultDistance(), v, MOI.Interval(v,v)) ≈ MOI.distance_to_set(MOI.DefaultDistance(), v, MOI.Interval(-v,v)) ≈ 0
            @test MOI.distance_to_set(MOI.DefaultDistance(), v, MOI.Interval(-v, 0.0)) ≈ MOI.distance_to_set(MOI.DefaultDistance(), -v, MOI.Interval(0.0, v)) ≈ v
        end
    end
    @testset "$n-dimensional norm cones" for n in 2:5:15
        x = rand(n)
        tsum = sum(x)
        vsum = vcat(tsum, x)
        @test MOI.distance_to_set(MOI.DefaultDistance(), vsum, MOI.NormOneCone(n+1)) ≈ MOI.distance_to_set(MOI.DefaultDistance(), vsum, MOI.NormInfinityCone(n+1)) ≈ 0
        tmax = maximum(x)
        vmax = vcat(tmax, x)
        @test MOI.distance_to_set(MOI.DefaultDistance(), vmax, MOI.NormOneCone(n+1)) > 0
        @test MOI.distance_to_set(MOI.DefaultDistance(), vmax, MOI.NormInfinityCone(n+1)) ≈ 0 atol=eps(Float64)
        tmin = 0
        vmin = vcat(tmin, x)
        @test MOI.distance_to_set(MOI.DefaultDistance(), vmin, MOI.NormInfinityCone(n+1)) ≈ tmax
        @test MOI.distance_to_set(MOI.DefaultDistance(), vmin, MOI.NormOneCone(n+1)) ≈ tsum

        tvalid = sqrt(n) # upper bound on the norm2
        vok_soc = vcat(tvalid, x)
        @test MOI.distance_to_set(MOI.DefaultDistance(), vok_soc, MOI.SecondOrderCone(n+1) ) ≈ 0 atol=eps(Float64)
        vko_soc = vcat(-2, x)
        @test MOI.distance_to_set(MOI.DefaultDistance(), vko_soc, MOI.SecondOrderCone(n+1) ) ≈ 2 + LinearAlgebra.norm2(x)

        vko_soc = vcat(-2, x)
        @test MOI.distance_to_set(MOI.DefaultDistance(), vko_soc, MOI.SecondOrderCone(n+1) ) ≈ 2 + LinearAlgebra.norm2(x)

        t_ko_rot = u_ko_rot = LinearAlgebra.norm2(x) / 2
        vko_roc = vcat(t_ko_rot, u_ko_rot, x)
        @test MOI.distance_to_set(MOI.DefaultDistance(), vko_roc, MOI.RotatedSecondOrderCone(n+2)) ≈ LinearAlgebra.dot(x,x) / 2
        vok_roc = vcat(t_ko_rot * 2, u_ko_rot, x)
        @test MOI.distance_to_set(MOI.DefaultDistance(), vok_roc, MOI.RotatedSecondOrderCone(n+2)) ≈ 0 atol=10eps(Float64)
    end

    @testset "Geometric Mean cone dimension $n" for n in 2:5:15
        x = rand(n)
        t = 0.5 * prod(x)^(inv(n))
        vok = vcat(t, x)
        @test MOI.distance_to_set(MOI.DefaultDistance(), vok, MOI.GeometricMeanCone(n+1)) ≈ 0 atol=eps(Float64)
        @test MOI.distance_to_set(MOI.DefaultDistance(), vcat(t / 2, x), MOI.GeometricMeanCone(n+1)) ≈ 0 atol=eps(Float64)
        # negative x always means positive distance
        @test MOI.distance_to_set(MOI.DefaultDistance(), vcat(t / 2, vcat(x, -1)), MOI.GeometricMeanCone(n+2)) > 0
        @test MOI.distance_to_set(MOI.DefaultDistance(), vcat(t / 2, -x), MOI.GeometricMeanCone(n+1)) > 0
    end
    
    @testset "Exponential and power cones" begin
        for _ in 1:30
            (x, y, z) = rand(3)
            y += 1 # ensure y > 0
            if y * exp(x/y) <= z
                @test MOI.distance_to_set(MOI.DefaultDistance(), [x, y, z], MOI.ExponentialCone()) ≈ 0 atol=eps(Float64)
                @test MOI.distance_to_set(MOI.DefaultDistance(), [x, -1, z], MOI.ExponentialCone()) ≈ 1 atol=eps(Float64)
            else
                @test MOI.distance_to_set(MOI.DefaultDistance(), [x, y, z], MOI.ExponentialCone()) ≈ y * exp(x/y) - z 
            end
            (u, v, w) = randn(3)
            if u != 0.0 # just in case not to blow up
                if -u*exp(v/u) < ℯ * w && u < 0
                    @test MOI.distance_to_set(MOI.DefaultDistance(), [u, v, w], MOI.DualExponentialCone()) ≈ 0 atol=eps(Float64)
                elseif u < 0
                    @test MOI.distance_to_set(MOI.DefaultDistance(), [u, v, w], MOI.DualExponentialCone()) ≈ -u*exp(v/u) - ℯ * w
                end
                
            end
            (x, y) = randn(2)
            if x < 0 || y < 0
                for e in  (10 * rand(10) .- 5) # e in [-5, 5]
                    @test MOI.distance_to_set(MOI.DefaultDistance(), [x, y, 0.0], MOI.PowerCone(e)) > 0
                end
            else
                for e in  (10 * rand(10) .- 5) # e in [-5, 5]
                    r = x^e * y^(1-e)
                    for z in -r:-0.5:r
                        @test MOI.distance_to_set(MOI.DefaultDistance(), [x, y, z], MOI.PowerCone(e)) ≈ 0 atol=eps(Float64)
                    end
                    @test MOI.distance_to_set(MOI.DefaultDistance(), [x, y, 3r], MOI.PowerCone(e)) ≈ MOI.distance_to_set(MOI.DefaultDistance(), [x, y, -3r], MOI.PowerCone(e)) > 0
                end
            end
    
            (u, v, w) = 10 * rand(3)
            e = rand()
            if 0 < e < 1 # avoid exponents of negatives
                @test MOI.distance_to_set(MOI.DefaultDistance(), [u, v, 0.0], MOI.DualPowerCone(e)) ≈ 0 atol = 10eps(Float64)
                @test MOI.distance_to_set(MOI.DefaultDistance(), [u, v, u^e * v^(1-e) / (e^e * (1-e)^(1-e))], MOI.DualPowerCone(e)) ≈ 0 atol=100eps(Float64)
                @test MOI.distance_to_set(MOI.DefaultDistance(), [u, v, 1 + u^e * v^(1-e) / (e^e * (1-e)^(1-e))], MOI.DualPowerCone(e)) ≈ 1
            end
        end
    end    
end

struct DummyDistance <: MOI.AbstractDistance end

MOI.distance_to_set(::DummyDistance, v, s) = MOI.distance_to_set(MOI.DefaultDistance(), v, s) / 2

@testset "Set non-default distance" begin
    for n in 1:3
        v = rand(n)
        for s in (MOI.Reals(n), MOI.Zeros(n), MOI.Nonnegatives(n), MOI.Nonpositives(n))
            @test MOI.distance_to_set(MOI.DefaultDistance(), v, MOI.Reals(n)) ≈ 2 * MOI.distance_to_set(DummyDistance(), v, MOI.Reals(n)) atol=eps(Float64)
            @test MOI.distance_to_set(MOI.DefaultDistance(), -v, MOI.Reals(n)) ≈ 2 * MOI.distance_to_set(DummyDistance(), -v, MOI.Reals(n)) atol=eps(Float64)
        end
    end
end
