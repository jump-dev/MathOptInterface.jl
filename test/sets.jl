using Test

using MathOptInterface
const MOI = MathOptInterface

include("dummy.jl")

"""
    MutLessThan{T<:Real} <: MOI.AbstractScalarSet

A mutable `LessThan`-like set to test `copy` of indicator set
"""
mutable struct MutLessThan{T<:Real} <: MOI.AbstractScalarSet
    upper::T
    MutLessThan(v::T) where {T<:Real} = new{T}(v)
end

Base.copy(mlt::MutLessThan) = MutLessThan(Base.copy(mlt.upper))

@testset "Sets" begin
    @testset "==" begin
        # By default, `==` redirects to `===`, it works for bits type
        # but not for `BigInt`s. We define functions creating different
        # instances so that `a() !== a()`.
        a() = big(1)
        b() = big(2)
        @test a() !== a()
        @test b() !== b()
        for S in [MOI.LessThan, MOI.GreaterThan, MOI.EqualTo, MOI.PowerCone, MOI.DualPowerCone]
            @test S(a()) == S(a())
            @test S(a()) != S(b())
            @test S(b()) == S(b())
            @test S(b()) == S(b())
            @test S(b()) == S(b())
        end
        for S in [MOI.Interval, MOI.Semicontinuous, MOI.Semiinteger]
            @test S(a(), b()) == S(a(), b())
            @test S(a(), b()) != S(b(), a())
            @test S(a(), b()) != S(b(), b())
            @test S(a(), b()) != S(a(), a())
            @test S(a(), a()) != S(b(), b())
            @test S(a(), a()) == S(a(), a())
        end
        S = MOI.IndicatorSet
        A() = MOI.LessThan(a())
        B() = MOI.LessThan(b())
        @test S{MOI.ACTIVATE_ON_ZERO}(A()) == S{MOI.ACTIVATE_ON_ZERO}(A())
        @test S{MOI.ACTIVATE_ON_ZERO}(A()) != S{MOI.ACTIVATE_ON_ONE}(A())
        @test S{MOI.ACTIVATE_ON_ZERO}(A()) != S{MOI.ACTIVATE_ON_ZERO}(B())
        @test S{MOI.ACTIVATE_ON_ONE}(A()) != S{MOI.ACTIVATE_ON_ONE}(B())
    end
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
            s3 = MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO}(MutLessThan(4.0))
            s3_copy = copy(s3)
            @test s3.set.upper ≈ 4.0
            s3_copy.set.upper = 5.0
            @test s3.set.upper ≈ 4.0
            @test s3_copy.set.upper ≈ 5.0
        end
    end
    @testset "Broadcast" begin
        model = DummyModelWithAdd()
        x = MOI.add_variables(model, 3)
        cis = MOI.add_constraint.(model, x, MOI.EqualTo(0.0))
        @test cis isa Vector{MOI.ConstraintIndex{MOI.SingleVariable,
                                                 MOI.EqualTo{Float64}}}
        @test length(cis) == 3
    end
end
