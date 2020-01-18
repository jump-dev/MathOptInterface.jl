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
            @test S(b()) != S(a())
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

    @testset "dimension" begin
        @test MOI.dimension(MOI.ExponentialCone()) == 3
        @test MOI.dimension(MOI.DualExponentialCone()) == 3
        @test MOI.dimension(MOI.PowerCone(1/2)) == 3
        @test MOI.dimension(MOI.DualPowerCone(1/2)) == 3
        @test MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(5)) == 15
        @test MOI.dimension(MOI.PositiveSemidefiniteConeSquare(5)) == 25
        @test MOI.dimension(MOI.LogDetConeTriangle(5)) == 17
        @test MOI.dimension(MOI.LogDetConeSquare(5)) == 27
        @test MOI.dimension(MOI.RootDetConeTriangle(5)) == 16
        @test MOI.dimension(MOI.RootDetConeSquare(5)) == 26
        @test MOI.dimension(MOI.SOS1([1.0, 2.0])) == 2
        @test MOI.dimension(MOI.SOS2([1.0, 2.0])) == 2
        @test MOI.dimension(MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(1.0))) == 2
        @test MOI.dimension(MOI.Complements(5)) == 10
    end

    @testset "Dual Set" begin
        function dual_set_test(set1, set2)
            @test MOI.dual_set(set1) == set2
            @test MOI.dual_set_type(typeof(set1)) == typeof(set2)
            @test MOI.dual_set(set2) == set1
            @test MOI.dual_set_type(typeof(set2)) == typeof(set1)
        end
        function self_dual_set_test(set)
            @test MOI.dual_set(set) == set
            @test MOI.dual_set_type(typeof(set)) == typeof(set)
        end
        # Nonpositives
        nonpositives3 = MOI.Nonpositives(3)
        nonpositives4 = MOI.Nonpositives(4)
        self_dual_set_test(nonpositives3)
        @test MOI.dual_set(nonpositives3) != nonpositives4
        self_dual_set_test(nonpositives4)
        # Nonnegatives
        nonnegatives3 = MOI.Nonnegatives(3)
        nonnegatives4 = MOI.Nonnegatives(4)
        self_dual_set_test(nonnegatives3)
        @test MOI.dual_set(nonnegatives3) != nonnegatives4
        self_dual_set_test(nonnegatives4)
        # Zeros and Reals
        zeros3 = MOI.Zeros(3)
        zeros4 = MOI.Zeros(4)
        reals3 = MOI.Reals(3)
        reals4 = MOI.Reals(4)
        dual_set_test(zeros3, reals3)
        @test MOI.dual_set(reals3) != zeros4
        dual_set_test(zeros4, reals4)
        @test MOI.dual_set(zeros4) != reals3
        # Norm-1 and norm-∞ cones
        norminf2 = MOI.NormInfinityCone(2)
        norminf3 = MOI.NormInfinityCone(3)
        normone2 = MOI.NormOneCone(2)
        normone3 = MOI.NormOneCone(3)
        dual_set_test(norminf2, normone2)
        dual_set_test(norminf3, normone3)
        @test MOI.dual_set(norminf2) != normone3
        @test MOI.dual_set(normone2) != norminf3
        # SOC
        soc2 = MOI.SecondOrderCone(2)
        soc3 = MOI.SecondOrderCone(3)
        self_dual_set_test(soc2)
        @test MOI.dual_set(soc2) != soc3
        self_dual_set_test(soc3)
        # RSOC
        rsoc2 = MOI.RotatedSecondOrderCone(2)
        rsoc3 = MOI.RotatedSecondOrderCone(3)
        self_dual_set_test(rsoc2)
        @test MOI.dual_set(rsoc2) != rsoc3
        self_dual_set_test(rsoc3)
        # Norm-spectral and norm-nuclear cones
        normspec22 = MOI.NormSpectralCone(2, 2)
        normspec23 = MOI.NormSpectralCone(2, 3)
        normnuc22 = MOI.NormNuclearCone(2, 2)
        normnuc23 = MOI.NormNuclearCone(2, 3)
        dual_set_test(normspec23, normnuc23)
        dual_set_test(normspec22, normnuc22)
        @test MOI.dual_set(normspec22) != normnuc23
        @test MOI.dual_set(normnuc22) != normspec23
        # PSDtriangle
        psd2 = MOI.PositiveSemidefiniteConeTriangle(2)
        psd3 = MOI.PositiveSemidefiniteConeTriangle(3)
        self_dual_set_test(psd2)
        @test MOI.dual_set(psd2) != psd3
        self_dual_set_test(psd3)
        # Exponential
        exp = MOI.ExponentialCone()
        dual_exp = MOI.DualExponentialCone()
        dual_set_test(exp, dual_exp)
        @test MOI.dual_set(exp) != exp
        dual_set_test(dual_exp, exp)
        @test MOI.dual_set(dual_exp) != dual_exp
        # Power
        pow03 = MOI.PowerCone(0.3)
        pow04 = MOI.PowerCone(0.4)
        dual_pow03 = MOI.DualPowerCone(0.3)
        dual_set_test(pow03, dual_pow03)
        @test MOI.dual_set(pow03) != pow03
        dual_set_test(dual_pow03, pow03)
        @test MOI.dual_set(dual_pow03) != pow04
        @test MOI.dual_set(dual_pow03) != dual_pow03
        # PSDSquare error
        s = MOI.PositiveSemidefiniteConeSquare(4)
        err = ErrorException("""Dual of `PositiveSemidefiniteConeSquare` is not defined in MathOptInterface.
                                For more details see the comments in `src/Bridges/Constraint/square.jl`.""")
        @test_throws err MOI.dual_set(MOI.PositiveSemidefiniteConeSquare(4))
        @test_throws err MOI.dual_set_type(MOI.PositiveSemidefiniteConeSquare)
        # Not implemented
        s = MOI.LogDetConeTriangle(4)
        err = ErrorException("Dual of $s is not implemented.")
        @test_throws err MOI.dual_set(MOI.LogDetConeTriangle(4))
        err = ErrorException("Dual type of $(typeof(s)) is not implemented.")
        @test_throws err MOI.dual_set_type(MOI.LogDetConeTriangle)
    end
end
