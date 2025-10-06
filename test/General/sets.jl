# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestSets

using Test
import MathOptInterface as MOI

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

function test_sets_equals()
    # By default, `==` redirects to `===`, it works for bits type
    # but not for `BigInt`s. We define functions creating different
    # instances so that `a() !== a()`.
    a() = big(1)
    b() = big(2)
    @test a() !== a()
    @test b() !== b()
    for S in [
        MOI.LessThan,
        MOI.GreaterThan,
        MOI.EqualTo,
        MOI.PowerCone,
        MOI.DualPowerCone,
    ]
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
    S = MOI.Indicator
    A() = MOI.LessThan(a())
    B() = MOI.LessThan(b())
    @test S{MOI.ACTIVATE_ON_ZERO}(A()) == S{MOI.ACTIVATE_ON_ZERO}(A())
    @test S{MOI.ACTIVATE_ON_ZERO}(A()) != S{MOI.ACTIVATE_ON_ONE}(A())
    @test S{MOI.ACTIVATE_ON_ZERO}(A()) != S{MOI.ACTIVATE_ON_ZERO}(B())
    @test S{MOI.ACTIVATE_ON_ONE}(A()) != S{MOI.ACTIVATE_ON_ONE}(B())
end

function test_sets_sos1_copy()
    s = MOI.SOS1([1.0])
    s_copy = copy(s)
    s_copy.weights[1] = 2.0
    @test s.weights[1] == 1.0
end

function test_sets_sos2_copy()
    s = MOI.SOS2([1.0])
    s_copy = copy(s)
    s_copy.weights[1] = 2.0
    @test s.weights[1] == 1.0
end

function test_sets_indicator_copy()
    s1 = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(4.0))
    s2 = MOI.Indicator{MOI.ACTIVATE_ON_ZERO}(MOI.GreaterThan(4.0))
    s1_copy = copy(s1)
    s2_copy = copy(s2)
    @test s1_copy isa MOI.Indicator{MOI.ACTIVATE_ON_ONE}
    @test s1 == s1_copy
    @test s2_copy isa MOI.Indicator{MOI.ACTIVATE_ON_ZERO}
    @test s2 == s2_copy
    s3 = MOI.Indicator{MOI.ACTIVATE_ON_ZERO}(MutLessThan(4.0))
    s3_copy = copy(s3)
    @test s3.set.upper ≈ 4.0
    s3_copy.set.upper = 5.0
    @test s3.set.upper ≈ 4.0
    @test s3_copy.set.upper ≈ 5.0
end

function test_sets_broadcast()
    model = DummyModelWithAdd()
    x = MOI.add_variables(model, 3)
    cis = MOI.add_constraint.(model, x, MOI.EqualTo(0.0))
    @test cis isa
          Vector{MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}}}
    @test length(cis) == 3
end

function test_sets_dimension()
    @test MOI.dimension(MOI.ExponentialCone()) == 3
    @test MOI.dimension(MOI.DualExponentialCone()) == 3
    @test MOI.dimension(MOI.PowerCone(1 / 2)) == 3
    @test MOI.dimension(MOI.DualPowerCone(1 / 2)) == 3
    @test MOI.dimension(MOI.ScaledPositiveSemidefiniteConeTriangle(5)) == 15
    @test MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(5)) == 15
    @test MOI.dimension(MOI.PositiveSemidefiniteConeSquare(5)) == 25
    @test MOI.dimension(MOI.LogDetConeTriangle(5)) == 17
    @test MOI.dimension(MOI.LogDetConeSquare(5)) == 27
    @test MOI.dimension(MOI.RootDetConeTriangle(5)) == 16
    @test MOI.dimension(MOI.RootDetConeSquare(5)) == 26
    @test MOI.dimension(MOI.SOS1([1.0, 2.0])) == 2
    @test MOI.dimension(MOI.SOS2([1.0, 2.0])) == 2
    @test MOI.dimension(
        MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(1.0)),
    ) == 2
    @test MOI.dimension(MOI.Complements(10)) == 10
end

function test_sets_bin_packing_errors()
    @test_throws DomainError MOI.BinPacking(-1.0, [1.0, 2.0])
    @test_throws DomainError MOI.BinPacking(1.0, [1.0, -2.0])
    return
end

function test_sets_DimensionMismatch()
    for (S, min_dimension) in (
        (MOI.Reals, 0),
        (MOI.Zeros, 0),
        (MOI.Nonnegatives, 0),
        (MOI.Nonpositives, 0),
        (MOI.NormInfinityCone, 1),
        (MOI.NormOneCone, 1),
        (MOI.SecondOrderCone, 1),
        (MOI.RotatedSecondOrderCone, 2),
        (MOI.GeometricMeanCone, 2),
        (MOI.Complements, 0),
        (MOI.RelativeEntropyCone, 1),
        (MOI.ScaledPositiveSemidefiniteConeTriangle, 0),
        (MOI.PositiveSemidefiniteConeTriangle, 0),
        (MOI.PositiveSemidefiniteConeSquare, 0),
        (MOI.LogDetConeTriangle, 0),
        (MOI.LogDetConeSquare, 0),
        (MOI.RootDetConeTriangle, 0),
        (MOI.RootDetConeSquare, 0),
        (MOI.AllDifferent, 0),
        (MOI.CountDistinct, 1),
        (MOI.CountGreaterThan, 2),
        (MOI.Cumulative, 1),
        (MOI.Circuit, 0),
    )
        @test_throws DimensionMismatch S(min_dimension - 1)
        @test S(min_dimension) isa S
    end
    @test_throws DimensionMismatch MOI.NormSpectralCone(-1, 0)
    @test_throws DimensionMismatch MOI.NormSpectralCone(0, -1)
    @test MOI.NormSpectralCone(0, 0) isa MOI.NormSpectralCone
    @test_throws DimensionMismatch MOI.NormNuclearCone(-1, 0)
    @test_throws DimensionMismatch MOI.NormNuclearCone(0, -1)
    @test MOI.NormNuclearCone(0, 0) isa MOI.NormNuclearCone
    # Other dimension checks
    @test_throws DimensionMismatch MOI.RelativeEntropyCone(2)
    @test_throws DimensionMismatch MOI.Complements(-3)
    @test_throws DimensionMismatch MOI.Complements(3)
    @test_throws DimensionMismatch MOI.CountBelongs(0, Set([1, 2]))
    @test_throws DimensionMismatch MOI.CountAtLeast(1, [-1, 2], Set([1, 2]))
    @test_throws DimensionMismatch MOI.NormCone(4, 0)
    @test MOI.NormCone(4, 1) isa MOI.NormCone
    @test_throws ArgumentError MOI.NormCone(0.99, 5)
    @test_throws ArgumentError MOI.NormCone(0.5, 5)
    @test_throws ArgumentError MOI.NormCone(-2, 5)
    return
end

function _dual_set_test(set1, set2)
    @test MOI.dual_set(set1) == set2
    @test MOI.dual_set_type(typeof(set1)) == typeof(set2)
    @test MOI.dual_set(set2) == set1
    @test MOI.dual_set_type(typeof(set2)) == typeof(set1)
end

function _self_dual_set_test(set)
    @test MOI.dual_set(set) == set
    @test MOI.dual_set_type(typeof(set)) == typeof(set)
end

function test_sets_dual_nonpositives()
    nonpositives3 = MOI.Nonpositives(3)
    nonpositives4 = MOI.Nonpositives(4)
    _self_dual_set_test(nonpositives3)
    @test MOI.dual_set(nonpositives3) != nonpositives4
    _self_dual_set_test(nonpositives4)
    return
end

function test_sets_dual_nonnegatives()
    nonnegatives3 = MOI.Nonnegatives(3)
    nonnegatives4 = MOI.Nonnegatives(4)
    _self_dual_set_test(nonnegatives3)
    @test MOI.dual_set(nonnegatives3) != nonnegatives4
    _self_dual_set_test(nonnegatives4)
    return
end

function test_sets_dual_zeroreal()
    zeros3 = MOI.Zeros(3)
    zeros4 = MOI.Zeros(4)
    reals3 = MOI.Reals(3)
    reals4 = MOI.Reals(4)
    _dual_set_test(zeros3, reals3)
    @test MOI.dual_set(reals3) != zeros4
    _dual_set_test(zeros4, reals4)
    @test MOI.dual_set(zeros4) != reals3
    return
end

function test_sets_dual_norm()
    norminf2 = MOI.NormInfinityCone(2)
    norminf3 = MOI.NormInfinityCone(3)
    normone2 = MOI.NormOneCone(2)
    normone3 = MOI.NormOneCone(3)
    _dual_set_test(norminf2, normone2)
    _dual_set_test(norminf3, normone3)
    @test MOI.dual_set(norminf2) != normone3
    @test MOI.dual_set(normone2) != norminf3
    return
end

function test_sets_dual_soc()
    soc2 = MOI.SecondOrderCone(2)
    soc3 = MOI.SecondOrderCone(3)
    _self_dual_set_test(soc2)
    @test MOI.dual_set(soc2) != soc3
    _self_dual_set_test(soc3)
    return
end

function test_sets_dual_rsoc()
    rsoc2 = MOI.RotatedSecondOrderCone(2)
    rsoc3 = MOI.RotatedSecondOrderCone(3)
    _self_dual_set_test(rsoc2)
    @test MOI.dual_set(rsoc2) != rsoc3
    _self_dual_set_test(rsoc3)
    return
end

function test_sets_dual_normspectral()
    normspec22 = MOI.NormSpectralCone(2, 2)
    normspec23 = MOI.NormSpectralCone(2, 3)
    normnuc22 = MOI.NormNuclearCone(2, 2)
    normnuc23 = MOI.NormNuclearCone(2, 3)
    _dual_set_test(normspec23, normnuc23)
    _dual_set_test(normspec22, normnuc22)
    @test MOI.dual_set(normspec22) != normnuc23
    @test MOI.dual_set(normnuc22) != normspec23
    return
end

function _test_sets_dual_psdtriangle(psd2, psd3)
    _self_dual_set_test(psd2)
    @test MOI.dual_set(psd2) != psd3
    _self_dual_set_test(psd3)
    return
end

function test_sets_dual_psdtriangle()
    _test_sets_dual_psdtriangle(
        MOI.PositiveSemidefiniteConeTriangle(2),
        MOI.PositiveSemidefiniteConeTriangle(3),
    )
    _test_sets_dual_psdtriangle(
        MOI.ScaledPositiveSemidefiniteConeTriangle(2),
        MOI.ScaledPositiveSemidefiniteConeTriangle(3),
    )
    _test_sets_dual_psdtriangle(
        MOI.HermitianPositiveSemidefiniteConeTriangle(2),
        MOI.HermitianPositiveSemidefiniteConeTriangle(3),
    )
    _test_sets_dual_psdtriangle(
        MOI.Scaled(MOI.HermitianPositiveSemidefiniteConeTriangle(2)),
        MOI.Scaled(MOI.HermitianPositiveSemidefiniteConeTriangle(3)),
    )
    return
end

function test_sets_dual_exponential()
    exp = MOI.ExponentialCone()
    dual_exp = MOI.DualExponentialCone()
    _dual_set_test(exp, dual_exp)
    @test MOI.dual_set(exp) != exp
    _dual_set_test(dual_exp, exp)
    @test MOI.dual_set(dual_exp) != dual_exp
    return
end

function test_sets_dual_power()
    pow03 = MOI.PowerCone(0.3)
    pow04 = MOI.PowerCone(0.4)
    dual_pow03 = MOI.DualPowerCone(0.3)
    _dual_set_test(pow03, dual_pow03)
    @test MOI.dual_set(pow03) != pow03
    _dual_set_test(dual_pow03, pow03)
    @test MOI.dual_set(dual_pow03) != pow04
    @test MOI.dual_set(dual_pow03) != dual_pow03
    return
end

function test_sets_dual_psdsquare()
    s = MOI.PositiveSemidefiniteConeSquare(4)
    err = ErrorException(
        """Dual of `PositiveSemidefiniteConeSquare` is not defined in MathOptInterface.
            For more details see the comments in `src/Bridges/Constraint/bridges/square.jl`.""",
    )
    @test_throws err MOI.dual_set(s)
    @test_throws err MOI.dual_set_type(MOI.PositiveSemidefiniteConeSquare)
end

function test_sets_dual_nonimplemented()
    s = MOI.LogDetConeTriangle(4)
    err = ErrorException("Dual of $s is not implemented.")
    @test_throws err MOI.dual_set(MOI.LogDetConeTriangle(4))
    err = ErrorException("Dual type of $(typeof(s)) is not implemented.")
    @test_throws err MOI.dual_set_type(MOI.LogDetConeTriangle)
end

function test_sets_Interval()
    @test MOI.Interval(MOI.GreaterThan(1.0)) === MOI.Interval(1.0, Inf)
    @test MOI.Interval(MOI.LessThan(2.0)) === MOI.Interval(-Inf, 2.0)
    @test MOI.Interval(MOI.EqualTo(3.0)) === MOI.Interval(3.0, 3.0)

    @test MOI.Interval(MOI.GreaterThan(1.0f0)) === MOI.Interval(1.0f0, Inf32)
    @test MOI.Interval(MOI.LessThan(2.0f0)) === MOI.Interval(-Inf32, 2.0f0)
    @test MOI.Interval(MOI.EqualTo(3.0f0)) === MOI.Interval(3.0f0, 3.0f0)

    @test_throws MethodError MOI.Interval(MOI.GreaterThan(false))
    @test_throws MethodError MOI.Interval(MOI.LessThan(true))
    @test MOI.Interval(MOI.EqualTo(true)) === MOI.Interval(true, true)

    @test_throws MethodError MOI.Interval(MOI.GreaterThan(1))
    @test_throws MethodError MOI.Interval(MOI.LessThan(2))
    @test MOI.Interval(MOI.EqualTo(3)) === MOI.Interval(3, 3)

    @test MOI.Interval(MOI.Interval(1, 2)) == MOI.Interval(1, 2)
end

function test_sets_hyperrectangle()
    @test_throws ArgumentError MOI.HyperRectangle([1.0], [2.0, 3.0])
    return
end

function test_sets_reified()
    set = MOI.Reified(MOI.AllDifferent(2))
    @test MOI.dimension(set) == 3
    @test set == copy(set)
    set = MOI.Reified(MOI.GreaterThan(1))
    @test MOI.dimension(set) == 2
    @test set == copy(set)
    return
end

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$name", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
end

function test_interval_promote()
    for S in (MOI.Interval, MOI.Semiinteger, MOI.Semicontinuous)
        set = S(1.0, π)
        @test set isa S{Float64}
        @test set.lower == 1.0
        @test set.upper ≈ π
        set = S(big(1), 2)
        @test set isa S{BigInt}
        @test set.lower == 1
        @test set.upper == 2
    end
    return
end

mutable struct TestSetNoCopy <: MOI.AbstractVectorSet
    data::BigInt
end

function test_set_not_isbitstype_copy()
    @test !isbitstype(TestSetNoCopy)
    @test_throws(
        ErrorException(
            "Base.copy(::$TestSetNoCopy) is not implemented for this set.",
        ),
        copy(TestSetNoCopy(BigInt(2))),
    )
    return
end

function test_deprecated_PositiveSemidefiniteConeTriangle()
    set = MOI.Scaled(MOI.PositiveSemidefiniteConeTriangle(2))
    @test set.side_dimension == 2
    @test !hasfield(typeof(set), :side_dimension)
    return
end

function test_update_dimension()
    @test !MOI.supports_dimension_update(MOI.ExponentialCone)
    for S in (MOI.Reals, MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives)
        @test MOI.supports_dimension_update(S)
        set = S(1)
        new_set = MOI.update_dimension(set, 2)
        @test MOI.dimension(set) == 1
        @test MOI.dimension(new_set) == 2
    end
    return
end

function test_VectorNonlinearOracle()
    @test_throws(
        DimensionMismatch,
        MOI.VectorNonlinearOracle(;
            dimension = 3,
            l = [0.0, 0.0, 1.0],
            u = [1.0, 0.0],
            eval_f = (ret, x) -> nothing,
            jacobian_structure = Tuple{Int,Int}[],
            eval_jacobian = (ret, x) -> nothing,
        ),
    )
    set = MOI.VectorNonlinearOracle(;
        dimension = 3,
        l = [0.0, 0.0],
        u = [1.0, 0.0],
        eval_f = (ret, x) -> begin
            ret[1] = x[2]^2
            ret[2] = x[3]^2 + x[4]^3 - x[1]
            return
        end,
        jacobian_structure = [(1, 2), (2, 1), (2, 3), (2, 4)],
        eval_jacobian = (ret, x) -> begin
            ret[1] = 2.0 * x[2]
            ret[2] = -1.0
            ret[3] = 2.0 * x[3]
            ret[4] = 3.0 * x[4]^2
            return
        end,
        hessian_lagrangian_structure = [(2, 2), (3, 3), (4, 4)],
        eval_hessian_lagrangian = (ret, x, u) -> begin
            ret[1] = 2.0 * u[1]
            ret[2] = 2.0 * u[2]
            ret[3] = 6.0 * x[4] * u[2]
            return
        end,
    )
    contents = """
    VectorNonlinearOracle{Float64}(;
        dimension = 3,
        l = [0.0, 0.0],
        u = [1.0, 0.0],
        ...,
    )"""
    @test sprint(show, set) == contents
    return
end

end  # module

TestSets.runtests()
