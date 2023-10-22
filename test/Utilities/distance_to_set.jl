# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestFeasibilityChecker

using Test

import LinearAlgebra
import MathOptInterface as MOI

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

function _test_set(set, pairs...; mismatch = nothing)
    if mismatch !== nothing
        @test_throws(
            DimensionMismatch,
            MOI.Utilities.distance_to_set(mismatch, set),
        )
    end
    for (x, d) in pairs
        @test MOI.Utilities.distance_to_set(x, set) ≈ d
    end
    return
end

function test_unsupported()
    @test_throws(
        ErrorException,
        MOI.Utilities.distance_to_set([1.0, 1.0], MOI.Complements(2)),
    )
    return
end

function test_lessthan()
    @test MOI.Utilities.distance_to_set(1.0, MOI.LessThan(2.0)) ≈ 0.0
    @test MOI.Utilities.distance_to_set(1.0, MOI.LessThan(0.5)) ≈ 0.5
    return
end

function test_greaterthan()
    @test MOI.Utilities.distance_to_set(1.0, MOI.GreaterThan(2.0)) ≈ 1.0
    @test MOI.Utilities.distance_to_set(1.0, MOI.GreaterThan(0.5)) ≈ 0.0
    return
end

function test_equalto()
    @test MOI.Utilities.distance_to_set(1.0, MOI.EqualTo(2.0)) ≈ 1.0
    @test MOI.Utilities.distance_to_set(1.0, MOI.EqualTo(0.5)) ≈ 0.5
    return
end

function test_interval()
    @test MOI.Utilities.distance_to_set(1.0, MOI.Interval(1.0, 2.0)) ≈ 0.0
    @test MOI.Utilities.distance_to_set(0.5, MOI.Interval(1.0, 2.0)) ≈ 0.5
    @test MOI.Utilities.distance_to_set(2.75, MOI.Interval(1.0, 2.0)) ≈ 0.75
    return
end

function test_zeroone()
    @test MOI.Utilities.distance_to_set(0.6, MOI.ZeroOne()) ≈ 0.4
    @test MOI.Utilities.distance_to_set(-0.01, MOI.ZeroOne()) ≈ 0.01
    @test MOI.Utilities.distance_to_set(1.01, MOI.ZeroOne()) ≈ 0.01
    return
end

function test_integer()
    @test MOI.Utilities.distance_to_set(0.6, MOI.Integer()) ≈ 0.4
    @test MOI.Utilities.distance_to_set(3.1, MOI.Integer()) ≈ 0.1
    @test MOI.Utilities.distance_to_set(-0.01, MOI.Integer()) ≈ 0.01
    @test MOI.Utilities.distance_to_set(1.01, MOI.Integer()) ≈ 0.01
    return
end

function test_semicontinuous()
    s = MOI.Semicontinuous(2.0, 4.0)
    @test MOI.Utilities.distance_to_set(-2.0, s) ≈ 2.0
    @test MOI.Utilities.distance_to_set(0.5, s) ≈ 0.5
    @test MOI.Utilities.distance_to_set(1.9, s) ≈ 0.1
    @test MOI.Utilities.distance_to_set(2.1, s) ≈ 0.0
    @test MOI.Utilities.distance_to_set(4.1, s) ≈ 0.1
    return
end

function test_semiintger()
    s = MOI.Semiinteger(1.9, 4.0)
    @test MOI.Utilities.distance_to_set(-2.0, s) ≈ 2.0
    @test MOI.Utilities.distance_to_set(0.5, s) ≈ 0.5
    @test MOI.Utilities.distance_to_set(1.9, s) ≈ 0.1
    @test MOI.Utilities.distance_to_set(2.1, s) ≈ 0.1
    @test MOI.Utilities.distance_to_set(4.1, s) ≈ 0.1
    return
end

function test_nonnegatives()
    @test_throws(
        DimensionMismatch,
        MOI.Utilities.distance_to_set([-1.0, 1.0], MOI.Nonnegatives(1))
    )
    @test MOI.Utilities.distance_to_set([-1.0, 1.0], MOI.Nonnegatives(2)) ≈ 1.0
    return
end

function test_nonpositives()
    @test_throws(
        DimensionMismatch,
        MOI.Utilities.distance_to_set([-1.0, 1.0], MOI.Nonpositives(1))
    )
    @test MOI.Utilities.distance_to_set([-1.0, 1.0], MOI.Nonpositives(2)) ≈ 1.0
    return
end

function test_reals()
    @test_throws(
        DimensionMismatch,
        MOI.Utilities.distance_to_set([-1.0, 1.0], MOI.Reals(1))
    )
    @test MOI.Utilities.distance_to_set([-1.0, 1.0], MOI.Reals(2)) ≈ 0.0
    return
end

function test_zeros()
    @test_throws(
        DimensionMismatch,
        MOI.Utilities.distance_to_set([-1.0, 1.0], MOI.Zeros(1))
    )
    @test MOI.Utilities.distance_to_set([-1.0, 1.0], MOI.Zeros(2)) ≈ sqrt(2)
    return
end

function test_secondordercone()
    @test_throws(
        DimensionMismatch,
        MOI.Utilities.distance_to_set([-1.0, 1.0], MOI.SecondOrderCone(3))
    )
    set = MOI.SecondOrderCone(3)
    @test MOI.Utilities.distance_to_set([sqrt(2), 1, 1], set) ≈ 0.0
    @test MOI.Utilities.distance_to_set([-sqrt(2), 1, 1], set) ≈ 2
    @test MOI.Utilities.distance_to_set([-2, 1, 1], set) ≈ sqrt(6)
    # According to Boyd, (t, x) = (1, [1, 1]), projects to:
    d = ((1 / 2) * (1 + 1 / √2) * [√2, 1, 1]) .- [1, 1, 1]
    @test MOI.Utilities.distance_to_set([1, 1, 1], set) ≈ LinearAlgebra.norm(d)
    return
end

function test_rotatedsecondordercone()
    _test_set(
        MOI.RotatedSecondOrderCone(4),
        [1.0, 1.0, 1.0, 1.0] => 0.0,
        [-1.0, 1.0, 1.0, 1.0] => sqrt(1 + 4^2);
        mismatch = [1.0],
    )
    return
end

function test_exponential()
    _test_set(
        MOI.ExponentialCone(),
        [1.0, 1.0, 1.0] => exp(1) - 1,
        [1.0, 1.0, 3.0] => 0.0,
        [1.0, -1.0, 1.0] => sqrt(2^2 + (exp(1) - 1)^2);
        mismatch = [1.0],
    )
    return
end

function test_dualexponential()
    _test_set(
        MOI.DualExponentialCone(),
        [1.0, 1.0, 1.0] => 2.0,
        [-1.0, 1.0, 3.0] => 0.0,
        [-2.0, 3.0, 0.1] => 2 * exp(3 / -2) - 0.1 * exp(1);
        mismatch = [1.0],
    )
    return
end

function test_geometricmeancone()
    _test_set(
        MOI.GeometricMeanCone(3),
        [1.0, 1.0, 1.0] => 0.0,
        [1.5, 1.0, 2.0] => 1.5 - sqrt(2),
        [3.5, 3.0, 2.0] => 3.5 - sqrt(6),
        [1.5, -1.0, 2.0] => sqrt(1 + 1.5^2);
        mismatch = [1.0],
    )
    return
end

function test_powercone()
    _test_set(
        MOI.PowerCone(0.5),
        [1.0, 1.0, 1.0] => 0.0,
        [-1.0, 1.0, 1.0] => sqrt(2),
        [1.0, -1.0, 2.0] => sqrt(5),
        [1.5, 1.0, -2.0] => 2 - 1.5^0.5 * 1^0.5,
        [1.5, 1.0, 2.0] => 2 - 1.5^0.5 * 1^0.5;
        mismatch = [1.0],
    )
    return
end

function test_dualpowercone()
    _test_set(
        MOI.DualPowerCone(0.5),
        [1.0, 1.0, 1.0] => 0.0,
        [-1.5, 1.0, -3.0] => sqrt(1.5^2 + 3^2),
        [1.5, -1.0, 3.0] => sqrt(1.0^2 + 3^2),
        [1.5, 1.0, -2.0] => 0.0,
        [1.5, 1.0, -3.0] => 3 - sqrt(3) * sqrt(2),
        [1.5, 1.0, 3.0] => 3 - sqrt(3) * sqrt(2);
        mismatch = [1.0],
    )
    return
end

function test_normonecone()
    _test_set(
        MOI.NormOneCone(3),
        [1.0, 1.0, 1.0] => 1.0,
        [1.5, 1.0, -2.0] => 1.5,
        [3.5, 1.0, -2.0] => 0.0;
        mismatch = [1.0],
    )
    return
end

function test_norminfinitycone()
    _test_set(
        MOI.NormInfinityCone(3),
        [1.0, 1.0, 1.0] => 0.0,
        [1.5, 1.0, -2.0] => 0.5,
        [3.5, 1.0, -2.0] => 0.0;
        mismatch = [1.0],
    )
    return
end

function test_relativeentropycone()
    _test_set(
        MOI.RelativeEntropyCone(5),
        [1.0, 1.0, 1.0, 1.0, 1.0] => 0.0,
        [-2.0, 1.0, 1.0, 1.0, 1.0] => 2.0,
        [1.0, 1.0, 2.0, 3.0, 1.0] => 3 * log(3 / 1) + 1 * log(1 / 2) - 1,
        [4.0, 1.0, 2.0, 3.0, 1.0] => 0.0,
        [4.0, -1.0, 2.0, 3.0, 1.0] => 2.0,
        [4.0, 1.0, -2.0, 3.0, 1.0] => 3.0,
        [4.0, 1.0, -2.0, 3.0, -1.0] => sqrt(3^2 + 2^2),
        [0.0, 1.0, -2.0, 3.0, -1.0] => sqrt(3^2 + 2^2 + (3 * log(3))^2);
        mismatch = [1.0],
    )
    return
end

function test_hyperrectangle()
    _test_set(
        MOI.HyperRectangle([0.0, 1.0], [1.0, 2.0]),
        [0.0, 1.0] => 0.0,
        [0.5, 1.2] => 0.0,
        [-1.0, 1.5] => 1.0,
        [0.5, 2.5] => 0.5,
        [2.0, 0.0] => sqrt(2);
        mismatch = [1.0],
    )
    return
end

function test_normcone()
    _test_set(
        MOI.NormCone(3, 4),
        [1.0, 2.0, 3.0, 4.0] => LinearAlgebra.norm([2, 3, 4], 3) - 1,
        [1.5, -2.0, 3.0, 4.0] => LinearAlgebra.norm([-2, 3, 4], 3) - 1.5;
        mismatch = [1.0],
    )
    return
end

function test_sos1()
    _test_set(
        MOI.SOS1([1.0, 3.0, 2.0]),
        [0.0, 1.0, 0.0] => 0.0,
        [-0.5, 0.0, 0.0] => 0.0,
        [1.0, 1.0, 0.0] => 1.0,
        [-0.5, 1.5, 1.0] => sqrt(1 + 0.5^2);
        mismatch = [1.0],
    )
    return
end

function test_sos2()
    _test_set(
        MOI.SOS2([1.0, 3.0, 2.0]),
        [0.0, 1.0, 0.0] => 0.0,
        # [-0.5, 0.0, 0.0] => 0.0,
        [0.0, 1.0, 1.0] => 0.0,
        # [-0.5, 0.0, 0.5] => 0.0,
        # [1.0, 1.0, 0.0] => 1.0,
        # [-0.5, 0.6, 0.0] => 0.5,
        # [-0.5, 1.5, 1.0] => 0.5;
        mismatch = [1.0],
    )
    return
end

end

TestFeasibilityChecker.runtests()
