# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestFeasibilityChecker

using Test

import LinearAlgebra
import MathOptInterface

const MOI = MathOptInterface

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
    d = ((1/2) * (1 + 1 / √2) * [√2, 1, 1]) .- [1, 1, 1]
    @test MOI.Utilities.distance_to_set([1, 1, 1], set) ≈ LinearAlgebra.norm(d)
    return
end

function test_exponentialcone()
    @test_throws(
        DimensionMismatch,
        MOI.Utilities.distance_to_set([-1.0, 1.0], MOI.ExponentialCone())
    )
    set = MOI.ExponentialCone()
    @test MOI.Utilities.distance_to_set([1, 1, exp(1)], set) ≈ 0.0
    @test MOI.Utilities.distance_to_set([2, 3, 3], set) ≈ 3 * exp(2 / 3) - 3
    @test MOI.Utilities.distance_to_set([2, -1, 3], set) ≈ sqrt(2^2 + (exp(2) - 3)^2)
    return
end

end

TestFeasibilityChecker.runtests()
