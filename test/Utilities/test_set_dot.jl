# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestSetDot

using Test

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

function test_set_dot(T = Int)
    @test MOI.Utilities._set_dot(1, MOI.ZeroOne(), T) == 1
    psd = MOI.PositiveSemidefiniteConeTriangle(2)
    n = 3
    soc = MOI.SecondOrderCone(n)
    a = MOI.Utilities.ZeroVector{T}(n)
    @test eltype(a) == T
    @test length(a) == n
    @test size(a) == (n,)
    for i in 1:n
        b = MOI.Utilities.CanonicalVector{T}(i, n)
        @test eltype(b) == T
        @test length(b) == n
        @test size(b) == (n,)
        c = MOI.Utilities.CanonicalVector{T}(mod1(i + 1, 3), n)
        @test a[i] == 0
        @test b[i] == 1
        @test c[i] == 0
        for set in [psd, soc]
            @test iszero(MOI.Utilities.set_dot(a, b, set))
            @test iszero(MOI.Utilities.set_dot(b, a, set))
            @test iszero(MOI.Utilities.set_dot(b, c, set))
            @test iszero(MOI.Utilities.set_dot(c, b, set))
        end
        @test MOI.Utilities.set_dot(b, b, soc) == 1
        @test MOI.Utilities._set_dot(i, soc, T) == 1
        expected = i == 2 ? 2 : 1
        @test MOI.Utilities.set_dot(b, b, psd) == expected
        @test MOI.Utilities._set_dot(i, psd, T) == expected
    end
end

end

TestSetDot.runtests()
