# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestLazyIterators

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

function _test_EmptyVector(T)
    v = MOI.Utilities.EmptyVector{T}()
    @test size(v) == (0,)
    @test length(v) == 0
    @test isempty(v)
    @test eltype(v) == T
    @test iterate(v) === nothing
    c = collect(v)
    @test c isa Vector{T}
    @test isempty(c)
    return
end

test_EmptyVector_Int() = _test_EmptyVector(Int)
test_EmptyVector_Float64() = _test_EmptyVector(Float64)

function _test_lazy_map(T)
    for (M{T}, a) in [
        (MOI.Utilities.LazyMap, Iterators.drop(1:3, 1)),
        (MOI.Utilities.VectorLazyMap, [2, 3]),
    ]
        v = MOI.Utilities.lazy_map(T, x -> x^2, a)
        @test v isa M
        @test length(v) == 2
        @test !isempty(v)
        @test eltype(v) == T
        c = collect(v)
        @test c isa Vector{T}
        @test c == [4, 9]
        if a isa AbstractVector
            @test size(v) == (2,)
            @test v[1] == 4
            @test v[2] == 9
            @test collect(Iterators.reverse(v)) == [9, 4]
        end
    end
    return
end

test_lazy_map_Int() = _test_lazy_map(Int)
test_lazy_map_Float64() = _test_lazy_map(Float64)

end  # module

TestLazyIterators.runtests()
