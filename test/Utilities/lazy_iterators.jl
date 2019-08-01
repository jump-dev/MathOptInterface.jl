using Test
using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

@testset "EmptyVector{$T}" for T in [Int, Float64]
    v = MOIU.EmptyVector{T}()
    @test size(v) == (0,)
    @test isempty(v)
    @test eltype(v) == T
    @test iterate(v) === nothing
    c = collect(v)
    @test c isa Vector{T}
    @test isempty(c)
end

@testset "LazyMap{$T}" for T in [Int, Float64]
    v = MOIU.LazyMap{T}(x -> x^2, [2, 3])
    @test size(v) == (2,)
    @test !isempty(v)
    @test eltype(v) == T
    c = collect(v)
    @test c isa Vector{T}
    @test c == [4, 9]
end

@testset "LazyFilter{$T}" for T in [Int32, Int64]
    v = MOIU.LazyFilter(isodd, T[2, 3, 5, 4])
    @test eltype(v) == T
    c = collect(v)
    @test c isa Vector{T}
    @test c == [3, 5]
end

@testset "LazyCat{$T}" for T in [Int, Float64]
    v1 = T[2, 3]
    v2 = T[5]
    v = MOIU.LazyCat((v1, v2))
    c = collect(v)
    @test c isa Vector{T}
    @test c == T[2, 3, 5]
end
