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
