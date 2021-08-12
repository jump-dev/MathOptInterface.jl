module TestLazyIterators

using MathOptInterface
using Test

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

function _test_EmptyVector(T)
    v = MOI.Bridges.EmptyVector{T}()
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

function _test_LazyMap(T)
    v = MOI.Bridges.LazyMap{T}(x -> x^2, [2, 3])
    @test size(v) == (2,)
    @test length(v) == 2
    @test !isempty(v)
    @test eltype(v) == T
    c = collect(v)
    @test c isa Vector{T}
    @test c == [4, 9]
    return
end

test_LazyMap_Int() = _test_LazyMap(Int)
test_LazyMap_Float64() = _test_LazyMap(Float64)

end  # module

TestLazyIterators.runtests()
