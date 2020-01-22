using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIU = MathOptInterface.Utilities

struct DummyOptimizer <: MOI.AbstractOptimizer end
MOI.is_empty(::DummyOptimizer) = true

@testset "Instantiate with $T" for T in [Float64, Int]
    f() = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{T}()))
    uninstantiated = MOI.parametrize(f, MOI.Silent() => true, :a => 1, "b" => 2)
    optimizer = MOI.instantiate(uninstantiated)
    @test optimizer isa MOIU.MockOptimizer{MOIU.UniversalFallback{MOIU.Model{T}}}
    @test MOI.get(optimizer, MOI.Silent())
    for with_names in [true, false]
        optimizer = MOI.instantiate_with_bridges(uninstantiated, with_names, T)
        @test optimizer isa MOI.Bridges.LazyBridgeOptimizer{MOIU.MockOptimizer{MOIU.UniversalFallback{MOIU.Model{T}}}}
        @test MOI.get(optimizer, MOI.Silent())
        @test MOI.get(optimizer, MOI.RawParameter("a")) == 1
        @test MOI.get(optimizer, MOI.RawParameter("b")) == 2
    end

    uninstantiated = MOI.UninstantiatedOptimizer(DummyOptimizer, [])
    optimizer = MOI.instantiate(uninstantiated)
    @test optimizer isa DummyOptimizer
    for with_names in [true, false]
        optimizer = MOI.instantiate_with_bridges(uninstantiated, with_names, T)
        @test optimizer isa MOI.Bridges.LazyBridgeOptimizer{MOIU.CachingOptimizer{DummyOptimizer, MOIU.UniversalFallback{MOIU.Model{T}}}}
    end

    err = ErrorException("The provided `uninstantiated` returned a non-empty optimizer.")
    function g()
        model = f()
        MOI.add_variable(model)
        return model
    end
    @test_throws err MOI.instantiate(g)
    uninstantiated = MOI.parametrize(g)
    @test_throws err MOI.instantiate(uninstantiated)

    err = ErrorException(MOI._instantiate_not_callable_message)
    @test_throws err MOI.parametrize(1)
    @test_throws err MOI.parametrize(1, MOI.Silent() => true)
    @test_throws err MOI.instantiate(1)

    err = ErrorException("The provided `uninstantiated` returned an object of type " *
        "$Int. Expected a MathOptInterface.AbstractOptimizer.")
    h() = 1
    @test_throws err MOI.instantiate(h)
    uninstantiated = MOI.parametrize(h)
    @test_throws err MOI.instantiate(uninstantiated)
end
