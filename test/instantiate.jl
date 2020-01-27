using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIU = MathOptInterface.Utilities

struct DummyOptimizer <: MOI.AbstractOptimizer end
MOI.is_empty(::DummyOptimizer) = true

@testset "Instantiate with $T" for T in [Float64, Int]
    f() = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{T}()))
    optimizer_constructor = MOI.OptimizerWithAttributes(f, MOI.Silent() => true, "a" => 1, "b" => 2)
    optimizer = optimizer_constructor()
    @test optimizer isa MOIU.MockOptimizer{MOIU.UniversalFallback{MOIU.Model{T}}}
    @test MOI.get(optimizer, MOI.Silent())
    for with_names in [true, false]
        optimizer = optimizer_constructor(with_bridges=true, with_names=with_names, T=T)
        @test optimizer isa MOI.Bridges.LazyBridgeOptimizer{MOIU.MockOptimizer{MOIU.UniversalFallback{MOIU.Model{T}}}}
        @test MOI.get(optimizer, MOI.Silent())
        @test MOI.get(optimizer, MOI.RawParameter("a")) == 1
        @test MOI.get(optimizer, MOI.RawParameter("b")) == 2
    end

    optimizer_constructor = MOI.OptimizerWithAttributes(DummyOptimizer, [])
    optimizer = optimizer_constructor()
    @test optimizer isa DummyOptimizer
    for with_names in [true, false]
        optimizer = optimizer_constructor(with_bridges=true, with_names=with_names, T=T)
        @test optimizer isa MOI.Bridges.LazyBridgeOptimizer{MOIU.CachingOptimizer{DummyOptimizer, MOIU.UniversalFallback{MOIU.Model{T}}}}
    end

    err = ErrorException("The provided `optimizer_constructor` returned a non-empty optimizer.")
    function g()
        model = f()
        MOI.add_variable(model)
        return model
    end
    @test_throws err MOI.instantiate(g)
    @test_throws err MOI.instantiate_with_bridges(g)
    optimizer_constructor = MOI.OptimizerWithAttributes(g)
    @test_throws err optimizer_constructor()
    @test_throws err optimizer_constructor(with_bridges=true)

    err = ErrorException(MOI._instantiate_not_callable_message)
    @test_throws err MOI.OptimizerWithAttributes(1)
    @test_throws err MOI.OptimizerWithAttributes(1, MOI.Silent() => true)
    @test_throws err MOI.instantiate(1)
    @test_throws err MOI.instantiate_with_bridges(1)

    err = ErrorException("The provided `optimizer_constructor` returned an object of type " *
        "$Int. Expected a MathOptInterface.AbstractOptimizer.")
    h() = 1
    @test_throws err MOI.instantiate(h)
    @test_throws err MOI.instantiate_with_bridges(h)
    optimizer_constructor = MOI.OptimizerWithAttributes(h)
    @test_throws err optimizer_constructor()
    @test_throws err optimizer_constructor(with_bridges=true)
end
