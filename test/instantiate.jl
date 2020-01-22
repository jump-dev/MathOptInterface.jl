using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIU = MathOptInterface.Utilities

struct DummyOptimizer <: MOI.AbstractOptimizer end
MOI.is_empty(::DummyOptimizer) = true

@testset "Instantiate with $T" for T in [Float64, Int]
    f() = MOIU.UniversalFallback(MOIU.Model{T}())
    uninstantiated = MOI.UninstantiatedOptimizer(f, [MOI.Silent() => true])
    optimizer = MOI.instantiate(uninstantiated)
    @test optimizer isa MOIU.UniversalFallback{MOIU.Model{T}}
    @test MOI.get(optimizer, MOI.Silent())
    for with_names in [true, false]
        optimizer = MOI.instantiate_with_bridges(uninstantiated, with_names, T)
        @test optimizer isa MOI.Bridges.LazyBridgeOptimizer{MOIU.UniversalFallback{MOIU.Model{T}}}
        @test MOI.get(optimizer, MOI.Silent())
    end
    uninstantiated = MOI.UninstantiatedOptimizer(DummyOptimizer, [])
    optimizer = MOI.instantiate(uninstantiated)
    @test optimizer isa DummyOptimizer
    for with_names in [true, false]
        optimizer = MOI.instantiate_with_bridges(uninstantiated, with_names, T)
        @test optimizer isa MOI.Bridges.LazyBridgeOptimizer{MOIU.CachingOptimizer{DummyOptimizer, MOIU.UniversalFallback{MOIU.Model{T}}}}
    end
end
