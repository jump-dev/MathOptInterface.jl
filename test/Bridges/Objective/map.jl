using Test
using MathOptInterface
const MOI = MathOptInterface
const MOIB = MOI.Bridges

struct ObjectiveDummyBridge <: MOIB.Objective.AbstractBridge
    id::Int
end

function test_empty(map)
    @test isempty(map)
    @test length(map) == 0
    @test isempty(values(map))
    @test iterate(map) === nothing
    @test MOIB.Objective.function_type(map) === nothing
end

map= MOIB.Objective.Map()
test_empty(map)

empty!(map)
test_empty(map)

@testset "EmptyMap" begin
    map = MOIB.Objective.EmptyMap()
    test_empty(map)
    empty!(map)
    test_empty(map)
end
