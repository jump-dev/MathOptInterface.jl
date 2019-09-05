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

map = MOIB.Objective.Map()
test_empty(map)
@test sprint(MOIB.print_num_bridges, map) == "\nwith 0 objective bridges"

x = MOI.VariableIndex(1)
fx = MOI.SingleVariable(x)
MOIB.Objective.add_key_for_bridge(map, ObjectiveDummyBridge(1), fx)
@test MOIB.Objective.root_bridge(map) == ObjectiveDummyBridge(1)
@test sprint(MOIB.print_num_bridges, map) == "\nwith 1 objective bridge"
func = 1.0fx
MOIB.Objective.add_key_for_bridge(map, ObjectiveDummyBridge(2), func)
@test MOIB.Objective.root_bridge(map) == ObjectiveDummyBridge(2)
@test sprint(MOIB.print_num_bridges, map) == "\nwith 2 objective bridges"

empty!(map)
test_empty(map)
@test sprint(MOIB.print_num_bridges, map) == "\nwith 0 objective bridges"

@testset "EmptyMap" begin
    map = MOIB.Objective.EmptyMap()
    test_empty(map)
    empty!(map)
    test_empty(map)
    @test sprint(MOIB.print_num_bridges, map) == ""
end
