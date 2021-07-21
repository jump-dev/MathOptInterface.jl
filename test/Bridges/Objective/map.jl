module TestObjectiveMap

using Test

using MathOptInterface
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

struct ObjectiveDummyBridge <: MOI.Bridges.Objective.AbstractBridge
    id::Int
end

function _test_empty(map)
    @test isempty(map)
    @test length(map) == 0
    @test isempty(values(map))
    @test iterate(map) === nothing
    @test MOI.Bridges.Objective.function_type(map) === nothing
end

function test_Map()
    map = MOI.Bridges.Objective.Map()
    _test_empty(map)
    @test sprint(MOI.Bridges.print_num_bridges, map) ==
          "\nwith 0 objective bridges"
    x = MOI.VariableIndex(1)
    fx = MOI.SingleVariable(x)
    MOI.Bridges.Objective.add_key_for_bridge(map, ObjectiveDummyBridge(1), fx)
    @test MOI.Bridges.Objective.root_bridge(map) == ObjectiveDummyBridge(1)
    @test sprint(MOI.Bridges.print_num_bridges, map) ==
          "\nwith 1 objective bridge"
    func = 1.0fx
    MOI.Bridges.Objective.add_key_for_bridge(map, ObjectiveDummyBridge(2), func)
    @test MOI.Bridges.Objective.root_bridge(map) == ObjectiveDummyBridge(2)
    @test sprint(MOI.Bridges.print_num_bridges, map) ==
          "\nwith 2 objective bridges"
    empty!(map)
    _test_empty(map)
    @test sprint(MOI.Bridges.print_num_bridges, map) ==
          "\nwith 0 objective bridges"
    return
end

function test_EmptyMap()
    map = MOI.Bridges.Objective.EmptyMap()
    _test_empty(map)
    empty!(map)
    _test_empty(map)
    @test sprint(MOI.Bridges.print_num_bridges, map) == ""
    return
end

end  # module

TestObjectiveMap.runtests()
