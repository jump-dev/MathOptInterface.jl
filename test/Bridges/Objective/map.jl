# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

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
    @test sprint(show, map) == "\nwith 0 objective bridges"
    x = MOI.VariableIndex(1)
    MOI.Bridges.Objective.add_key_for_bridge(map, ObjectiveDummyBridge(1), x)
    @test MOI.Bridges.Objective.root_bridge(map) == ObjectiveDummyBridge(1)
    @test sprint(show, map) == "\nwith 1 objective bridge"
    func = 1.0x
    MOI.Bridges.Objective.add_key_for_bridge(map, ObjectiveDummyBridge(2), func)
    @test MOI.Bridges.Objective.root_bridge(map) == ObjectiveDummyBridge(2)
    @test sprint(show, map) == "\nwith 2 objective bridges"
    empty!(map)
    _test_empty(map)
    @test sprint(show, map) == "\nwith 0 objective bridges"
    return
end

function test_EmptyMap()
    map = MOI.Bridges.Objective.EmptyMap()
    _test_empty(map)
    empty!(map)
    _test_empty(map)
    @test sprint(show, map) == ""
    return
end

end  # module

TestObjectiveMap.runtests()
