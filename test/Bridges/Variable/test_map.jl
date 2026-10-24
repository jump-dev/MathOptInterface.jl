# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestVariableMap

using Test

import MathOptInterface as MOI

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

struct VariableDummyBridge <: MOI.Bridges.Variable.AbstractBridge
    id::Int
end

function MOI.Bridges.Variable.unbridged_map(
    ::VariableDummyBridge,
    ::MOI.VariableIndex,
)
    return nothing
end

function test_empty()
    map = MOI.Bridges.Variable.Map()
    @test isempty(map)
    @test !MOI.Bridges.Variable.has_bridges(map)
    @test isempty(keys(map))
    return
end

"""
    test_map()

TODO(odow): this test is too intertwined. It should be refactored into something
simpler.
"""
function test_map()
    map = MOI.Bridges.Variable.Map()
    b1 = VariableDummyBridge(1)
    set1 = MOI.EqualTo(0.0)
    F1 = MOI.VariableIndex
    S1 = typeof(set1)
    v1, c1 = MOI.Bridges.Variable.add_key_for_bridge(map, () -> b1, set1)
    MOI.is_valid(map, c1)
    cannot_unbridge_err = MOI.GetAttributeNotAllowed(
        MOI.ConstraintFunction(),
        "Cannot unbridge function because some variables are bridged by variable" *
        " bridges that do not support reverse mapping, for example, `ZerosBridge`.",
    )
    @test v1.value == c1.value == 1
    @test MOI.Bridges.Variable.constraint(map, v1) == c1
    @test MOI.Bridges.Variable.length_of_vector_of_variables(map, v1) == 0
    @test haskey(map, v1)
    @test map[v1] == b1
    @test MOI.Bridges.Variable.constrained_set(map, v1) == S1
    @test MOI.Bridges.Variable.number_with_set(map, S1) == 1
    @test MOI.Bridges.Variable.constraints_with_set(map, S1) == [c1]
    @test MOI.Bridges.Variable.function_for(map, c1) == v1
    @test_throws cannot_unbridge_err MOI.Bridges.Variable.unbridged_function(
        map,
        MOI.VariableIndex(1),
    )

    @test MOI.Bridges.Variable.number_of_variables(map) == 1
    @test MOI.Bridges.Variable.list_of_constraint_types(map) == Set([(F1, S1)])
    @test length(map) == 1
    @test !isempty(map)
    @test MOI.Bridges.Variable.has_bridges(map)
    @test collect(keys(map)) == [v1]
    @test collect(values(map)) == [b1]

    b2 = VariableDummyBridge(2)
    set2 = MOI.Zeros(4)
    F2 = MOI.VectorOfVariables
    S2 = typeof(set2)
    v2, c2 = MOI.Bridges.Variable.add_keys_for_bridge(map, () -> b2, set2)
    MOI.is_valid(map, c2)
    @test v2[1].value == 2
    @test c2.value == 1
    @test MOI.Bridges.Variable.has_keys(map, v2)
    @test !MOI.Bridges.Variable.has_keys(map, v2[4:-1:1])
    for i in 1:4
        @test MOI.Bridges.Variable.constraint(map, v2[i]) == c2
        @test haskey(map, v2[i])
        @test map[v2[i]] == b2
        @test MOI.Bridges.Variable.constrained_set(map, v2[i]) == S2
        @test MOI.Bridges.Variable.length_of_vector_of_variables(map, v2[i]) ==
              4
        @test MOI.Bridges.Variable.index_in_vector_of_variables(map, v2[i]) ==
              MOI.Bridges.IndexInVector(i)
    end
    @test MOI.Bridges.Variable.number_with_set(map, S2) == 1
    @test MOI.Bridges.Variable.constraints_with_set(map, S2) == [c2]
    @test MOI.Bridges.Variable.function_for(map, c2) ==
          MOI.VectorOfVariables(v2)
    @test_throws cannot_unbridge_err MOI.Bridges.Variable.unbridged_function(
        map,
        MOI.VariableIndex(1),
    )

    @test MOI.Bridges.Variable.number_of_variables(map) == 5
    @test MOI.Bridges.Variable.list_of_constraint_types(map) ==
          Set([(F1, S1), (F2, S2)])
    @test length(map) == 2
    @test !isempty(map)
    @test MOI.Bridges.Variable.has_bridges(map)
    @test collect(keys(map)) == [v1; v2]
    @test collect(values(map)) == [b1, b2]

    b3 = VariableDummyBridge(3)
    set3 = MOI.Zeros(0)
    v3, c3 = MOI.Bridges.Variable.add_keys_for_bridge(map, () -> b3, set3)

    @test isempty(v3)
    @test c3.value == 0
    bridges = collect(values(map))
    @test sort([b.id for b in bridges]) == 1:2
    elements = sort(collect(map), by = el -> el.second.id)
    @test elements[1].first == v1
    @test elements[1].second == b1
    @test elements[2].first == v2[1]
    @test elements[2].second == b2

    delete!(map, v1)
    @test MOI.Bridges.Variable.number_of_variables(map) == 4
    @test length(map) == 1
    @test !isempty(map)
    @test MOI.Bridges.Variable.has_bridges(map)
    elements = collect(map)
    @test elements[1].first == v2[1]
    @test elements[1].second == b2
    @test MOI.Bridges.Variable.number_with_set(map, S1) == 0
    @test MOI.Bridges.Variable.number_with_set(map, S2) == 1
    @test isempty(MOI.Bridges.Variable.constraints_with_set(map, S1))
    @test MOI.Bridges.Variable.constraints_with_set(map, S2) == [c2]
    @test MOI.Bridges.Variable.list_of_constraint_types(map) == Set([(F2, S2)])
    @test collect(keys(map)) == v2
    @test collect(values(map)) == [b2]
    @test !haskey(map, v1)
    @test MOI.Bridges.Variable.has_keys(map, v2)

    rev_v2 = reverse(v2)
    err = ArgumentError(
        "`$rev_v2` is not a valid key vector as returned by `add_keys_for_bridge`.",
    )
    @test_throws err delete!(map, rev_v2)

    delete!(map, v2[3])
    left = [1, 2, 4]
    @test MOI.Bridges.Variable.number_of_variables(map) == 3
    @test length(map) == 1
    @test !isempty(map)
    @test MOI.Bridges.Variable.has_bridges(map)
    elements = collect(map)
    @test elements[1].first == v2[1]
    @test elements[1].second == b2
    @test MOI.Bridges.Variable.number_with_set(map, S1) == 0
    @test MOI.Bridges.Variable.number_with_set(map, S2) == 1
    @test isempty(MOI.Bridges.Variable.constraints_with_set(map, S1))
    @test MOI.Bridges.Variable.constraints_with_set(map, S2) == [c2]
    @test MOI.Bridges.Variable.list_of_constraint_types(map) == Set([(F2, S2)])
    @test collect(keys(map)) == v2[left]
    @test collect(values(map)) == [b2]
    @test !haskey(map, v1)
    @test !haskey(map, v2[3])
    @test !MOI.Bridges.Variable.has_keys(map, v2)
    @test MOI.Bridges.Variable.has_keys(map, v2[left])
    for (j, i) in enumerate(left)
        @test MOI.Bridges.Variable.constraint(map, v2[i]) == c2
        @test haskey(map, v2[i])
        @test map[v2[i]] == b2
        @test MOI.Bridges.Variable.constrained_set(map, v2[i]) == S2
        @test MOI.Bridges.Variable.length_of_vector_of_variables(map, v2[i]) ==
              3
        @test MOI.Bridges.Variable.index_in_vector_of_variables(map, v2[i]) ==
              MOI.Bridges.IndexInVector(j)
    end
    @test MOI.Bridges.Variable.function_for(map, c2) ==
          MOI.VectorOfVariables(v2[left])

    delete!(map, v2[1])
    left = [2, 4]
    @test MOI.Bridges.Variable.number_of_variables(map) == 2
    @test length(map) == 1
    @test !isempty(map)
    @test MOI.Bridges.Variable.has_bridges(map)
    elements = collect(map)
    @test elements[1].first == v2[1]
    @test elements[1].second == b2
    @test MOI.Bridges.Variable.number_with_set(map, S1) == 0
    @test MOI.Bridges.Variable.number_with_set(map, S2) == 1
    @test isempty(MOI.Bridges.Variable.constraints_with_set(map, S1))
    @test MOI.Bridges.Variable.constraints_with_set(map, S2) == [c2]
    @test MOI.Bridges.Variable.list_of_constraint_types(map) == Set([(F2, S2)])
    @test collect(keys(map)) == v2[left]
    @test collect(values(map)) == [b2]
    @test !haskey(map, v1)
    @test !haskey(map, v2[1])
    @test !haskey(map, v2[3])
    @test !MOI.Bridges.Variable.has_keys(map, v2)
    @test !MOI.Bridges.Variable.has_keys(map, v2[[2, 3, 4]])
    @test !MOI.Bridges.Variable.has_keys(map, v2[[1, 2, 4]])
    @test MOI.Bridges.Variable.has_keys(map, v2[left])
    for (j, i) in enumerate(left)
        @test MOI.Bridges.Variable.constraint(map, v2[i]) == c2
        @test haskey(map, v2[i])
        @test map[v2[i]] == b2
        @test MOI.Bridges.Variable.constrained_set(map, v2[i]) == S2
        @test MOI.Bridges.Variable.length_of_vector_of_variables(map, v2[i]) ==
              2
        @test MOI.Bridges.Variable.index_in_vector_of_variables(map, v2[i]) ==
              MOI.Bridges.IndexInVector(j)
    end
    @test MOI.Bridges.Variable.function_for(map, c2) ==
          MOI.VectorOfVariables(v2[left])

    delete!(map, v2[left])
    @test MOI.Bridges.Variable.number_of_variables(map) == 0
    @test length(map) == 0
    @test isempty(map)
    @test MOI.Bridges.Variable.has_bridges(map)
    @test isempty(collect(map))
    @test MOI.Bridges.Variable.number_with_set(map, S1) == 0
    @test MOI.Bridges.Variable.number_with_set(map, S2) == 0
    @test isempty(MOI.Bridges.Variable.constraints_with_set(map, S1))
    @test isempty(MOI.Bridges.Variable.constraints_with_set(map, S2))
    @test isempty(MOI.Bridges.Variable.list_of_constraint_types(map))
    @test isempty(keys(map))
    @test isempty(values(map))
    @test !haskey(map, v1)
    @test !MOI.Bridges.Variable.has_keys(map, v2)
    @test !MOI.Bridges.Variable.has_keys(map, v2[left])
    return
end

function test_EmptyMap()
    S1 = MOI.EqualTo{Float64}
    S2 = MOI.Zeros
    map = MOI.Bridges.Variable.EmptyMap()
    empty!(map)
    @test isempty(map)
    @test length(map) == 0
    @test isempty(keys(map))
    @test isempty(values(map))
    @test !MOI.Bridges.Variable.has_bridges(map)
    @test iszero(MOI.Bridges.Variable.number_of_variables(map))
    @test iszero(MOI.Bridges.Variable.number_with_set(map, S1))
    @test iszero(MOI.Bridges.Variable.number_with_set(map, S2))
    @test isempty(MOI.Bridges.Variable.constraints_with_set(map, S1))
    @test isempty(MOI.Bridges.Variable.constraints_with_set(map, S2))
    c1 = MOI.ConstraintIndex{MOI.VariableIndex,S1}(1)
    @test !MOI.is_valid(map, c1)
    c2 = MOI.ConstraintIndex{MOI.VectorOfVariables,S2}(1)
    @test !MOI.is_valid(map, c2)
    @test sprint(show, map) == ""
    return
end

function test_double_variable_bound()
    map = MOI.Bridges.Variable.Map()
    b1 = VariableDummyBridge(1)
    set1 = MOI.EqualTo(0.0)
    v1, c1 = MOI.Bridges.Variable.add_key_for_bridge(map, () -> b1, set1)
    MOI.is_valid(map, c1)
    MOI.add_constraint(map, v1, MOI.Integer())
    cint = MOI.ConstraintIndex{typeof(v1),MOI.Integer}(v1.value)
    MOI.delete(map, cint)
    set2 = MOI.LessThan(1.0)
    err = MOI.UpperBoundAlreadySet{typeof(set1),typeof(set2)}(v1)
    @test_throws err MOI.add_constraint(map, v1, set2)
    return
end

function test_index_space()
    V = MOI.Bridges.Variable
    space = V.IndexSpace()
    space.last_identity = 3
    @test space.mapping === nothing
    @test V.outer_to_inner(space, Int64(2), nothing) == 2
    @test V.inner_to_outer(space, Int64(2), nothing) == 2
    @test V.inner_to_outer(space, Int64(4), nothing) === nothing
    @test V.allocate!(space, V.BridgeSlot(1)) == 4
    @test V.allocate!(space, V.InnerIndex(4)) == 5
    @test V.destination(space, Int64(4)) == V.BridgeSlot(1)
    @test V.outer_to_inner(space, Int64(4), nothing) === nothing
    @test V.outer_to_inner(space, Int64(5), nothing) == 4
    @test V.inner_to_outer(space, Int64(4), nothing) == 5
    V.remove_mapping!(space, Int64(5))
    @test V.outer_to_inner(space, Int64(5), nothing) === nothing
    @test V.inner_to_outer(space, Int64(4), nothing) === nothing
    @test V.allocate!(space, nothing; minimum = 8) == 8
    @test V.allocate!(space, V.InnerIndex(5)) == 9
    @test V.destination(space, Int64(7)) === nothing
    other = V.IndexSpace()
    @test V.allocate!(other, V.BridgeSlot(1)) == 1
    @test V.allocate!(other, V.InnerIndex(1)) == 2
    @test something(other.mapping).reverse[V.BridgeSlot(1)] == 1
    @test V.inner_to_outer(other, Int64(1), nothing) == 2
    return
end

function test_inner_to_outer_invalid()
    map = MOI.Bridges.Variable.Map()
    vi = MOI.VariableIndex(1)
    @test_throws MOI.InvalidIndex(vi) MOI.Bridges.Variable.inner_to_outer(
        map,
        vi,
    )
    return
end

function test_function_for_deleted_variable_in_vector()
    map = MOI.Bridges.Variable.Map()
    # The scalar bridge disables unbridging, see `test_map`
    MOI.Bridges.Variable.add_key_for_bridge(
        map,
        () -> VariableDummyBridge(1),
        MOI.EqualTo(0.0),
    )
    vis, ci = MOI.Bridges.Variable.add_keys_for_bridge(
        map,
        () -> VariableDummyBridge(2),
        MOI.Nonnegatives(3),
    )
    delete!(map, vis[2])
    @test MOI.Bridges.Variable.function_for(map, ci) ==
          MOI.VectorOfVariables(vis[[1, 3]])
    return
end

end  # module

TestVariableMap.runtests()
