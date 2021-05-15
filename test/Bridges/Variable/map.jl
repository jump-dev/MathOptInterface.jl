using Test
using MathOptInterface
const MOI = MathOptInterface
const MOIB = MOI.Bridges

struct VariableDummyBridge <: MOIB.Variable.AbstractBridge
    id::Int
end
function MOIB.Variable.unbridged_map(::VariableDummyBridge, ::MOI.VariableIndex)
    return nothing
end

map = MOIB.Variable.Map()
@testset "Empty" begin
    @test isempty(map)
    @test !MOIB.Variable.has_bridges(map)
    @test isempty(keys(map))
end

b1 = VariableDummyBridge(1)
set1 = MOI.EqualTo(0.0)
F1 = MOI.SingleVariable
S1 = typeof(set1)
v1, c1 = MOIB.Variable.add_key_for_bridge(map, () -> b1, set1)
cannot_unbridge_err = ErrorException(
    "Cannot unbridge function because some variables are bridged by variable" *
    " bridges that do not support reverse mapping, e.g., `ZerosBridge`.",
)
@testset "Scalar set" begin
    @test v1.value == c1.value == -1
    @test MOIB.Variable.constraint(map, v1) == c1
    @test haskey(map, v1)
    @test map[v1] == b1
    @test MOIB.Variable.constrained_set(map, v1) == S1
    @test MOIB.Variable.number_with_set(map, S1) == 1
    @test MOIB.Variable.constraints_with_set(map, S1) == [c1]
    @test MOIB.Variable.function_for(map, c1) == MOI.SingleVariable(v1)
    @test_throws cannot_unbridge_err MOIB.Variable.unbridged_function(
        map,
        MOI.VariableIndex(1),
    )

    @test MOIB.Variable.number_of_variables(map) == 1
    @test MOIB.Variable.list_of_constraint_types(map) == Set([(F1, S1)])
    @test length(map) == 1
    @test !isempty(map)
    @test MOIB.Variable.has_bridges(map)
    @test collect(keys(map)) == [v1]
    @test collect(values(map)) == [b1]
end

b2 = VariableDummyBridge(2)
set2 = MOI.Zeros(4)
F2 = MOI.VectorOfVariables
S2 = typeof(set2)
v2, c2 = MOIB.Variable.add_keys_for_bridge(map, () -> b2, set2)
@testset "Vector set" begin
    @test v2[1].value == c2.value == -2
    @test MOIB.Variable.has_keys(map, v2)
    @test !MOIB.Variable.has_keys(map, v2[4:-1:1])
    for i in 1:4
        @test MOIB.Variable.constraint(map, v2[i]) == c2
        @test haskey(map, v2[i])
        @test map[v2[i]] == b2
        @test MOIB.Variable.constrained_set(map, v2[i]) == S2
        @test MOIB.Variable.length_of_vector_of_variables(map, v2[i]) == 4
        @test MOIB.Variable.index_in_vector_of_variables(map, v2[i]) ==
              MOIB.IndexInVector(i)
    end
    @test MOIB.Variable.number_with_set(map, S2) == 1
    @test MOIB.Variable.constraints_with_set(map, S2) == [c2]
    @test MOIB.Variable.function_for(map, c2) == MOI.VectorOfVariables(v2)
    @test_throws cannot_unbridge_err MOIB.Variable.unbridged_function(
        map,
        MOI.VariableIndex(1),
    )

    @test MOIB.Variable.number_of_variables(map) == 5
    @test MOIB.Variable.list_of_constraint_types(map) ==
          Set([(F1, S1), (F2, S2)])
    @test length(map) == 2
    @test sprint(MOIB.print_num_bridges, map) == "\nwith 2 variable bridges"
    @test !isempty(map)
    @test MOIB.Variable.has_bridges(map)
    @test collect(keys(map)) == [v1; v2]
    @test collect(values(map)) == [b1, b2]
end

b3 = VariableDummyBridge(3)
set3 = MOI.Zeros(0)
v3, c3 = MOIB.Variable.add_keys_for_bridge(map, () -> b3, set3)
@testset "Vector set of length 0" begin
    @test isempty(v3)
    @test c3.value == 0

    bridges = collect(values(map))
    @test sort([b.id for b in bridges]) == 1:2
    elements = sort(collect(map), by = el -> el.second.id)
    @test elements[1].first == v1
    @test elements[1].second == b1
    @test elements[2].first == v2[1]
    @test elements[2].second == b2
end

@testset "Delete" begin
    delete!(map, v1)
    @test MOIB.Variable.number_of_variables(map) == 4
    @test length(map) == 1
    @test sprint(MOIB.print_num_bridges, map) == "\nwith 1 variable bridge"
    @test !isempty(map)
    @test MOIB.Variable.has_bridges(map)
    elements = collect(map)
    @test elements[1].first == v2[1]
    @test elements[1].second == b2
    @test MOIB.Variable.number_with_set(map, S1) == 0
    @test MOIB.Variable.number_with_set(map, S2) == 1
    @test isempty(MOIB.Variable.constraints_with_set(map, S1))
    @test MOIB.Variable.constraints_with_set(map, S2) == [c2]
    @test MOIB.Variable.list_of_constraint_types(map) == Set([(F2, S2)])
    @test collect(keys(map)) == v2
    @test collect(values(map)) == [b2]
    @test !haskey(map, v1)
    @test MOIB.Variable.has_keys(map, v2)

    rev_v2 = reverse(v2)
    err = ArgumentError(
        "`$rev_v2` is not a valid key vector as returned by `add_keys_for_bridge`.",
    )
    @test_throws err delete!(map, rev_v2)

    delete!(map, v2[3])
    left = [1, 2, 4]
    @test MOIB.Variable.number_of_variables(map) == 3
    @test length(map) == 1
    @test !isempty(map)
    @test MOIB.Variable.has_bridges(map)
    elements = collect(map)
    @test elements[1].first == v2[1]
    @test elements[1].second == b2
    @test MOIB.Variable.number_with_set(map, S1) == 0
    @test MOIB.Variable.number_with_set(map, S2) == 1
    @test isempty(MOIB.Variable.constraints_with_set(map, S1))
    @test MOIB.Variable.constraints_with_set(map, S2) == [c2]
    @test MOIB.Variable.list_of_constraint_types(map) == Set([(F2, S2)])
    @test collect(keys(map)) == v2[left]
    @test collect(values(map)) == [b2]
    @test !haskey(map, v1)
    @test !haskey(map, v2[3])
    @test !MOIB.Variable.has_keys(map, v2)
    @test MOIB.Variable.has_keys(map, v2[left])
    for (j, i) in enumerate(left)
        @test MOIB.Variable.constraint(map, v2[i]) == c2
        @test haskey(map, v2[i])
        @test map[v2[i]] == b2
        @test MOIB.Variable.constrained_set(map, v2[i]) == S2
        @test MOIB.Variable.length_of_vector_of_variables(map, v2[i]) == 3
        @test MOIB.Variable.index_in_vector_of_variables(map, v2[i]) ==
              MOIB.IndexInVector(j)
    end
    @test MOIB.Variable.function_for(map, c2) == MOI.VectorOfVariables(v2[left])

    delete!(map, v2[1])
    left = [2, 4]
    @test MOIB.Variable.number_of_variables(map) == 2
    @test length(map) == 1
    @test !isempty(map)
    @test MOIB.Variable.has_bridges(map)
    elements = collect(map)
    @test elements[1].first == v2[1]
    @test elements[1].second == b2
    @test MOIB.Variable.number_with_set(map, S1) == 0
    @test MOIB.Variable.number_with_set(map, S2) == 1
    @test isempty(MOIB.Variable.constraints_with_set(map, S1))
    @test MOIB.Variable.constraints_with_set(map, S2) == [c2]
    @test MOIB.Variable.list_of_constraint_types(map) == Set([(F2, S2)])
    @test collect(keys(map)) == v2[left]
    @test collect(values(map)) == [b2]
    @test !haskey(map, v1)
    @test !haskey(map, v2[1])
    @test !haskey(map, v2[3])
    @test !MOIB.Variable.has_keys(map, v2)
    @test !MOIB.Variable.has_keys(map, v2[[2, 3, 4]])
    @test !MOIB.Variable.has_keys(map, v2[[1, 2, 4]])
    @test MOIB.Variable.has_keys(map, v2[left])
    for (j, i) in enumerate(left)
        @test MOIB.Variable.constraint(map, v2[i]) == c2
        @test haskey(map, v2[i])
        @test map[v2[i]] == b2
        @test MOIB.Variable.constrained_set(map, v2[i]) == S2
        @test MOIB.Variable.length_of_vector_of_variables(map, v2[i]) == 2
        @test MOIB.Variable.index_in_vector_of_variables(map, v2[i]) ==
              MOIB.IndexInVector(j)
    end
    @test MOIB.Variable.function_for(map, c2) == MOI.VectorOfVariables(v2[left])

    delete!(map, v2[left])
    @test MOIB.Variable.number_of_variables(map) == 0
    @test length(map) == 0
    @test isempty(map)
    @test MOIB.Variable.has_bridges(map)
    @test isempty(collect(map))
    @test MOIB.Variable.number_with_set(map, S1) == 0
    @test MOIB.Variable.number_with_set(map, S2) == 0
    @test isempty(MOIB.Variable.constraints_with_set(map, S1))
    @test isempty(MOIB.Variable.constraints_with_set(map, S2))
    @test isempty(MOIB.Variable.list_of_constraint_types(map))
    @test isempty(keys(map))
    @test isempty(values(map))
    @test !haskey(map, v1)
    @test !MOIB.Variable.has_keys(map, v2)
    @test !MOIB.Variable.has_keys(map, v2[left])
end

@testset "EmptyMap" begin
    map = MOIB.Variable.EmptyMap()
    empty!(map)
    @test isempty(map)
    @test length(map) == 0
    @test isempty(keys(map))
    @test isempty(values(map))
    @test !MOIB.Variable.has_bridges(map)
    @test iszero(MOIB.Variable.number_of_variables(map))
    @test iszero(MOIB.Variable.number_with_set(map, S1))
    @test iszero(MOIB.Variable.number_with_set(map, S2))
    @test isempty(MOIB.Variable.constraints_with_set(map, S1))
    @test isempty(MOIB.Variable.constraints_with_set(map, S2))
    @test sprint(MOIB.print_num_bridges, map) == ""
end
