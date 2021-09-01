module TestConstraintMap

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

struct ConstraintDummyBridge <: MOI.Bridges.Constraint.AbstractBridge
    id::Int
end

function test_map()
    map = MOI.Bridges.Constraint.Map()

    @test isempty(map)
    @test !MOI.Bridges.Constraint.has_bridges(map)

    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    b1 = ConstraintDummyBridge(1)
    c1 = MOI.Bridges.Constraint.add_key_for_bridge(map, b1, x, MOI.EqualTo(0.0))

    @test c1.value == x.value
    @test haskey(map, c1)
    @test map[c1] == b1
    @test MOI.Bridges.Constraint.number_of_type(map, typeof(c1)) == 1
    @test collect(MOI.Bridges.Constraint.keys_of_type(map, typeof(c1))) == [c1]

    @test length(map) == 1
    @test !isempty(map)
    @test MOI.Bridges.Constraint.has_bridges(map)

    b2 = ConstraintDummyBridge(2)
    vov = MOI.VectorOfVariables([x, y])
    c2 = MOI.Bridges.Constraint.add_key_for_bridge(
        map,
        b2,
        vov,
        MOI.SecondOrderCone(2),
    )
    @test c2.value == -1
    @test haskey(map, c2)
    @test map[c2] == b2
    @test MOI.Bridges.Constraint.number_of_type(map, typeof(c2)) == 1
    @test collect(MOI.Bridges.Constraint.keys_of_type(map, typeof(c2))) == [c2]

    @test length(map) == 2
    @test !isempty(map)
    @test MOI.Bridges.Constraint.has_bridges(map)

    b3 = ConstraintDummyBridge(3)
    c3 = MOI.Bridges.Constraint.add_key_for_bridge(
        map,
        b3,
        1.0x + 2.0,
        MOI.EqualTo(0.0),
    )
    @test haskey(map, c3)
    @test map[c3] == b3
    @test MOI.Bridges.Constraint.number_of_type(map, typeof(c3)) == 1
    @test collect(MOI.Bridges.Constraint.keys_of_type(map, typeof(c3))) == [c3]

    @test length(map) == 3
    @test !isempty(map)
    @test MOI.Bridges.Constraint.has_bridges(map)

    bridges = collect(values(map))
    @test sort([b.id for b in bridges]) == 1:3
    elements = sort(collect(map), by = el -> el.second.id)
    @test elements[1].first == c1
    @test elements[1].second == b1
    @test elements[2].first == c2
    @test elements[2].second == b2
    @test elements[3].first == c3
    @test elements[3].second == b3

    @test MOI.Bridges.Constraint.list_of_key_types(map) == Set([
        (MOI.VectorOfVariables, MOI.SecondOrderCone),
        (MOI.VariableIndex, MOI.EqualTo{Float64}),
        (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}),
    ])
    @test MOI.Bridges.Constraint.variable_constraints(map, x) == [c1]
    @test isempty(MOI.Bridges.Constraint.variable_constraints(map, y))

    delete!(map, c1)
    @test length(map) == 2
    @test sprint(MOI.Bridges.print_num_bridges, map) ==
          "\nwith 2 constraint bridges"
    @test !isempty(map)
    @test MOI.Bridges.Constraint.has_bridges(map)

    @test MOI.Bridges.Constraint.list_of_key_types(map) == Set([
        (MOI.VectorOfVariables, MOI.SecondOrderCone),
        (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}),
    ])
    @test isempty(MOI.Bridges.Constraint.variable_constraints(map, x))
    @test isempty(MOI.Bridges.Constraint.variable_constraints(map, y))
    @test MOI.Bridges.Constraint.number_of_type(map, typeof(c1)) == 0
    @test MOI.Bridges.Constraint.number_of_type(map, typeof(c2)) == 1
    @test MOI.Bridges.Constraint.number_of_type(map, typeof(c3)) == 1

    delete!(map, c2)
    @test length(map) == 1
    @test sprint(MOI.Bridges.print_num_bridges, map) ==
          "\nwith 1 constraint bridge"
    @test !isempty(map)
    @test MOI.Bridges.Constraint.has_bridges(map)

    @test MOI.Bridges.Constraint.list_of_key_types(map) ==
          Set([(MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})])
    @test MOI.Bridges.Constraint.number_of_type(map, typeof(c1)) == 0
    @test MOI.Bridges.Constraint.number_of_type(map, typeof(c2)) == 0
    @test MOI.Bridges.Constraint.number_of_type(map, typeof(c3)) == 1

    delete!(map, c3)
    @test length(map) == 0
    @test isempty(map)
    @test MOI.Bridges.Constraint.has_bridges(map)

    @test isempty(MOI.Bridges.Constraint.list_of_key_types(map))
    @test MOI.Bridges.Constraint.number_of_type(map, typeof(c1)) == 0
    @test MOI.Bridges.Constraint.number_of_type(map, typeof(c2)) == 0
    @test MOI.Bridges.Constraint.number_of_type(map, typeof(c3)) == 0

    @test isempty(collect(map))

    map = MOI.Bridges.Constraint.EmptyMap()
    empty!(map)
    @test isempty(map)
    @test isempty(keys(map))
    @test isempty(values(map))
    @test !MOI.Bridges.Constraint.has_bridges(map)
    @test iszero(MOI.Bridges.Constraint.number_of_type(map, typeof(c1)))
    @test iszero(MOI.Bridges.Constraint.number_of_type(map, typeof(c2)))
    @test iszero(MOI.Bridges.Constraint.number_of_type(map, typeof(c3)))
    @test sprint(MOI.Bridges.print_num_bridges, map) == ""
end

end  # module

TestConstraintMap.runtests()
