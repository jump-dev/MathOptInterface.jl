module TestProductOfSets

using Test

import MathOptInterface
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

MOI.Utilities.@mix_of_scalar_sets(
    _ScalarSets,
    MOI.EqualTo{T},
    MOI.GreaterThan{T},
    MOI.LessThan{T},
    MOI.Interval{T},
)

function test_scalar_set_index()
    sets = _ScalarSets{Float64}()
    @test MOI.Utilities.set_index(sets, MOI.EqualTo{Float64}) == 1
    @test MOI.Utilities.set_index(sets, MOI.GreaterThan{Float64}) == 2
    @test MOI.Utilities.set_index(sets, MOI.Interval{Float64}) == 4
    @test MOI.Utilities.set_index(sets, MOI.LessThan{Float64}) == 3
    @test MOI.Utilities.set_index(sets, MOI.ZeroOne) === nothing
end

function test_scalar_set_types()
    sets = _ScalarSets{Float64}()
    @test MOI.Utilities.set_types(sets) == [
        MOI.EqualTo{Float64},
        MOI.GreaterThan{Float64},
        MOI.LessThan{Float64},
        MOI.Interval{Float64},
    ]
end

function test_scalar_basic()
    sets = _ScalarSets{Float64}()
    ci = MOI.ConstraintIndex{
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    }(
        12345,
    )
    @test !MOI.is_valid(sets, ci)
    i = MOI.Utilities.set_index(sets, MOI.EqualTo{Float64})
    ci_value = MOI.Utilities.add_set(sets, i)
    ci = MOI.ConstraintIndex{
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    }(
        ci_value,
    )
    @test MOI.is_valid(sets, ci)
    @test MOI.Utilities.indices(sets, ci) == ci.value
end

function test_scalar_dimension()
    sets = _ScalarSets{Float64}()
    @test MOI.dimension(sets) == 0
    MOI.Utilities.add_set(sets, 1)
    @test MOI.dimension(sets) == 1
    MOI.Utilities.add_set(sets, 1)
    @test MOI.dimension(sets) == 2
    MOI.Utilities.add_set(sets, 2)
    @test MOI.dimension(sets) == 3
end

function test_scalar_empty()
    sets = _ScalarSets{Float64}()
    @test MOI.is_empty(sets)
    MOI.Utilities.add_set(sets, 1)
    @test !MOI.is_empty(sets)
    MOI.empty!(sets)
    @test MOI.is_empty(sets)
end

function test_scalar_ConstraintTypesPresent()
    sets = _ScalarSets{Float64}()
    @test MOI.get(sets, MOI.ListOfConstraintTypesPresent()) == []
    MOI.Utilities.add_set(sets, 1)
    @test MOI.get(sets, MOI.ListOfConstraintTypesPresent()) ==
          [(MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})]
end

function test_scalar_NumberOfConstraints()
    sets = _ScalarSets{Float64}()
    MOI.Utilities.add_set(sets, 1)
    MOI.Utilities.add_set(sets, 1)
    MOI.Utilities.add_set(sets, 2)
    @test MOI.get(
        sets,
        MOI.NumberOfConstraints{MOI.SingleVariable,MOI.ZeroOne}(),
    ) == 0
    for (x, S) in zip([2, 1, 0, 0], MOI.Utilities.set_types(sets))
        @test MOI.get(
            sets,
            MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},S}(),
        ) == x
    end
    return
end

function test_scalar_ListOfConstraintIndices()
    sets = _ScalarSets{Float64}()
    MOI.Utilities.add_set(sets, 2)
    MOI.Utilities.add_set(sets, 4)
    MOI.Utilities.add_set(sets, 1)
    for (x, S) in zip([[3], [1], [], [2]], MOI.Utilities.set_types(sets))
        ci = MOI.get(
            sets,
            MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64},S}(),
        )
        @test ci == MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},S}.(x)
    end
    return
end

MOI.Utilities.@product_of_sets(
    _VectorSets,
    MOI.Nonpositives,
    MOI.Nonnegatives,
    MOI.EqualTo{T},
)

function test_vector_set_index()
    sets = _VectorSets{Float64}()
    @test MOI.Utilities.set_index(sets, MOI.Nonpositives) == 1
    @test MOI.Utilities.set_index(sets, MOI.Nonnegatives) == 2
    @test MOI.Utilities.set_index(sets, MOI.EqualTo{Float64}) == 3
    @test MOI.Utilities.set_index(sets, MOI.LessThan{Float64}) === nothing
end

function test_vector_set_types()
    sets = _VectorSets{Float64}()
    @test MOI.Utilities.set_types(sets) ==
          [MOI.Nonpositives, MOI.Nonnegatives, MOI.EqualTo{Float64}]
end

function test_vector_basic()
    sets = _VectorSets{Float64}()
    ci =
        MOI.ConstraintIndex{MOI.VectorAffineFunction{Float64},MOI.Nonnegatives}(
            12345,
        )
    @test !MOI.is_valid(sets, ci)
    i = MOI.Utilities.set_index(sets, MOI.Nonnegatives)
    ci_value = MOI.Utilities.add_set(sets, i, 2)
    @test ci_value == 0
    ci =
        MOI.ConstraintIndex{MOI.VectorAffineFunction{Float64},MOI.Nonnegatives}(
            ci_value,
        )
    @test MOI.is_valid(sets, ci)
    @test MOI.Utilities.indices(sets, ci) == 1:2

    i = MOI.Utilities.set_index(sets, MOI.EqualTo{Float64})
    ci_value = MOI.Utilities.add_set(sets, i)
    @test ci_value == 0
    ci = MOI.ConstraintIndex{
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    }(
        ci_value,
    )
    @test MOI.is_valid(sets, ci)
    @test MOI.Utilities.indices(sets, ci) == 3
end

function test_vector_dimension()
    sets = _VectorSets{Float64}()
    @test MOI.dimension(sets) == 0
    MOI.Utilities.add_set(sets, 1, 1)
    @test MOI.dimension(sets) == 1
    MOI.Utilities.add_set(sets, 1, 2)
    @test MOI.dimension(sets) == 3
    MOI.Utilities.add_set(sets, 2, 3)
    @test MOI.dimension(sets) == 6
    MOI.Utilities.add_set(sets, 3)
    @test MOI.dimension(sets) == 7
end

function test_vector_empty()
    sets = _VectorSets{Float64}()
    @test MOI.is_empty(sets)
    MOI.Utilities.add_set(sets, 1, 1)
    @test !MOI.is_empty(sets)
    MOI.empty!(sets)
    @test MOI.is_empty(sets)
end

function test_vector_ConstraintTypesPresent()
    sets = _VectorSets{Float64}()
    @test MOI.get(sets, MOI.ListOfConstraintTypesPresent()) == []
    MOI.Utilities.add_set(sets, 1, 1)
    MOI.Utilities.add_set(sets, 3)
    @test MOI.get(sets, MOI.ListOfConstraintTypesPresent()) == [
        (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives),
        (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}),
    ]
end

function test_vector_NumberOfConstraints()
    sets = _VectorSets{Float64}()
    MOI.Utilities.add_set(sets, 1, 2)
    MOI.Utilities.add_set(sets, 1, 2)
    MOI.Utilities.add_set(sets, 2, 2)
    MOI.Utilities.add_set(sets, 3)
    @test MOI.get(
        sets,
        MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}(),
    ) == 0
    for (x, S) in zip([2, 1], MOI.Utilities.set_types(sets)[1:2])
        @test MOI.get(
            sets,
            MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},S}(),
        ) == x
    end
    @test MOI.get(
        sets,
        MOI.NumberOfConstraints{
            MOI.ScalarAffineFunction{Float64},
            MOI.EqualTo{Float64},
        }(),
    ) == 1
    return
end

function test_vector_ListOfConstraintIndices()
    sets = _VectorSets{Float64}()
    MOI.Utilities.add_set(sets, 2, 2)
    MOI.Utilities.add_set(sets, 1, 4)
    MOI.Utilities.add_set(sets, 2, 3)
    MOI.Utilities.add_set(sets, 3)
    for (x, S) in zip([[0], [0, 2]], MOI.Utilities.set_types(sets)[1:2])
        ci = MOI.get(
            sets,
            MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64},S}(),
        )
        @test ci == MOI.ConstraintIndex{MOI.VectorAffineFunction{Float64},S}.(x)
    end
    F = MOI.ScalarAffineFunction{Float64}
    S = MOI.EqualTo{Float64}
    @test MOI.get(sets, MOI.ListOfConstraintIndices{F,S}()) ==
          [MOI.ConstraintIndex{F,S}(0)]
    return
end

end

TestProductOfSets.runtests()
