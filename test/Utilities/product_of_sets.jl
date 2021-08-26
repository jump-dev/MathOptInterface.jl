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
    @test MOI.Utilities.rows(sets, ci) == ci.value
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
    MOI.Utilities.final_touch(sets)
    @test MOI.get(
        sets,
        MOI.NumberOfConstraints{MOI.VariableIndex,MOI.ZeroOne}(),
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
    nonneg_i = MOI.Utilities.add_set(
        sets,
        MOI.Utilities.set_index(sets, MOI.Nonnegatives),
        2,
    )
    equalto_i = MOI.Utilities.add_set(
        sets,
        MOI.Utilities.set_index(sets, MOI.EqualTo{Float64}),
    )
    MOI.Utilities.final_touch(sets)
    VAF = MOI.VectorAffineFunction{Float64}
    @test !MOI.is_valid(sets, MOI.ConstraintIndex{VAF,MOI.Nonnegatives}(12345))
    nonneg_ci = MOI.ConstraintIndex{VAF,MOI.Nonnegatives}(nonneg_i)
    @test MOI.is_valid(sets, nonneg_ci)
    @test MOI.Utilities.rows(sets, nonneg_ci) == 1:2
    SAF = MOI.ScalarAffineFunction{Float64}
    equalto_ci = MOI.ConstraintIndex{SAF,MOI.EqualTo{Float64}}(equalto_i)
    @test MOI.is_valid(sets, equalto_ci)
    @test MOI.Utilities.rows(sets, equalto_ci) == 3
end

function test_vector_dimension()
    sets = _VectorSets{Float64}()
    MOI.Utilities.final_touch(sets)
    @test MOI.dimension(sets) == 0

    sets = _VectorSets{Float64}()
    MOI.Utilities.add_set(sets, 1, 1)
    MOI.Utilities.final_touch(sets)
    @test MOI.dimension(sets) == 1

    sets = _VectorSets{Float64}()
    MOI.Utilities.add_set(sets, 1, 1)
    MOI.Utilities.add_set(sets, 1, 2)
    MOI.Utilities.final_touch(sets)
    @test MOI.dimension(sets) == 3

    sets = _VectorSets{Float64}()
    MOI.Utilities.add_set(sets, 1, 1)
    MOI.Utilities.add_set(sets, 1, 2)
    MOI.Utilities.add_set(sets, 2, 3)
    MOI.Utilities.final_touch(sets)
    @test MOI.dimension(sets) == 6

    sets = _VectorSets{Float64}()
    MOI.Utilities.add_set(sets, 1, 1)
    MOI.Utilities.add_set(sets, 1, 2)
    MOI.Utilities.add_set(sets, 2, 3)
    MOI.Utilities.add_set(sets, 3)
    MOI.Utilities.final_touch(sets)
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
    MOI.Utilities.final_touch(sets)
    @test MOI.get(sets, MOI.ListOfConstraintTypesPresent()) == []
    sets = _VectorSets{Float64}()
    MOI.Utilities.add_set(sets, 1, 1)
    MOI.Utilities.add_set(sets, 3)
    MOI.Utilities.final_touch(sets)
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
    MOI.Utilities.final_touch(sets)
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
    MOI.Utilities.final_touch(sets)
    VAF = MOI.VectorAffineFunction{Float64}
    @test MOI.get(sets, MOI.ListOfConstraintIndices{VAF,MOI.Zeros}()) ==
          MOI.ConstraintIndex{VAF,MOI.Zeros}[]
    for (x, S) in zip([[1], [1, 3]], MOI.Utilities.set_types(sets)[1:2])
        ci = MOI.get(sets, MOI.ListOfConstraintIndices{VAF,S}())
        @test ci == MOI.ConstraintIndex{VAF,S}.(x)
    end
    F, S = MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}
    @test MOI.get(sets, MOI.ListOfConstraintIndices{F,S}()) ==
          [MOI.ConstraintIndex{F,S}(1)]
    return
end

"""
    test_vector_ListOfConstraintIndices2()

Test a more complicated sequence of dimensions to check the `_UnevenIterator`
works appropriately.
"""
function test_vector_ListOfConstraintIndices2()
    sets = _VectorSets{Float64}()
    MOI.Utilities.add_set(sets, 2, 2)
    MOI.Utilities.add_set(sets, 2, 3)
    MOI.Utilities.add_set(sets, 2, 2)
    MOI.Utilities.add_set(sets, 2, 4)
    MOI.Utilities.final_touch(sets)
    S = MOI.Utilities.set_types(sets)[2]
    VAF = MOI.VectorAffineFunction{Float64}
    indices = MOI.get(sets, MOI.ListOfConstraintIndices{VAF,S}())
    @test indices == MOI.ConstraintIndex{VAF,S}.([1, 3, 6, 8])
end

end

TestProductOfSets.runtests()
