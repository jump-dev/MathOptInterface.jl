# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestProductOfSets

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
    F, S = MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}
    ci = MOI.ConstraintIndex{F,S}(12345)
    @test !MOI.is_valid(sets, ci)
    i = MOI.Utilities.set_index(sets, MOI.EqualTo{Float64})
    ci_value = MOI.Utilities.add_set(sets, i)
    ci = MOI.ConstraintIndex{F,S}(ci_value)
    @test MOI.is_valid(sets, ci)
    @test MOI.Utilities.rows(sets, ci) == ci.value
    ci = MOI.ConstraintIndex{F,MOI.ZeroOne}(1)
    @test !MOI.is_valid(sets, ci)
    return
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
    @test MOI.Utilities.num_rows(sets, MOI.Nonnegatives) == 2
    @test MOI.Utilities.num_rows(sets, MOI.EqualTo{Float64}) == 1
    @test MOI.Utilities.num_rows(sets, MOI.Nonpositives) == 0
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
    for (x, S) in zip([[1], [1, 2]], MOI.Utilities.set_types(sets)[1:2])
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
    @test indices == MOI.ConstraintIndex{VAF,S}.([1, 2, 3, 4])
    return
end

function test_zero_dimensional_function()
    sets = _VectorSets{Int}()
    i = MOI.Utilities.set_index(sets, MOI.Nonnegatives)
    MOI.Utilities.add_set(sets, i, 0)
    MOI.Utilities.add_set(sets, i, 2)
    MOI.Utilities.add_set(sets, i, 0)
    MOI.Utilities.add_set(sets, i, 1)
    MOI.Utilities.add_set(sets, i, 0)
    MOI.Utilities.final_touch(sets)
    F, S = MOI.VectorAffineFunction{Int}, MOI.Nonnegatives
    @test (F, S) in MOI.get(sets, MOI.ListOfConstraintTypesPresent())
    @test MOI.get(sets, MOI.NumberOfConstraints{F,S}()) == 5
    c = MOI.ConstraintIndex{F,S}.(1:5)
    @test MOI.get(sets, MOI.ListOfConstraintIndices{F,S}()) == c
    @test all(MOI.is_valid(sets, ci) for ci in c)
    return
end

function test_zero_dimensional_function_mix_of_sets()
    sets = _VectorSets{Int}()
    i1 = MOI.Utilities.set_index(sets, MOI.Nonpositives)
    @test i1 == 1  # The tests below explicitly use this ordering.
    i2 = MOI.Utilities.set_index(sets, MOI.Nonnegatives)
    @test i2 == 2  # The tests below explicitly use this ordering.
    i3 = MOI.Utilities.set_index(sets, MOI.EqualTo{Int})
    @test i3 == 3  # The tests below explicitly use this ordering.
    MOI.Utilities.add_set(sets, i1, 0)
    MOI.Utilities.add_set(sets, i1, 2)
    MOI.Utilities.add_set(sets, i2, 0)
    MOI.Utilities.add_set(sets, i2, 1)
    MOI.Utilities.add_set(sets, i1, 0)
    MOI.Utilities.add_set(sets, i1, 1)
    MOI.Utilities.add_set(sets, i3)
    MOI.Utilities.add_set(sets, i1, 0)
    @test MOI.Utilities.num_rows(sets, MOI.Nonpositives) == 3
    @test MOI.Utilities.num_rows(sets, MOI.Nonnegatives) == 1
    @test MOI.Utilities.num_rows(sets, MOI.EqualTo{Int}) == 1
    MOI.Utilities.final_touch(sets)
    # MOI.Nonpositives
    F, S = MOI.VectorAffineFunction{Int}, MOI.Nonpositives
    @test (F, S) in MOI.get(sets, MOI.ListOfConstraintTypesPresent())
    @test MOI.get(sets, MOI.NumberOfConstraints{F,S}()) == 5
    c = MOI.ConstraintIndex{F,S}.(1:5)
    @test MOI.get(sets, MOI.ListOfConstraintIndices{F,S}()) == c
    @test all(MOI.is_valid(sets, ci) for ci in c)
    @test MOI.Utilities.num_rows(sets, S) == 3
    @test MOI.Utilities.rows(sets, c[1]) == 1:0
    @test MOI.Utilities.rows(sets, c[2]) == 1:2
    @test MOI.Utilities.rows(sets, c[3]) == 3:2
    @test MOI.Utilities.rows(sets, c[4]) == 3:3
    @test MOI.Utilities.rows(sets, c[5]) == 4:3
    # MOI.Nonnegatives
    F, S = MOI.VectorAffineFunction{Int}, MOI.Nonnegatives
    @test (F, S) in MOI.get(sets, MOI.ListOfConstraintTypesPresent())
    @test MOI.get(sets, MOI.NumberOfConstraints{F,S}()) == 2
    c = MOI.ConstraintIndex{F,S}.(1:2)
    @test MOI.get(sets, MOI.ListOfConstraintIndices{F,S}()) == c
    @test all(MOI.is_valid(sets, ci) for ci in c)
    @test MOI.Utilities.num_rows(sets, S) == 1
    @test MOI.Utilities.rows(sets, c[1]) == 4:3
    @test MOI.Utilities.rows(sets, c[2]) == 4:4
    # MOI.EqualTo
    F, S = MOI.ScalarAffineFunction{Int}, MOI.EqualTo{Int}
    @test (F, S) in MOI.get(sets, MOI.ListOfConstraintTypesPresent())
    @test MOI.get(sets, MOI.NumberOfConstraints{F,S}()) == 1
    c = MOI.ConstraintIndex{F,S}.(1:1)
    @test MOI.get(sets, MOI.ListOfConstraintIndices{F,S}()) == c
    @test all(MOI.is_valid(sets, ci) for ci in c)
    @test MOI.Utilities.num_rows(sets, S) == 1
    @test MOI.Utilities.rows(sets, c[1]) == 5
    return
end

function test_zero_dimensional_function_only()
    sets = _VectorSets{Int}()
    i = MOI.Utilities.set_index(sets, MOI.Nonnegatives)
    MOI.Utilities.add_set(sets, i, 0)
    MOI.Utilities.add_set(sets, i, 0)
    MOI.Utilities.add_set(sets, i, 0)
    MOI.Utilities.final_touch(sets)
    F, S = MOI.VectorAffineFunction{Int}, MOI.Nonnegatives
    @test (F, S) in MOI.get(sets, MOI.ListOfConstraintTypesPresent())
    @test MOI.get(sets, MOI.NumberOfConstraints{F,S}()) == 3
    c = MOI.ConstraintIndex{F,S}.(1:3)
    @test MOI.get(sets, MOI.ListOfConstraintIndices{F,S}()) == c
    @test all(MOI.is_valid(sets, ci) for ci in c)
    return
end

function test_ordered_product_of_sets_is_valid()
    sets = _VectorSets{Int}()
    i = MOI.Utilities.set_index(sets, MOI.Nonnegatives)
    MOI.Utilities.add_set(sets, i, 2)
    MOI.Utilities.final_touch(sets)
    F, S = MOI.VectorAffineFunction{Int}, MOI.Nonnegatives
    @test MOI.is_valid(sets, MOI.ConstraintIndex{F,S}(1))
    for ci in MOI.ConstraintIndex[
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(1),
        MOI.ConstraintIndex{F,S}(-1),
        MOI.ConstraintIndex{F,S}(0),
        MOI.ConstraintIndex{F,S}(2),
        MOI.ConstraintIndex{F,S}(12345),
    ]
        @test !MOI.is_valid(sets, ci)
    end
    return
end

function test_property_num_sets()
    sets = _VectorSets{Int}()
    i1 = MOI.Utilities.set_index(sets, MOI.Nonpositives)
    i2 = MOI.Utilities.set_index(sets, MOI.Nonnegatives)
    i3 = MOI.Utilities.set_index(sets, MOI.EqualTo{Int})
    MOI.Utilities.add_set(sets, i1, 0)
    MOI.Utilities.add_set(sets, i1, 2)
    MOI.Utilities.add_set(sets, i2, 0)
    MOI.Utilities.add_set(sets, i2, 1)
    MOI.Utilities.add_set(sets, i1, 0)
    MOI.Utilities.add_set(sets, i1, 1)
    MOI.Utilities.add_set(sets, i3)
    MOI.Utilities.add_set(sets, i1, 0)
    @test sets.num_rows == [3, 1, 1]
    MOI.Utilities.final_touch(sets)
    @test sets.num_rows == [3, 4, 5]
    return
end

end

TestProductOfSets.runtests()
