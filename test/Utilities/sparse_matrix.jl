module TestSparseMatrix

import SparseArrays
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

"""
    test_empty_size()

Julia 1.7 is going to outlaw constructing empty sparse matrices. Test that we
can still do so.
"""
function test_empty_size()
    A = MOI.Utilities.MutableSparseMatrixCSC{
        Float64,
        Int,
        MOI.Utilities.ZeroBasedIndexing,
    }()
    MOI.empty!(A)
    @test A.rowval == Int[]
    @test A.nzval == Float64[]
    @test A.colptr == Int[0]
end

function test_empty_large()
    A = MOI.Utilities.MutableSparseMatrixCSC{
        Float64,
        Int,
        MOI.Utilities.ZeroBasedIndexing,
    }()
    MOI.empty!(A)
    MOI.Utilities.add_column(A)
    MOI.Utilities.add_column(A)
    MOI.Utilities.set_number_of_rows(A, 5)
    B = convert(SparseArrays.SparseMatrixCSC{Float64,Int}, A)
    @test size(B) == (5, 2)
end

function test_VectorAffine_ZeroBased()
    A = MOI.Utilities.MutableSparseMatrixCSC{
        Float64,
        Int,
        MOI.Utilities.ZeroBasedIndexing,
    }()
    MOI.empty!(A)
    x = MOI.VariableIndex.(1:3)
    f = MOI.VectorAffineFunction(
        vcat(
            MOI.VectorAffineTerm.(1, MOI.ScalarAffineTerm.(1.0, x)),
            MOI.VectorAffineTerm.(2, MOI.ScalarAffineTerm.([2.0, 3.0], x[2:3])),
        ),
        [0.5, 1.2],
    )

    index_map = MOI.Utilities.IndexMap()
    for i in 1:3
        MOI.Utilities.add_column(A)
        index_map[x[i]] = x[i]
    end
    MOI.Utilities.allocate_terms(A, index_map, f)
    MOI.Utilities.set_number_of_rows(A, 2)
    MOI.Utilities.load_terms(A, index_map, f, 0)
    MOI.Utilities.final_touch(A)
    B = convert(SparseArrays.SparseMatrixCSC{Float64,Int}, A)
    @test B == [1.0 1.0 1.0; 0.0 2.0 3.0]
    @test A.rowval == [0, 0, 1, 0, 1]
    @test A.nzval == [1.0, 1.0, 2.0, 1.0, 3.0]
    @test A.colptr == [0, 1, 3, 5]
end

function test_extract_function()
    A = MOI.Utilities.MutableSparseMatrixCSC{
        Float64,
        Int,
        MOI.Utilities.ZeroBasedIndexing,
    }()
    MOI.empty!(A)
    x = MOI.VariableIndex.(1:3)
    f = MOI.VectorAffineFunction(
        vcat(
            MOI.VectorAffineTerm.(1, MOI.ScalarAffineTerm.(1.0, x)),
            MOI.VectorAffineTerm.(2, MOI.ScalarAffineTerm.([2.0, 3.0], x[2:3])),
        ),
        [0.5, 1.2],
    )

    index_map = MOI.Utilities.IndexMap()
    MOI.Utilities.add_columns(A, 3)
    for i in 1:3
        index_map[x[i]] = x[i]
    end
    MOI.Utilities.allocate_terms(A, index_map, f)
    MOI.Utilities.set_number_of_rows(A, 2)
    MOI.Utilities.load_terms(A, index_map, f, 0)
    MOI.Utilities.final_touch(A)
    row_1 = MOI.Utilities.extract_function(A, 1, 0.5)
    @test row_1 ≈ MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x), 0.5)
    @test MOI.Utilities.extract_function(A, 1:2, [0.5, 1.2]) ≈ f
    empty_f = MOI.Utilities.extract_function(A, 1:0, Float64[])
    @test isempty(empty_f)
    return
end

function test_VectorAffine_OneBased()
    A = MOI.Utilities.MutableSparseMatrixCSC{
        Float64,
        Int,
        MOI.Utilities.OneBasedIndexing,
    }()
    MOI.empty!(A)
    x = MOI.VariableIndex.(1:3)
    f = MOI.VectorAffineFunction(
        vcat(
            MOI.VectorAffineTerm.(1, MOI.ScalarAffineTerm.(1.0, x)),
            MOI.VectorAffineTerm.(2, MOI.ScalarAffineTerm.([2.0, 3.0], x[2:3])),
        ),
        [0.5, 1.2],
    )

    index_map = MOI.Utilities.IndexMap()
    for i in 1:3
        MOI.Utilities.add_column(A)
        index_map[x[i]] = x[i]
    end
    MOI.Utilities.allocate_terms(A, index_map, f)
    MOI.Utilities.set_number_of_rows(A, 2)
    MOI.Utilities.load_terms(A, index_map, f, 0)
    MOI.Utilities.final_touch(A)
    B = convert(SparseArrays.SparseMatrixCSC{Float64,Int}, A)
    @test B == [1.0 1.0 1.0; 0.0 2.0 3.0]
    @test A.rowval == [1, 1, 2, 1, 2]
    @test A.nzval == [1.0, 1.0, 2.0, 1.0, 3.0]
    @test A.colptr == [1, 2, 4, 6]
end

function test_ScalarAffine_ZeroBased()
    A = MOI.Utilities.MutableSparseMatrixCSC{
        Float64,
        Int,
        MOI.Utilities.ZeroBasedIndexing,
    }()
    MOI.empty!(A)
    x = MOI.VariableIndex.(1:3)
    f1 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x), 0.5)
    f2 =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0, 3.0], x[2:3]), 1.2)
    index_map = MOI.Utilities.IndexMap()
    for i in 1:3
        MOI.Utilities.add_column(A)
        index_map[x[i]] = x[i]
    end
    MOI.Utilities.allocate_terms(A, index_map, f1)
    MOI.Utilities.allocate_terms(A, index_map, f2)
    MOI.Utilities.set_number_of_rows(A, 2)
    MOI.Utilities.load_terms(A, index_map, f1, 0)
    MOI.Utilities.load_terms(A, index_map, f2, 1)
    MOI.Utilities.final_touch(A)
    B = convert(SparseArrays.SparseMatrixCSC{Float64,Int}, A)
    @test B == [1.0 1.0 1.0; 0.0 2.0 3.0]
    @test A.rowval == [0, 0, 1, 0, 1]
    @test A.nzval == [1.0, 1.0, 2.0, 1.0, 3.0]
    @test A.colptr == [0, 1, 3, 5]
end

function test_ScalarAffine_OneBased()
    A = MOI.Utilities.MutableSparseMatrixCSC{
        Float64,
        Int,
        MOI.Utilities.OneBasedIndexing,
    }()
    MOI.empty!(A)
    x = MOI.VariableIndex.(1:3)
    f1 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x), 0.5)
    f2 =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0, 3.0], x[2:3]), 1.2)
    index_map = MOI.Utilities.IndexMap()
    for i in 1:3
        MOI.Utilities.add_column(A)
        index_map[x[i]] = x[i]
    end
    MOI.Utilities.allocate_terms(A, index_map, f1)
    MOI.Utilities.allocate_terms(A, index_map, f2)
    MOI.Utilities.set_number_of_rows(A, 2)
    MOI.Utilities.load_terms(A, index_map, f1, 0)
    MOI.Utilities.load_terms(A, index_map, f2, 1)
    MOI.Utilities.final_touch(A)
    B = convert(SparseArrays.SparseMatrixCSC{Float64,Int}, A)
    @test B == [1.0 1.0 1.0; 0.0 2.0 3.0]
    @test A.rowval == [1, 1, 2, 1, 2]
    @test A.nzval == [1.0, 1.0, 2.0, 1.0, 3.0]
    @test A.colptr == [1, 2, 4, 6]
end

end

TestSparseMatrix.runtests()
