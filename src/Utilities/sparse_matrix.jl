import SparseArrays

"""
    abstract type AbstractIndexing end

Indexing to be used for storing the row and column indices of
`MutableSparseMatrixCSC`. See [`ZeroBasedIndexing`](@ref) and
[`OneBasedIndexing`](@ref).
"""
abstract type AbstractIndexing end

"""
    struct ZeroBasedIndexing <: AbstractIndexing end

Zero-based indexing: the `i`th row or column has index `i - 1`. This is useful
when the vectors of row and column indices need to be communicated to a
library using zero-based indexing such as C libraries.
"""
struct ZeroBasedIndexing <: AbstractIndexing end

"""
    struct ZeroBasedIndexing <: AbstractIndexing end

One-based indexing: the `i`th row or column has index `i`. This enables an
allocation-free conversion of [`MutableSparseMatrixCSC`](@ref) to
`SparseArrays.SparseMatrixCSC`.
"""
struct OneBasedIndexing <: AbstractIndexing end

_first_index(::ZeroBasedIndexing) = 0
_first_index(::OneBasedIndexing) = 1
_shift(x, ::ZeroBasedIndexing, ::ZeroBasedIndexing) = x
_shift(x::Integer, ::ZeroBasedIndexing, ::OneBasedIndexing) = x + 1
_shift(x::Array{<:Integer}, ::ZeroBasedIndexing, ::OneBasedIndexing) = x .+ 1
_shift(x::Integer, ::OneBasedIndexing, ::ZeroBasedIndexing) = x - 1
_shift(x, ::OneBasedIndexing, ::OneBasedIndexing) = x

"""
    mutable struct MutableSparseMatrixCSC{Tv,Ti<:Integer,I<:AbstractIndexing}
        indexing::I
        m::Int
        n::Int
        colptr::Vector{Ti}
        rowval::Vector{Ti}
        nzval::Vector{Tv}
    end

Matrix type loading sparse matrices in the Compressed Sparse Column format.
The indexing used is `indexing`, see [`AbstractIndexing`](@ref). The other
fields have the same meaning than for `SparseArrays.SparseMatrixCSC` except
that the indexing is different unless `indexing` is `OneBasedIndexing`.

The matrix is loaded in 5 steps:
1) `MOI.empty!` is called.
2) `MOI.Utilities.add_column` and `MOI.Utilities.allocate_terms` are called in
   any order.
3) `MOI.Utilities.set_number_of_rows` is called.
4) `MOI.Utilities.load_terms` is called for each affine function.
5) `MOI.Utilities.final_touch` is called.
"""
mutable struct MutableSparseMatrixCSC{Tv,Ti<:Integer,I<:AbstractIndexing}
    indexing::I
    m::Int # Number of rows
    n::Int # Number of columns
    colptr::Vector{Ti}
    rowval::Vector{Ti}
    nzval::Vector{Tv}
    function MutableSparseMatrixCSC{Tv,Ti,I}() where {Tv,Ti<:Integer,I}
        return new{Tv,Ti,I}(I(), 0, 0, Ti[], Ti[], Tv[])
    end
end

function MOI.empty!(A::MutableSparseMatrixCSC)
    A.m = 0
    A.n = 0
    resize!(A.colptr, 1)
    A.colptr[1] = 0
    empty!(A.rowval)
    return empty!(A.nzval)
end

"""
    add_column(A::MutableSparseMatrixCSC)

Add a column to the matrix `A`.
"""
function add_column(A::MutableSparseMatrixCSC)
    A.n += 1
    push!(A.colptr, 0)
    return
end

"""
    set_number_of_rows(coefficients, n)

This function sets the number of rows to `coefficients`. This allows it
to preallocate necessary datastructures before the data is loaded with
[`load_terms`](@ref).
"""
function set_number_of_rows(A::MutableSparseMatrixCSC, num_rows)
    A.m = num_rows
    for i in 3:length(A.colptr)
        A.colptr[i] += A.colptr[i-1]
    end
    resize!(A.rowval, A.colptr[end])
    return resize!(A.nzval, A.colptr[end])
end

"""
    final_touch(A::MutableSparseMatrixCSC)

Informs the matrix `A` that all functions have been added with `load_terms`.
No more modification is allowed unless `MOI.empty!` is called.
"""
function final_touch(A::MutableSparseMatrixCSC)
    for i in length(A.colptr):-1:2
        A.colptr[i] = _shift(A.colptr[i-1], ZeroBasedIndexing(), A.indexing)
    end
    return A.colptr[1] = _first_index(A.indexing)
end

_variable(term::MOI.ScalarAffineTerm) = term.variable
_variable(term::MOI.VectorAffineTerm) = _variable(term.scalar_term)
function _allocate_terms(colptr, index_map, terms)
    for term in terms
        colptr[index_map[_variable(term)].value+1] += 1
    end
end

"""
    allocate_terms(A::MutableSparseMatrixCSC, index_map, func)

Informs `A` that the terms of the function `func` where the variable
indices are mapped with `index_map` will be loaded with [`load_terms`](@ref).
The function `func` should be canonicalized, see [`is_canonical`](@ref).
"""
function allocate_terms(A::MutableSparseMatrixCSC, index_map, func)
    return _allocate_terms(A.colptr, index_map, func.terms)
end

_output_index(::MOI.ScalarAffineTerm) = 1
_output_index(term::MOI.VectorAffineTerm) = term.output_index
_coefficient(term::MOI.ScalarAffineTerm) = term.coefficient
_coefficient(term::MOI.VectorAffineTerm) = _coefficient(term.scalar_term)
function _load_terms(colptr, rowval, nzval, index_map, terms, offset)
    for term in terms
        ptr = colptr[index_map[_variable(term)].value] += 1
        rowval[ptr] = offset + _output_index(term)
        nzval[ptr] = _coefficient(term)
    end
end

"""
    load_terms(A::MutableSparseMatrixCSC, index_map, func, offset)

Loads the terms of `func` to `A` mapping the variable indices with `index_map`.
The `i`th dimension of `func` is loaded at the `(offset + i)`th row of `A`. The
function should be allocated first with [`allocate_terms`](@ref). The function
`func` should be canonicalized, see [`is_canonical`](@ref).
"""
function load_terms(A::MutableSparseMatrixCSC, index_map, func, offset)
    return _load_terms(
        A.colptr,
        A.rowval,
        A.nzval,
        index_map,
        func.terms,
        _shift(offset, OneBasedIndexing(), A.indexing),
    )
end

"""
    Base.convert(::Type{SparseMatrixCSC{Tv, Ti}}, A::MutableSparseMatrixCSC{Tv, Ti, I}) where {Tv, Ti, I}

Converts `A` to a `SparseMatrixCSC`. Note that the field `A.nzval` is **not
copied** so if `A` is modified after the call of this function, it can still
affect the value returned. Moreover, if `I` is `OneBasedIndexing`, `colptr`
and `rowval` are not copied either, i.e., the conversion is allocation-free.
"""
function Base.convert(
    ::Type{SparseArrays.SparseMatrixCSC{Tv,Ti}},
    A::MutableSparseMatrixCSC{Tv,Ti},
) where {Tv,Ti}
    return SparseArrays.SparseMatrixCSC{Tv,Ti}(
        A.m,
        A.n,
        _shift(A.colptr, A.indexing, OneBasedIndexing()),
        _shift(A.rowval, A.indexing, OneBasedIndexing()),
        A.nzval,
    )
end
