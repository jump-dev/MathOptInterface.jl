import SparseArrays

abstract type AbstractIndexing end
struct ZeroBasedIndexing <: AbstractIndexing end
struct OneBasedIndexing <: AbstractIndexing end

first_index(::ZeroBasedIndexing) = 0
first_index(::OneBasedIndexing) = 1
shift(x, ::ZeroBasedIndexing, ::ZeroBasedIndexing) = x
shift(x::Integer, ::ZeroBasedIndexing, ::OneBasedIndexing) = x + 1
shift(x::Array{<:Integer}, ::ZeroBasedIndexing, ::OneBasedIndexing) = x .+ 1
shift(x::Integer, ::OneBasedIndexing, ::ZeroBasedIndexing) = x - 1
shift(x, ::OneBasedIndexing, ::OneBasedIndexing) = x

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
function add_column(A::MutableSparseMatrixCSC)
    A.n += 1
    push!(A.colptr, 0)
    return
end
function set_number_of_columns(A::MutableSparseMatrixCSC, num_cols)
    A.n = num_cols
    resize!(A.colptr, A.n + 1)
    fill!(A.colptr, 0)
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
function final_touch(A::MutableSparseMatrixCSC)
    for i in length(A.colptr):-1:2
        A.colptr[i] = shift(A.colptr[i-1], ZeroBasedIndexing(), A.indexing)
    end
    return A.colptr[1] = first_index(A.indexing)
end
_variable(term::MOI.ScalarAffineTerm) = term.variable
_variable(term::MOI.VectorAffineTerm) = _variable(term.scalar_term)
function _allocate_terms(colptr, indexmap, terms)
    for term in terms
        colptr[indexmap[_variable(term)].value+1] += 1
    end
end
function allocate_terms(A::MutableSparseMatrixCSC, indexmap, func)
    return _allocate_terms(A.colptr, indexmap, func.terms)
end
_output_index(::MOI.ScalarAffineTerm) = 1
_output_index(term::MOI.VectorAffineTerm) = term.output_index
_coefficient(term::MOI.ScalarAffineTerm) = term.coefficient
_coefficient(term::MOI.VectorAffineTerm) = _coefficient(term.scalar_term)
function _load_terms(colptr, rowval, nzval, indexmap, terms, offset)
    for term in terms
        ptr = colptr[indexmap[_variable(term)].value] += 1
        rowval[ptr] = offset + _output_index(term)
        nzval[ptr] = _coefficient(term)
    end
end
function load_terms(A::MutableSparseMatrixCSC, indexmap, func, offset)
    return _load_terms(
        A.colptr,
        A.rowval,
        A.nzval,
        indexmap,
        func.terms,
        shift(offset, OneBasedIndexing(), A.indexing),
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
        shift(A.colptr, A.indexing, OneBasedIndexing()),
        shift(A.rowval, A.indexing, OneBasedIndexing()),
        A.nzval,
    )
end
