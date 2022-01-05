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

_first_index(::Type{T}, ::ZeroBasedIndexing) where {T} = T(0)
_first_index(::Type{T}, ::OneBasedIndexing) where {T} = T(1)

_shift(x, ::T, ::T) where {T<:AbstractIndexing} = x
_shift(x::Integer, ::ZeroBasedIndexing, ::OneBasedIndexing) = x + 1
_shift(x::Integer, ::OneBasedIndexing, ::ZeroBasedIndexing) = x - 1
_shift(x::Array{<:Integer}, ::ZeroBasedIndexing, ::OneBasedIndexing) = x .+ 1

"""
    mutable struct MutableSparseMatrixCSC{Tv,Ti<:Integer,I<:AbstractIndexing}
        indexing::I
        m::Int
        n::Int
        colptr::Vector{Ti}
        rowval::Vector{Ti}
        nzval::Vector{Tv}
        nz_added::Vector{Ti}
    end

Matrix type loading sparse matrices in the Compressed Sparse Column format.
The indexing used is `indexing`, see [`AbstractIndexing`](@ref). The other
fields have the same meaning than for `SparseArrays.SparseMatrixCSC` except
that the indexing is different unless `indexing` is `OneBasedIndexing`. In
addition, `nz_added` is used to cache the number of non-zero terms that have
been added to each column due to the incremental nature of `load_terms`.

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
    nz_added::Vector{Ti}
    function MutableSparseMatrixCSC{Tv,Ti,I}() where {Tv,Ti<:Integer,I}
        indexing = I()
        colptr = [_first_index(Ti, indexing)]
        return new{Tv,Ti,I}(indexing, 0, 0, colptr, Ti[], Tv[], Ti[])
    end
end

function SparseArrays.nzrange(A::MutableSparseMatrixCSC, col)
    start = _shift(A.colptr[col], A.indexing, OneBasedIndexing())
    stop = _shift(A.colptr[col+1], A.indexing, ZeroBasedIndexing())
    return start:stop
end

function MOI.empty!(A::MutableSparseMatrixCSC{Tv,Ti}) where {Tv,Ti}
    A.m = 0
    A.n = 0
    resize!(A.colptr, 1)
    A.colptr[1] = _first_index(Ti, A.indexing)
    empty!(A.rowval)
    empty!(A.nzval)
    empty!(A.nz_added)
    return
end

function add_column(A::MutableSparseMatrixCSC{Tv,Ti}) where {Tv,Ti}
    A.n += 1
    push!(A.colptr, _first_index(Ti, A.indexing))
    push!(A.nz_added, Ti(0))
    return
end

function add_columns(A::MutableSparseMatrixCSC{Tv,Ti}, n) where {Tv,Ti}
    for _ in 1:n
        add_column(A)
    end
    return
end

function set_number_of_rows(A::MutableSparseMatrixCSC, num_rows)
    A.m = num_rows
    offset = _first_index(Int, A.indexing)
    for i in 3:length(A.colptr)
        A.colptr[i] += A.colptr[i-1] - offset
    end
    resize!(A.rowval, A.colptr[end] - offset)
    resize!(A.nzval, A.colptr[end] - offset)
    return
end

final_touch(::MutableSparseMatrixCSC) = nothing

_variable(term::MOI.ScalarAffineTerm) = term.variable
_variable(term::MOI.VectorAffineTerm) = _variable(term.scalar_term)

function allocate_terms(
    A::MutableSparseMatrixCSC,
    index_map,
    func::Union{MOI.ScalarAffineFunction,MOI.VectorAffineFunction},
)
    for term in func.terms
        A.colptr[index_map[_variable(term)].value+1] += 1
    end
    return
end

_output_index(::MOI.ScalarAffineTerm) = 1
_output_index(term::MOI.VectorAffineTerm) = term.output_index

function load_terms(
    A::MutableSparseMatrixCSC,
    index_map,
    func::Union{MOI.ScalarAffineFunction,MOI.VectorAffineFunction},
    offset::Int,
)
    row_offset = _shift(offset, OneBasedIndexing(), A.indexing)
    zero_offset = 1 - _first_index(Int, A.indexing)
    for term in func.terms
        col = index_map[_variable(term)].value
        ptr = A.colptr[col] + A.nz_added[col] + zero_offset
        A.rowval[ptr] = row_offset + _output_index(term)
        A.nzval[ptr] = MOI.coefficient(term)
        A.nz_added[col] += 1
    end
    return
end

"""
    Base.convert(
        ::Type{SparseMatrixCSC{Tv,Ti}},
        A::MutableSparseMatrixCSC{Tv,Ti,I},
    ) where {Tv,Ti,I}

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

function _first_in_column(
    A::MutableSparseMatrixCSC{Tv,Ti},
    row::Integer,
    col::Integer,
) where {Tv,Ti}
    range = SparseArrays.nzrange(A, col)
    row = _shift(row, OneBasedIndexing(), A.indexing)
    idx = searchsortedfirst(view(A.rowval, range), row)
    return get(range, idx, last(range) + 1)
end

function extract_function(
    A::MutableSparseMatrixCSC{T},
    row::Integer,
    constant::T,
) where {T}
    func = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], constant)
    for col in 1:A.n
        idx = _first_in_column(A, row, col)
        if idx > last(SparseArrays.nzrange(A, col))
            continue
        end
        r = _shift(A.rowval[idx], A.indexing, OneBasedIndexing())
        if r == row
            push!(
                func.terms,
                MOI.ScalarAffineTerm(A.nzval[idx], MOI.VariableIndex(col)),
            )
        end
    end
    return func
end

function extract_function(
    A::MutableSparseMatrixCSC{T},
    rows::UnitRange,
    constants::Vector{T},
) where {T}
    func = MOI.VectorAffineFunction(MOI.VectorAffineTerm{T}[], constants)
    if isempty(rows)
        return func
    end
    idx = [_first_in_column(A, first(rows), col) for col in 1:A.n]
    for output_index in eachindex(rows)
        for col in 1:A.n
            if idx[col] > last(SparseArrays.nzrange(A, col))
                continue
            end
            row = _shift(A.rowval[idx[col]], A.indexing, OneBasedIndexing())
            if row != rows[output_index]
                continue
            end
            push!(
                func.terms,
                MOI.VectorAffineTerm(
                    output_index,
                    MOI.ScalarAffineTerm(
                        A.nzval[idx[col]],
                        MOI.VariableIndex(col),
                    ),
                ),
            )
            idx[col] += 1
        end
    end
    return func
end
