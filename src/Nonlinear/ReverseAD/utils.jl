# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    _UnsafeVectorView(offset, len, ptr)

Lightweight unsafe view for vectors.

## Motivation

`_UnsafeVectorView` is needed as an allocation-free equivalent of `view`. Other
alternatives, like `view(x, 1:len)` or a struct like
```
struct _SafeView{T}
    x::Vector{T}
    len::Int
end
```
will allocate so that `x` can be tracked by Julia's GC.

`_UnsafeVectorView` relies on the fact that the use-cases of `_UnsafeVectorView`
only temporarily wrap a long-lived vector like `d.jac_storage` so that we don't
have to worry about the GC removing `d.jac_storage` while `_UnsafeVectorView`
exists. This lets us use a `Ptr{T}` and create a struct that is `isbitstype` and
therefore does not allocate.

## Example

Instead of `view(x, 1:10)`, use `_UnsafeVectorView(0, 10, pointer(x))`.

## Unsafe behavior

`_UnsafeVectorView` is unsafe because it assumes that the vector `x` that the
pointer `ptr` refers to remains valid during the usage of `_UnsafeVectorView`.
"""
struct _UnsafeVectorView{T} <: DenseVector{T}
    offset::Int
    len::Int
    ptr::Ptr{T}
end

function Base.getindex(x::_UnsafeVectorView, i::Integer)
    return unsafe_load(x.ptr, i + x.offset)
end

Base.getindex(x::_UnsafeVectorView, i::CartesianIndex{1}) = getindex(x, i[1])

function Base.setindex!(x::_UnsafeVectorView, value, i::Integer)
    # We don't need to worry about `value` being the right type here because
    # x.ptr is a `::Ptr{T}`, so even though it is called `unsafe_store!`, there
    # is still a type convertion that happens so that we're not just chucking
    # the bits of value into `x.ptr`.
    unsafe_store!(x.ptr, value, i + x.offset)
    return value
end

function Base.setindex!(x::_UnsafeVectorView, value, i::CartesianIndex{1})
    return setindex!(x, value, i[1])
end

Base.length(v::_UnsafeVectorView) = v.len

Base.size(v::_UnsafeVectorView) = (v.len,)

"""
    _UnsafeVectorView(x::Vector, N::Int)

Create a new [`_UnsafeVectorView`](@ref) from `x`, and resize `x` if needed to
ensure it has a length of at least `N`.

## Unsafe behavior

In addition to the usafe behavior of `_UnsafeVectorView`, this constructor is
additionally unsafe because it may resize `x`. Only call it if you are sure that
the usage of `_UnsafeVectorView(x, N)` is short-lived, and that there are no
other views to `x` while the returned value is within scope.
"""
function _UnsafeVectorView(x::Vector, N::Int)
    if length(x) < N
        resize!(x, N)
    end
    return _UnsafeVectorView(0, N, pointer(x))
end

"""
    _UnsafeLowerTriangularMatrixView(x, N)

Lightweight unsafe view that converts a vector `x` into the lower-triangular
component of a symmetric `N`-by-`N` matrix.

## Motivation

`_UnsafeLowerTriangularMatrixView` is needed as an allocation-free equivalent of
`view`. Other alternatives, like `reshape(view(x, 1:N^2), N, N)` or a struct
like
```julia
struct _SafeView{T}
    x::Vector{T}
    len::Int
end
```
will allocate so that `x` can be tracked by Julia's GC.
`_UnsafeLowerTriangularMatrixView` relies on the fact that the use-cases of
`_UnsafeLowerTriangularMatrixView` only temporarily wrap a long-lived vector
like `d.jac_storage` so that we don't have to worry about the GC removing
`d.jac_storage` while `_UnsafeLowerTriangularMatrixView` exists. This lets us
use a `Ptr{T}` and create a struct that is `isbitstype` and therefore does not
allocate.

## Unsafe behavior

`_UnsafeLowerTriangularMatrixView` is unsafe because it assumes that the vector
`x` remains valid during the usage of `_UnsafeLowerTriangularMatrixView`.
"""
struct _UnsafeLowerTriangularMatrixView <: AbstractMatrix{Float64}
    N::Int
    ptr::Ptr{Float64}
end

Base.size(x::_UnsafeLowerTriangularMatrixView) = (x.N, x.N)

function _linear_index(row, col)
    if row < col
        error("Unable to access upper-triangular component: ($row, $col)")
    end
    return div((row - 1) * row, 2) + col
end

function Base.getindex(x::_UnsafeLowerTriangularMatrixView, i, j)
    return unsafe_load(x.ptr, _linear_index(i, j))
end

function Base.setindex!(x::_UnsafeLowerTriangularMatrixView, value, i, j)
    unsafe_store!(x.ptr, value, _linear_index(i, j))
    return value
end

"""
    _UnsafeLowerTriangularMatrixView(x::Vector{Float64}, N::Int)

Create a new [`_UnsafeLowerTriangularMatrixView`](@ref) from `x`, zero the
elements in `x`, and resize `x` if needed to ensure it has a length of at least
`N * (N + 1) / 2`.

## Unsafe behavior

In addition to the usafe behavior of `_UnsafeLowerTriangularMatrixView`, this
constructor is additionally unsafe because it may resize `x`. Only call it if
you are sure that the usage of `_UnsafeLowerTriangularMatrixView(x, N)` is
short-lived, and that there are no other views to `x` while the returned value
is within scope.
"""
function _UnsafeLowerTriangularMatrixView(x::Vector{Float64}, N::Int)
    z = div(N * (N + 1), 2)
    if length(x) < z
        resize!(x, z)
    end
    for i in 1:z
        x[i] = 0.0
    end
    return _UnsafeLowerTriangularMatrixView(N, pointer(x))
end

"""
    _reinterpret_unsafe(::Type{T}, x::Vector{R}) where {T,R}

Return an `_UnsafeVectorView` that act as a vector of element type
`T` over the same bytes as `x`. Note that if `length(x) * sizeof(R)` is not
a multiple of `sizeof(T)`, the last bits will be ignored. This is a key
difference with `reinterpret` which errors in that case.

Given a vector of `Float64` of length equal to the maximum number of nodes of a
set of expressions time the maximum chunk size, this function is used to
reinterpret it as a vector of `ForwardDiff.Partials{N,T}` where `N` is the
chunk size of one of the expressions of the set. In that case, we know that
the vector has enough bytes and we don't care about the leftover bytes at the
end.

## Examples

```jldoctest
julia> import MathOptInterface as MOI

julia> x = [(1, 2, 3), (4, 5, 6), (7, 8, 9)]
3-element Vector{Tuple{Int64, Int64, Int64}}:
 (1, 2, 3)
 (4, 5, 6)
 (7, 8, 9)

julia> MOI.Nonlinear.ReverseAD._reinterpret_unsafe(NTuple{2,Int}, x)
4-element MathOptInterface.Nonlinear.ReverseAD._UnsafeVectorView{Tuple{Int64, Int64}}:
 (1, 2)
 (3, 4)
 (5, 6)
 (7, 8)
```
"""
function _reinterpret_unsafe(::Type{T}, x::Vector{R}) where {T,R}
    @assert isbitstype(T) && isbitstype(R)
    # how many T's fit into x?
    len = length(x) * sizeof(R)
    p = reinterpret(Ptr{T}, pointer(x))
    return _UnsafeVectorView(0, div(len, sizeof(T)), p)
end
