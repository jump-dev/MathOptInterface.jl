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

Base.getindex(x::_UnsafeVectorView, i) = unsafe_load(x.ptr, i + x.offset)

function Base.setindex!(x::_UnsafeVectorView, value, i)
    unsafe_store!(x.ptr, value, i + x.offset)
    return value
end

Base.length(v::_UnsafeVectorView) = v.len

Base.size(v::_UnsafeVectorView) = (v.len,)

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

function _UnsafeLowerTriangularMatrixView(x::Vector, N::Int)
    z = div(N * (N + 1), 2)
    if length(x) < z
        resize!(x, z)
    end
    for i in 1:z
        x[i] = 0.0
    end
    return _UnsafeLowerTriangularMatrixView(N, pointer(x))
end

function _reinterpret_unsafe(::Type{T}, x::Vector{R}) where {T,R}
    # how many T's fit into x?
    @assert isbitstype(T) && isbitstype(R)
    len = length(x) * sizeof(R)
    p = reinterpret(Ptr{T}, pointer(x))
    return _UnsafeVectorView(0, div(len, sizeof(T)), p)
end
