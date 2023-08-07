# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

struct EmptyVector{T} <: AbstractVector{T} end

Base.size(::EmptyVector) = (0,)

Base.isempty(::EmptyVector) = true

Base.eltype(::EmptyVector{T}) where {T} = T

Base.iterate(::EmptyVector) = nothing

abstract type AbstractLazyMap{T} end

"""
    struct LazyMap{T,VT,F}
        f::F
        data::VT
    end

Iterator over the elements of `data` mapped by `f`.  This is similar to
[`Utilities.LazyMap`](@ref) except that `VT` should be a subtype of
`AbstractVector{T}` and `VectorLazyMap` is a subtype of `AbstractVector{T}` as
well.  Use [`Utilities.lazy_map`](@ref) to create a `VectorLazyMap` or `LazyMap`
depending on the type of `data`.
"""
struct LazyMap{T,VT,F}
    f::F
    data::VT
end

function LazyMap{T}(f, data) where {T}
    return LazyMap{T,typeof(data),typeof(f)}(f, data)
end

"""
    lazy_map(::Type{T}, f, data)

Return an [`Utilities.VectorLazyMap`](@ref) is an `AbstractVector` and a
[`Utilities.LazyMap`](@ref) otherwise.  These are similar to
`Base.Generator(f, data)` except that their `eltype` is fixed to `T`
while the `eltype` of `Base.Generator(f, data)` is `Any`.
"""
lazy_map(::Type{T}, f, data) where {T} = LazyMap{T}(f, data)

"""
    struct VectorLazyMap{T,VT<:AbstractVector{T},F} <: AbstractVector{T}
        f::F
        data::VT
    end

Iterator over the elements of `data` mapped by `f`. This is similar to
[`Utilities.LazyMap`](@ref) except that `VT` should be a subtype of
`AbstractVector{T}` and `VectorLazyMap` is a subtype of `AbstractVector{T}` as
well.  Use [`Utilities.lazy_map`](@ref) to create a `VectorLazyMap` or `LazyMap`
depending on the type of `data`.
"""
struct VectorLazyMap{T,VT<:AbstractVector,F} <: AbstractVector{T}
    f::F
    data::VT
end

function VectorLazyMap{T}(f, data) where {T}
    return VectorLazyMap{T,typeof(data),typeof(f)}(f, data)
end

function lazy_map(::Type{T}, f, data::AbstractVector) where {T}
    return VectorLazyMap{T}(f, data)
end

const AnyLazyMap{T} = Union{LazyMap{T}, VectorLazyMap{T}}

Base.size(it::AnyLazyMap) = size(it.data)

Base.length(it::AnyLazyMap) = length(it.data)

function Base.iterate(it::AnyLazyMap, args...)
    elem_state = iterate(it.data, args...)
    if elem_state === nothing
        return
    else
        return it.f(elem_state[1]), elem_state[2]
    end
end

Base.IteratorSize(it::AnyLazyMap) = Base.IteratorSize(it.data)

Base.eltype(::AnyLazyMap{T}) where {T} = T

function Iterators.reverse(it::AnyLazyMap{T}) where {T}
    return AnyLazyMap{T}(it.f, Iterators.reverse(it.data))
end

function Base.getindex(it::AnyLazyMap, i)
    return it.f(getindex(it.data, i))
end
