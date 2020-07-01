struct EmptyVector{T} <: AbstractVector{T} end
Base.size(::EmptyVector) = (0,)
Base.isempty(::EmptyVector) = true
Base.eltype(::EmptyVector{T}) where {T} = T
Base.iterate(::EmptyVector) = nothing

"""
    struct LazyMap{T, VT}
        f::Function
        data::VT
    end

Iterator over the elements of `data` mapped by `f`. This is similar to
`Base.Generator(f, data)` except that the `eltype` of a `LazyMap` is given at
construction while the `eltype` of `Base.Generator(f, data)` is `Any`.
"""
struct LazyMap{T, VT}
    f::Function
    data::VT
end
function LazyMap{T}(f::Function, data) where {T}
    return LazyMap{T, typeof(data)}(f, data)
end
Base.size(it::LazyMap) = size(it.data)
Base.length(it::LazyMap) = length(it.data)
function Base.iterate(it::LazyMap, args...)
    elem_state = iterate(it.data, args...)
    if elem_state === nothing
        return nothing
    else
        return it.f(elem_state[1]), elem_state[2]
    end
end
Base.IteratorSize(it::LazyMap) = Base.IteratorSize(it.data)
Base.eltype(::LazyMap{T}) where {T} = T
