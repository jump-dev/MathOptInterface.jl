struct EmptyVector{T} <: AbstractVector{T} end
Base.size(::EmptyVector) = (0,)
Base.isempty(::EmptyVector) = true
Base.eltype(::EmptyVector{T}) where {T} = T
Base.iterate(::EmptyVector) = nothing

struct LazyMap{T, VT}
    f::Function
    data::VT
end
function LazyMap{T}(f::Function, data) where {T}
    return LazyMap{T, typeof(data)}(f, data)
end
Base.size(it::LazyMap) = size(it.data)
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

struct LazyFilter{T, VT}
    f::Function
    data::VT
end
LazyFilter(f::Function, data) = LazyFilter{eltype(data), typeof(data)}(f, data)
function Base.iterate(it::LazyFilter, args...)
    elem_state = iterate(it.data, args...)
    while elem_state !== nothing && !it.f(elem_state[1])
        elem_state = iterate(it.data, elem_state[2])
    end
    if elem_state === nothing
        return nothing
    else
        return elem_state
    end
end
Base.IteratorSize(::LazyFilter) = Base.SizeUnknown()
Base.eltype(::LazyFilter{T}) where {T} = T

struct LazyCat{VT}
    data::VT
end
function _iterate(it::LazyCat, i, state)
    while i < length(it.data) && state === nothing
        i += 1
        state = iterate(it.data[i])
    end
    if state === nothing
        return nothing
    else
        return state[1], (i, state[2])
    end
end
Base.iterate(it::LazyCat) = _iterate(it, 0, nothing)
function Base.iterate(it::LazyCat, state)
    i = state[1]
    return _iterate(it, i, iterate(it.data[i], state[2]))
end
Base.IteratorSize(::LazyCat) = Base.SizeUnknown()
Base.IteratorEltype(::LazyCat) = Base.EltypeUnknown()
