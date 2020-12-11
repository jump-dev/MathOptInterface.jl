# Index types

"""
    ConstraintIndex{F, S}

A type-safe wrapper for `Int64` for use in referencing `F`-in-`S` constraints in
a model.
The parameter `F` is the type of the function in the constraint, and the
parameter `S` is the type of set in the constraint. To allow for deletion,
indices need not be consecutive. Indices within a constraint type (i.e. `F`-in-`S`)
must be unique, but non-unique indices across different constraint types are allowed.
If `F` is [`SingleVariable`](@ref) then the index is equal to the index of the
variable. That is for an `index::ConstraintIndex{SingleVariable}`, we always
have
```julia
index.value == MOI.get(model, MOI.ConstraintFunction(), index).variable.value
```
"""
struct ConstraintIndex{F,S}
    value::Int64
end
index_value(ci::ConstraintIndex) = ci.value

"""
    VariableIndex

A type-safe wrapper for `Int64` for use in referencing variables in a model.
To allow for deletion, indices need not be consecutive.
"""
struct VariableIndex
    value::Int64
end
index_value(vi::VariableIndex) = vi.value

# The default hash is slow. It's important for the performance of dictionaries
# of VariableIndices to define our own.
# https://github.com/JuliaLang/julia/issues/10208
Base.hash(v::VariableIndex, h::UInt) = hash(v.value, h)

# No need to define isequal because the default matches our implementation of
# hash.

const Index = Union{ConstraintIndex,VariableIndex}

"""
    struct InvalidIndex{IndexType<:Index} <: Exception
        index::IndexType
    end

An error indicating that the index `index` is invalid.
"""
struct InvalidIndex{IndexType<:Index} <: Exception
    index::IndexType
end

function Base.showerror(io::IO, err::InvalidIndex)
    return print("The index $(err.index) is invalid. Note that an index becomes invalid after it has been deleted.")
end

"""
    is_valid(model::ModelLike, index::Index)::Bool

Return a `Bool` indicating whether this index refers to a valid object in the model `model`.
"""
is_valid(model::ModelLike, ref::Index) = false

"""
    throw_if_not_valid(model::ModelLike, index::Index)

Throw an `InvalidIndex(index)` error if `MOI.is_valid(model, index)` returns
`false`.
"""
function throw_if_not_valid(model::ModelLike, index::Index)
    if !is_valid(model, index)
        throw(InvalidIndex(index))
    end
end

"""
    struct DeleteNotAllowed{IndexType <: Index} <: NotAllowedError
        index::IndexType
        message::String
    end

An error indicating that the index `index` cannot be deleted.
"""
struct DeleteNotAllowed{IndexType<:Index} <: NotAllowedError
    index::IndexType
    message::String
end
DeleteNotAllowed(index::Index) = DeleteNotAllowed(index, "")

function operation_name(err::DeleteNotAllowed)
    return "Deleting the index $(err.index)"
end

"""
    delete(model::ModelLike, index::Index)

Delete the referenced object from the model. Throw [`DeleteNotAllowed`](@ref) if
if `index` cannot be deleted.

The following modifications also take effect if `Index` is [`VariableIndex`](@ref):
* If `index` used in the objective function, it is removed from the function,
  i.e., it is substituted for zero.
* For each `func`-in-`set` constraint of the model:
  - If `func isa SingleVariable` and `func.variable == index` then the
    constraint is deleted.
  - If `func isa VectorOfVariables` and `index in func.variable` then
    * if `length(func.variable) == 1` is one, the constraint is deleted;
    * if `length(func.variable) > 1` and `supports_dimension_update(set)` then
      then the variable is removed from `func` and `set` is replaced by
      `update_dimension(set, MOI.dimension(set) - 1)`.
    * Otherwise, a [`DeleteNotAllowed`](@ref) error is thrown.
  - Otherwise, the variable is removed from `func`, i.e., it is substituted for
    zero.
"""
delete(model::ModelLike, index::Index) = throw(DeleteNotAllowed(index))

"""
    delete(model::ModelLike, indices::Vector{R<:Index}) where {R}

Delete the referenced objects in the vector `indices` from the model.
It may be assumed that `R` is a concrete type. The default fallback sequentially
deletes the individual items in `indices`, although specialized implementations
may be more efficient.
"""
function delete(model::ModelLike, indices::Vector{<:Index})
    for index in indices
        delete(model, index)
    end
end
