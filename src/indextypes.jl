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
struct ConstraintIndex{F, S}
    value::Int64
end

"""
    VariableIndex

A type-safe wrapper for `Int64` for use in referencing variables in a model.
To allow for deletion, indices need not be consecutive.
"""
struct VariableIndex
    value::Int64
end

# The default hash is slow. It's important for the performance of dictionaries
# of VariableIndices to define our own.
# https://github.com/JuliaLang/julia/issues/10208
Base.hash(v::VariableIndex, h::UInt) = hash(v.value, h)

"""
    AttributeIndex{AttrType}

A type-safe wrapper for `Int64` for use in referencing elements added for
attribute of type `AttrType`.
"""
struct AttributeIndex{AttrType}
    attr::AttrType
    value::Int64
end

# No need to define isequal because the default matches our implementation of
# hash.

const Index = Union{ConstraintIndex, VariableIndex, AttributeIndex}

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
    print("The index $(err.index) is invalid. Note that an index becomes invalid after it has been deleted.")
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
struct DeleteNotAllowed{IndexType <: Index} <: NotAllowedError
    index::IndexType
    message::String
end
DeleteNotAllowed(index::Index) = DeleteNotAllowed(index, "")

function operation_name(err::DeleteNotAllowed)
    return "Deleting the index $(err.index)"
end

"""
    delete(model::ModelLike, index::Index)

Delete the referenced object from the model.
"""
delete(model::ModelLike, index::Index) = throw(DeleteNotAllowed(index))

"""
    delete{R}(model::ModelLike, indices::Vector{R<:Index})

Delete the referenced objects in the vector `indices` from the model.
It may be assumed that `R` is a concrete type.
"""
function delete(model::ModelLike, indices::Vector{<:Index})
    for index in indices
        delete(model, index)
    end
end
