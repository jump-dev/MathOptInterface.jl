# Index types

"""
    ConstraintIndex{F,S}

A type-safe wrapper for `Int64` for use in referencing `F`-in-`S` constraints in a model.
The parameter `F` is the type of the function in the constraint, and the parameter `S` is the type of set in the constraint.
To allow for deletion, indices need not be consecutive.
"""
struct ConstraintIndex{F,S}
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

const Index = Union{ConstraintIndex,VariableIndex}

"""
    candelete(model::ModelLike, index::Index)::Bool

Return a `Bool` indicating whether the object referred to by `index` can be removed from the model `model`.
"""
candelete(model::ModelLike, ref::Index) = false

"""
    isvalid(model::ModelLike, index::Index)::Bool

Return a `Bool` indicating whether this index refers to a valid object in the model `model`.
"""
isvalid(model::ModelLike, ref::Index) = false

"""
    delete!(model::ModelLike, index::Index)

Delete the referenced object from the model.

    delete!{R}(model::ModelLike, indices::Vector{R<:Index})

Delete the referenced objects in the vector `indices` from the model.
It may be assumed that `R` is a concrete type.
"""
Base.delete!(model::ModelLike, index::Index) = throw(MethodError(Base.delete!, (model, index)))

candelete(model::ModelLike, indices::Vector{<:Index}) = all(candelete.(model, indices))
function Base.delete!(model::ModelLike, indices::Vector{<:Index})
    for index in indices
        Base.delete!(model, index)
    end
end
