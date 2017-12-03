# Index types

"""
    ConstraintIndex{F,S}

A type-safe wrapper for `Int64` for use in referencing `F`-in-`S` constraints in an instance.
The parameter `F` is the type of the function in the constraint, and the parameter `S` is the type of set in the constraint.
To allow for deletion, indices need not be consecutive.
"""
struct ConstraintIndex{F,S}
    value::Int64
end

"""
    VariableIndex

A type-safe wrapper for `Int64` for use in referencing variables in an instance.
To allow for deletion, indices need not be consecutive.
"""
struct VariableIndex
    value::Int64
end

const Index = Union{ConstraintIndex,VariableIndex}

"""
    candelete(instance::AbstractInstance, index::Index)::Bool

Return a `Bool` indicating whether the object referred to by `index` can be removed from the instance `instance`.
"""
candelete(instance::AbstractInstance, ref::Index) = false

"""
    isvalid(instance::AbstractInstance, index::Index)::Bool

Return a `Bool` indicating whether this index refers to a valid object in the instance `instance`.
"""
isvalid(instance::AbstractInstance, ref::Index) = false

"""
    delete!(instance::AbstractInstance, index::Index)

Delete the referenced object from the instance.

    delete!{R}(instance::AbstractInstance, indices::Vector{R<:Index})

Delete the referenced objects in the vector `indices` from the instance.
It may be assumed that `R` is a concrete type.
"""
Base.delete!(instance::AbstractInstance, index::Index) = throw(MethodError(Base.delete!, (instance, index)))
Base.delete!(instance::AbstractInstance, indices::Vector{<:Index}) = throw(MethodError(Base.delete!, (instance, indices)))
