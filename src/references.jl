# References

"""
    ConstraintReference{F,S}

A lightweight object used to reference `F`-in-`S` constraints in an instance.
The parameter `F` is the type of the function in the constraint, and the parameter `S` is the type of set in the constraint.

The `instance` field is used to help validate the ownership of the reference by a
particular instance.
"""
struct ConstraintReference{F,S}
    instance::UInt64
    value::UInt64
end

"""
    VariableReference

A lightweight object used to reference variables in an instance.

The `instance` field is used to help validate the ownership of the reference by a
particular instance.
"""
struct VariableReference
    instance::UInt64
    value::UInt64
end

const AnyReference = Union{ConstraintReference,VariableReference}

"""
    candelete(instance::AbstractInstance, ref::AnyReference)::Bool

Return a `Bool` indicating whether the object referred to by `ref` can be removed from the instance `instance`.
"""
candelete(instance::AbstractInstance, ref::AnyReference) = false

"""
    isvalid(instance::AbstractInstance, ref::AnyReference)::Bool

Return a `Bool` indicating whether this reference refers to a valid object in the instance `instance`.
"""
isvalid(instance::AbstractInstance, ref::AnyReference) = false

"""
    delete!(instance::AbstractInstance, ref::AnyReference)

Delete the referenced object from the instance.

    delete!{R}(instance::AbstractInstance, refs::Vector{R<:AnyReference})

Delete the referenced objects in the vector `refs` from the instance.
It may be assumed that `R` is a concrete type.
"""
Base.delete!(instance::AbstractInstance, ref::AnyReference) = throw(MethodError(Base.delete!, (instance, ref)))
Base.delete!(instance::AbstractInstance, refs::Vector{<:AnyReference}) = throw(MethodError(Base.delete!, (instance, refs)))
