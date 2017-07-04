# References

"""
    ConstraintReference{F,S}

A lightweight object used to reference `F`-in-`S` constraints in a solver instance.
The parameter `F` is the type of the function in the constraint, and the parameter `S` is the type of set in the constraint.
"""
struct ConstraintReference{F,S}
    value::UInt64
end

"""
    VariableReference

A lightweight object used to reference variables in a solver instance.
"""
struct VariableReference
    value::UInt64
end

const AnyReference = Union{ConstraintReference,VariableReference}

"""
    candelete(m::AbstractSolverInstance, ref::AnyReference)::Bool

Return a `Bool` indicating whether the object referred to by `ref` can be removed from the solver instance `m`.
"""
candelete(m::AbstractSolverInstance, ref::AnyReference) = throw(MethodError())

"""
    isvalid(m::AbstractSolverInstance, ref::AnyReference)::Bool

Return a `Bool` indicating whether this reference refers to a valid object in the solver instance `m`.
"""
isvalid(m::AbstractSolverInstance, ref::AnyReference) = throw(MethodError())

"""
    delete!(m::AbstractSolverInstance, ref::AnyReference)

Delete the referenced object from the solver instance.

    delete!(m::AbstractSolverInstance, refs::Vector{<:AnyReference})

Delete the referenced objects in the vector `refs` from the solver instance.
It may be assumed that `R` is a concrete type.
"""
Base.delete!(m::AbstractSolverInstance, ref::AnyReference) = throw(MethodError())
Base.delete!(m::AbstractSolverInstance, refs::Vector{<:AnyReference}) = throw(MethodError())
