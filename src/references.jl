# References

"""
    VariablewiseConstraintReference{T}

A lightweight object used to reference variablewise constraints in a instance.
The parameter `T` is the type of set constraint referenced.
"""
struct VariablewiseConstraintReference{T}
    value::UInt64
end

"""
    AffineConstraintReference{T}

A lightweight object used to reference affine-in-set constraints in a instance.
The parameter `T` is the type of set constraint referenced.
"""
struct AffineConstraintReference{T}
    value::UInt64
end

"""
    QuadraticConstraintReference{T}

A lightweight object used to reference quadratic-in-set constraints in a instance.
The parameter `T` is the type of set constraint referenced.
"""
struct QuadraticConstraintReference{T}
    value::UInt64
end

const ConstraintReference = Union{VariablewiseConstraintReference, AffineConstraintReference, QuadraticConstraintReference}

"""
    candelete(m::AbstractInstance, ref::ConstraintReference)::Bool

Return a `Bool` indicating whether this constraint can be removed from the instance `m`.
"""
candelete(m::AbstractInstance, ref::ConstraintReference) = throw(MethodError())

"""
    isvalid(m::AbstractInstance, ref::ConstraintReference)::Bool

Return a `Bool` indicating whether this reference is valid for an active constraint in the instance `m`.
"""
isvalid(m::AbstractInstance, ref::ConstraintReference) = throw(MethodError())

"""
    delete!(m::AbstractInstance, ref::ConstraintReference)

Delete the referenced constraint from the instance.

    delete!(m::AbstractInstance, refs::Vector{ConstraintReference})

Delete the referenced constraints in the vector `refs` from the instance.
"""
Base.delete!(m::AbstractInstance, ref::ConstraintReference) = throw(MethodError())
Base.delete!(m::AbstractInstance, refs::Vector{ConstraintReference}) = throw(MethodError())

"""
    VariableReference

A lightweight object used to reference variables in a instance.
"""
struct VariableReference
    value::UInt64
end

"""
    candelete(m::AbstractInstance, ref::VariableReference)::Bool

Return a `Bool` indicating whether this variable can be removed from the instance `m`.
"""
candelete(m::AbstractInstance, ref::VariableReference) = throw(MethodError())

"""
    isvalid(m::AbstractInstance, ref::VariableReference)::Bool

Return a `Bool` indicating whether this reference is valid for an active variable in the instance `m`.
"""
isvalid(m::AbstractInstance, ref::VariableReference) = throw(MethodError())

"""
    delete!(m::AbstractInstance, ref::VariableReference)

Delete the referenced variable from the instance.

    delete!(m::AbstractInstance, refs::Vector{VariableReference})

Delete the referenced variables in the vector `refs` from the instance.
"""
Base.delete!(m::AbstractInstance, ref::VariableReference) = throw(MethodError())
Base.delete!(m::AbstractInstance, refs::Vector{VariableReference}) = throw(MethodError())
