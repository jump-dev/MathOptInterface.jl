# References

"""
    VariablewiseConstraintReference{T}

A lightweight object used to reference variablewise constraints in a solver instance.
The parameter `T` is the type of set constraint referenced.
"""
struct VariablewiseConstraintReference{T}
    value::UInt64
end

"""
    AffineConstraintReference{T}

A lightweight object used to reference affine-in-set constraints in a solver instance.
The parameter `T` is the type of set constraint referenced.
"""
struct AffineConstraintReference{T}
    value::UInt64
end

"""
    QuadraticConstraintReference{T}

A lightweight object used to reference quadratic-in-set constraints in a solver instance.
The parameter `T` is the type of set constraint referenced.
"""
struct QuadraticConstraintReference{T}
    value::UInt64
end

const ConstraintReference = Union{VariablewiseConstraintReference, AffineConstraintReference, QuadraticConstraintReference}

"""
    candelete(m::AbstractSolverInstance, ref::ConstraintReference)::Bool

Return a `Bool` indicating whether this constraint can be removed from the solver instance `m`.
"""
candelete(m::AbstractSolverInstance, ref::ConstraintReference) = throw(MethodError())

"""
    isvalid(m::AbstractSolverInstance, ref::ConstraintReference)::Bool

Return a `Bool` indicating whether this reference is valid for an active constraint in the solver instance `m`.
"""
isvalid(m::AbstractSolverInstance, ref::ConstraintReference) = throw(MethodError())

"""
    delete!(m::AbstractSolverInstance, ref::ConstraintReference)

Delete the referenced constraint from the solver instance.

    delete!(m::AbstractSolverInstance, refs::Vector{ConstraintReference})

Delete the referenced constraints in the vector `refs` from the solver instance.
"""
Base.delete!(m::AbstractSolverInstance, ref::ConstraintReference) = throw(MethodError())
Base.delete!(m::AbstractSolverInstance, refs::Vector{ConstraintReference}) = throw(MethodError())

"""
    VariableReference

A lightweight object used to reference variables in a solver instance.
"""
struct VariableReference
    value::UInt64
end

"""
    candelete(m::AbstractSolverInstance, ref::VariableReference)::Bool

Return a `Bool` indicating whether this variable can be removed from the solver instance `m`.
"""
candelete(m::AbstractSolverInstance, ref::VariableReference) = throw(MethodError())

"""
    isvalid(m::AbstractSolverInstance, ref::VariableReference)::Bool

Return a `Bool` indicating whether this reference is valid for an active variable in the solver instance `m`.
"""
isvalid(m::AbstractSolverInstance, ref::VariableReference) = throw(MethodError())

"""
    delete!(m::AbstractSolverInstance, ref::VariableReference)

Delete the referenced variable from the solver instance.

    delete!(m::AbstractSolverInstance, refs::Vector{VariableReference})

Delete the referenced variables in the vector `refs` from the solver instance.
"""
Base.delete!(m::AbstractSolverInstance, ref::VariableReference) = throw(MethodError())
Base.delete!(m::AbstractSolverInstance, refs::Vector{VariableReference}) = throw(MethodError())
