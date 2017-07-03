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
