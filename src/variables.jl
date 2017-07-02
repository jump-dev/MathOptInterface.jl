# Variables

"""
    addvariables!(m::AbstractSolverInstance, n::Int)::Vector{VariableReference}

Add `n` scalar variables to the solver instance, returning a vector of variable references.
"""
function addvariables! end

"""
    addvariable!(m::AbstractSolverInstance)::VariableReference

Add a scalar variable to the solver instance, returning a variable reference.
In addition, there is a special case for adding variables to existing linear problems.

    addvariable!(m::AbstractSolverInstance, cref::Vector{Union{AffineConstraintRef{NonPositive}, AffineConstraintRef{NonNegative}, AffineConstraintRef{Zero}, AffineConstraintRef{Interval}}}, coefs)::VariableReference

Add a variable with coefficients specified by `coefs` in the existing affine constraints given by the constraint references `cref`.
To add a variable with coefficients in a constraint that is not listed here, use `addvariable!(m)` and then `modifyconstraint!` instead.
"""
function addvariable! end
