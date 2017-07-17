# Variables

"""
    addvariables!(m::AbstractSolverInstance, n::Int)::Vector{VariableReference}

Add `n` scalar variables to the solver instance, returning a vector of variable references.
"""
function addvariables! end

"""
    addvariable!(m::AbstractSolverInstance)::VariableReference

Add a scalar variable to the solver instance, returning a variable reference.
"""
function addvariable! end
