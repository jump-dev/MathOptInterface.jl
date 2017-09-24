# Variables

"""
    addvariables!(m::AbstractInstance, n::Int)::Vector{VariableReference}

Add `n` scalar variables to the instance, returning a vector of variable references.
"""
function addvariables! end

"""
    addvariable!(m::AbstractInstance)::VariableReference

Add a scalar variable to the instance, returning a variable reference.
"""
function addvariable! end
