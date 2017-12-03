# Variables

"""
    addvariables!(instance::AbstractInstance, n::Int)::Vector{VariableIndex}

Add `n` scalar variables to the instance, returning a vector of variable indices.
"""
function addvariables! end

"""
    addvariable!(instance::AbstractInstance)::VariableIndex

Add a scalar variable to the instance, returning a variable index.
"""
function addvariable! end
