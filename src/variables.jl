# Variables

"""
    canaddvariable(instance::AbstractInstance)::Bool

Return a `Bool` indicating whether it is possible to add a variable to the instance `instance`.
"""
function canaddvariable end

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
