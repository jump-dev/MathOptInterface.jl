# Variables

"""
    canaddvariable(model::ModelLike)::Bool

Return a `Bool` indicating whether it is possible to add a variable to the model `model`.
"""
function canaddvariable end

"""
    addvariables!(model::ModelLike, n::Int)::Vector{VariableIndex}

Add `n` scalar variables to the model, returning a vector of variable indices.
"""
function addvariables! end

"""
    addvariable!(model::ModelLike)::VariableIndex

Add a scalar variable to the model, returning a variable index.
"""
function addvariable! end
