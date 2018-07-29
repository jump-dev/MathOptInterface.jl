# Variables

"""
    CannotAddVariable <: CannotError

Variables cannot be added in the current state of the model.
"""
struct CannotAddVariable <: CannotError end

operation_name(::CannotAddVariable) = "Adding variables"

"""
    addvariables!(model::ModelLike, n::Int)::Vector{VariableIndex}

Add `n` scalar variables to the model, returning a vector of variable indices.
"""
addvariables!(model::ModelLike, n) = throw(CannotAddVariable())

"""
    addvariable!(model::ModelLike)::VariableIndex

Add a scalar variable to the model, returning a variable index.
"""
addvariable!(model::ModelLike) = throw(CannotAddVariable())
