# Variables

"""
    struct CannotAddVariable <: CannotError
        message::String # Human-friendly explanation why the attribute cannot be set
    end

An error indicating that variables cannot be added in the current state of the
model.
"""
struct CannotAddVariable <: CannotError
    message::String # Human-friendly explanation why the attribute cannot be set
end

operation_name(::CannotAddVariable) = "Adding variables"
message(err::CannotAttConstraint) = err.message

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
