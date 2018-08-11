# Variables

"""
    struct CannotAddVariable <: CannotTryResetError
        message::String # Human-friendly explanation why the attribute cannot be set
    end

An error indicating that variables cannot be added to the model.
"""
struct CannotAddVariable <: CannotTryResetError
    message::String # Human-friendly explanation why the attribute cannot be set
end
CannotAddVariable() = CannotAddVariable("")

operation_name(::CannotAddVariable) = "Adding variables"

"""
    addvariables!(model::ModelLike, n::Int)::Vector{VariableIndex}

Add `n` scalar variables to the model, returning a vector of variable indices.

A [`CannotAddVariable`](@ref) error is thrown if adding variables cannot be
done in the current state of the model `model`.
"""
addvariables!(model::ModelLike, n) = throw(CannotAddVariable())

"""
    addvariable!(model::ModelLike)::VariableIndex

Add a scalar variable to the model, returning a variable index.

A [`CannotAddVariable`](@ref) error is thrown if adding variables cannot be
done in the current state of the model `model`.
"""
addvariable!(model::ModelLike) = throw(CannotAddVariable())
