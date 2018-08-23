# Variables

"""
    struct AddVariableNotAllowed <: NotAllowedError
        message::String # Human-friendly explanation why the attribute cannot be set
    end

An error indicating that variables cannot be added to the model.
"""
struct AddVariableNotAllowed <: NotAllowedError
    message::String # Human-friendly explanation why the attribute cannot be set
end
AddVariableNotAllowed() = AddVariableNotAllowed("")

operation_name(::AddVariableNotAllowed) = "Adding variables"

"""
    add_variables(model::ModelLike, n::Int)::Vector{VariableIndex}

Add `n` scalar variables to the model, returning a vector of variable indices.

A [`AddVariableNotAllowed`](@ref) error is thrown if adding variables cannot be
done in the current state of the model `model`.
"""
add_variables(model::ModelLike, n) = throw(AddVariableNotAllowed())

"""
    add_variable(model::ModelLike)::VariableIndex

Add a scalar variable to the model, returning a variable index.

A [`AddVariableNotAllowed`](@ref) error is thrown if adding variables cannot be
done in the current state of the model `model`.
"""
add_variable(model::ModelLike) = throw(AddVariableNotAllowed())
