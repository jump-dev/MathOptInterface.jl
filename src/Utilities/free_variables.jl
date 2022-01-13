"""
    mutable struct FreeVariables <: MOI.ModelLike
        n::Int64
        FreeVariables() = new(0)
    end

A struct for storing free variables that can be used as the `variables` field
of [`GenericModel`](@ref) or [`GenericModel`](@ref).
"""
mutable struct FreeVariables <: MOI.ModelLike
    n::Int64
    FreeVariables() = new(0)
end

function MOI.empty!(model::FreeVariables)
    model.n = 0
    return
end

MOI.is_empty(model::FreeVariables) = iszero(model.n)

function MOI.add_variable(model::FreeVariables)
    model.n += 1
    return MOI.VariableIndex(model.n)
end

function MOI.get(model::FreeVariables, ::MOI.ListOfVariableIndices)
    return MOI.VariableIndex[MOI.VariableIndex(i) for i in 1:model.n]
end

function MOI.is_valid(model::FreeVariables, vi::MOI.VariableIndex)
    return 1 <= vi.value <= model.n
end

MOI.get(model::FreeVariables, ::MOI.NumberOfVariables) = model.n

function MOI.get(model::FreeVariables, ::MOI.ListOfConstraintTypesPresent)
    return Tuple{Type,Type}[]
end
