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

"""
    add_constrained_variable(
        model::ModelLike,
        set::AbstractScalarSet
    )::Tuple{MOI.VariableIndex,
             MOI.ConstraintIndex{MOI.SingleVariable, typeof(set)}}

Add to `model` a scalar variable constrained to belong to `set`, returning the
index of the variable created and the index of the constraint constraining the
variable to belong to `set`.

By default, this function falls back to creating a free variable with
[`add_variable`](@ref) and then constraining it to belong to `set` with
[`add_constraint`](@ref).
"""
function add_constrained_variable(model::ModelLike, set::AbstractScalarSet)
    variable = add_variable(model)
    constraint = add_constraint(model, SingleVariable(variable), set)
    return variable, constraint
end

"""
    add_constrained_variables(
        model::ModelLike,
        sets:AbstractVector{<:AbstractScalarSet}
    )::Tuple{Vector{MOI.VariableIndex},
             Vector{MOI.ConstraintIndex{MOI.SingleVariable, eltype(sets)}}}

Add to `model` scalar variables constrained to belong to `sets`, returning the
indices of the variables created and the indices of the constraints constraining
the variables to belong to each set in `sets`. That is, if it returns `variables`
and `constraints`, `constraints[i]` is the index of the constraint constraining
`variable[i]` to belong to `sets[i]`.

By default, this function falls back to calling
[`add_constrained_variable`](@ref) on each set.
"""
function add_constrained_variables(model::ModelLike, sets::AbstractVector{<:AbstractScalarSet})
    variables = Vector{VariableIndex}(undef, length(sets))
    constraints = Vector{ConstraintIndex{SingleVariable, eltype(sets)}}(undef, length(sets))
    for (i, set) in enumerate(sets)
        variables[i], constraints[i] = add_constrained_variable(model, set)
    end
    return variables, constraints
end

"""
    add_constrained_variables(
        model::ModelLike,
        set::AbstractVectorSet
    )::Tuple{Vector{MOI.VariableIndex},
             MOI.ConstraintIndex{MOI.VectorOfVariables, typeof(set)}}

Add to `model` a vector of variables constrained to belong to `set`, returning
the indices of the variables created and the index of the constraint
constraining the vector of variables to belong to `set`.

By default, this function falls back to creating free variables with
[`add_variables`](@ref) and then constraining it to belong to `set` with
[`add_constraint`](@ref). Therefore, there is no need to implement this function
if `model` supports creating free variables and supports transforming free
variables into variables in `set`. However,

* if `model` does not support creating variables, it should only implement
  `add_constrained_variables` and not [`add_variable`](@ref) nor
  [`add_constraint`](@ref) for [`VectorOfVariables`](@ref)-in-`typeof(set)`.
  In addition, it should implement `supports_constraint(::Optimizer,
  ::Type{VectorOfVariables}, ::Type{Reals})` and return `false` so that free
  variables are bridged, see [`supports_constraint`](@ref).
* if `model` supports free variables but does not support transforming free
  variables into variables in `set`, then it should implement both
  [`add_variable`](@ref) and `add_constraint_variables` but should not implement
  any method for [`add_constraint`](@ref) for
  [`VectorOfVariables`](@ref)-in-`typeof(set)`.
"""
function add_constrained_variables(model::ModelLike, set::AbstractVectorSet)
    variables = add_variables(model, dimension(set))
    constraint = add_constraint(model, VectorOfVariables(variables), set)
    return variables, constraint
end
