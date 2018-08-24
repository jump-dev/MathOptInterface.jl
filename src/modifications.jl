"""
    struct ModifyConstraintNotAllowed{F<:AbstractFunction, S<:AbstractSet,
                                             C<:AbstractFunctionModification} <: NotAllowedError
        constraint_index::ConstraintIndex{F, S}
        change::C
        message::String
    end

An error indicating that the constraint modification `change` cannot be applied
to the constraint of index `ci`.
"""
struct ModifyConstraintNotAllowed{F<:AbstractFunction, S<:AbstractSet,
                                         C<:AbstractFunctionModification} <: NotAllowedError
    constraint_index::ConstraintIndex{F, S}
    change::C
    message::String
end
function ModifyConstraintNotAllowed(ci::ConstraintIndex{F, S},
                                change::AbstractFunctionModification) where {F<:AbstractFunction, S<:AbstractSet}
    ModifyConstraintNotAllowed{F, S, typeof(change)}(ci, change, "")
end

operation_name(err::ModifyConstraintNotAllowed{F, S}) where {F, S} = "Modifying the constraints $(err.constraint_index) with $(err.change)"

"""
    struct ModifyObjectiveNotAllowed{C<:AbstractFunctionModification} <: NotAllowedError
        change::C
        message::String
    end

An error indicating that the objective modification `change` cannot be applied
to the objective.
"""
struct ModifyObjectiveNotAllowed{C<:AbstractFunctionModification} <: NotAllowedError
    change::C
    message::String
end
function ModifyObjectiveNotAllowed(change::AbstractFunctionModification)
    ModifyObjectiveNotAllowed(change, "")
end

operation_name(err::ModifyObjectiveNotAllowed) = "Modifying the objective function with $(err.change)"

"""
## Constraint Function

    modify!(model::ModelLike, ci::ConstraintIndex, change::AbstractFunctionModification)

Apply the modification specified by `change` to the function of constraint `ci`.

An [`ModifyConstraintNotAllowed`](@ref) error is thrown if modifying
constraints is not supported by the model `model`.

### Examples

```julia
modify!(model, ci, ScalarConstantChange(10.0))
```

## Objective Function

    modify!(model::ModelLike, ::ObjectiveFunction, change::AbstractFunctionModification)

Apply the modification specified by `change` to the objective function of
`model`. To change the function completely, call `set` instead.

An [`ModifyObjectiveNotAllowed`](@ref) error is thrown if modifying
objectives is not supported by the model `model`.

### Examples

```julia
modify!(model, ObjectiveFunction{ScalarAffineFunction{Float64}}(), ScalarConstantChange(10.0))
```
"""
function modify! end

function modify!(model::ModelLike, ci::ConstraintIndex{F, S},
                 change::AbstractFunctionModification) where {F, S}
    throw(ModifyConstraintNotAllowed(ci, change))
end

function modify!(model::ModelLike, ::ObjectiveFunction,
                 change::AbstractFunctionModification)
    throw(ModifyObjectiveNotAllowed(change))
end
