"""
    struct CannotModifyConstraint{F<:AbstractFunction, S<:AbstractSet,
                                             C<:AbstractFunctionModification} <: CannotError
        change::C
        message::String
    end

An error indicating that constraints of type `F`-in-`S` do not support the
constraint modification `change`.
"""
struct CannotModifyConstraint{F<:AbstractFunction, S<:AbstractSet,
                                         C<:AbstractFunctionModification} <: CannotError
    change::C
    message::String
end
function CannotModifyConstraint{F, S}(change::AbstractFunctionModification) where {F<:AbstractFunction, S<:AbstractSet}
    CannotModifyConstraint{F, S, typeof(change)}(change, "")
end

operation_name(err::CannotModifyConstraint{F, S}) where {F, S} = "Modifying `$F`-in-`$S` constraints with $(err.change)"

"""
    struct CannotModifyObjective{C<:AbstractFunctionModification} <: CannotError
        change::C
        message::String
    end

An error indicating that the objective function does not support the constraint
modification `change`.
"""
struct CannotModifyObjective{C<:AbstractFunctionModification} <: CannotError
    change::C
    message::String
end

operation_name(err::CannotModifyObjective) = "Modifying the objective function with $(err.change)"

"""
## Constraint Function

    modify!(model::ModelLike, ci::ConstraintIndex, change::AbstractFunctionModification)

Apply the modification specified by `change` to the function of constraint `ci`.

An [`CannotModifyConstraint`](@ref) error is thrown if modifying
constraints is not supported by the model `model`.

### Examples

```julia
modify!(model, ci, ScalarConstantChange(10.0))
```

## Objective Function

    modify!(model::ModelLike, ::ObjectiveFunction, change::AbstractFunctionModification)

Apply the modification specified by `change` to the objective function of
`model`. To change the function completely, call `set!` instead.

An [`CannotModifyObjective`](@ref) error is thrown if modifying
objectives is not supported by the model `model`.

### Examples

```julia
modify!(model, ObjectiveFunction{ScalarAffineFunction{Float64}}(), ScalarConstantChange(10.0))
```
"""
function modify! end

function modify!(model::ModelLike, ci::ConstraintIndex{F, S},
                 change::AbstractFunctionModification) where {F, S}
    throw(CannotModifyConstraint{F, S}(change))
end

function modify!(model::ModelLike, ::ObjectiveFunction,
                 change::AbstractFunctionModification)
    throw(CannotModifyObjective(change))
end
