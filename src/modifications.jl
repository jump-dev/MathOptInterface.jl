"""
    struct UnsupportedConstraintModification{F<:AbstractFunction, S<:AbstractSet,
                                             C<:AbstractFunctionModification} <: UnsupportedError
        change::C
    end

Constraints of type `F`-in-`S` do not support the constraint modification `change`.
"""
struct UnsupportedConstraintModification{F<:AbstractFunction, S<:AbstractSet,
                                         C<:AbstractFunctionModification} <: UnsupportedError
    change::C
end
function UnsupportedConstraintModification{F, S}(change::AbstractFunctionModification) where {F<:AbstractFunction, S<:AbstractSet}
    UnsupportedConstraintModification{F, S, typeof(change)}(change)
end

operation_name(err::UnsupportedConstraintModification{F, S}) where {F, S} = "Modifying `$F`-in-`$S` constraints with $(err.change)"

"""
    struct UnsupportedObjectiveModification{C<:AbstractFunctionModification} <: UnsupportedError
        change::C
    end

The objective function dos not support the constraint modification `change`.
"""
struct UnsupportedObjectiveModification{C<:AbstractFunctionModification} <: UnsupportedError
    change::C
end

operation_name(err::UnsupportedObjectiveModification) = "Modifying the objective function with $(err.change)"

"""
## Constraint Function

    modify!(model::ModelLike, ci::ConstraintIndex, change::AbstractFunctionModification)

Apply the modification specified by `change` to the function of constraint `ci`.

### Examples

```julia
modify!(model, ci, ScalarConstantChange(10.0))
```

## Objective Function

    modify!(model::ModelLike, ::ObjectiveFunction, change::AbstractFunctionModification)

Apply the modification specified by `change` to the objective function of
`model`. To change the function completely, call `set!` instead.

### Examples

```julia
modify!(model, ObjectiveFunction{ScalarAffineFunction{Float64}}(), ScalarConstantChange(10.0))
```
"""
function modify! end

function modify!(model::ModelLike, ci::ConstraintIndex{F, S},
                 change::AbstractFunctionModification) where {F, S}
    throw(UnsupportedConstraintModification{F, S}(change))
end

function modify!(model::ModelLike, ::ObjectiveFunction,
                 change::AbstractFunctionModification)
    throw(UnsupportedObjectiveModification(change))
end
