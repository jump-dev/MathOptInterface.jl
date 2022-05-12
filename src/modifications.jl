# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

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
struct ModifyConstraintNotAllowed{
    F<:AbstractFunction,
    S<:AbstractSet,
    C<:AbstractFunctionModification,
} <: NotAllowedError
    constraint_index::ConstraintIndex{F,S}
    change::C
    message::String
end
function ModifyConstraintNotAllowed(
    ci::ConstraintIndex{F,S},
    change::AbstractFunctionModification,
    message = "",
) where {F<:AbstractFunction,S<:AbstractSet}
    return ModifyConstraintNotAllowed{F,S,typeof(change)}(ci, change, message)
end
function throw_modify_not_allowed(ci::ConstraintIndex, args...)
    return throw(ModifyConstraintNotAllowed(ci, args...))
end

function operation_name(err::ModifyConstraintNotAllowed{F,S}) where {F,S}
    return "Modifying the constraints $(err.constraint_index) with $(err.change)"
end

"""
    struct ModifyObjectiveNotAllowed{C<:AbstractFunctionModification} <: NotAllowedError
        change::C
        message::String
    end

An error indicating that the objective modification `change` cannot be applied
to the objective.
"""
struct ModifyObjectiveNotAllowed{C<:AbstractFunctionModification} <:
       NotAllowedError
    change::C
    message::String
end
function ModifyObjectiveNotAllowed(change::AbstractFunctionModification)
    return ModifyObjectiveNotAllowed(change, "")
end
function throw_modify_not_allowed(::ObjectiveFunction, args...)
    return throw(ModifyObjectiveNotAllowed(args...))
end

function operation_name(err::ModifyObjectiveNotAllowed)
    return "Modifying the objective function with $(err.change)"
end

"""
## Constraint Function

    modify(model::ModelLike, ci::ConstraintIndex, change::AbstractFunctionModification)

Apply the modification specified by `change` to the function of constraint `ci`.

An [`ModifyConstraintNotAllowed`](@ref) error is thrown if modifying
constraints is not supported by the model `model`.

### Examples

```julia
modify(model, ci, ScalarConstantChange(10.0))
```

## Objective Function

    modify(model::ModelLike, ::ObjectiveFunction, change::AbstractFunctionModification)

Apply the modification specified by `change` to the objective function of
`model`. To change the function completely, call `set` instead.

An [`ModifyObjectiveNotAllowed`](@ref) error is thrown if modifying
objectives is not supported by the model `model`.

### Examples

```julia
modify(model, ObjectiveFunction{ScalarAffineFunction{Float64}}(), ScalarConstantChange(10.0))
```

## Multiple modifications in Constraint Functions

    modify(
        model::ModelLike,
        cis::AbstractVector{<:ConstraintIndex},
        changes::AbstractVector{<:AbstractFunctionModification},
    )

Apply multiple modifications specified by `changes` to the functions of constraints `cis`.

A [`ModifyConstraintNotAllowed`](@ref) error is thrown if modifying
constraints is not supported by `model`.

### Examples

```julia
modify(
    model,
    [ci, ci],
    [
        ScalarCoefficientChange{Float64}(VariableIndex(1), 1.0),
        ScalarCoefficientChange{Float64}(VariableIndex(2), 0.5),
    ],
)
```

## Multiple modifications in the Objective Function

    modify(
        model::ModelLike,
        attr::ObjectiveFunction,
        changes::AbstractVector{<:AbstractFunctionModification},
    )

Apply multiple modifications specified by `changes` to the functions of constraints `cis`.

A [`ModifyObjectiveNotAllowed`](@ref) error is thrown if modifying
objective coefficients is not supported by `model`.

### Examples

```julia
modify(
    model,
    ObjectiveFunction{ScalarAffineFunction{Float64}}(),
    [
        ScalarCoefficientChange{Float64}(VariableIndex(1), 1.0),
        ScalarCoefficientChange{Float64}(VariableIndex(2), 0.5),
    ],
)
```
"""
function modify end

function modify(
    model::ModelLike,
    ci::ConstraintIndex,
    change::AbstractFunctionModification,
)
    return throw_modify_not_allowed(ci, change)
end

function modify(
    model::ModelLike,
    cis::AbstractVector{<:ConstraintIndex},
    changes::AbstractVector{<:AbstractFunctionModification},
)
    @assert length(cis) == length(changes)
    for (ci, change) in zip(cis, changes)
        modify(model, ci, change)
    end
    return
end

function modify(
    model::ModelLike,
    attr::ObjectiveFunction,
    change::AbstractFunctionModification,
)
    return throw_modify_not_allowed(attr, change)
end

function modify(
    model::ModelLike,
    attr::ObjectiveFunction,
    changes::AbstractVector{<:AbstractFunctionModification},
)
    for change in changes
        modify(model, attr, change)
    end
    return
end
