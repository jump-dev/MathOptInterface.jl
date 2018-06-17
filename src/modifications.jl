"""
## Constraint Function

    canmodify(model::ModelLike, ::Type{CI}, ::Type{M})::Bool where CI<:ConstraintIndex where M<:AbstractFunctionModification

Return a `Bool` indicating whether it is possible to apply a modification of
type `M` to the function of constraint of type `CI`.

### Examples

```julia
canmodify(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}}, ScalarConstantChange{Float64})
```

## Objective Function

    canmodify(model::ModelLike, ::ObjectiveFunction, ::Type{M})::Bool where M<:AbstractFunctionModification

Return a `Bool` indicating whether it is possible to apply a modification of
type `M` to the objective function of model `model`.

### Examples

```julia
canmodify(model, ObjectiveFunction{ScalarAffineFunction{Float64}}(), ScalarConstantChange{Float64})
```
"""
function canmodify end
canmodify(model::ModelLike, ref, change) = false

"""
## Constraint Function

    modify!(model::ModelLike, c::ConstraintIndex, change::AbstractFunctionModification)

Apply the modification specified by `change` to the function of constraint `c`.

### Examples

```julia
modify!(model, c, ScalarConstantChange(10.0))
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
