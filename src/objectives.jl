# Objectives

"""
    modifyobjective!(model::ModekLike, change::AbstractFunctionModification)

Apply the modification specified by `change` to the objective function of `model`.
To change the function completely, call `setobjective!` instead.

### Examples

```julia
modifyobjective!(model, ScalarConstantChange(10.0))
```
"""
function modifyobjective! end

"""
    canmodifyobjective(model::ModelLike, ::Type{M})::Bool where M<:AbstractFunctionModification

Return a `Bool` indicating whether it is possible to apply a modification of type `M` to the objective function of model `model`.

### Examples

```julia
canmodifyobjective(model, ScalarConstantChange{Float64})
```
"""
function canmodifyobjective end
canmodifyobjective(model::ModelLike, change) = false
