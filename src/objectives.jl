# Objectives

"""
    modifyobjective!(instance::AbstractInstance, change::AbstractFunctionModification)

Apply the modification specified by `change` to the objective function of `instance`.
To change the function completely, call `setobjective!` instead.

### Examples

```julia
modifyobjective!(instance, ScalarConstantChange(10.0))
```
"""
function modifyobjective! end

"""
    canmodifyobjective(instance::AbstractInstance, ::Type{M})::Bool where M<:AbstractFunctionModification

Return a `Bool` indicating whether it is possible to apply a modification of type `M` to the objective function of instance `instance`.

### Examples

```julia
canmodifyobjective(instance, ScalarConstantChange{Float64})
```
"""
function canmodifyobjective end
canmodifyobjective(instance::AbstractInstance, change) = false
