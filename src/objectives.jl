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
    canmodifyobjective(instance::AbstractInstance, change::AbstractFunctionModification)::Bool

Return a `Bool` indicating whether it is possible to apply the modification
specified by `change` to the objective function of `instance`.

### Examples

```julia
canmodifyobjective(instance, ScalarConstantChange(10.0))
```
"""
function canmodifyobjective end
canmodifyobjective(instance::AbstractSolverInstance, change) = false
