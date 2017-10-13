# Objectives

"""
    modifyobjective!(m::AbstractInstance, change::AbstractFunctionModification)

Apply the modification specified by `change` to the objective function of `m`.
To change the function completely, call `setobjective!` instead.

### Examples

```julia
modifyobjective!(m, ScalarConstantChange(10.0))
```
"""
function modifyobjective! end

"""
    canmodifyobjective(m::AbstractInstance, change::AbstractFunctionModification)::Bool

Return a `Bool` indicating whether it is possible to apply the modification
specified by `change` to the objective function of `m`.

### Examples

```julia
canmodifyobjective(m, ScalarConstantChange(10.0))
```
"""
function canmodifyobjective end
canmodifyobjective(m::AbstractSolverInstance, change) = false
