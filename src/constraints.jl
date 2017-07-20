# Constraints

"""
    addconstraint!(m::AbstractSolverInstance, func::F, set::S)::ConstraintReference{F,S}

Add the constraint ``f(x) \\in \\mathcal{S}`` where ``f`` is defined by `func`, and ``\\mathcal{S}`` is defined by `set`.

"""
function addconstraint! end

# TODO: method to query if solver supports this type of modification

"""
## Modify Function

    modifyconstraint!(m::AbstractSolverInstance, c::ConstraintReference, func::F)

Replace the function in constraint `c` with `func`. `F` must match the original function type used to define the constraint.

### Examples

If `c` is a `ConstraintReference{ScalarAffineFunction,S}` and `v1` and `v2` are `VariableReference` objects,

```julia
modifyconstraint!(m, c, ScalarAffineFunction([v1,v2],[1.0,2.0],5.0))
modifyconstraint!(m, c, SingleVariable(v1)) # Error
```

## Modify Set

    modifyconstraint!(m::AbstractSolverInstance, c::ConstraintReference, S::S)

Change the set of constraint `c` to the new set `S` which should be of the same type as the original set.

### Examples

If `c` is a `ConstraintReference{F,Interval}`

```julia
modifyconstraint!(m, c, Interval(0, 5))
modifyconstraint!(m, c, NonPositives) # Error
```

## Partial Modifications

    modifyconstraint!(m::AbstractSolverInstance, c::ConstraintReference, change::AbstractFunctionModification)

Apply the modification specified by `change` to the function of constraint `c`.

### Examples

```julia
modifyconstraint!(m, c, ScalarConstantChange(10.0))
```
"""
function modifyconstraint! end
