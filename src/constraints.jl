# Constraints

"""
    addconstraint!(m::AbstractSolverInstance, func::F, set::S)::ConstraintReference{F,S} where {F,S}

Add the constraint ``f(x) \\in \\mathcal{S}`` where ``f`` is defined by `func`, and ``\\mathcal{S}`` is defined by `set`.

"""
function addconstraint! end

"""
    addconstraints!(m::AbstractSolverInstance, funcs::Vector{F}, sets::Vector{S})::Vector{ConstraintReference{F,S}} where {F,S}

Add the set of constraints specified by each function-set pair in `funcs` and `sets`. `F` and `S` should be concrete types.
This call is equivalent to `addconstraint!.(m, funcs, sets)` but may be more efficient.
"""
function addconstraints! end

# default fallback
addconstraints!(m::AbstractSolverInstance, funcs, sets) = addconstraint!.(m, funcs, sets)

"""
## Modify Function

    canmodifyconstraint(m::AbstractSolverInstance, c::ConstraintReference{F,S}, func::F)::Bool

Return a `Bool` indicating whether it is possible to replace the function in constraint `c` with `func`. `F` must match the original function type used to define the constraint.

### Examples

If `c` is a `ConstraintReference{ScalarAffineFunction,S}` and `v1` and `v2` are `VariableReference` objects,

```julia
canmodifyconstraint(m, c, ScalarAffineFunction([v1,v2],[1.0,2.0],5.0))
canmodifyconstraint(m, c, SingleVariable(v1)) # false
```

## Modify Set

    canmodifyconstraint(m::AbstractSolverInstance, c::ConstraintReference{F,S}, set::S)::Bool

Return a `Bool` indicating whether it is possible to change the set of constraint `c` to the new set `set` which should be of the same type as the original set.

### Examples

If `c` is a `ConstraintReference{F,Interval}`

```julia
canmodifyconstraint(m, c, Interval(0, 5))
canmodifyconstraint(m, c, NonPositives) # false
```

## Partial Modifications

    canmodifyconstraint(m::AbstractSolverInstance, c::ConstraintReference, change::AbstractFunctionModification)::Bool

Return a `Bool` indicating whether it is possible to apply the modification specified by `change` to the function of constraint `c`.

### Examples

```julia
canmodifyconstraint(m, c, ScalarConstantChange(10.0))
```
"""
function canmodifyconstraint end
canmodifyconstraint(m::AbstractSolverInstance, c::ConstraintReference, change) = false

"""
## Modify Function

    modifyconstraint!(m::AbstractSolverInstance, c::ConstraintReference{F,S}, func::F)

Replace the function in constraint `c` with `func`. `F` must match the original function type used to define the constraint.

### Examples

If `c` is a `ConstraintReference{ScalarAffineFunction,S}` and `v1` and `v2` are `VariableReference` objects,

```julia
modifyconstraint!(m, c, ScalarAffineFunction([v1,v2],[1.0,2.0],5.0))
modifyconstraint!(m, c, SingleVariable(v1)) # Error
```

## Modify Set

    modifyconstraint!(m::AbstractSolverInstance, c::ConstraintReference{F,S}, set::S)

Change the set of constraint `c` to the new set `set` which should be of the same type as the original set.

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
