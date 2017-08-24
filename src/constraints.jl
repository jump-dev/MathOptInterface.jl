# Constraints

"""
    addconstraint!(m::AbstractSolverInstance, func::F, set::S)::ConstraintReference{F,S} where {F,S}

Add the constraint ``f(x) \\in \\mathcal{S}`` where ``f`` is defined by `func`, and ``\\mathcal{S}`` is defined by `set`.

    addconstraint!(m::AbstractSolverInstance, v::VariableReference, set::S)::ConstraintReference{SingleVariable,S} where {S}
    addconstraint!(m::AbstractSolverInstance, vec::Vector{VariableReference}, set::S)::ConstraintReference{VectorOfVariables,S} where {S}

Add the constraint ``v \\in \\mathcal{S}`` where ``v`` is the variable (or vector of variables) referenced by `v` and ``\\mathcal{S}`` is defined by `set`.
"""
function addconstraint! end

# fallbacks
addconstraint!(m::AbstractSolverInstance, v::VariableReference, set) = addconstraint!(m, SingleVariable(v), set)
addconstraint!(m::AbstractSolverInstance, v::Vector{VariableReference}, set) = addconstraint!(m, VectorOfVariables(v), set)

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


"""
## Transform Constraint Set

    transformconstraint!(m::AbstractSolverInstance, c::ConstraintReference{F,S1}, newset::S2)::ConstraintReference{F,S2}

Replace the set in constraint `c` with `newset`. The constraint reference `c`
will no longer be valid, and the function returns a new constraint reference.

Solvers may only support a subset of constraint transforms that they perform
efficiently (for example, changing from a `LessThan` to `GreaterThan` set). In
addition, set modification (where `S1 = S2`) should be performed via the
`modifyconstraint!` function.


Typically, the user should delete the constraint and add a new one.

### Examples

If `c` is a `ConstraintReference{ScalarAffineFunction{Float64},LessThan{Float64}}`,

```julia
c2 = transformconstraint!(m, c, GreaterThan(0.0))
transformconstraint!(m, c, LessThan(0.0)) # errors
```
"""
function transformconstraint! end

# default fallback
function transformconstraint!(m::AbstractSolverInstance, c::ConstraintReference, newset)
    f = getattribute(m, ConstraintFunction(), c)
    delete!(m, c)
    addconstraint!(m, f, newset)
end

"""
## Transform Constraint Set

    cantransformconstraint(m::AbstractSolverInstance, c::ConstraintReference{F,S1}, newset::S2)::Bool

Return a `Bool` is the set in constraint `c` can be replaced with `newset`.

### Examples

If `c` is a `ConstraintReference{ScalarAffineFunction{Float64},LessThan{Float64}}`,

```julia
cantransformconstraint(m, c, GreaterThan(0.0)) # true
cantransformconstraint(m, c, ZeroOne())        # false
```
"""
function cantransformconstraint end
cantransformconstraint(m::AbstractSolverInstance, c::ConstraintReference, newset) = false
