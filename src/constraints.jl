# Constraints

"""
    supportsconstraint(model::ModelLike, ::Type{F}, ::Type{S})::Bool where {F<:AbstractFunction,S<:AbstractSet}

Return a `Bool` indicating whether `model` supports `F`-in-`S` constraints, that is,
`copy!(model, src)` does not return `CopyUnsupportedConstraint` when `src` contains `F`-in-`S` constraints.
If `F`-in-`S` constraints are only not supported in specific circumstances, e.g. `F`-in-`S` constraints cannot be combined with another type of constraint, it should still return `true`.
"""
supportsconstraint(model::ModelLike, ::Type{<:AbstractFunction}, ::Type{<:AbstractSet}) = false

"""
    canaddconstraint(model::ModelLike, ::Type{F}, ::Type{S})::Bool where {F<:AbstractFunction,S<:AbstractSet}

Return a `Bool` indicating whether it is possible to add a constraint ``f(x) \\in \\mathcal{S}`` where ``f`` is of type `F`, and ``\\mathcal{S}`` is of type `S`.
"""
canaddconstraint(model::ModelLike, ::Type{<:AbstractFunction}, ::Type{<:AbstractSet}) = false

"""
    addconstraint!(model::ModelLike, func::F, set::S)::ConstraintIndex{F,S} where {F,S}

Add the constraint ``f(x) \\in \\mathcal{S}`` where ``f`` is defined by `func`, and ``\\mathcal{S}`` is defined by `set`.

    addconstraint!(model::ModelLike, v::VariableIndex, set::S)::ConstraintIndex{SingleVariable,S} where {S}
    addconstraint!(model::ModelLike, vec::Vector{VariableIndex}, set::S)::ConstraintIndex{VectorOfVariables,S} where {S}

Add the constraint ``v \\in \\mathcal{S}`` where ``v`` is the variable (or vector of variables) referenced by `v` and ``\\mathcal{S}`` is defined by `set`.
"""
function addconstraint! end

# convenient shorthands TODO: document
addconstraint!(model::ModelLike, v::VariableIndex, set::AbstractScalarSet) = addconstraint!(model, SingleVariable(v), set)
addconstraint!(model::ModelLike, v::Vector{VariableIndex}, set::AbstractVectorSet) = addconstraint!(model, VectorOfVariables(v), set)

"""
    addconstraints!(model::ModelLike, funcs::Vector{F}, sets::Vector{S})::Vector{ConstraintIndex{F,S}} where {F,S}

Add the set of constraints specified by each function-set pair in `funcs` and `sets`. `F` and `S` should be concrete types.
This call is equivalent to `addconstraint!.(model, funcs, sets)` but may be more efficient.
"""
function addconstraints! end

# default fallback
addconstraints!(model::ModelLike, funcs, sets) = addconstraint!.(Ref(model), funcs, sets)

"""
## Modify Function

    canmodifyconstraint(model::ModelLike, c::ConstraintIndex{F,S}, ::Type{F})::Bool

Return a `Bool` indicating whether the function in constraint `c` can be replaced by another function of the same type `F` as the original function.

## Modify Set

    canmodifyconstraint(model::ModelLike, c::ConstraintIndex{F,S}, ::Type{S})::Bool

Return a `Bool` indicating whether the set in constraint `c` can be replaced by another set of the same type `S` as the original set.

## Partial Modifications

    canmodifyconstraint(model::ModelLike, c::ConstraintIndex, ::Type{M})::Bool where M<:AbstractFunctionModification

Return a `Bool` indicating whether it is possible to apply a modification of type `M` to the function of constraint `c`.

### Examples

```julia
canmodifyconstraint(model, c, ScalarConstantChange{Float64})
```
"""
function canmodifyconstraint end
canmodifyconstraint(model::ModelLike, c::ConstraintIndex, change) = false

"""
## Modify Function

    modifyconstraint!(model::ModelLike, c::ConstraintIndex{F,S}, func::F)

Replace the function in constraint `c` with `func`. `F` must match the original function type used to define the constraint.

### Examples

If `c` is a `ConstraintIndex{ScalarAffineFunction,S}` and `v1` and `v2` are `VariableIndex` objects,

```julia
modifyconstraint!(model, c, ScalarAffineFunction([v1,v2],[1.0,2.0],5.0))
modifyconstraint!(model, c, SingleVariable(v1)) # Error
```

## Modify Set

    modifyconstraint!(model::ModelLike, c::ConstraintIndex{F,S}, set::S)

Change the set of constraint `c` to the new set `set` which should be of the same type as the original set.

### Examples

If `c` is a `ConstraintIndex{F,Interval}`

```julia
modifyconstraint!(model, c, Interval(0, 5))
modifyconstraint!(model, c, NonPositives) # Error
```

## Partial Modifications

    modifyconstraint!(model::ModelLike, c::ConstraintIndex, change::AbstractFunctionModification)

Apply the modification specified by `change` to the function of constraint `c`.

### Examples

```julia
modifyconstraint!(model, c, ScalarConstantChange(10.0))
```
"""
function modifyconstraint! end


"""
## Transform Constraint Set

    transformconstraint!(model::ModelLike, c::ConstraintIndex{F,S1}, newset::S2)::ConstraintIndex{F,S2}

Replace the set in constraint `c` with `newset`. The constraint index `c`
will no longer be valid, and the function returns a new constraint index with
the correct type.

Solvers may only support a subset of constraint transforms that they perform
efficiently (for example, changing from a `LessThan` to `GreaterThan` set). In
addition, set modification (where `S1 = S2`) should be performed via the
`modifyconstraint!` function.


Typically, the user should delete the constraint and add a new one.

### Examples

If `c` is a `ConstraintIndex{ScalarAffineFunction{Float64},LessThan{Float64}}`,

```julia
c2 = transformconstraint!(model, c, GreaterThan(0.0))
transformconstraint!(model, c, LessThan(0.0)) # errors
```
"""
function transformconstraint! end

# default fallback
function transformconstraint!(model::ModelLike, c::ConstraintIndex, newset)
    f = get(model, ConstraintFunction(), c)
    delete!(model, c)
    addconstraint!(model, f, newset)
end

"""
## Transform Constraint Set

    cantransformconstraint(model::ModelLike, c::ConstraintIndex{F,S1}, ::Type{S2})::Bool where S2<:AbstractSet

Return a `Bool` indicating whether the set of type `S1` in constraint `c` can be replaced by a set of type `S2`.

### Examples

If `c` is a `ConstraintIndex{ScalarAffineFunction{Float64},LessThan{Float64}}`,

```julia
cantransformconstraint(model, c, GreaterThan(0.0)) # true
cantransformconstraint(model, c, ZeroOne())        # false
```
"""
function cantransformconstraint end

# default fallback
function cantransformconstraint(model::ModelLike, c::ConstraintIndex{F}, ::Type{S}) where {F<:AbstractFunction, S<:AbstractSet}
    canget(model, ConstraintFunction(), typeof(c)) && candelete(model, c) && canaddconstraint(model, F, S)
end
