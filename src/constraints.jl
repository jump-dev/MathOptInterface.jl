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
addconstraints!(model::ModelLike, funcs, sets) = addconstraint!.(model, funcs, sets)

"""
## Transform Constraint Set

    transform!(model::ModelLike, c::ConstraintIndex{F,S1}, newset::S2)::ConstraintIndex{F,S2}

Replace the set in constraint `c` with `newset`. The constraint index `c`
will no longer be valid, and the function returns a new constraint index with
the correct type.

Solvers may only support a subset of constraint transforms that they perform
efficiently (for example, changing from a `LessThan` to `GreaterThan` set). In
addition, set modification (where `S1 = S2`) should be performed via the
`modify!` function.


Typically, the user should delete the constraint and add a new one.

### Examples

If `c` is a `ConstraintIndex{ScalarAffineFunction{Float64},LessThan{Float64}}`,

```julia
c2 = transform!(model, c, GreaterThan(0.0))
transform!(model, c, LessThan(0.0)) # errors
```
"""
function transform! end

# default fallback
function transform!(model::ModelLike, c::ConstraintIndex, newset)
    f = get(model, ConstraintFunction(), c)
    delete!(model, c)
    addconstraint!(model, f, newset)
end

"""
## Transform Constraint Set

    cantransform(model::ModelLike, c::ConstraintIndex{F,S1}, ::Type{S2})::Bool where S2<:AbstractSet

Return a `Bool` indicating whether the set of type `S1` in constraint `c` can be
replaced by a set of type `S2`.

### Examples

If `c` is a `ConstraintIndex{ScalarAffineFunction{Float64},LessThan{Float64}}`,

```julia
cantransform(model, c, GreaterThan(0.0)) # true
cantransform(model, c, ZeroOne())        # it is expected that most solvers will
                                         # return false, but this does not
                                         # preclude solvers from supporting
                                         # constraints such as this.
```
"""
function cantransform end

# default fallback
function cantransform(model::ModelLike, c::ConstraintIndex{F}, ::Type{S}) where {F<:AbstractFunction, S<:AbstractSet}
    canget(model, ConstraintFunction(), typeof(c)) && candelete(model, c) && canaddconstraint(model, F, S)
end
