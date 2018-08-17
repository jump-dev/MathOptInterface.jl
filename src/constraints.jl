# Constraints

"""
    supportsconstraint(model::ModelLike, ::Type{F}, ::Type{S})::Bool where {F<:AbstractFunction,S<:AbstractSet}

Return a `Bool` indicating whether `model` supports `F`-in-`S` constraints, that is,
`copy!(model, src)` does not return `CopyUnsupportedConstraint` when `src` contains `F`-in-`S` constraints.
If `F`-in-`S` constraints are only not supported in specific circumstances, e.g. `F`-in-`S` constraints cannot be combined with another type of constraint, it should still return `true`.
"""
supportsconstraint(model::ModelLike, ::Type{<:AbstractFunction}, ::Type{<:AbstractSet}) = false

"""
    struct UnsupportedConstraint{F<:AbstractFunction, S<:AbstractSet} <: UnsupportedError
        message::String # Human-friendly explanation why the attribute cannot be set
    end

An error indicating that constraints of type `F`-in-`S` are not supported by
the model, i.e. that [`supportsconstraint`](@ref) returns `false`.
"""
struct UnsupportedConstraint{F<:AbstractFunction, S<:AbstractSet} <: UnsupportedError
    message::String # Human-friendly explanation why the attribute cannot be set
end
UnsupportedConstraint{F, S}() where {F, S} = UnsupportedConstraint{F, S}("")

element_name(::UnsupportedConstraint{F, S}) where {F, S} = "`$F`-in-`$S` constraints"

"""
    struct AddConstraintNotAllowed{F<:AbstractFunction, S<:AbstractSet} <: NotAllowedError
        message::String # Human-friendly explanation why the attribute cannot be set
    end

An error indicating that constraints of type `F`-in-`S` are supported (see
[`supportsconstraint`](@ref)) but cannot be added.
"""
struct AddConstraintNotAllowed{F<:AbstractFunction, S<:AbstractSet} <: NotAllowedError
    message::String # Human-friendly explanation why the attribute cannot be set
end
AddConstraintNotAllowed{F, S}() where {F, S} = AddConstraintNotAllowed{F, S}("")

operation_name(::AddConstraintNotAllowed{F, S}) where {F, S} = "Adding `$F`-in-`$S` constraints"

"""
    addconstraint!(model::ModelLike, func::F, set::S)::ConstraintIndex{F,S} where {F,S}

Add the constraint ``f(x) \\in \\mathcal{S}`` where ``f`` is defined by `func`, and ``\\mathcal{S}`` is defined by `set`.

    addconstraint!(model::ModelLike, v::VariableIndex, set::S)::ConstraintIndex{SingleVariable,S} where {S}
    addconstraint!(model::ModelLike, vec::Vector{VariableIndex}, set::S)::ConstraintIndex{VectorOfVariables,S} where {S}

Add the constraint ``v \\in \\mathcal{S}`` where ``v`` is the variable (or vector of variables) referenced by `v` and ``\\mathcal{S}`` is defined by `set`.

An [`UnsupportedConstraint`](@ref) error is thrown if `model` does not support
`F`-in-`S` constraints and a [`AddConstraintNotAllowed`](@ref) error is thrown if
it supports `F`-in-`S` constraints but it cannot add the constraint(s) in its
current state.
"""
function addconstraint!(model::ModelLike, func::AbstractFunction, set::AbstractSet)
    if supportsconstraint(model, typeof(func), typeof(set))
        throw(AddConstraintNotAllowed{typeof(func), typeof(set)}())
    else
        throw(UnsupportedConstraint{typeof(func), typeof(set)}())
    end
end

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
