# Constraints

"""
    supports_constraint(model::ModelLike, ::Type{F}, ::Type{S})::Bool where {F<:AbstractFunction,S<:AbstractSet}

Return a `Bool` indicating whether `model` supports `F`-in-`S` constraints, that
is, `copy_to(model, src)` does not throw [`UnsupportedConstraint`](@ref) when
`src` contains `F`-in-`S` constraints. If `F`-in-`S` constraints are only not
supported in specific circumstances, e.g. `F`-in-`S` constraints cannot be
combined with another type of constraint, it should still return `true`.

    supports_constraint(model::ModelLike, ::Type{VectorOfVariables}, ::Type{Reals})::Bool

Return a `Bool` indicating whether `model` supports free variables. That is,
`copy_to(model, src)` does error when `src` contains variables that are not
constrained by any [`SingleVariable`](@ref) or [`VectorOfVariables`](@ref)
constraint. By default, this method returns `true` so it should only be
implemented if `model` does not support free variables.

Note that free variables are not explicitly set to be free by calling
[`add_constraint`](@ref) with the set [`Reals`](@ref), instead, free variables
are created with [`add_variable`](@ref) and [`add_variables`](@ref).
If `model` does not support free variables, it should not implement
[`add_variable`](@ref) nor [`add_variables`](@ref) but should implement
this method and return `false`. This allows free variables to be bridged as the
sum of a nonnegative and a nonpositive variables.
""" # Implemented as only one method to avoid ambiguity
function supports_constraint(model::ModelLike, F::Type{<:AbstractFunction},
                             S::Type{<:AbstractSet})
    return F == VectorOfVariables && S == Reals
end

"""
    struct UnsupportedConstraint{F<:AbstractFunction, S<:AbstractSet} <: UnsupportedError
        message::String # Human-friendly explanation why the attribute cannot be set
    end

An error indicating that constraints of type `F`-in-`S` are not supported by
the model, i.e. that [`supports_constraint`](@ref) returns `false`.
"""
struct UnsupportedConstraint{F<:AbstractFunction, S<:AbstractSet} <: UnsupportedError
    message::String # Human-friendly explanation why the attribute cannot be set
end
UnsupportedConstraint{F, S}() where {F, S} = UnsupportedConstraint{F, S}("")

element_name(::UnsupportedConstraint{F, S}) where {F, S} = "`$F`-in-`$S` constraint"

"""
    struct AddConstraintNotAllowed{F<:AbstractFunction, S<:AbstractSet} <: NotAllowedError
        message::String # Human-friendly explanation why the attribute cannot be set
    end

An error indicating that constraints of type `F`-in-`S` are supported (see
[`supports_constraint`](@ref)) but cannot be added.
"""
struct AddConstraintNotAllowed{F<:AbstractFunction, S<:AbstractSet} <: NotAllowedError
    message::String # Human-friendly explanation why the attribute cannot be set
end
AddConstraintNotAllowed{F, S}() where {F, S} = AddConstraintNotAllowed{F, S}("")

operation_name(::AddConstraintNotAllowed{F, S}) where {F, S} = "Adding `$F`-in-`$S` constraints"

"""
    struct ScalarFunctionConstantNotZero{T, F, S} <: Exception
        constant::T
    end

An error indicating that the constant part of the function in the constraint
`F`-in-`S` is nonzero.
"""
struct ScalarFunctionConstantNotZero{T, F, S} <: Exception
    constant::T
end

function Base.showerror(io::IO,
                    err::ScalarFunctionConstantNotZero{T, F, S}) where {T, F, S}
    print(io, "In `$F`-in-`$S` constraint: Constant $(err.constant) of the ",
          "function is not zero. The function constant should be moved to the ",
          "set. You can use `MOI.Utilities.add_scalar_constraint` which does ",
          "this automatically.")
end

"""
    add_constraint(model::ModelLike, func::F, set::S)::ConstraintIndex{F,S} where {F,S}

Add the constraint ``f(x) \\in \\mathcal{S}`` where ``f`` is defined by `func`, and ``\\mathcal{S}`` is defined by `set`.

    add_constraint(model::ModelLike, v::VariableIndex, set::S)::ConstraintIndex{SingleVariable,S} where {S}
    add_constraint(model::ModelLike, vec::Vector{VariableIndex}, set::S)::ConstraintIndex{VectorOfVariables,S} where {S}

Add the constraint ``v \\in \\mathcal{S}`` where ``v`` is the variable (or vector of variables) referenced by `v` and ``\\mathcal{S}`` is defined by `set`.

* An [`UnsupportedConstraint`](@ref) error is thrown if `model` does not support
  `F`-in-`S` constraints,
* a [`AddConstraintNotAllowed`](@ref) error is thrown if it supports `F`-in-`S`
  constraints but it cannot add the constraint(s) in its current state and
* a [`ScalarFunctionConstantNotZero`](@ref) error may be thrown if
  `func` is an `AbstractScalarFunction` with nonzero constant and `set`
  is [`EqualTo`](@ref), [`GreaterThan`](@ref), [`LessThan`](@ref) or
  [`Interval`](@ref).
* a [`LowerBoundAlreadySet`](@ref) error is thrown if `F` is a
  [`SingleVariable`](@ref) and a constraint was already added to this variable
  that sets a lower bound.
* a [`UpperBoundAlreadySet`](@ref) error is thrown if `F` is a
  [`SingleVariable`](@ref) and a constraint was already added to this variable
  that sets an upper bound.
"""
function add_constraint(model::ModelLike, func::AbstractFunction,
                        set::AbstractSet)
    throw_add_constraint_error_fallback(model, func, set)
end

# throw_add_constraint_error_fallback checks whether func and set are both
# scalar or both vector. If it is the case, it calls
# `correct_throw_add_constraint_error_fallback`
function throw_add_constraint_error_fallback(model::ModelLike,
                                             func::AbstractScalarFunction,
                                             set::AbstractScalarSet;
                                             kwargs...)
    correct_throw_add_constraint_error_fallback(model, func, set; kwargs...)
end
function throw_add_constraint_error_fallback(model::ModelLike,
                                             func::AbstractVectorFunction,
                                             set::AbstractVectorSet;
                                             kwargs...)
    correct_throw_add_constraint_error_fallback(model, func, set; kwargs...)
end
function throw_add_constraint_error_fallback(model::ModelLike,
                                             func::AbstractScalarFunction,
                                             set::AbstractVectorSet;
                                             kwargs...)
    error("Cannot add a constraint of the form `ScalarFunction`-in-`VectorSet`")
end
function throw_add_constraint_error_fallback(model::ModelLike,
                                             func::AbstractVectorFunction,
                                             set::AbstractScalarSet;
                                             kwargs...)
    error("Cannot add a constraint of the form `VectorFunction`-in-`ScalarSet`")
end

# func and set are both scalar or both vector
function correct_throw_add_constraint_error_fallback(model::ModelLike,
                                                     func::AbstractFunction,
                                                     set::AbstractSet;
                                                     error_if_supported=AddConstraintNotAllowed{typeof(func), typeof(set)}())
    if supports_constraint(model, typeof(func), typeof(set))
        throw(error_if_supported)
    else
        throw(UnsupportedConstraint{typeof(func), typeof(set)}())
    end
end

# convenient shorthands TODO: document
add_constraint(model::ModelLike, v::VariableIndex, set::AbstractScalarSet) = add_constraint(model, SingleVariable(v), set)
add_constraint(model::ModelLike, v::Vector{VariableIndex}, set::AbstractVectorSet) = add_constraint(model, VectorOfVariables(v), set)

"""
    add_constraints(model::ModelLike, funcs::Vector{F}, sets::Vector{S})::Vector{ConstraintIndex{F,S}} where {F,S}

Add the set of constraints specified by each function-set pair in `funcs` and `sets`. `F` and `S` should be concrete types.
This call is equivalent to `add_constraint.(model, funcs, sets)` but may be more efficient.
"""
function add_constraints end

# default fallback
add_constraints(model::ModelLike, funcs, sets) = add_constraint.(model, funcs, sets)

"""
    LowerBoundAlreadySet{S1, S2}

Error thrown when setting a `SingleVariable`-in-`S2` when a
`SingleVariable`-in-`S1` has already been added and the sets `S1`, `S2` both
set a lower bound, i.e. they are [`EqualTo`](@ref), [`GreaterThan`](@ref),
[`Interval`](@ref), [`Semicontinuous`](@ref) or [`Semiinteger`](@ref).
"""
struct LowerBoundAlreadySet{S1, S2}
    vi::VariableIndex
end

function Base.showerror(io::IO, err::LowerBoundAlreadySet{S1, S2}) where {S1, S2}
    print(io, typeof(err), ": Cannot add `SingleVariable`-in`", S2,
          "` constraint for variable ", err.vi, " as a `SingleVariable`-in`",
          S1, "` constraint was already set for this variable and both",
          " constraints set a lower bound.")
end

"""
    UpperBoundAlreadySet{S1, S2}

Error thrown when setting a `SingleVariable`-in-`S2` when a
`SingleVariable`-in-`S1` has already been added and the sets `S1`, `S2` both
set an upper bound, i.e. they are [`EqualTo`](@ref), [`LessThan`](@ref),
[`Interval`](@ref), [`Semicontinuous`](@ref) or [`Semiinteger`](@ref).
"""
struct UpperBoundAlreadySet{S1, S2}
    vi::VariableIndex
end

function Base.showerror(io::IO, err::UpperBoundAlreadySet{S1, S2}) where {S1, S2}
    print(io, typeof(err), ": Cannot add `SingleVariable`-in`", S2,
          "` constraint for variable ", err.vi, " as a `SingleVariable`-in`",
          S1, "` constraint was already set for this variable and both",
          " constraints set an upper bound.")
end

"""
## Transform Constraint Set

    transform(model::ModelLike, c::ConstraintIndex{F,S1}, newset::S2)::ConstraintIndex{F,S2}

Replace the set in constraint `c` with `newset`. The constraint index `c`
will no longer be valid, and the function returns a new constraint index with
the correct type.

Solvers may only support a subset of constraint transforms that they perform
efficiently (for example, changing from a `LessThan` to `GreaterThan` set). In
addition, set modification (where `S1 = S2`) should be performed via the
`modify` function.


Typically, the user should delete the constraint and add a new one.

### Examples

If `c` is a `ConstraintIndex{ScalarAffineFunction{Float64},LessThan{Float64}}`,

```julia
c2 = transform(model, c, GreaterThan(0.0))
transform(model, c, LessThan(0.0)) # errors
```
"""
function transform end

# default fallback
function transform(model::ModelLike, c::ConstraintIndex, newset)
    f = get(model, ConstraintFunction(), c)
    delete(model, c)
    add_constraint(model, f, newset)
end
