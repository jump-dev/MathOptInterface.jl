# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    supports_constraint(
        model::ModelLike,
        F::Type{<:AbstractFunction},
        F::Type{<:AbstractSet},
    )::Bool

Return a `Bool` indicating whether `model` supports `F`-in-`S` constraints, that
is, `copy_to(model, src)` does not throw [`UnsupportedConstraint`](@ref) when
`src` contains `F`-in-`S` constraints. If `F`-in-`S` constraints are only not
supported in specific circumstances, e.g. `F`-in-`S` constraints cannot be
combined with another type of constraint, it should still return `true`.
"""
function supports_constraint(
    ::ModelLike,
    ::Type{<:AbstractFunction},
    ::Type{<:AbstractSet},
)
    return false
end

"""
    UnsupportedConstraint{F<:AbstractFunction,S<:AbstractSet}(
        message::String = "",
    )

An error indicating that constraints of type `F`-in-`S` are not supported by
the model.

This error should be thrown only if [`supports_constraint`](@ref) returns `false`.

See [`AddConstraintNotAllowed`](@ref) for the error if the solver supports the
constraint type in general, but cannot add it to the current model.
"""
struct UnsupportedConstraint{F<:AbstractFunction,S<:AbstractSet} <:
       UnsupportedError
    message::String
end

UnsupportedConstraint{F,S}() where {F,S} = UnsupportedConstraint{F,S}("")

function element_name(::UnsupportedConstraint{F,S}) where {F,S}
    return "`$F`-in-`$S` constraint"
end

"""
    AddConstraintNotAllowed{F<:AbstractFunction,S<:AbstractSet}(
        message::String = "",
    )

An error indicating that constraints of type `F`-in-`S` are supported by the
optimizer but cannot be added to the current model.

This error should be thrown only if [`supports_constraint`](@ref) returns `true`.

See [`UnsupportedConstraint`](@ref) for the error if the solver does not support
the constraint type.
"""
struct AddConstraintNotAllowed{F<:AbstractFunction,S<:AbstractSet} <:
       NotAllowedError
    message::String
end

AddConstraintNotAllowed{F,S}() where {F,S} = AddConstraintNotAllowed{F,S}("")

function operation_name(::AddConstraintNotAllowed{F,S}) where {F,S}
    return "Adding `$F`-in-`$S` constraints"
end

"""
    ScalarFunctionConstantNotZero{T,F,S}(constant::T)

An error indicating that the constant part of the function in the constraint
`F`-in-`S` is nonzero.
"""
struct ScalarFunctionConstantNotZero{T,F,S} <: Exception
    constant::T
end

function Base.showerror(
    io::IO,
    err::ScalarFunctionConstantNotZero{T,F,S},
) where {T,F,S}
    return print(
        io,
        "In `$F`-in-`$S` constraint: Constant $(err.constant) of the function ",
        "is not zero. The function constant should be moved to the set. You ",
        "can use `MOI.Utilities.normalize_and_add_constraint` which does this ",
        "automatically.",
    )
end

"""
    throw_if_scalar_and_constant_not_zero(
        f::AbstractFunction,
        ::Type{S<:AbstractSet},
    )

Throw a [`ScalarFunctionConstantNotZero`](@ref) error if `f` is a scalar
function whose constant is not zero.
"""
function throw_if_scalar_and_constant_not_zero(
    f::F,
    ::Type{S},
) where {F<:AbstractScalarFunction,S<:AbstractScalarSet}
    c = constant(f)
    if !iszero(c)
        throw(ScalarFunctionConstantNotZero{typeof(c),F,S}(c))
    end
    return
end

function throw_if_scalar_and_constant_not_zero(
    ::VariableIndex,
    ::Type{<:AbstractScalarSet},
)
    return
end

function throw_if_scalar_and_constant_not_zero(
    ::AbstractVectorFunction,
    ::Type{<:AbstractVectorSet},
)
    return
end

"""
    add_constraint(
        model::ModelLike,
        func::F,
        set::S,
    )::ConstraintIndex{F,S} where {F,S}

Add the constraint ``f(x) \\in \\mathcal{S}`` where ``f`` is defined by `func`,
and ``\\mathcal{S}`` is defined by `set`.

## Errors

 * An [`UnsupportedConstraint`](@ref) error is thrown if `model` does not
   support `F`-in-`S` constraints
 * An [`AddConstraintNotAllowed`](@ref) error is thrown if `model` supports
   `F`-in-`S` constraints but it cannot add the constraint in the current state
 * A [`ScalarFunctionConstantNotZero`](@ref) error may be thrown if `func` is an
   `AbstractScalarFunction` with nonzero constant and `set` is [`EqualTo`](@ref),
   [`GreaterThan`](@ref), [`LessThan`](@ref), or [`Interval`](@ref)
 * A [`LowerBoundAlreadySet`](@ref) error is thrown if `F` is a [`VariableIndex`](@ref)
   and a constraint was already added to this variable that sets a lower bound
 * An [`UpperBoundAlreadySet`](@ref) error is thrown if `F` is a [`VariableIndex`](@ref)
   and a constraint was already added to this variable that sets an upper bound.
"""
function add_constraint(
    model::ModelLike,
    ::F,
    ::S,
) where {F<:AbstractFunction,S<:AbstractSet}
    if supports_constraint(model, F, S)
        throw(AddConstraintNotAllowed{F,S}())
    end
    return throw(UnsupportedConstraint{F,S}())
end

# TODO(odow): remove this?
function add_constraint(
    model::ModelLike,
    v::Vector{VariableIndex},
    set::AbstractVectorSet,
)
    return add_constraint(model, VectorOfVariables(v), set)
end

"""
    add_constraints(
        model::ModelLike,
        funcs::Vector{F},
        sets::Vector{S},
    )::Vector{ConstraintIndex{F,S}} where {F,S}

Add the set of constraints specified by each function-set pair in `funcs` and
`sets`. `F` and `S` should be concrete types.

This call is equivalent to `add_constraint.(model, funcs, sets)`, but it may be
more efficient.
"""
function add_constraints(model::ModelLike, funcs, sets)
    return add_constraint.(model, funcs, sets)
end

"""
    LowerBoundAlreadySet{S1, S2}

Error thrown when setting a `VariableIndex`-in-`S2` when a
`VariableIndex`-in-`S1` has already been added and the sets `S1`, `S2` both
set a lower bound, that is, they are [`EqualTo`](@ref), [`GreaterThan`](@ref),
[`Interval`](@ref), [`Semicontinuous`](@ref), or [`Semiinteger`](@ref).
"""
struct LowerBoundAlreadySet{S1,S2}
    vi::VariableIndex
end

function Base.showerror(io::IO, err::LowerBoundAlreadySet{S1,S2}) where {S1,S2}
    return print(
        io,
        typeof(err),
        ": Cannot add `VariableIndex`-in-`$(S2)` constraint for variable ",
        "$(err.vi) as a `VariableIndex`-in-`$(S1)` constraint was already ",
        "set for this variable and both constraints set a lower bound.",
    )
end

"""
    UpperBoundAlreadySet{S1,S2}

Error thrown when setting a `VariableIndex`-in-`S2` when a
`VariableIndex`-in-`S1` has already been added and the sets `S1`, `S2` both
set an upper bound, that is, they are [`EqualTo`](@ref), [`LessThan`](@ref),
[`Interval`](@ref), [`Semicontinuous`](@ref), or [`Semiinteger`](@ref).
"""
struct UpperBoundAlreadySet{S1,S2}
    vi::VariableIndex
end

function Base.showerror(io::IO, err::UpperBoundAlreadySet{S1,S2}) where {S1,S2}
    return print(
        io,
        typeof(err),
        ": Cannot add `VariableIndex`-in-`$(S2)` constraint for variable ",
        "$(err.vi) as a `VariableIndex`-in-`$(S1)` constraint was already ",
        "set for this variable and both constraints set an upper bound.",
    )
end

"""
    transform(
        model::ModelLike,
        c::ConstraintIndex{F,S1},
        set::S2,
    )::ConstraintIndex{F,S2}

Replace the set in constraint `c` with `set`. The constraint index `c`
will no longer be valid, and the function returns a new constraint index with
the correct type.

Solvers may support only a subset of constraint transforms that they perform
efficiently (for example, changing from a [`LessThan`](@ref) to [`GreaterThan`](@ref)
set).

Set modification (where `S1 == S2`) must be performed via the [`modify`](@ref)
function.

## Example

```jldoctest
julia> import MathOptInterface as MOI

julia> model = MOI.Utilities.Model{Float64}()
MOIU.Model{Float64}

julia> x = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> c = MOI.add_constraint(model, x, MOI.LessThan(0.0))
MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.LessThan{Float64}}(1)

julia> c2 = MOI.transform(model, c, MOI.GreaterThan(0.0))
MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.GreaterThan{Float64}}(1)
```
"""
function transform(model::ModelLike, c::ConstraintIndex, newset)
    f = get(model, ConstraintFunction(), c)
    delete(model, c)
    return add_constraint(model, f, newset)
end
