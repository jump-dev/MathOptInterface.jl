# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    AbstractOptimizerAttribute

Abstract supertype for attribute objects that can be used to set or get
attributes (properties) of the optimizer.

## Notes

The difference between `AbstractOptimizerAttribute` and
[`AbstractModelAttribute`](@ref) lies in the behavior of [`is_empty`](@ref),
[`empty!`](@ref) and [`copy_to`](@ref). Typically optimizer attributes affect
only how the model is solved.
"""
abstract type AbstractOptimizerAttribute end

"""
    AbstractModelAttribute

Abstract supertype for attribute objects that can be used to set or get
attributes (properties) of the model.
"""
abstract type AbstractModelAttribute end

"""
    AbstractVariableAttribute

Abstract supertype for attribute objects that can be used to set or get
attributes (properties) of variables in the model.
"""
abstract type AbstractVariableAttribute end

"""
    AbstractConstraintAttribute

Abstract supertype for attribute objects that can be used to set or get
attributes (properties) of constraints in the model.
"""
abstract type AbstractConstraintAttribute end

# Attributes should not contain any `VariableIndex` or `ConstraintIndex` as the
# set is passed unmodified during `copy_to`.
const AnyAttribute = Union{
    AbstractOptimizerAttribute,
    AbstractModelAttribute,
    AbstractVariableAttribute,
    AbstractConstraintAttribute,
}

# This allows to use attributes in broadcast calls without the need to
# embed it in a `Ref`
Base.broadcastable(attribute::AnyAttribute) = Ref(attribute)

"""
    attribute_value_type(attr::AnyAttribute)

Given an attribute `attr`, return the type of value expected by [`get`](@ref),
or returned by [`set`](@ref).

## Notes

 * Only implement this if it make sense to do so. If un-implemented, the default
   is `Any`.
"""
attribute_value_type(::AnyAttribute) = Any

"""
    struct UnsupportedAttribute{AttrType} <: UnsupportedError
        attr::AttrType
        message::String
    end

An error indicating that the attribute `attr` is not supported by the model,
that is, that [`supports`](@ref) returns `false`.
"""
struct UnsupportedAttribute{AttrType<:AnyAttribute} <: UnsupportedError
    attr::AttrType
    message::String
end

UnsupportedAttribute(attr::AnyAttribute) = UnsupportedAttribute(attr, "")

element_name(err::UnsupportedAttribute) = "Attribute $(err.attr)"

"""
    struct SetAttributeNotAllowed{AttrType} <: NotAllowedError
        attr::AttrType
        message::String
    end

An error indicating that the attribute `attr` is supported (see
[`supports`](@ref)) but cannot be set for some reason (see the error string).
"""
struct SetAttributeNotAllowed{AttrType<:AnyAttribute} <: NotAllowedError
    attr::AttrType
    message::String # Human-friendly explanation why the attribute cannot be set
end

SetAttributeNotAllowed(attr::AnyAttribute) = SetAttributeNotAllowed(attr, "")

operation_name(err::SetAttributeNotAllowed) = "Setting attribute $(err.attr)"

message(err::SetAttributeNotAllowed) = err.message

"""
    struct GetAttributeNotAllowed{AttrType} <: NotAllowedError
        attr::AttrType
        message::String
    end

An error indicating that the attribute `attr` cannot be got for some reason (see
the error string).
"""
struct GetAttributeNotAllowed{AttrType<:AnyAttribute} <: NotAllowedError
    attr::AttrType
    message::String

    function GetAttributeNotAllowed(attr::AnyAttribute, message::String = "")
        return new{typeof(attr)}(attr, message)
    end
end

operation_name(err::GetAttributeNotAllowed) = "Getting attribute $(err.attr)"

message(err::GetAttributeNotAllowed) = err.message

"""
    AbstractSubmittable

Abstract supertype for objects that can be submitted to the model.
"""
abstract type AbstractSubmittable end

# This allows to use submittable in broadcast calls without the need to embed
# it in a `Ref`.
Base.broadcastable(sub::AbstractSubmittable) = Ref(sub)

"""
    struct UnsupportedSubmittable{SubmitType} <: UnsupportedError
        sub::SubmitType
        message::String
    end

An error indicating that the submittable `sub` is not supported by the model,
that is, that [`supports`](@ref) returns `false`.
"""
struct UnsupportedSubmittable{SubmitType<:AbstractSubmittable} <:
       UnsupportedError
    sub::SubmitType
    message::String
end

"""
    struct SubmitNotAllowed{SubmitTyp<:AbstractSubmittable} <: NotAllowedError
        sub::SubmitType
        message::String
    end

An error indicating that the submittable `sub` is supported (see
[`supports`](@ref)) but cannot be added for some reason (see the error string).
"""
struct SubmitNotAllowed{SubmitType<:AbstractSubmittable} <: NotAllowedError
    sub::SubmitType
    message::String # Human-friendly explanation why the attribute cannot be set
end

SubmitNotAllowed(sub::AbstractSubmittable) = SubmitNotAllowed(sub, "")

operation_name(err::SubmitNotAllowed) = "Submitting $(err.sub)"

message(err::SubmitNotAllowed) = err.message

"""
    struct ResultIndexBoundsError{AttrType} <: Exception
        attr::AttrType
        result_count::Int
    end

An error indicating that the requested attribute `attr` could not be retrieved,
because the solver returned too few results compared to what was requested.
For instance, the user tries to retrieve `VariablePrimal(2)` when only one
solution is available, or when the model is infeasible and has no solution.

See also: [`check_result_index_bounds`](@ref).
"""
struct ResultIndexBoundsError{AttrType} <: Exception
    attr::AttrType
    result_count::Int
end

"""
    check_result_index_bounds(model::ModelLike, attr)

This function checks whether enough results are available in the `model` for
the requested `attr`, using its `result_index` field. If the model
does not have sufficient results to answer the query, it throws a
[`ResultIndexBoundsError`](@ref).
"""
function check_result_index_bounds(model::ModelLike, attr)
    result_count = get(model, ResultCount())
    if !(1 <= attr.result_index <= result_count)
        throw(ResultIndexBoundsError(attr, result_count))
    end
    return
end

function Base.showerror(io::IO, err::ResultIndexBoundsError)
    return print(
        io,
        "Result index of attribute $(err.attr) out of bounds. There are " *
        "currently $(err.result_count) solution(s) in the model.",
    )
end

"""
    struct ConflictIndexBoundsError{AttrType} <: Exception
        attr::AttrType
        conflict_count::Int
    end

An error indicating that the requested attribute `attr` could not be retrieved,
because the solver returned too few conflicts compared to what was requested.
For instance, the user tries to retrieve `ConstraintConflictStatus(2)` when
only one conflict is available, or when the model is feasible.

See also: [`check_conflict_index_bounds`](@ref).
"""
struct ConflictIndexBoundsError{AttrType} <: Exception
    attr::AttrType
    conflict_count::Int
end

"""
    check_conflict_index_bounds(model::ModelLike, attr)

This function checks whether enough conflicts are available in the `model` for
the requested `attr`, using its `conflict_index` field. If the model
does not have sufficient conflicts to answer the query, it throws a
[`ConflictIndexBoundsError`](@ref).
"""
function check_conflict_index_bounds(model::ModelLike, attr)
    conflict_count = get(model, ConflictCount())
    if !(1 <= attr.conflict_index <= conflict_count)
        throw(ConflictIndexBoundsError(attr, conflict_count))
    end
    return
end

function Base.showerror(io::IO, err::ConflictIndexBoundsError)
    return print(
        io,
        "Conflict index of attribute $(err.attr) out of bounds. There are " *
        "currently $(err.conflict_count) conflict(s) in the model.",
    )
end

"""
    supports(model::ModelLike, sub::AbstractSubmittable)::Bool

Return a `Bool` indicating whether `model` supports the submittable `sub`.

```julia
supports(model::ModelLike, attr::AbstractOptimizerAttribute)::Bool
```

Return a `Bool` indicating whether `model` supports the optimizer attribute
`attr`. That is, it returns `false` if `copy_to(model, src)` shows a warning in
case `attr` is in the [`ListOfOptimizerAttributesSet`](@ref) of `src`; see
[`copy_to`](@ref) for more details on how unsupported optimizer attributes are
handled in copy.

```julia
supports(model::ModelLike, attr::AbstractModelAttribute)::Bool
```

Return a `Bool` indicating whether `model` supports the model attribute `attr`.
That is, it returns `false` if `copy_to(model, src)` cannot be performed in case
`attr` is in the [`ListOfModelAttributesSet`](@ref) of `src`.

```julia
supports(
    model::ModelLike,
    attr::AbstractVariableAttribute,
    ::Type{VariableIndex},
)::Bool
```

Return a `Bool` indicating whether `model` supports the variable attribute
`attr`. That is, it returns `false` if `copy_to(model, src)` cannot be performed
in case `attr` is in the [`ListOfVariableAttributesSet`](@ref) of `src`.

```julia
supports(
    model::ModelLike,
    attr::AbstractConstraintAttribute,
    ::Type{ConstraintIndex{F,S}},
)::Bool where {F,S}
```

Return a `Bool` indicating whether `model` supports the constraint attribute
`attr` applied to an `F`-in-`S` constraint. That is, it returns `false` if
`copy_to(model, src)` cannot be performed in case `attr` is in the
[`ListOfConstraintAttributesSet`](@ref) of `src`.

For all five methods, if the attribute is only not supported in specific
circumstances, it should still return `true`.

Note that `supports` is only defined for attributes for which
[`is_copyable`](@ref) returns `true` as other attributes do not appear in the
list of attributes set obtained by `ListOfXXXAttributesSet`.
"""
function supports(model::ModelLike, attr::AnyAttribute, args...)
    return supports_fallback(model, attr, args...)
end

supports(::ModelLike, ::AbstractSubmittable) = false

function supports_fallback(
    ::ModelLike,
    attr::Union{AbstractModelAttribute,AbstractOptimizerAttribute},
)
    if !is_copyable(attr)
        throw(
            ArgumentError(
                "`supports` is not defined for $attr, it is only" *
                " defined for attributes such that `is_copyable`" *
                " returns `true`.",
            ),
        )
    end
    return false
end

function supports_fallback(
    ::ModelLike,
    attr::Union{AbstractVariableAttribute,AbstractConstraintAttribute},
    ::Type{<:Index},
)
    if !is_copyable(attr)
        throw(
            ArgumentError(
                "`supports` is not defined for $attr, it is only" *
                " defined for attributes such that `is_copyable`" *
                " returns `true`.",
            ),
        )
    end
    return false
end

"""
    get(optimizer::AbstractOptimizer, attr::AbstractOptimizerAttribute)

Return an attribute `attr` of the optimizer `optimizer`.

```julia
get(model::ModelLike, attr::AbstractModelAttribute)
```

Return an attribute `attr` of the model `model`.

```julia
get(model::ModelLike, attr::AbstractVariableAttribute, v::VariableIndex)
```

If the attribute `attr` is set for the variable `v` in the model `model`, return
its value, return `nothing` otherwise. If the attribute `attr` is not supported
by `model` then an error should be thrown instead of returning `nothing`.

```julia
get(model::ModelLike, attr::AbstractVariableAttribute, v::Vector{VariableIndex})
```

Return a vector of attributes corresponding to each variable in the collection
`v` in the model `model`.

```julia
get(model::ModelLike, attr::AbstractConstraintAttribute, c::ConstraintIndex)
```

If the attribute `attr` is set for the constraint `c` in the model `model`,
return its value, return `nothing` otherwise. If the attribute `attr` is not
supported by `model` then an error should be thrown instead of returning
`nothing`.

```julia
get(
    model::ModelLike,
    attr::AbstractConstraintAttribute,
    c::Vector{ConstraintIndex{F,S}},
) where {F,S}
```

Return a vector of attributes corresponding to each constraint in the collection
`c` in the model `model`.

```julia
get(model::ModelLike, ::Type{VariableIndex}, name::String)
```

If a variable with name `name` exists in the model `model`, return the
corresponding index, otherwise return `nothing`. Errors if two variables
have the same name.

```julia
get(
    model::ModelLike,
    ::Type{ConstraintIndex{F,S}},
    name::String,
) where {F,S}
```

If an `F`-in-`S` constraint with name `name` exists in the model `model`, return
the corresponding index, otherwise return `nothing`. Errors if two constraints
have the same name.

```julia
get(model::ModelLike, ::Type{ConstraintIndex}, name::String)
```

If *any* constraint with name `name` exists in the model `model`, return the
corresponding index, otherwise return `nothing`. This version is available for
convenience but may incur a performance penalty because it is not type stable.
Errors if two constraints have the same name.
"""
function get(model::ModelLike, attr::AnyAttribute, args...)
    return get_fallback(model, attr, args...)
end

# !!! danger
#     We want to avoid being too specific in the type arguments to avoid method
#     ambiguity. For example,
#         get(::ModelLike, ::AbstractVariableAttribute, ::Vector{VariableIndex})
#     would not allow us to define get(::SomeModel, ::AnyAttribute, ::Vector).
function get(model::ModelLike, attr::AnyAttribute, idxs::Vector)
    if isempty(idxs)
        return Vector{attribute_value_type(attr)}()
    end
    return get.(model, attr, idxs)
end

function get_fallback(
    model::ModelLike,
    attr::Union{AbstractModelAttribute,AbstractOptimizerAttribute},
)
    return throw(
        GetAttributeNotAllowed(
            attr,
            "$(typeof(model)) does not support getting the attribute $(attr).",
        ),
    )
end

function get_fallback(
    model::ModelLike,
    attr::AbstractVariableAttribute,
    ::VariableIndex,
)
    return throw(
        GetAttributeNotAllowed(
            attr,
            "$(typeof(model)) does not support getting the attribute $(attr).",
        ),
    )
end

function get_fallback(
    model::ModelLike,
    attr::AbstractConstraintAttribute,
    ::ConstraintIndex,
)
    return throw(
        GetAttributeNotAllowed(
            attr,
            "$(typeof(model)) does not support getting the attribute $(attr).",
        ),
    )
end

function get_fallback(::ModelLike, attr::AnyAttribute, args...)
    return throw(
        GetAttributeNotAllowed(
            attr,
            "Unable to get attribute $(attr): invalid arguments $(args).",
        ),
    )
end

"""
    get!(output, model::ModelLike, args...)

An in-place version of [`get`](@ref).

The signature matches that of [`get`](@ref) except that the result is placed in
the vector `output`.
"""
function get!(output, model::ModelLike, attr::AnyAttribute, args...)
    output .= get(model, attr, args...)
    return
end

"""
    set(optimizer::AbstractOptimizer, attr::AbstractOptimizerAttribute, value)

Assign `value` to the attribute `attr` of the optimizer `optimizer`.

```julia
set(model::ModelLike, attr::AbstractModelAttribute, value)
```

Assign `value` to the attribute `attr` of the model `model`.

```julia
set(model::ModelLike, attr::AbstractVariableAttribute, v::VariableIndex, value)
```

Assign `value` to the attribute `attr` of variable `v` in model `model`.

```julia
set(
    model::ModelLike,
    attr::AbstractVariableAttribute,
    v::Vector{VariableIndex},
    vector_of_values,
)
```

Assign a value respectively to the attribute `attr` of each variable in the
collection `v` in model `model`.

```julia
set(
    model::ModelLike,
    attr::AbstractConstraintAttribute,
    c::ConstraintIndex,
    value,
)
```

Assign a value to the attribute `attr` of constraint `c` in model `model`.

```julia
set(
    model::ModelLike,
    attr::AbstractConstraintAttribute,
    c::Vector{ConstraintIndex{F,S}},
    vector_of_values,
) where {F,S}
```

Assign a value respectively to the attribute `attr` of each constraint in the
collection `c` in model `model`.

An [`UnsupportedAttribute`](@ref) error is thrown if `model` does not support
the attribute `attr` (see [`supports`](@ref)) and a [`SetAttributeNotAllowed`](@ref)
error is thrown if it supports the attribute `attr` but it cannot be set.

```julia
set(
    model::ModelLike,
    ::ConstraintSet,
    c::ConstraintIndex{F,S},
    set::S,
) where {F,S}
```

Change the set of constraint `c` to the new set `set` which should be of the
same type as the original set.

```julia
set(
    model::ModelLike,
    ::ConstraintFunction,
    c::ConstraintIndex{F,S},
    func::F,
) where {F,S}
```

Replace the function in constraint `c` with `func`. `F` must match the original
function type used to define the constraint.

!!! note
    Setting the constraint function is not allowed if `F` is
    [`VariableIndex`](@ref); a [`SettingVariableIndexNotAllowed`](@ref) error is
    thrown instead. This is because, it would require changing the index `c`
    since the index of [`VariableIndex`](@ref) constraints must be the same as
    the index of the variable.
"""
function set(model::ModelLike, attr::AnyAttribute, args...)
    return throw_set_error_fallback(model, attr, args...)
end

# See note with get
function set(
    model::ModelLike,
    attr::Union{AbstractVariableAttribute,AbstractConstraintAttribute},
    idxs::Vector,
    vector_of_values::Vector,
)
    if length(idxs) != length(vector_of_values)
        throw(
            DimensionMismatch(
                "Number of indices ($(length(idxs))) does " *
                "not match the number of values " *
                "($(length(vector_of_values))) set to `$attr`.",
            ),
        )
    end
    return set.(model, attr, idxs, vector_of_values)
end

# throw_set_error_fallback is included so that we can return type-specific error
# messages without needing to overload set and cause ambiguity errors. For
# examples, see ConstraintSet and ConstraintFunction.
#
# throw_set_error_fallback should not be overloaded by users of MOI.
function throw_set_error_fallback(
    model::ModelLike,
    attr::Union{AbstractModelAttribute,AbstractOptimizerAttribute},
    value;
    error_if_supported = SetAttributeNotAllowed(attr),
)
    if supports(model, attr)
        throw(error_if_supported)
    else
        throw(UnsupportedAttribute(attr))
    end
end

function throw_set_error_fallback(
    model::ModelLike,
    attr::Union{AbstractVariableAttribute,AbstractConstraintAttribute},
    index::Index,
    value;
    error_if_supported = SetAttributeNotAllowed(attr),
)
    if supports(model, attr, typeof(index))
        throw(error_if_supported)
    else
        throw(UnsupportedAttribute(attr))
    end
end

"""
    SettingVariableIndexNotAllowed()

Error type that should be thrown when the user calls [`set`](@ref) to change
the [`ConstraintFunction`](@ref) of a [`VariableIndex`](@ref) constraint.
"""
struct SettingVariableIndexNotAllowed <: Exception end

"""
    submit(
        optimizer::AbstractOptimizer,
        sub::AbstractSubmittable,
        values...,
    )::Nothing

Submit `values` to the submittable `sub` of the optimizer `optimizer`.

An [`UnsupportedSubmittable`](@ref) error is thrown if `model` does not support
the attribute `attr` (see [`supports`](@ref)) and a [`SubmitNotAllowed`](@ref)
error is thrown if it supports the submittable `sub` but it cannot be submitted.
"""
function submit(model::ModelLike, sub::AbstractSubmittable, args...)
    if supports(model, sub)
        throw(
            ArgumentError(
                "Submitting $(typeof.(args)) for `$(typeof(sub))` is not valid.",
            ),
        )
    else
        throw(
            UnsupportedSubmittable(
                sub,
                "submit(::$(typeof(model)), ::$(typeof(sub))) is not supported.",
            ),
        )
    end
end

"""
    LazyConstraint(callback_data)

Lazy constraint `func`-in-`set` submitted as `func, set`. The optimal
solution returned by [`VariablePrimal`](@ref) will satisfy all lazy
constraints that have been submitted.

This can be submitted only from the [`LazyConstraintCallback`](@ref). The
field `callback_data` is a solver-specific callback type that is passed as the
argument to the feasible solution callback.

## Example

Suppose `x` and `y` are [`VariableIndex`](@ref)s of `optimizer`. To add a
`LazyConstraint` for `2x + 3y <= 1`, write
```julia
func = 2.0x + 3.0y
set = MOI.LessThan(1.0)
MOI.submit(optimizer, MOI.LazyConstraint(callback_data), func, set)
```
inside a [`LazyConstraintCallback`](@ref) of data `callback_data`.
"""
struct LazyConstraint{CallbackDataType} <: AbstractSubmittable
    callback_data::CallbackDataType
end

@_documented_enum(
    """
        HeuristicSolutionStatus

    An Enum of possible return values for [`submit`](@ref) with
    [`HeuristicSolution`](@ref).

    This status informs whether the heuristic solution was accepted or rejected.
    """,
    HeuristicSolutionStatus,
    "The heuristic solution was accepted.",
    HEURISTIC_SOLUTION_ACCEPTED,
    "The heuristic solution was rejected.",
    HEURISTIC_SOLUTION_REJECTED,
    "No information available on the acceptance.",
    HEURISTIC_SOLUTION_UNKNOWN,
)

"""
    HeuristicSolution(callback_data)

Heuristically obtained feasible solution. The solution is submitted as
`(variables, values)::Tuple{Vector{MOI.VariableIndex},Vector{T}}`, where
`values[i]` gives the value of `variables[i]`.

The [`submit`](@ref) call returns a [`HeuristicSolutionStatus`](@ref) indicating
whether the provided solution was accepted or rejected.

This can be submitted only from the [`HeuristicCallback`](@ref). The
field `callback_data` is a solver-specific callback type that is passed as the
argument to the heuristic callback.

Some solvers require a complete solution, others only partial solutions.
"""
struct HeuristicSolution{CallbackDataType} <: AbstractSubmittable
    callback_data::CallbackDataType
end

"""
    UserCut(callback_data)

Constraint `func`-to-`set` suggested to help the solver detect the solution
given by [`CallbackVariablePrimal`](@ref) as infeasible. The cut is submitted
as the tuple `(func, set)::Tuple{MOI.AbstractFunction,MOI.AbstractSet}`.

Typically [`CallbackVariablePrimal`](@ref) will violate integrality constraints,
and a cut would be of the form [`ScalarAffineFunction`](@ref)-in-[`LessThan`](@ref)
or [`ScalarAffineFunction`](@ref)-in-[`GreaterThan`](@ref).

Note that, as opposed to [`LazyConstraint`](@ref), the provided constraint
must not modify the feasible set. The constraint should be redundant, for
example, it may be a consequence of affine and integrality constraints.

This can be submitted only from the [`UserCutCallback`](@ref). The
field `callback_data` is a solver-specific callback type that is passed as the
argument to the infeasible solution callback.

Note that the solver may silently ignore the provided constraint.
"""
struct UserCut{CallbackDataType} <: AbstractSubmittable
    callback_data::CallbackDataType
end

"""
    struct InvalidCallbackUsage{C, S} <: Exception
        callback::C
        submittable::S
    end

An error indicating that `submittable` cannot be submitted inside `callback`.

For example, [`UserCut`](@ref) cannot be submitted inside
[`LazyConstraintCallback`](@ref).
"""
struct InvalidCallbackUsage{C,S} <: Exception
    callback::C
    submittable::S
end

function Base.showerror(io::IO, err::InvalidCallbackUsage)
    return print(
        io,
        "InvalidCallbackUsage: Cannot submit $(err.submittable) inside a $(err.callback).",
    )
end

@_documented_enum(
    """
        CallbackNodeStatusCode

    An Enum for the value of the [`CallbackNodeStatus`](@ref) attribute.
    """,
    CallbackNodeStatusCode,
    """
    The primal solution available from [`CallbackVariablePrimal`](@ref) is
    integer feasible.
    """,
    CALLBACK_NODE_STATUS_INTEGER,
    """
    The primal solution available from [`CallbackVariablePrimal`](@ref) is
    integer infeasible.
    """,
    CALLBACK_NODE_STATUS_FRACTIONAL,
    """
    The status of the primal solution available from [`CallbackVariablePrimal`](@ref)
    is unknown.
    """,
    CALLBACK_NODE_STATUS_UNKNOWN,
)

"""
    CallbackNodeStatus(callback_data)

An optimizer attribute describing the (in)feasibility of the primal solution
available from [`CallbackVariablePrimal`](@ref) during a callback identified by
`callback_data`.

Returns a [`CallbackNodeStatusCode`](@ref) Enum.
"""
struct CallbackNodeStatus{CallbackDataType} <: AbstractOptimizerAttribute
    callback_data::CallbackDataType
end

is_set_by_optimize(::CallbackNodeStatus) = true

attribute_value_type(::CallbackNodeStatus) = CallbackNodeStatusCode

"""
    ListOfOptimizerAttributesSet()

An [`AbstractOptimizerAttribute`](@ref) for the `Vector{AbstractOptimizerAttribute}`
of all optimizer attributes that were set.

## Implementation

Optimizers should implement the following methods:
```
MOI.get(
    ::Optimizer,
    ::MOI.ListOfOptimizerAttributesSet,
)::Vector{MOI.AbstractOptimizerAttribute}
```

They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct ListOfOptimizerAttributesSet <: AbstractOptimizerAttribute end

"""
    SolverName()

An [`AbstractOptimizerAttribute`](@ref) for the string identifying the solver.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.SolverName)::String
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct SolverName <: AbstractOptimizerAttribute end

attribute_value_type(::SolverName) = String

"""
    SolverVersion()

An [`AbstractOptimizerAttribute`](@ref) for the string identifying the version
of the solver.

## Versioning systems

For solvers supporting [semantic versioning](https://semver.org), the
[`SolverVersion`](@ref) should be a string of the form "vMAJOR.MINOR.PATCH", so
that it can be converted to a Julia `VersionNumber` (for example,
`VersionNumber("v1.2.3")).

We do not require Semantic Versioning because some solvers use alternate
versioning systems. For example, CPLEX uses Calendar Versioning, so
[`SolverVersion`](@ref) will return a string like `"202001"`.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.SolverVersion)::String
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct SolverVersion <: AbstractOptimizerAttribute end

attribute_value_type(::SolverVersion) = String

"""
    Silent()

An [`AbstractOptimizerAttribute`](@ref) for silencing the output of an
optimizer.

When `set` to `true`, this attribute takes precedence over any other attribute
controlling verbosity and requires the optimizer to produce no output.

The default value is `false` which has no effect. In this case the verbosity is
controlled by other optimizer-specific attributes.

## Value and default

The provided value must be a `Bool`.

The default value is `false`.

## Note

Every optimizer should have verbosity on by default. For instance, if a solver
has a solver-specific log level attribute, the MOI implementation should set it
to `1` by default. If the user sets `Silent` to `true`, then the log level
should be set to `0`, even if the user specifically sets a value of log level.
If the value of `Silent` is `false` then the log level set to the solver is the
value given by the user for this solver-specific parameter or `1` if none is
given.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.Silent)::Bool
MOI.set(::Optimizer, ::MOI.Silent, ::Bool)::Nothing
MOI.supports(::Optimizer, ::MOI.Silent)::Bool
```
"""
struct Silent <: AbstractOptimizerAttribute end

attribute_value_type(::Silent) = Bool

"""
    TimeLimitSec()

An [`AbstractOptimizerAttribute`](@ref) for setting a time limit (in seconds)
for a call to [`optimize!`](@ref).

## Value and default

The provided limit must be a `Union{Nothing,Real}`.

When `set` to `nothing`, it deactivates the time limit.

The default value is `nothing`.

## TerminationStatus

The optimizer may stop when the [`SolveTimeSec`](@ref) is larger than the
[`TimeLimitSec`](@ref). If stopped because of this limit, the
[`TerminationStatus`](@ref) must be [`TIME_LIMIT`](@ref).

Note that most optimizers do not strictly respect a time limit. Instead, they
terminate at the first convenient time after the time limit has been exceeded.
Thus, you may find that the [`SolveTimeSec`](@ref) exceeds the [`TimeLimitSec`](@ref)
by a few seconds.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.TimeLimitSec)::Union{Nothing,Float64}
MOI.set(::Optimizer, ::MOI.TimeLimitSec, ::Union{Nothing,Real})::Nothing
MOI.supports(::Optimizer, ::MOI.TimeLimitSec)::Bool
```
"""
struct TimeLimitSec <: AbstractOptimizerAttribute end

attribute_value_type(::TimeLimitSec) = Union{Nothing,Float64}

"""
    ObjectiveLimit()

An [`AbstractOptimizerAttribute`](@ref) for setting a limit on the objective
value.

## Value and default

The provided limit must be a `Union{Nothing,Real}`.

When `set` to `nothing`, the limit reverts to the solver's default.

The default value is `nothing`.

## TerminationStatus

The solver may stop when the [`ObjectiveValue`](@ref) is better (lower for
minimization, higher for maximization) than the `ObjectiveLimit`. If stopped,
because of this limit, the [`TerminationStatus`](@ref) should be
[`OBJECTIVE_LIMIT`](@ref).

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.ObjectiveLimit)::Union{Nothing,Float64}
MOI.set(::Optimizer, ::MOI.ObjectiveLimit, ::Union{Nothing,Real})::Nothing
MOI.supports(::Optimizer, ::MOI.ObjectiveLimit)::Bool
```
"""
struct ObjectiveLimit <: AbstractOptimizerAttribute end

"""
    SolutionLimit()

An [`AbstractOptimizerAttribute`](@ref) for setting a limit on the number of
available feasible solutions.

## Value and default

The provided limit must be a `Union{Nothing,Int}`.

When `set` to `nothing`, the limit reverts to the solver's default.

The default value is `nothing`.

## Termination criteria

The solver may stop when the [`ResultCount`](@ref) is larger than or equal to
the [`SolutionLimit`](@ref). If stopped because of this attribute, the
[`TerminationStatus`](@ref) must be [`SOLUTION_LIMIT`](@ref).

## Solution quality

The quality of the available solutions is solver-dependent. The set of resulting
solutions is not guaranteed to contain an optimal solution.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.SolutionLimit)::Union{Nothing,Int}
MOI.set(::Optimizer, ::MOI.SolutionLimit, ::Union{Nothing,Int})::Nothing
MOI.supports(::Optimizer, ::MOI.SolutionLimit)::Bool
```
"""
struct SolutionLimit <: AbstractOptimizerAttribute end

attribute_value_type(::SolutionLimit) = Union{Nothing,Int}

"""
    NodeLimit()

An [`AbstractOptimizerAttribute`](@ref) for setting a limit on the number of
branch-and-bound nodes explored by a mixed-integer program (MIP) solver.

## Value and default

The provided limit must be a `Union{Nothing,Int}`.

When `set` to `nothing`, the limit reverts to the solver's default.

The default value is `nothing`.

## Termination criteria

The solver may stop when the [`NodeCount`](@ref) is larger than or equal to
the [`NodeLimit`](@ref). If stopped because of this attribute, the
[`TerminationStatus`](@ref) must be [`NODE_LIMIT`](@ref).

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.NodeLimit)::Union{Nothing,Int}
MOI.set(::Optimizer, ::MOI.NodeLimit, ::Union{Nothing,Int})::Nothing
MOI.supports(::Optimizer, ::MOI.NodeLimit)::Bool
```
"""
struct NodeLimit <: AbstractOptimizerAttribute end

attribute_value_type(::NodeLimit) = Union{Nothing,Int}

"""
    RawOptimizerAttribute(name::String)

An [`AbstractOptimizerAttribute`](@ref) for the solver-specific parameter
identified by `name`.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.RawOptimizerAttribute)::Any
MOI.set(::Optimizer, ::MOI.RawOptimizerAttribute, ::Any)::Nothing
MOI.supports(::Optimizer, ::MOI.RawOptimizerAttribute)::Bool
```
"""
struct RawOptimizerAttribute <: AbstractOptimizerAttribute
    name::String
end

"""
    NumberOfThreads()

An [`AbstractOptimizerAttribute`](@ref) for setting the number of threads used
for an optimization.

## Value and default

The provided value must be `nothing` or a positive `Int`.

When `set` to `nothing`, the value reverts to the solver's default.

The default value is `nothing`.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.NumberOfThreads)::Union{Nothing,Int}
MOI.set(::Optimizer, ::MOI.NumberOfThreads, ::Union{Nothing,Int})::Nothing
MOI.supports(::Optimizer, ::MOI.NumberOfThreads)::Bool
```
"""
struct NumberOfThreads <: AbstractOptimizerAttribute end

attribute_value_type(::NumberOfThreads) = Union{Nothing,Int}

"""
    RelativeGapTolerance()

An [`AbstractOptimizerAttribute`](@ref) for setting the relative gap tolerance
for an optimization.

## Definition

The mathematical definition of "relative gap" and its allowed range are
solver-dependent. Typically, solvers expect a value between `0.0` and `1.0`.

## Value and default

The provided value must be a `Union{Nothing,Float64}`.

When set to `nothing`, the limit reverts to the solver's default.

## TerminationStatus

The optimizer may stop when the [`RelativeGap`](@ref) is smaller than the
[`RelativeGapTolerance`](@ref). If stopped because of this limit, the
[`TerminationStatus`](@ref) may be [`OPTIMAL`](@ref).

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.RelativeGapTolerance)::Union{Nothing,Float64}
MOI.set(
    ::Optimizer,
    ::MOI.RelativeGapTolerance,
    ::Union{Nothing,Float64},
)::Nothing
MOI.supports(::Optimizer, ::MOI.RelativeGapTolerance)::Bool
```
"""
struct RelativeGapTolerance <: AbstractOptimizerAttribute end

attribute_value_type(::RelativeGapTolerance) = Union{Nothing,Float64}

"""
    AbsoluteGapTolerance()

An [`AbstractOptimizerAttribute`](@ref) for setting the absolute gap tolerance
for an optimization.

## Definition

The mathematical definition of "absolute gap" and its allowed range are
solver-dependent. However, most solvers that implement this attribute will stop
once ``|f - b| ≤ g_{abs}``, where ``b`` is the best bound, ``f`` is the best
feasible objective value, and ``g_{abs}`` is the absolute gap.

## Value and default

The provided value must be a `Union{Nothing,Float64}`.

When set to `nothing`, the limit reverts to the solver's default.

## TerminationStatus

The optimizer may stop when the absolute difference between [`ObjectiveValue`](@ref)
and [`ObjectiveBound`](@ref) is smaller than the [`AbsoluteGapTolerance`](@ref).
If stopped because of this limit, the [`TerminationStatus`](@ref) may be
[`OPTIMAL`](@ref).

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.AbsoluteGapTolerance)::Union{Nothing,Float64}
MOI.set(
    ::Optimizer,
    ::MOI.AbsoluteGapTolerance,
    ::Union{Nothing,Float64},
)::Nothing
MOI.supports(::Optimizer, ::MOI.AbsoluteGapTolerance)::Bool
```
"""
struct AbsoluteGapTolerance <: AbstractOptimizerAttribute end

attribute_value_type(::AbsoluteGapTolerance) = Union{Nothing,Float64}

"""
    struct OptimizeInProgress{AttrType<:AnyAttribute} <: Exception
        attr::AttrType
    end

Error thrown from optimizer when `MOI.get(optimizer, attr)` is called inside an
[`AbstractCallback`](@ref) while it is only defined once [`optimize!`](@ref) has
completed. This can only happen when `is_set_by_optimize(attr)` is `true`.
"""
struct OptimizeInProgress{AttrType<:AnyAttribute} <: Exception
    attr::AttrType
end

function Base.showerror(io::IO, err::OptimizeInProgress)
    return print(
        io,
        typeof(err),
        ": Cannot get result as the `MOI.optimize!` has not",
        " finished.",
    )
end

"""
    abstract type AbstractCallback <: AbstractModelAttribute end

Abstract type for a model attribute representing a callback function. The
value set to subtypes of `AbstractCallback` is a function that may be called
during [`optimize!`](@ref). As [`optimize!`](@ref) is in progress, the result
attributes (that is, the attributes `attr` such that `is_set_by_optimize(attr)`)
may not be accessible from the callback, hence trying to get result attributes
might throw a [`OptimizeInProgress`](@ref) error.

At most one callback of each type can be registered. If an optimizer already has
a function for a callback type, and the user registers a new function, then the
old one is replaced.

The value of the attribute should be a function taking only one argument,
commonly called `callback_data`, that can be used for instance in
[`LazyConstraintCallback`](@ref), [`HeuristicCallback`](@ref) and
[`UserCutCallback`](@ref).
"""
abstract type AbstractCallback <: AbstractModelAttribute end

attribute_value_type(::AbstractCallback) = Function

"""
    LazyConstraintCallback() <: AbstractCallback

The callback can be used to reduce the feasible set given the current primal
solution by submitting a [`LazyConstraint`](@ref). For instance, it may be
called at an incumbent of a mixed-integer problem. Note that there is no
guarantee that the callback is called at *every* feasible primal solution.

The current primal solution is accessed through
[`CallbackVariablePrimal`](@ref). Trying to access other result
attributes will throw [`OptimizeInProgress`](@ref) as discussed in
[`AbstractCallback`](@ref).

## Example

```julia
x = MOI.add_variables(optimizer, 8)
MOI.set(optimizer, MOI.LazyConstraintCallback(), callback_data -> begin
    sol = MOI.get(optimizer, MOI.CallbackVariablePrimal(callback_data), x)
    if # should add a lazy constraint
        func = # computes function
        set = # computes set
        MOI.submit(optimizer, MOI.LazyConstraint(callback_data), func, set)
    end
end)
```
"""
struct LazyConstraintCallback <: AbstractCallback end

"""
    HeuristicCallback() <: AbstractCallback

The callback can be used to submit [`HeuristicSolution`](@ref) given the
current primal solution. For example, it may be called at fractional (that is,
non-integer) nodes in the branch and bound tree of a mixed-integer problem. Note
that there is no guarantee that the callback is called *every* time the solver
has an infeasible solution.

The current primal solution is accessed through [`CallbackVariablePrimal`](@ref).
Trying to access other result attributes will throw [`OptimizeInProgress`](@ref)
as discussed in [`AbstractCallback`](@ref).

## Example

```julia
x = MOI.add_variables(optimizer, 8)
MOI.set(optimizer, MOI.HeuristicCallback(), callback_data -> begin
    sol = MOI.get(optimizer, MOI.CallbackVariablePrimal(callback_data), x)
    if # can find a heuristic solution
        values = # computes heuristic solution
        MOI.submit(optimizer, MOI.HeuristicSolution(callback_data), x,
                   values)
    end
end
```
"""
struct HeuristicCallback <: AbstractCallback end

"""
    UserCutCallback() <: AbstractCallback

The callback can be used to submit [`UserCut`](@ref) given the current primal
solution. For instance, it may be called at fractional (that is, non-integer) nodes
in the branch and bound tree of a mixed-integer problem. Note that there is not
guarantee that the callback is called *everytime* the solver has an infeasible
solution.

The infeasible solution is accessed through [`CallbackVariablePrimal`](@ref).
Trying to access other result attributes will throw [`OptimizeInProgress`](@ref)
as discussed in [`AbstractCallback`](@ref).

## Example

```julia
x = MOI.add_variables(optimizer, 8)
MOI.set(optimizer, MOI.UserCutCallback(), callback_data -> begin
    sol = MOI.get(optimizer, MOI.CallbackVariablePrimal(callback_data), x)
    if # can find a user cut
        func = # computes function
        set = # computes set
        MOI.submit(optimizer, MOI.UserCut(callback_data), func, set)
    end
end
```
"""
struct UserCutCallback <: AbstractCallback end

"""
    ListOfModelAttributesSet()

An [`AbstractModelAttribute`](@ref) for the `Vector{AbstractModelAttribute}` of all model
attributes `attr` such that:

 1. `is_copyable(attr)` returns `true`, and
 2. the attribute was set to the model
"""
struct ListOfModelAttributesSet <: AbstractModelAttribute end

"""
    Name()

An [`AbstractModelAttribute`](@ref) for the string identifying the model.

It has a default value of `""` if not set.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.Name)::String
MOI.set(::Optimizer, ::MOI.Name, ::String)::Nothing
MOI.supports(::Optimizer, ::MOI.Name)::Bool
```
"""
struct Name <: AbstractModelAttribute end

attribute_value_type(::Name) = String

@_documented_enum(
    """
        OptimizationSense

    An Enum for the value of the [`ObjectiveSense`](@ref) attribute.
    """,
    OptimizationSense,
    "The goal is to minimize the objective function.",
    MIN_SENSE,
    "The goal is to maximize the objective function.",
    MAX_SENSE,
    "The model does not have an objective function.",
    FEASIBILITY_SENSE,
)

"""
    ObjectiveSense()

An [`AbstractModelAttribute`](@ref) for the objective sense of the objective
function, which must be an [`OptimizationSense`](@ref).

The default is [`FEASIBILITY_SENSE`](@ref).

## Interaction with [`ObjectiveFunction`](@ref)

Setting the sense to [`FEASIBILITY_SENSE`](@ref) unsets the [`ObjectiveFunction`](@ref)
attribute. That is, if you first set [`ObjectiveFunction`](@ref) and then set
`ObjectiveSense` to be [`FEASIBILITY_SENSE`](@ref), no objective function will
be passed to the solver.

In addition, some reformulations of [`ObjectiveFunction`](@ref) via bridges rely
on the value of [`ObjectiveSense`](@ref). Therefore, you should set
[`ObjectiveSense`](@ref) before setting [`ObjectiveFunction`](@ref).

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.ObjectiveSense)::MOI.OptimizationSense
MOI.set(::Optimizer, ::MOI.ObjectiveSense, ::MOI.OptimizationSense)::Nothing
MOI.supports(::Optimizer, ::MOI.ObjectiveSense)::Bool
```
"""
struct ObjectiveSense <: AbstractModelAttribute end

attribute_value_type(::ObjectiveSense) = OptimizationSense

"""
    NumberOfVariables()

An [`AbstractModelAttribute`](@ref) for the number of variables in the model.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.NumberOfVariables)::Int64
```

They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct NumberOfVariables <: AbstractModelAttribute end

attribute_value_type(::NumberOfVariables) = Int64

"""
    ListOfVariableIndices()

An [`AbstractModelAttribute`](@ref) for querying the `Vector{MOI.VariableIndex}`
of all [`MOI.VariableIndex`] present in the model.

## Order

The variables must be returned in the order in which they were added to the
model.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.ListOfVariableIndices)::Vector{MOI.VariableIndex}
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct ListOfVariableIndices <: AbstractModelAttribute end

"""
    ListOfConstraintIndices{F,S}()

An [`AbstractModelAttribute`](@ref) for the `Vector{MOI.ConstraintIndex{F,S}}`
of all constraint indices of type `F`-in-`S` in the model.

## Order

The constraints must be returned in the order in which they were added to the
model.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(
    ::Optimizer,
    ::MOI.ListOfConstraintIndices{F,S},
)::Vector{MOI.ConstraintIndex{F,S}} where {F<:MOI.AbstractFunction,MOI.AbstractSet}
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct ListOfConstraintIndices{F,S} <: AbstractModelAttribute end

function get_fallback(
    model::ModelLike,
    ::ListOfConstraintIndices{F,S},
) where {F,S}
    # Throw error here so that we don't return an incorrect value if solvers
    # don't implement this method for some constraints they support.
    if supports_constraint(model, F, S) ||
       (S === VariableIndex && supports_add_constrained_variable(model, S)) ||
       (S === VectorOfVariables && supports_add_constrained_variables(model, S))
        throw(GetAttributeNotAllowed(ListOfConstraintIndices{F,S}()))
    end
    return ConstraintIndex{F,S}[]
end

"""
    NumberOfConstraints{F,S}()

An [`AbstractModelAttribute`](@ref) for querying the number of constraints of
the type `F`-in-`S` present in the model.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(
    ::Optimizer,
    ::MOI.NumberOfConstraints{F,S},
)::Int64 where {F<:MOI.AbstractFunction,MOI.AbstractSet}
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct NumberOfConstraints{F,S} <: AbstractModelAttribute end

attribute_value_type(::NumberOfConstraints) = Int64

function get_fallback(model::ModelLike, ::NumberOfConstraints{F,S}) where {F,S}
    # Throw error here so that we don't return an incorrect value if solvers
    # don't implement this method for constraints they support.
    if supports_constraint(model, F, S) ||
       (S === VariableIndex && supports_add_constrained_variable(model, S)) ||
       (S === VectorOfVariables && supports_add_constrained_variables(model, S))
        throw(GetAttributeNotAllowed(NumberOfConstraints{F,S}()))
    end
    return Int64(0)
end

"""
    ListOfConstraintTypesPresent()

An [`AbstractModelAttribute`](@ref) for the list of tuples of the form `(F, S)`,
indicating that the attribute [`NumberOfConstraints{F,S}`](@ref) has a value
greater than zero.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(
    ::Optimizer,
    ::MOI.ListOfConstraintTypesPresent,
)::Vector{Tuple{Type,Type}}
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct ListOfConstraintTypesPresent <: AbstractModelAttribute end

"""
    ObjectiveFunction{F<:AbstractScalarFunction}()

An [`AbstractModelAttribute`](@ref) for the objective function which has a type
`F<:AbstractScalarFunction`.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(
    ::Optimizer,
    ::MOI.ObjectiveFunction{F},
)::F where {F<:MOI.AbstractFunction}
MOI.set(
    ::Optimizer,
    ::MOI.ObjectiveFunction{F},
    ::F,
)::F where {F<:MOI.AbstractFunction}
MOI.supports(::Optimizer, ::MOI.ObjectiveFunction{<:MOI.AbstractFunction})::Bool
```

When implementing `get`, `F` may to be equivalent but not necessarily identical
to the function type set by the user. If the objective function cannot be
converted to `F`, an `InexactError` must be thrown.
"""
struct ObjectiveFunction{F<:AbstractFunction} <: AbstractModelAttribute end

attribute_value_type(::ObjectiveFunction{F}) where {F} = F

"""
    ObjectiveFunctionType()

An [`AbstractModelAttribute`](@ref) for the type `F` of the objective function
set using the [`ObjectiveFunction{F}`](@ref) attribute.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(
    ::Optimizer,
    ::MOI.ObjectiveFunctionType,
)::Type{<:MOI.AbstractFunction}
```
They should not implement [`set`](@ref) or [`supports`](@ref).

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model)
MOI.VariableIndex(1)

julia> MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)

julia> MOI.get(model, MOI.ObjectiveFunctionType())
MathOptInterface.VariableIndex
```
"""
struct ObjectiveFunctionType <: AbstractModelAttribute end

attribute_value_type(::ObjectiveFunctionType) = Type{<:AbstractFunction}

"""
    ObjectiveValue(result_index::Int = 1)

An [`AbstractModelAttribute`](@ref) for the objective value of the primal
solution `result_index`.

## PrimalStatus

Before quering this attribute you should first check [`PrimalStatus`](@ref) to
confirm that a primal solution is avaiable.

If the [`PrimalStatus`](@ref) is [`NO_SOLUTION`](@ref) the result of querying
this attribute is undefined.

## `result_index`

The optimizer may return multiple primal solutions. See [`ResultCount`](@ref)
for information on how the results are ordered.

If the solver does not have a primal value for the objective because the
`result_index` is beyond the available solutions (whose number is indicated by
the [`ResultCount`](@ref) attribute), getting this attribute must throw a
[`ResultIndexBoundsError`](@ref).

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(
    ::Optimizer,
    ::MOI.ObjectiveValue,
)::Union{T,Vector{T}} where {T<:Real}
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct ObjectiveValue <: AbstractModelAttribute
    result_index::Int
    ObjectiveValue(result_index::Int = 1) = new(result_index)
end

"""
    DualObjectiveValue(result_index::Int = 1)

An [`AbstractModelAttribute`](@ref) for the value of the objective function of
the dual solution `result_index`.

## DualStatus

Before quering this attribute you should first check [`DualStatus`](@ref) to
confirm that a dual solution is avaiable.

If the [`DualStatus`](@ref) is [`NO_SOLUTION`](@ref) the result of querying
this attribute is undefined.

## `result_index`

The optimizer may return multiple dual solutions. See [`ResultCount`](@ref)
for information on how the results are ordered.

If the solver does not have a dual value for the objective because the
`result_index` is beyond the available solutions (whose number is indicated by
the [`ResultCount`](@ref) attribute), getting this attribute must throw a
[`ResultIndexBoundsError`](@ref).

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(
    ::Optimizer,
    ::MOI.DualObjectiveValue,
)::Union{T,Vector{T}} where {T<:Real}
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct DualObjectiveValue <: AbstractModelAttribute
    result_index::Int
    DualObjectiveValue(result_index::Int = 1) = new(result_index)
end

"""
    ObjectiveBound()

An [`AbstractModelAttribute`](@ref) for the best known bound on the optimal objective value.
"""
struct ObjectiveBound <: AbstractModelAttribute end

"""
    RelativeGap()

An [`AbstractModelAttribute`](@ref) for the final relative optimality gap.

!!! warning
    The definition of this gap is solver-dependent. However, most solvers
    implementing this attribute define the relative gap as some variation of
    ``\\frac{|b-f|}{|f|}``, where ``b`` is the best bound and ``f`` is the best
    feasible objective value.
"""
struct RelativeGap <: AbstractModelAttribute end

attribute_value_type(::RelativeGap) = Float64

"""
    SolveTimeSec()

An [`AbstractModelAttribute`](@ref) for the total elapsed solution time (in seconds) as reported
by the optimizer.
"""
struct SolveTimeSec <: AbstractModelAttribute end

attribute_value_type(::SolveTimeSec) = Float64

"""
    SimplexIterations()

An [`AbstractModelAttribute`](@ref) for the cumulative number of simplex
iterations while solving a problem.

For a mixed-integer program (MIP), the return value is the total simplex
iterations for all nodes.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.SimplexIterations)::Int64
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct SimplexIterations <: AbstractModelAttribute end

attribute_value_type(::SimplexIterations) = Int64

"""
    BarrierIterations()

An [`AbstractModelAttribute`](@ref) for the cumulative number of barrier
iterations while solving a problem.

For a mixed-integer program (MIP), the return value is the total barrier
iterations for all nodes.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.BarrierIterations)::Int64
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct BarrierIterations <: AbstractModelAttribute end

attribute_value_type(::BarrierIterations) = Int64

"""
    NodeCount()

An [`AbstractModelAttribute`](@ref) for the total number of branch-and-bound
nodes explored while solving a mixed-integer program (MIP).

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.NodeCount)::Int64
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct NodeCount <: AbstractModelAttribute end

attribute_value_type(::NodeCount) = Int64

"""
    RawSolver()

An [`AbstractModelAttribute`](@ref) for the object that may be used to access a
solver-specific API for this optimizer.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.RawSolver)::Any
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct RawSolver <: AbstractModelAttribute end

"""
    ResultCount()

An [`AbstractModelAttribute`](@ref) for the number of results available.

## Order of solutions

A number of attributes contain an index, `result_index`, which is used to refer
to one of the available results. Thus, `result_index` must be an integer between
`1` and the number of available results.

As a general rule, the first result (`result_index = 1`) is the most important
result (for example, an optimal solution or an infeasibility certificate). Other
results will typically be alternate solutions that the solver found during the
search for the first result.

If a (local) optimal solution is available, that is, [`TerminationStatus`](@ref)
is [`OPTIMAL`](@ref) or [`LOCALLY_SOLVED`](@ref), the first result must
correspond to the (locally) optimal solution. Other results may be alternative
optimal solutions, or they may be other suboptimal solutions; use
[`ObjectiveValue`](@ref) to distinguish between them.

If a primal or dual infeasibility certificate is available, that is,
[`TerminationStatus`](@ref) is [`INFEASIBLE`](@ref) or [`DUAL_INFEASIBLE`](@ref)
and the corresponding [`PrimalStatus`](@ref) or [`DualStatus`](@ref) is
[`INFEASIBILITY_CERTIFICATE`](@ref), then the first result must be a certificate.
Other results may be alternate certificates, or infeasible points.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.ResultCount)::Int
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct ResultCount <: AbstractModelAttribute end

attribute_value_type(::ResultCount) = Int

@_documented_enum(
    """
        ConflictStatusCode

    An Enum of possible values for the [`ConflictStatus`](@ref) attribute.

    This attribute is meant to explain the reason why the conflict finder
    stopped executing in the most recent call to [`compute_conflict!`](@ref).
    """,
    ConflictStatusCode,
    """
    The function [`compute_conflict!`](@ref) has not yet been called.
    """,
    COMPUTE_CONFLICT_NOT_CALLED,
    "There is no conflict because the problem is feasible.",
    NO_CONFLICT_EXISTS,
    "The solver could not find a conflict.",
    NO_CONFLICT_FOUND,
    "The solver found a conflict.",
    CONFLICT_FOUND,
)

"""
    ConflictStatus()

An [`AbstractModelAttribute`](@ref) for the [`ConflictStatusCode`](@ref)
explaining why [`compute_conflict!`] stopped when computing the conflict.

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.ConflictStatus)::MOI.ConflictStatusCode
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct ConflictStatus <: AbstractModelAttribute end

attribute_value_type(::ConflictStatus) = ConflictStatusCode

"""
    ConflictCount()

An [`AbstractModelAttribute`](@ref) for the number of conflicts found by the
solver in the most recent call to [`compute_conflict!`](@ref).

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.ConflictCount)::Int
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct ConflictCount <: AbstractModelAttribute end

attribute_value_type(::ConflictCount) = Int

"""
    ListOfVariableAttributesSet()

An [`AbstractModelAttribute`](@ref) for the `Vector{AbstractVariableAttribute}`
of all variable attributes `attr` such that:

 1. `is_copyable(attr)` returns `true`
 2. the attribute was set for at least one variable in the model

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(
    ::Optimizer,
    ::MOI.ListOfVariableAttributesSet,
)::Vector{MOI.AbstractVariableAttribute}
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct ListOfVariableAttributesSet <: AbstractModelAttribute end

"""
    ListOfVariablesWithAttributeSet(attr::AbstractVariableAttribute)

An [`AbstractModelAttribute`](@ref) for the `Vector{MOI.VariableIndex}` of all
variables with the attribute `attr` set.

The returned list may not be minimal, so some elements may have their default
value set.

## Note

This is an optional attribute to implement. The default fallback is to get
[`ListOfVariableIndices`](@ref).

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(
    ::Optimizer,
    ::MOI.ListOfVariablesWithAttributeSet{<:MOI.AbstractVariableAttribute},
)::Vector{MOI.VarialbeIndex}
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct ListOfVariablesWithAttributeSet{A} <: AbstractModelAttribute
    attr::A
    function ListOfVariablesWithAttributeSet(attr::AbstractVariableAttribute)
        return new{typeof(attr)}(attr)
    end
end

function get_fallback(model::ModelLike, ::ListOfVariablesWithAttributeSet)
    return get(model, ListOfVariableIndices())
end

"""
    VariableName()

An [`AbstractVariableAttribute`](@ref) for a `String` identifying the variable.

The default name is `""` if not set by the user.

## Duplicate names

Two variables may have the same name; however, variables with duplicate names
cannot be looked up using [`get`](@ref).

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model);

julia> MOI.supports(model, MOI.VariableName(), MOI.VariableIndex)
true

julia> MOI.get(model, MOI.VariableName(), x)
""

julia> MOI.set(model, MOI.VariableName(), x, "x")

julia> MOI.get(model, MOI.VariableName(), x)
"x"
```

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.VariableName, ::MOI.VariableIndex)::String
MOI.set(::Optimizer, ::MOI.VariableName, ::MOI.VariableIndex, ::String)::Nothing
MOI.supports(::Optimizer, ::MOI.VariableName, ::Type{MOI.VariableIndex})::Bool
```
"""
struct VariableName <: AbstractVariableAttribute end

attribute_value_type(::VariableName) = String

"""
    VariablePrimalStart()

An [`AbstractVariableAttribute`](@ref) for the initial assignment to the
variable's primal value that the optimizer may use to warm-start the solve.

May be a number or `nothing` (unset).

## Example

```jldoctest
julia> model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}());

julia> x = MOI.add_variable(model);

julia> MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
true

julia> MOI.get(model, MOI.VariablePrimalStart(), x)

julia> MOI.set(model, MOI.VariablePrimalStart(), x, 1.0)

julia> MOI.get(model, MOI.VariablePrimalStart(), x)
1.0
```

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.VariablePrimalStart, ::MOI.VariableIndex)::Union{Nothing,T}
MOI.set(::Optimizer, ::MOI.VariablePrimalStart, ::MOI.VariableIndex, ::Union{Nothing,T})::Nothing
MOI.supports(::Optimizer, ::MOI.VariablePrimalStart, ::Type{MOI.VariableIndex})::Bool
```
"""
struct VariablePrimalStart <: AbstractVariableAttribute end

"""
    VariablePrimal(result_index::Int = 1)

An [`AbstractVariableAttribute`](@ref) for the variable's primal value in result
`result_index`.

## PrimalStatus

Before quering this attribute you should first check [`PrimalStatus`](@ref) to
confirm that a primal solution is avaiable.

If the [`PrimalStatus`](@ref) is [`NO_SOLUTION`](@ref) the result of querying
this attribute is undefined.

## `result_index`

The optimizer may return multiple primal solutions. See [`ResultCount`](@ref)
for information on how the results are ordered.

If the solver does not have a primal value for the variable because the
`result_index` is beyond the available solutions (whose number is indicated by
the [`ResultCount`](@ref) attribute), getting this attribute must throw a
[`ResultIndexBoundsError`](@ref).

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.VariablePrimal, ::MOI.VariableIndex)::T
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct VariablePrimal <: AbstractVariableAttribute
    result_index::Int
    VariablePrimal(result_index::Int = 1) = new(result_index)
end

"""
    CallbackVariablePrimal(callback_data)

An [`AbstractVariableAttribute`](@ref) for the assignment to the variable's
primal value during the callback identified by `callback_data`.
"""
struct CallbackVariablePrimal{CallbackDataType} <: AbstractVariableAttribute
    callback_data::CallbackDataType
end
is_set_by_optimize(::CallbackVariablePrimal) = true

@_documented_enum(
    """
        BasisStatusCode

    An Enum for the value of the [`ConstraintBasisStatus`](@ref) and
    [`VariableBasisStatus`](@ref) attributes, explaining the status of a given
    element with respect to an optimal solution basis.

    ## Notes

    When queried as part of [`ConstraintBasisStatus`](@ref), [`NONBASIC_AT_LOWER`](@ref)
    and [`NONBASIC_AT_UPPER`](@ref) should be returned only for constraints with
    the [`Interval`](@ref) set. In this case, they are necessary to distinguish
    which side of the constraint is active. One-sided constraints (for example,
    [`LessThan`](@ref) and [`GreaterThan`](@ref)) should use [`NONBASIC`](@ref)
    instead of the `NONBASIC_AT_*` values.

    This restriction does not apply to [`VariableBasisStatus`](@ref), which
    should return `NONBASIC_AT_*` regardless of whether the alternative bound
    exists.
    """,
    BasisStatusCode,
    "The element is in the basis.",
    BASIC,
    "The element is not in the basis.",
    NONBASIC,
    """
    The element is not in the basis and is at its lower bound.
    """,
    NONBASIC_AT_LOWER,
    """
    The element is not in the basis and is at its upper bound.
    """,
    NONBASIC_AT_UPPER,
    """
    The element is not in the basis but is also not at one of its bounds.

    In a linear program, this status occurs when a variable with no bounds is
    not in the basis, for example, because it takes the value `0.0`.
    """,
    SUPER_BASIC,
)

"""
    VariableBasisStatus(result_index::Int = 1)

An [`AbstractVariableAttribute`](@ref) for the [`BasisStatusCode`](@ref) of the
variable in result `result_index`, with respect to a basic solution.

## PrimalStatus

Before quering this attribute you should first check [`PrimalStatus`](@ref) to
confirm that a primal solution is avaiable.

If the [`PrimalStatus`](@ref) is [`NO_SOLUTION`](@ref) the result of querying
this attribute is undefined.

## `result_index`

The optimizer may return multiple primal solutions. See [`ResultCount`](@ref)
for information on how the results are ordered.

If the solver does not have a primal value for the variable because the
`result_index` is beyond the available solutions (whose number is indicated by
the [`ResultCount`](@ref) attribute), getting this attribute must throw a
[`ResultIndexBoundsError`](@ref).

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.VariableBasisStatus, ::MOI.VariableIndex)::MOI.BasisStatusCode
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct VariableBasisStatus <: AbstractVariableAttribute
    result_index::Int
    VariableBasisStatus(result_index::Int = 1) = new(result_index)
end

attribute_value_type(::VariableBasisStatus) = BasisStatusCode

"""
    ListOfConstraintAttributesSet{F, S}()

An [`AbstractModelAttribute`](@ref) for the `Vector{AbstractConstraintAttribute}` of all
constraint attributes `attr` such that:

 1. `is_copyable(attr)` returns `true` and
 2. the attribute was set to `F`-in-`S` constraints.

## Note

The attributes [`ConstraintFunction`](@ref) and [`ConstraintSet`](@ref) should
not be included in the list even if then have been set with [`set`](@ref).
"""
struct ListOfConstraintAttributesSet{F,S} <: AbstractModelAttribute end

"""
    ListOfConstraintsWithAttributeSet{F,S}(attr:AbstractConstraintAttribute)

An [`AbstractModelAttribute`](@ref) for the `Vector{ConstraintIndex{F,S}}` of all constraints with
the attribute `attr` set.

The returned list may not be minimal, so some elements may have their default
value set.

## Note

This is an optional attribute to implement. The default fallback is to get
[`ListOfConstraintIndices`](@ref).
"""
struct ListOfConstraintsWithAttributeSet{F,S,A} <: AbstractModelAttribute
    attr::A
    function ListOfConstraintsWithAttributeSet{F,S}(
        attr::AbstractConstraintAttribute,
    ) where {F,S}
        return new{F,S,typeof(attr)}(attr)
    end
end

function get_fallback(
    model::ModelLike,
    ::ListOfConstraintsWithAttributeSet{F,S},
) where {F,S}
    return get(model, ListOfConstraintIndices{F,S}())
end

"""
    ConstraintName()

An [`AbstractConstraintAttribute`](@ref) for a `String` identifying the
constraint.

The default name is `""` if not set by the user.

## Duplicate names

Two constraints may have the same name; however, constraints with duplicate
names cannot be looked up using [`get`](@ref), regardless of whether they have
the same `F`-in-`S` type.

## VariableIndex connstraints

You should _not_ implement [`ConstraintName`](@ref) for [`VariableIndex`](@ref)
constraints.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model);

julia> c = MOI.add_constraint(model, 1.0 * x, MOI.EqualTo(1.0));

julia> MOI.supports(model, MOI.ConstraintName(), typeof(c))
true

julia> MOI.get(model, MOI.ConstraintName(), c)
""

julia> MOI.set(model, MOI.ConstraintName(), c, "c")

julia> MOI.get(model, MOI.ConstraintName(), c)
"c"

julia> MOI.get(model, MOI.ConstraintIndex, "c")
MathOptInterface.ConstraintIndex{MathOptInterface.ScalarAffineFunction{Float64}, MathOptInterface.EqualTo{Float64}}(1)

julia> F, S = MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64};

julia> MOI.get(model, MOI.ConstraintIndex{F,S}, "c")
MathOptInterface.ConstraintIndex{MathOptInterface.ScalarAffineFunction{Float64}, MathOptInterface.EqualTo{Float64}}(1)
```

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(::Optimizer, ::MOI.ConstraintName, ::MOI.ConstraintIndex)::String
MOI.set(::Optimizer, ::MOI.ConstraintName, ::MOI.ConstraintIndex, ::String)::Nothing
MOI.supports(::Optimizer, ::MOI.ConstraintName, ::Type{<:MOI.ConstraintIndex})::Bool
MOI.get(::Optimizer, ::MOI.ConstraintIndex, ::MOI.ConstraintIndex, ::String)::MOI.ConstraintIndex
```
"""
struct ConstraintName <: AbstractConstraintAttribute end

attribute_value_type(::ConstraintName) = String

"""
    VariableIndexConstraintNameError()

An error to be thrown when the user tries to set `ConstraintName` on a
`VariableIndex` constraint.
"""
function VariableIndexConstraintNameError()
    return UnsupportedAttribute(
        ConstraintName(),
        "`ConstraintName`s are not supported for `VariableIndex` constraints.",
    )
end

function supports_fallback(
    ::ModelLike,
    ::ConstraintName,
    ::Type{ConstraintIndex{VariableIndex,S}},
) where {S}
    return throw(VariableIndexConstraintNameError())
end

"""
    ConstraintPrimalStart()

An [`AbstractConstraintAttribute`](@ref) for the initial assignment to the
constraint's [`ConstraintPrimal`](@ref) that the optimizer may use to warm-start
the solve.

May be `nothing` (unset), a number for [`AbstractScalarFunction`](@ref), or a
vector for [`AbstractVectorFunction`](@ref).
"""
struct ConstraintPrimalStart <: AbstractConstraintAttribute end

"""
    ConstraintDualStart()

An [`AbstractConstraintAttribute`](@ref) for the initial assignment to the
constraint's [`ConstraintDual`](@ref) that the optimizer may use to warm-start
the solve.

May be `nothing` (unset), a number for [`AbstractScalarFunction`](@ref), or a
vector for [`AbstractVectorFunction`](@ref).
"""
struct ConstraintDualStart <: AbstractConstraintAttribute end

"""
    ConstraintPrimal(result_index::Int = 1)

An [`AbstractConstraintAttribute`](@ref) for the constraint's primal value in
result `result_index`.

## Definition

If the constraint is ``f(x) \\in S``, then in most cases the [`ConstraintPrimal`](@ref)
is the value of ``f``, evaluated at the corresponding [`VariablePrimal`](@ref)
solution.

However, some conic solvers reformulate ``b - Ax \\in S`` to ``s = b - Ax`` and
``s \\in S``. These solvers may return the value of `s` for [`ConstraintPrimal`](@ref),
rather than `b - Ax`. (Although these are constrained by an equality constraint,
due to numerical tolerances they may not be identical.)

## PrimalStatus

Before quering this attribute you should first check [`PrimalStatus`](@ref) to
confirm that a primal solution is avaiable.

If the [`PrimalStatus`](@ref) is [`NO_SOLUTION`](@ref) the result of querying
this attribute is undefined.

## `result_index`

The optimizer may return multiple primal solutions. See [`ResultCount`](@ref)
for information on how the results are ordered.

If the solver does not have a primal value for the constraint because the
`result_index` is beyond the available solutions (whose number is indicated by
the [`ResultCount`](@ref) attribute), getting this attribute must throw a
[`ResultIndexBoundsError`](@ref).

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(
    ::Optimizer,
    ::MOI.ConstraintPrimal,
    ::MOI.ConstraintIndex{<:MOI.AbstractScalarFunction}
)::T
MOI.get(
    ::Optimizer,
    ::MOI.ConstraintPrimal,
    ::MOI.ConstraintIndex{<:MOI.AbstractVectorFunction}
)::Vector{T}
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct ConstraintPrimal <: AbstractConstraintAttribute
    result_index::Int
    ConstraintPrimal(result_index::Int = 1) = new(result_index)
end

"""
    ConstraintDual(result_index::Int = 1)

An [`AbstractConstraintAttribute`](@ref) for the constraint's dual value in
result `result_index`.

## DualStatus

Before quering this attribute you should first check [`DualStatus`](@ref) to
confirm that a dual solution is avaiable.

If the [`DualStatus`](@ref) is [`NO_SOLUTION`](@ref) the result of querying
this attribute is undefined.

## `result_index`

The optimizer may return multiple dual solutions. See [`ResultCount`](@ref)
for information on how the results are ordered.

If the solver does not have a dual value for the constraint because the
`result_index` is beyond the available solutions (whose number is indicated by
the [`ResultCount`](@ref) attribute), getting this attribute must throw a
[`ResultIndexBoundsError`](@ref).

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(
    ::Optimizer,
    ::MOI.ConstraintDual,
    ::MOI.ConstraintIndex{<:MOI.AbstractScalarFunction}
)::T
MOI.get(
    ::Optimizer,
    ::MOI.ConstraintDual,
    ::MOI.ConstraintIndex{<:MOI.AbstractVectorFunction}
)::Vector{T}
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct ConstraintDual <: AbstractConstraintAttribute
    result_index::Int
    ConstraintDual(result_index::Int = 1) = new(result_index)
end

"""
    ConstraintBasisStatus(result_index::Int = 1)

An [`AbstractConstraintAttribute`](@ref) for the [`BasisStatusCode`](@ref) of
the constraint in result `result_index`, with respect to a basic solution.

If `result_index` is omitted, it is 1 by default.

If the solver does not have a basis status for the constraint because the
`result_index` is beyond the available solutions (whose number is indicated by
the [`ResultCount`](@ref) attribute), getting this attribute must throw a
[`ResultIndexBoundsError`](@ref). Otherwise, if the result is unavailable for
another reason (for instance, only a dual solution is available), the result is
undefined. Users should first check [`PrimalStatus`](@ref) before accessing the
`ConstraintBasisStatus` attribute.

See [`ResultCount`](@ref) for information on how the results are ordered.

## Notes

For the basis status of a variable, query [`VariableBasisStatus`](@ref).

`ConstraintBasisStatus` does not apply to `VariableIndex` constraints. You
can infer the basis status of a [`VariableIndex`](@ref) constraint by looking
at the result of [`VariableBasisStatus`](@ref).
"""
struct ConstraintBasisStatus <: AbstractConstraintAttribute
    result_index::Int
    ConstraintBasisStatus(result_index::Int = 1) = new(result_index)
end

attribute_value_type(::ConstraintBasisStatus) = BasisStatusCode

function get_fallback(
    ::ModelLike,
    ::ConstraintBasisStatus,
    ::ConstraintIndex{VariableIndex,<:AbstractScalarSet},
)
    return error(
        "Querying the basis status of a `VariableIndex` constraint is not ",
        "supported. Use [`VariableBasisStatus`](@ref) instead.",
    )
end

"""
    CanonicalConstraintFunction()

An [`AbstractConstraintAttribute`](@ref) for a canonical representation of the
[`AbstractFunction`](@ref) object used to define the constraint.

Getting this attribute is guaranteed to return a function that is equivalent but
not necessarily identical to the function provided by the user.

## Fallback

By default, `MOI.get(model, MOI.CanonicalConstraintFunction(), ci)` falls back
to `MOI.Utilities.canonical(MOI.get(model, MOI.ConstraintFunction(), ci))`.

However, if `model` knows that the constraint function is canonical then it can
implement a specialized method that directly return the function without calling
[`Utilities.canonical`](@ref). Therefore, the value returned **cannot** be
assumed to be a copy of the function stored in `model`.

Moreover, [`Utilities.Model`](@ref) checks with [`Utilities.is_canonical`](@ref)
whether the function stored internally is already canonical and if it's the case,
then it returns the function stored internally instead of a copy.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model);

julia> c = MOI.add_constraint(model, 1.0 * x + 1.0 * x, MOI.GreaterThan(0.0));

julia> MOI.get(model, MOI.CanonicalConstraintFunction(), c)
0.0 + 2.0 MOI.VariableIndex(1)
```

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(
    ::Optimizer,
    ::MOI.CanonicalConstraintFunction,
    ::MOI.ConstraintIndex{F,S},
)::F where {F,S}
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct CanonicalConstraintFunction <: AbstractConstraintAttribute end

attribute_value_type(::CanonicalConstraintFunction) = AbstractFunction

function get_fallback(
    model::ModelLike,
    ::CanonicalConstraintFunction,
    ci::ConstraintIndex,
)
    func = get(model, ConstraintFunction(), ci)
    # In `Utilities.AbstractModel` and `Utilities.UniversalFallback`,
    # the function is canonicalized in `add_constraint` so it might already
    # be canonical. In other models, the constraint might have been copied from
    # from one of these two model so there is in fact a good chance of the
    # function being canonical in any model type.
    # As `is_canonical` is quite cheap compared to `canonical` which
    # requires a copy and sorting the terms, it is worth checking.
    if Utilities.is_canonical(func)
        return func
    else
        return Utilities.canonical(func)
    end
end

"""
    ConstraintFunction()

An [`AbstractConstraintAttribute`](@ref) for the [`AbstractFunction`](@ref)
object used to define the constraint.

It is guaranteed to be equivalent but not necessarily identical to the function
provided by the user.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model);

julia> c = MOI.add_constraint(model, 1.0 * x, MOI.GreaterThan(0.0));

julia> MOI.get(model, MOI.ConstraintFunction(), c)
0.0 + 1.0 MOI.VariableIndex(1)

julia> MOI.set(model, MOI.ConstraintFunction(), c, 2.0 * x)

julia> MOI.get(model, MOI.ConstraintFunction(), c)
0.0 + 2.0 MOI.VariableIndex(1)
```

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(
    ::Optimizer,
    ::MOI.ConstraintFunction,
    ::MOI.ConstraintIndex{F,S},
)::F where {F,S}
```
If the optimizer supports modifying an existing function, it should implement:
```julia
MOI.set(
    ::Optimizer,
    ::MOI.ConstraintFunction,
    ::MOI.ConstraintIndex{F,S},
    ::F,
)::Nothing where {F,S}
```
It should not implement [`supports`](@ref).
"""
struct ConstraintFunction <: AbstractConstraintAttribute end

attribute_value_type(::ConstraintFunction) = AbstractFunction

struct FunctionTypeMismatch{F1,F2} <: Exception end

function Base.showerror(io::IO, err::FunctionTypeMismatch{F1,F2}) where {F1,F2}
    return print(
        io,
        """$(typeof(err)): Cannot modify functions of different types.
  Constraint type is $F1 while the replacement function is of type $F2.""",
    )
end

function throw_set_error_fallback(
    ::ModelLike,
    attr::ConstraintFunction,
    ::ConstraintIndex{F,S},
    ::F;
    error_if_supported = SetAttributeNotAllowed(attr),
) where {F<:AbstractFunction,S}
    return throw(error_if_supported)
end

func_type(::ConstraintIndex{F,S}) where {F,S} = F

function throw_set_error_fallback(
    ::ModelLike,
    ::ConstraintFunction,
    ci::ConstraintIndex,
    func::AbstractFunction;
    kwargs...,
)
    return throw(FunctionTypeMismatch{func_type(ci),typeof(func)}())
end

"""
    ConstraintSet()

An [`AbstractConstraintAttribute`](@ref) for the [`AbstractSet`](@ref) object
used to define the constraint.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model);

julia> c = MOI.add_constraint(model, x, MOI.GreaterThan(0.0));

julia> MOI.get(model, MOI.ConstraintSet(), c)
MathOptInterface.GreaterThan{Float64}(0.0)

julia> MOI.set(model, MOI.ConstraintSet(), c, MOI.GreaterThan(1.0))

julia> MOI.get(model, MOI.ConstraintSet(), c)
MathOptInterface.GreaterThan{Float64}(1.0)
```

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(
    ::Optimizer,
    ::MOI.ConstraintSet,
    ::MOI.ConstraintIndex{F,S},
)::S where {F,S}
```
If the optimizer supports modifying an existing set, it should implement:
```julia
MOI.set(
    ::Optimizer,
    ::MOI.ConstraintSet,
    ::MOI.ConstraintIndex{F,S},
    ::S,
)::Nothing where {F,S}
```
It should not implement [`supports`](@ref).
"""
struct ConstraintSet <: AbstractConstraintAttribute end

attribute_value_type(::ConstraintSet) = AbstractSet

struct SetTypeMismatch{S1,S2} <: Exception end

function Base.showerror(io::IO, err::SetTypeMismatch{S1,S2}) where {S1,S2}
    return print(
        io,
        """$(typeof(err)): Cannot modify sets of different types. Constraint
  type is $S1 while the replacement set is of type $S2. Use `transform`
  instead.""",
    )
end

function throw_set_error_fallback(
    ::ModelLike,
    attr::ConstraintSet,
    ::ConstraintIndex{F,S},
    ::S;
    error_if_supported = SetAttributeNotAllowed(attr),
) where {F,S<:AbstractSet}
    return throw(error_if_supported)
end

set_type(::ConstraintIndex{F,S}) where {F,S} = S

function throw_set_error_fallback(
    ::ModelLike,
    ::ConstraintSet,
    constraint_index::ConstraintIndex,
    set::AbstractSet;
    kwargs...,
)
    return throw(SetTypeMismatch{set_type(constraint_index),typeof(set)}())
end

@_documented_enum(
    """
        ConflictParticipationStatusCode

    An Enum for the value of the [`ConstraintConflictStatus`](@ref) attribute.

    This attribute is meant to indicate whether a given constraint participates
    or not in the last computed conflict.
    """,
    ConflictParticipationStatusCode,
    "The constraint does not participate in the conflict.",
    NOT_IN_CONFLICT,
    "The constraint participates in the conflict.",
    IN_CONFLICT,
    """
    The solver was not able to prove whether the constraint is required to
    participate in the conflict.
    """,
    MAYBE_IN_CONFLICT,
)

"""
    ConstraintConflictStatus(conflict_index = 1)

A constraint attribute to query the [`ConflictParticipationStatusCode`](@ref)
indicating whether the constraint participates in the conflict.

## `conflict_index`

The optimizer may return multiple conflicts. See [`ConflictCount`](@ref)
for querying the number of conflicts found.

If the solver does not have a conflict because the
`conflict_index` is beyond the available solutions (whose number is indicated by
the [`ConflictCount`](@ref) attribute), then
`MOI.check_result_index_bounds(model, ConstraintConflictStatus(conflict_index))`
will throw a [`ResultIndexBoundsError`](@ref).

## Implementation

Optimizers should implement the following methods:
```julia
MOI.get(
    ::Optimizer,
    ::MOI.ConstraintConflictStatus,
    ::MOI.ConstraintIndex,
)::ConflictParticipationStatusCode
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct ConstraintConflictStatus <: AbstractConstraintAttribute
    conflict_index::Int
    ConstraintConflictStatus(conflict_index = 1) = new(conflict_index)
end

function attribute_value_type(::ConstraintConflictStatus)
    return ConflictParticipationStatusCode
end

"""
    UserDefinedFunction(name::Symbol, arity::Int) <: AbstractModelAttribute

Set this attribute to register a user-defined function by the name of `name`
with `arity` arguments.

Once registered, `name` will appear in [`ListOfSupportedNonlinearOperators`](@ref).

You cannot register multiple `UserDefinedFunction`s with the same `name` but
different `arity`.

## Value type

The value to be set is a tuple containing one, two, or three functions to
evaluate the function, the first-order derivative, and the second-order
derivative respectively. Both derivatives are optional, but if you pass the
second-order derivative you must also pass the first-order derivative.

For univariate functions with `arity == 1`, the functions in the tuple must
have the form:

 * `f(x::T)::T`: returns the value of the function at `x`
 * `∇f(x::T)::T`: returns the first-order derivative of `f` with respect to `x`
 * `∇²f(x::T)::T`: returns the second-order derivative of `f` with respect to
   `x`.

For multivariate functions with `arity > 1`, the functions in the tuple must
have the form:

 * `f(x::T...)::T`: returns the value of the function at `x`
 * `∇f(g::AbstractVector{T}, x::T...)::Nothing`: fills the components of `g`,
   with `g[i]` being the first-order partial derivative of `f` with respect to
   `x[i]`
 * `∇²f(H::AbstractMatrix{T}, x::T...)::Nothing`: fills the non-zero components
    of `H`, with `H[i, j]` being the second-order partial derivative of `f` with
    respect to `x[i]` and then `x[j]`. `H` is initialized to the zero matrix,
    so you do not need to set any zero elements.

## Example

```jldoctest
julia> f(x, y) = x^2 + y^2
f (generic function with 1 method)

julia> function ∇f(g, x, y)
           g .= 2 * x, 2 * y
           return
       end
∇f (generic function with 1 method)

julia> function ∇²f(H, x...)
           H[1, 1] = H[2, 2] = 2.0
           return
       end
∇²f (generic function with 1 method)

julia> model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}());

julia> MOI.set(model, MOI.UserDefinedFunction(:f, 2), (f,))

julia> MOI.set(model, MOI.UserDefinedFunction(:g, 2), (f, ∇f))

julia> MOI.set(model, MOI.UserDefinedFunction(:h, 2), (f, ∇f, ∇²f))

julia> x = MOI.add_variables(model, 2)
2-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(1)
 MOI.VariableIndex(2)

julia> MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)

julia> obj_f = MOI.ScalarNonlinearFunction(:f, Any[x[1], x[2]])
f(MOI.VariableIndex(1), MOI.VariableIndex(2))

julia> MOI.set(model, MOI.ObjectiveFunction{typeof(obj_f)}(), obj_f)

julia> print(model)
Minimize ScalarNonlinearFunction:
 f(v[1], v[2])

Subject to:

```
"""
struct UserDefinedFunction <: AbstractModelAttribute
    name::Symbol
    arity::Int
end

"""
    ListOfSupportedNonlinearOperators() <: AbstractModelAttribute

When queried with [`get`](@ref), return a `Vector{Symbol}` listing the operators
supported by the model. These operators may appear in the `head` field of
[`ScalarNonlinearFunction`](@ref).

## Implementation

Optimizers should implement the following methods:
```
MOI.get(
    ::Optimizer,
    ::MOI.ListOfSupportedNonlinearOperators,
)::Vector{Symbol}
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct ListOfSupportedNonlinearOperators <: AbstractOptimizerAttribute end

@_documented_enum(
    """
        TerminationStatusCode

    An Enum of possible values for the [`TerminationStatus`](@ref) attribute.

    This attribute explains why the optimizer stopped executing in the most
    recent call to [`optimize!`](@ref).
    """,
    TerminationStatusCode,
    "The algorithm has not started.",
    OPTIMIZE_NOT_CALLED,
    # OK
    """
    The algorithm found a globally optimal solution.
    """,
    OPTIMAL,
    """
    The algorithm proved that no primal feasible solution exists.
    """,
    INFEASIBLE,
    """
    The algorithm proved that no dual feasible solution exists.

    To check if the primal problem is feasible, set the objective sense to
    [`FEASIBILITY_SENSE`](@ref) and re-solve the problem.

    If a primal feasible point does not exist, the original problem is both
    primal and dual infeasible.

    If a primal feasible solution exists, this status typically implies that the
    problem is unbounded, with some technical exceptions (for example, if the
    problem is a conic optimization problem in which strong duality does not
    hold).

    The technical exceptions do not apply to linear programs. The combination of
    [`DUAL_INFEASIBLE`](@ref) and a primal feasible point means that the primal
    linear program is unbounded.
    """,
    DUAL_INFEASIBLE,
    """
    The algorithm converged to a stationary point, local optimal solution, could
    not find directions for improvement, or otherwise completed its search
    without global guarantees.
    """,
    LOCALLY_SOLVED,
    """
    The algorithm converged to an infeasible point or otherwise completed its
    search without finding a feasible solution, without guarantees that no
    feasible solution exists.

    If you know a primal feasible solution exists, use
    [`VariablePrimalStart`](@ref) to provide a feasible starting point to the
    solver.
    """,
    LOCALLY_INFEASIBLE,
    """
    The algorithm stopped because it proved that the problem is infeasible or
    unbounded, without distinguishing between the two cases.

    To distinguish between the two cases, set the objective sense to
    [`FEASIBILITY_SENSE`](@ref) and re-solve the problem. If a primal feasible
    point exists, the original problem is unbounded. If a primal feasible point
    does not exist, the original problem is infeasible.
    """,
    INFEASIBLE_OR_UNBOUNDED,
    # Solved to relaxed tolerances
    "The algorithm found a globally optimal solution to relaxed tolerances.",
    ALMOST_OPTIMAL,
    """
    The algorithm concluded that no feasible solution exists within relaxed
    tolerances.
    """,
    ALMOST_INFEASIBLE,
    """
    The algorithm concluded that no dual bound exists for the problem within
    relaxed tolerances.
    """,
    ALMOST_DUAL_INFEASIBLE,
    """
    The algorithm converged to a stationary point, local optimal solution, or
    could not find directions for improvement within relaxed tolerances.
    """,
    ALMOST_LOCALLY_SOLVED,
    # Limits
    """
    An iterative algorithm stopped after conducting the maximum number of
    iterations.
    """,
    ITERATION_LIMIT,
    """
    The algorithm stopped after a user-specified computation time.

    This status may be returned in relation to the [`TimeLimitSec`](@ref)
    attribute, or some other solver-specific attribute.
    """,
    TIME_LIMIT,
    """
    A branch-and-bound algorithm stopped because it explored a maximum number of
    nodes in the branch-and-bound tree.

    This status may be returned in relation to the [`NodeLimit`](@ref)
    attribute, or some other solver-specific attribute.
    """,
    NODE_LIMIT,
    """
    The algorithm stopped because it found the required number of solutions.
    This is often used in MIPs to get the solver to return the first feasible
    solution it encounters.

    This status may be returned in relation to the [`SolutionLimit`](@ref)
    attribute, or some other solver-specific attribute.
    """,
    SOLUTION_LIMIT,
    "The algorithm stopped because it ran out of memory.",
    MEMORY_LIMIT,
    """
    The algorithm stopped because it found a solution better than a minimum
    limit set by the user.

    This status may be returned in relation to the [`ObjectiveLimit`](@ref)
    attribute, or some other solver-specific attribute.
    """,
    OBJECTIVE_LIMIT,
    """
    The algorithm stopped because the norm of an iterate became too large.

    This typically means that the primal problem is unbounded, but that the
    solver could not prove so.
    """,
    NORM_LIMIT,
    """
    The algorithm stopped due to a limit not covered by one of the `_LIMIT_`
    statuses above.
    """,
    OTHER_LIMIT,
    # Problematic
    """
    The algorithm stopped because it was unable to continue making progress
    towards the solution.
    """,
    SLOW_PROGRESS,
    """
    The algorithm stopped because it encountered unrecoverable numerical error.
    """,
    NUMERICAL_ERROR,
    """
    The algorithm stopped because the model is invalid.

    The reason for this return code is solver-specific, but common causes are
    that the problem has zero variables or constraints, or that the problem data
    contains an invalid number such as `NaN`.
    """,
    INVALID_MODEL,
    """
    The algorithm stopped because it was provided an invalid option.
    """,
    INVALID_OPTION,
    """
    The algorithm stopped because of an interrupt signal.

    This typically means that the solver was interrupted by the user with
    `CTRL+C`.
    """,
    INTERRUPTED,
    """
    The algorithm stopped because of an error not covered by one of the statuses
    defined above. Check the solver log for further details.
    """,
    OTHER_ERROR,
)

"""
    AutomaticDifferentiationBackend() <: AbstractOptimizerAttribute

An [`AbstractOptimizerAttribute`](@ref) for setting the automatic
differentiation backend used by the solver.

The value must be a subtype of [`Nonlinear.AbstractAutomaticDifferentiation`](@ref).

## Implementation

Optimizers should implement the following methods:
```
MOI.get(
    ::Optimizer,
    ::MOI.AutomaticDifferentiationBackend,
)::MOI.Nonlinear.AbstractAutomaticDifferentiation
MOI.set(
    ::Optimizer,
    ::MOI.AutomaticDifferentiationBackend,
    ::MOI.Nonlinear.AbstractAutomaticDifferentiation,
)::Nothing
MOI.supports(::Optimizer, ::MOI.AutomaticDifferentiationBackend)::Bool
```
"""
struct AutomaticDifferentiationBackend <: AbstractOptimizerAttribute end

function attribute_value_type(::AutomaticDifferentiationBackend)
    return Nonlinear.AbstractAutomaticDifferentiation
end

"""
    TerminationStatus()

An [`AbstractModelAttribute`](@ref) for the [`TerminationStatusCode`](@ref)
explaining why the optimizer stopped.

## Implementation

Optimizers should implement the following methods:
```
MOI.get(::Optimizer, ::MOI.TerminationStatus)::MOI.TerminationStatusCode
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct TerminationStatus <: AbstractModelAttribute end

attribute_value_type(::TerminationStatus) = TerminationStatusCode

"""
    RawStatusString()

An [`AbstractModelAttribute`](@ref) for a solver specific string explaining why
the optimizer stopped.

## Implementation

Optimizers should implement the following methods:
```
MOI.get(::Optimizer, ::MOI.RawStatusString)::MOI.String
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct RawStatusString <: AbstractModelAttribute end

attribute_value_type(::RawStatusString) = String

@_documented_enum(
    """
        ResultStatusCode

    An Enum of possible values for the [`PrimalStatus`](@ref) and
    [`DualStatus`](@ref) attributes.

    The values indicate how to interpret the result vector.
    """,
    ResultStatusCode,
    "The result vector is empty.",
    NO_SOLUTION,
    "The result vector is a feasible point.",
    FEASIBLE_POINT,
    "The result vector is feasible if some constraint tolerances are relaxed.",
    NEARLY_FEASIBLE_POINT,
    "The result vector is an infeasible point.",
    INFEASIBLE_POINT,
    """
    The result vector is an infeasibility certificate.

    If the [`PrimalStatus`](@ref) is [`INFEASIBILITY_CERTIFICATE`](@ref), then
    the primal result vector is a certificate of dual infeasibility.

    If the [`DualStatus`](@ref) is [`INFEASIBILITY_CERTIFICATE`](@ref), then the
    dual result vector is a proof of primal infeasibility.
    """,
    INFEASIBILITY_CERTIFICATE,
    """
    The result satisfies a relaxed criterion for a certificate of infeasibility.

    If the [`PrimalStatus`](@ref) is [`NEARLY_INFEASIBILITY_CERTIFICATE`](@ref),
    then the primal result vector is a certificate of dual infeasibility.

    If the [`DualStatus`](@ref) is [`NEARLY_INFEASIBILITY_CERTIFICATE`](@ref),
    then the dual result vector is a proof of primal infeasibility.
    """,
    NEARLY_INFEASIBILITY_CERTIFICATE,
    """
    The result vector is an ill-posed certificate; see [this article](https://arxiv.org/abs/1408.4685)
    for details.

    If the [`PrimalStatus`](@ref) is [`REDUCTION_CERTIFICATE`](@ref), then the
    primal result vector is a proof that the dual problem is ill-posed.

    If the [`DualStatus`](@ref) is [`REDUCTION_CERTIFICATE`](@ref), then the
    dual result vector is a proof that the primal is ill-posed.
    """,
    REDUCTION_CERTIFICATE,
    """
    The result satisfies a relaxed criterion for an ill-posed certificate.
    """,
    NEARLY_REDUCTION_CERTIFICATE,
    """
    The result vector contains a solution with an unknown interpretation. Check
    the solver log for more details.
    """,
    UNKNOWN_RESULT_STATUS,
    """
    The result vector contains a solution with an interpretation not covered by
    one of the statuses defined above. Check the solver log for more details.
    """,
    OTHER_RESULT_STATUS,
)

"""
    PrimalStatus(result_index::Int = 1)

An [`AbstractModelAttribute`](@ref) for the [`ResultStatusCode`](@ref) of the
primal result `result_index`.

## `result_index`

If `result_index` is omitted, it defaults to 1.

See [`ResultCount`](@ref) for information on how the results are ordered.

If `result_index` is larger than the value of [`ResultCount`](@ref) then
[`NO_SOLUTION`](@ref) is returned.

## Implementation

Optimizers should implement the following methods:
```
MOI.get(::Optimizer, ::MOI.PrimalStatus)::MOI.ResultStatusCode
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct PrimalStatus <: AbstractModelAttribute
    result_index::Int
    PrimalStatus(result_index::Int = 1) = new(result_index)
end

attribute_value_type(::PrimalStatus) = ResultStatusCode

"""
    DualStatus(result_index::Int = 1)

An [`AbstractModelAttribute`](@ref) for the [`ResultStatusCode`](@ref) of the
dual result `result_index`.

## `result_index`

See [`ResultCount`](@ref) for information on how the results are ordered.

If `result_index` is larger than the value of [`ResultCount`](@ref) then
[`NO_SOLUTION`](@ref) is returned.

## Implementation

Optimizers should implement the following methods:
```
MOI.get(::Optimizer, ::MOI.PrimalStatus)::MOI.ResultStatusCode
```
They should not implement [`set`](@ref) or [`supports`](@ref).
"""
struct DualStatus <: AbstractModelAttribute
    result_index::Int
    DualStatus(result_index::Int = 1) = new(result_index)
end

attribute_value_type(::DualStatus) = ResultStatusCode

# Cost of bridging constrained variable in S
struct VariableBridgingCost{S<:AbstractSet} <: AbstractModelAttribute end

attribute_value_type(::VariableBridgingCost) = Float64

function get_fallback(
    model::ModelLike,
    ::VariableBridgingCost{S},
) where {S<:AbstractScalarSet}
    return supports_add_constrained_variable(model, S) ? 0.0 : Inf
end

function get_fallback(
    model::ModelLike,
    ::VariableBridgingCost{S},
) where {S<:AbstractVectorSet}
    return supports_add_constrained_variables(model, S) ? 0.0 : Inf
end

# Cost of bridging F-in-S constraints
struct ConstraintBridgingCost{F<:AbstractFunction,S<:AbstractSet} <:
       AbstractModelAttribute end

attribute_value_type(::ConstraintBridgingCost) = Float64

function get_fallback(
    model::ModelLike,
    ::ConstraintBridgingCost{F,S},
) where {F<:AbstractFunction,S<:AbstractSet}
    return supports_constraint(model, F, S) ? 0.0 : Inf
end

"""
    is_set_by_optimize(::AnyAttribute)

Return a `Bool` indicating whether the value of the attribute is set during an
[`optimize!`](@ref) call, that is, the attribute is used to query the result of
the optimization.

If an attribute can be set by the user, define [`is_copyable`](@ref) instead.

An attribute cannot be both [`is_copyable`](@ref) and
[`is_set_by_optimize`](@ref).

## Default fallback

This function returns `false` by default so it should be implemented for
attributes that are set by [`optimize!`](@ref).

## Undefined behavior

Querying the value of the attribute that [`is_set_by_optimize`](@ref) before a
call to [`optimize!`](@ref) is undefined and depends on solver-specific behavior.

## Example

```jldoctest
julia> MOI.is_set_by_optimize(MOI.ObjectiveValue())
true

julia> MOI.is_set_by_optimize(MOI.VariableName())
false
```
"""
is_set_by_optimize(::AnyAttribute) = false

function is_set_by_optimize(
    ::Union{
        ObjectiveValue,
        DualObjectiveValue,
        ObjectiveBound,
        RelativeGap,
        SolveTimeSec,
        SimplexIterations,
        BarrierIterations,
        NodeCount,
        RawSolver,
        ResultCount,
        ConflictStatus,
        ConflictCount,
        ConstraintConflictStatus,
        TerminationStatus,
        RawStatusString,
        PrimalStatus,
        DualStatus,
        VariablePrimal,
        ConstraintPrimal,
        ConstraintDual,
        ConstraintBasisStatus,
        VariableBasisStatus,
    },
)
    return true
end

"""
    is_copyable(::AnyAttribute)

Return a `Bool` indicating whether the value of the attribute may be copied
during [`copy_to`](@ref) using [`set`](@ref).

If an attribute `is_copyable`, then it cannot be modified by the optimizer, and
[`get`](@ref) must always return the value that was [`set`](@ref) by the user.

If an attribute is the result of an optimization, define
[`is_set_by_optimize`](@ref) instead.

An attribute cannot be both [`is_set_by_optimize`](@ref) and `is_copyable`.

## Default fallback

By default `is_copyable(attr)` returns `!is_set_by_optimize(attr)`, which is
most probably `true`.

If an attribute should not be copied, define `is_copyable(::MyAttribute) = false`.
"""
function is_copyable(attr::AnyAttribute)
    return !is_set_by_optimize(attr)
end

function is_copyable(
    ::Union{
        ListOfOptimizerAttributesSet,
        ListOfModelAttributesSet,
        ListOfConstraintAttributesSet,
        ListOfVariableAttributesSet,
        SolverName,
        RawSolver,
        NumberOfVariables,
        ListOfVariableIndices,
        NumberOfConstraints,
        ObjectiveFunctionType,
        ListOfConstraintIndices,
        ListOfConstraintTypesPresent,
        CanonicalConstraintFunction,
        ConstraintFunction,
        ConstraintSet,
        VariableBridgingCost,
        ConstraintBridgingCost,
    },
)
    return false
end
