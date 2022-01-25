# Attributes

"""
    AbstractOptimizerAttribute

Abstract supertype for attribute objects that can be used to set or get attributes (properties) of the optimizer.

### Note

The difference between `AbstractOptimizerAttribute` and `AbstractModelAttribute`
lies in the behavior of `is_empty`, `empty!` and `copy_to`. Typically optimizer
attributes only affect how the model is solved.
"""
abstract type AbstractOptimizerAttribute end

"""
    AbstractModelAttribute

Abstract supertype for attribute objects that can be used to set or get attributes (properties) of the model.
"""
abstract type AbstractModelAttribute end

"""
    AbstractVariableAttribute

Abstract supertype for attribute objects that can be used to set or get attributes (properties) of variables in the model.
"""
abstract type AbstractVariableAttribute end

"""
    AbstractConstraintAttribute

Abstract supertype for attribute objects that can be used to set or get attributes (properties) of constraints in the model.
"""
abstract type AbstractConstraintAttribute end

# Attributes should not contain any `VariableIndex` or `ConstraintIndex` as the
# set is passed unmodifed during `copy_to`.
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

# Notes

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
i.e. that [`supports`](@ref) returns `false`.
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
        message::String # Human-friendly explanation why the attribute cannot be set
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
i.e. that [`supports`](@ref) returns `false`.
"""
struct UnsupportedSubmittable{SubmitType<:AbstractSubmittable} <:
       UnsupportedError
    sub::SubmitType
    message::String
end

"""
    struct SubmitNotAllowed{SubmitTyp<:AbstractSubmittable} <: NotAllowedError
        sub::SubmitType
        message::String # Human-friendly explanation why the attribute cannot be set
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
end
function Base.showerror(io::IO, err::ResultIndexBoundsError)
    return print(
        io,
        "Result index of attribute $(err.attr) out of bounds. There are " *
        "currently $(err.result_count) solution(s) in the model.",
    )
end

"""
    supports(model::ModelLike, sub::AbstractSubmittable)::Bool

Return a `Bool` indicating whether `model` supports the submittable `sub`.

    supports(model::ModelLike, attr::AbstractOptimizerAttribute)::Bool

Return a `Bool` indicating whether `model` supports the optimizer attribute
`attr`. That is, it returns `false` if `copy_to(model, src)` shows a warning in
case `attr` is in the [`ListOfOptimizerAttributesSet`](@ref) of `src`; see
[`copy_to`](@ref) for more details on how unsupported optimizer attributes are
handled in copy.

    supports(model::ModelLike, attr::AbstractModelAttribute)::Bool

Return a `Bool` indicating whether `model` supports the model attribute `attr`.
That is, it returns `false` if `copy_to(model, src)` cannot be performed in case
`attr` is in the [`ListOfModelAttributesSet`](@ref) of `src`.

    supports(model::ModelLike, attr::AbstractVariableAttribute, ::Type{VariableIndex})::Bool

Return a `Bool` indicating whether `model` supports the variable attribute
`attr`. That is, it returns `false` if `copy_to(model, src)` cannot be performed
in case `attr` is in the [`ListOfVariableAttributesSet`](@ref) of `src`.

    supports(model::ModelLike, attr::AbstractConstraintAttribute, ::Type{ConstraintIndex{F,S}})::Bool where {F,S}

Return a `Bool` indicating whether `model` supports the constraint attribute
`attr` applied to an `F`-in-`S` constraint. That is, it returns `false` if
`copy_to(model, src)` cannot be performed in case `attr` is in the
[`ListOfConstraintAttributesSet`](@ref) of `src`.

For all five methods, if the attribute is only not supported in specific
circumstances, it should still return `true`.

Note that `supports` is only defined for attributes for which
[`is_copyable`](@ref) returns `true` as other attributes do not appear in the
list of attributes set obtained by `ListOf...AttributesSet`.
"""
function supports end
supports(::ModelLike, ::AbstractSubmittable) = false

function supports(model::ModelLike, attr::AnyAttribute, args...)
    return supports_fallback(model, attr, args...)
end

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

    get(model::ModelLike, attr::AbstractModelAttribute)

Return an attribute `attr` of the model `model`.

    get(model::ModelLike, attr::AbstractVariableAttribute, v::VariableIndex)

If the attribute `attr` is set for the variable `v` in the model `model`, return
its value, return `nothing` otherwise. If the attribute `attr` is not supported
by `model` then an error should be thrown instead of returning `nothing`.

    get(model::ModelLike, attr::AbstractVariableAttribute, v::Vector{VariableIndex})

Return a vector of attributes corresponding to each variable in the collection `v` in the model `model`.

    get(model::ModelLike, attr::AbstractConstraintAttribute, c::ConstraintIndex)

If the attribute `attr` is set for the constraint `c` in the model `model`,
return its value, return `nothing` otherwise. If the attribute `attr` is not
supported by `model` then an error should be thrown instead of returning
`nothing`.

    get(model::ModelLike, attr::AbstractConstraintAttribute, c::Vector{ConstraintIndex{F,S}})

Return a vector of attributes corresponding to each constraint in the collection `c` in the model `model`.

    get(model::ModelLike, ::Type{VariableIndex}, name::String)

If a variable with name `name` exists in the model `model`, return the
corresponding index, otherwise return `nothing`. Errors if two variables
have the same name.

    get(model::ModelLike, ::Type{ConstraintIndex{F,S}}, name::String) where {F<:AbstractFunction,S<:AbstractSet}

If an `F`-in-`S` constraint with name `name` exists in the model `model`, return
the corresponding index, otherwise return `nothing`. Errors if two constraints
have the same name.

    get(model::ModelLike, ::Type{ConstraintIndex}, name::String)

If *any* constraint with name `name` exists in the model `model`, return the
corresponding index, otherwise return `nothing`. This version is available for
convenience but may incur a performance penalty because it is not type stable.
Errors if two constraints have the same name.

### Examples

```julia
get(model, ObjectiveValue())
get(model, VariablePrimal(), ref)
get(model, VariablePrimal(5), [ref1, ref2])
get(model, OtherAttribute("something specific to cplex"))
get(model, VariableIndex, "var1")
get(model, ConstraintIndex{ScalarAffineFunction{Float64},LessThan{Float64}}, "con1")
get(model, ConstraintIndex, "con1")
```
"""
function get end
# We want to avoid being too specific in the type arguments to avoid method ambiguity.
# For model, get(::ModelLike, ::AbstractVariableAttribute, ::Vector{VariableIndex}) would not allow
# to define get(::SomeModel, ::AnyAttribute, ::Vector)
function get(model::ModelLike, attr::AnyAttribute, idxs::Vector)
    return get.(model, attr, idxs)
end

function get(model::ModelLike, attr::AnyAttribute, args...)
    return get_fallback(model, attr, args...)
end

function get_fallback(
    model::ModelLike,
    attr::Union{AbstractModelAttribute,AbstractOptimizerAttribute},
)
    return throw(
        ArgumentError(
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
        ArgumentError(
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
        ArgumentError(
            "$(typeof(model)) does not support getting the attribute $(attr).",
        ),
    )
end

function get_fallback(::ModelLike, attr::AnyAttribute, args...)
    return throw(
        ArgumentError(
            "Unable to get attribute $(attr): invalid arguments $(args).",
        ),
    )
end

"""
    get!(output, model::ModelLike, args...)

An in-place version of [`get`](@ref).

The signature matches that of [`get`](@ref) except that the the result is placed
in the vector `output`.
"""
function get!(output, model::ModelLike, attr::AnyAttribute, args...)
    output .= get(model, attr, args...)
    return
end

"""
    set(optimizer::AbstractOptimizer, attr::AbstractOptimizerAttribute, value)

Assign `value` to the attribute `attr` of the optimizer `optimizer`.

    set(model::ModelLike, attr::AbstractModelAttribute, value)

Assign `value` to the attribute `attr` of the model `model`.

    set(model::ModelLike, attr::AbstractVariableAttribute, v::VariableIndex, value)

Assign `value` to the attribute `attr` of variable `v` in model `model`.

    set(model::ModelLike, attr::AbstractVariableAttribute, v::Vector{VariableIndex}, vector_of_values)

Assign a value respectively to the attribute `attr` of each variable in the collection `v` in model `model`.

    set(model::ModelLike, attr::AbstractConstraintAttribute, c::ConstraintIndex, value)

Assign a value to the attribute `attr` of constraint `c` in model `model`.

    set(model::ModelLike, attr::AbstractConstraintAttribute, c::Vector{ConstraintIndex{F,S}}, vector_of_values)

Assign a value respectively to the attribute `attr` of each constraint in the collection `c` in model `model`.

An [`UnsupportedAttribute`](@ref) error is thrown if `model` does not support
the attribute `attr` (see [`supports`](@ref)) and a [`SetAttributeNotAllowed`](@ref)
error is thrown if it supports the attribute `attr` but it cannot be set.

### Replace set in a constraint

    set(model::ModelLike, ::ConstraintSet, c::ConstraintIndex{F,S}, set::S)

Change the set of constraint `c` to the new set `set` which should be of the
same type as the original set.

#### Examples

If `c` is a `ConstraintIndex{F,Interval}`

```julia
set(model, ConstraintSet(), c, Interval(0, 5))
set(model, ConstraintSet(), c, GreaterThan(0.0))  # Error
```

### Replace function in a constraint

    set(model::ModelLike, ::ConstraintFunction, c::ConstraintIndex{F,S}, func::F)

Replace the function in constraint `c` with `func`. `F` must match the original
function type used to define the constraint.

#### Note

Setting the constraint function is not allowed if `F` is
[`VariableIndex`](@ref), it throws a
[`SettingVariableIndexNotAllowed`](@ref) error. Indeed, it would
require changing the index `c` as the index of `VariableIndex` constraints
should be the same as the index of the variable.

#### Examples

If `c` is a `ConstraintIndex{ScalarAffineFunction,S}` and `v1` and `v2` are
`VariableIndex` objects,

```julia
set(model, ConstraintFunction(), c,
    ScalarAffineFunction(ScalarAffineTerm.([1.0, 2.0], [v1, v2]), 5.0))
set(model, ConstraintFunction(), c, v1) # Error
```
"""
function set end
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

function set(model::ModelLike, attr::AnyAttribute, args...)
    return throw_set_error_fallback(model, attr, args...)
end
# throw_set_error_fallback is included so that we can return type-specific error
# messages without needing to overload set and cause ambiguity errors. For
# examples, see ConstraintSet and ConstraintFunction. throw_set_error_fallback should
# not be overloaded by users of MOI.
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
    submit(optimizer::AbstractOptimizer, sub::AbstractSubmittable,
           values...)::Nothing

Submit `values` to the submittable `sub` of the optimizer `optimizer`.

An [`UnsupportedSubmittable`](@ref) error is thrown if `model` does not support
the attribute `attr` (see [`supports`](@ref)) and a [`SubmitNotAllowed`](@ref)
error is thrown if it supports the submittable `sub` but it cannot be submitted.
""" # TODO add an example once we have an attribute which can be submitted, e.g. Lazy constraint
function submit end
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

## Submittables

"""
    LazyConstraint(callback_data)

Lazy constraint `func`-in-`set` submitted as `func, set`. The optimal
solution returned by [`VariablePrimal`](@ref) will satisfy all lazy
constraints that have been submitted.

This can be submitted only from the [`LazyConstraintCallback`](@ref). The
field `callback_data` is a solver-specific callback type that is passed as the
argument to the feasible solution callback.

## Examples

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

"""
    HeuristicSolutionStatus

An Enum of possible return values for [`submit`](@ref) with
[`HeuristicSolution`](@ref).
This informs whether the heuristic solution was accepted or rejected.
Possible values are:
* `HEURISTIC_SOLUTION_ACCEPTED`: The heuristic solution was accepted.
* `HEURISTIC_SOLUTION_REJECTED`: The heuristic solution was rejected.
* `HEURISTIC_SOLUTION_UNKNOWN`: No information available on the acceptance.
"""
@enum(
    HeuristicSolutionStatus,
    HEURISTIC_SOLUTION_ACCEPTED,
    HEURISTIC_SOLUTION_REJECTED,
    HEURISTIC_SOLUTION_UNKNOWN
)

"""
    HeuristicSolution(callback_data)

Heuristically obtained feasible solution. The solution is submitted as
`variables, values` where `values[i]` gives the value of `variables[i]`,
similarly to [`set`](@ref). The [`submit`](@ref) call returns a
[`HeuristicSolutionStatus`](@ref) indicating whether the provided solution
was accepted or rejected.

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
as `func, set`.
Typically [`CallbackVariablePrimal`](@ref) will violate integrality constraints,
and a cut would be of the form [`ScalarAffineFunction`](@ref)-in-[`LessThan`](@ref)
or [`ScalarAffineFunction`](@ref)-in-[`GreaterThan`](@ref). Note that, as
opposed to [`LazyConstraint`](@ref), the provided constraint cannot modify the
feasible set, the constraint should be redundant, e.g., it may be a consequence
of affine and integrality constraints.

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

"""
    CallbackNodeStatusCode

An Enum of possible return values from calling [`get`](@ref) with
[`CallbackNodeStatus`](@ref).

Possible values are:

* `CALLBACK_NODE_STATUS_INTEGER`: the primal solution available from
  [`CallbackVariablePrimal`](@ref) is integer feasible.
* `CALLBACK_NODE_STATUS_FRACTIONAL`: the primal solution available from
  [`CallbackVariablePrimal`](@ref) is integer infeasible.
* `CALLBACK_NODE_STATUS_UNKNOWN`: the primal solution available from
  [`CallbackVariablePrimal`](@ref) might be integer feasible or infeasible.
"""
@enum(
    CallbackNodeStatusCode,
    CALLBACK_NODE_STATUS_INTEGER,
    CALLBACK_NODE_STATUS_FRACTIONAL,
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

## Optimizer attributes

"""
    ListOfOptimizerAttributesSet()

An optimizer attribute for the `Vector{AbstractOptimizerAttribute}` of all optimizer attributes that were set.
"""
struct ListOfOptimizerAttributesSet <: AbstractOptimizerAttribute end

"""
    SolverName()

An optimizer attribute for the string identifying the solver/optimizer.
"""
struct SolverName <: AbstractOptimizerAttribute end

attribute_value_type(::SolverName) = String

"""
    SolverVersion()

An optimizer attribute for the string identifying the version of the solver.

!!! note

    For solvers supporting [semantic versioning](https://semver.org), the `SolverVersion` should be a string
    of the form "vMAJOR.MINOR.PATCH", so that it can be converted to
    a Julia `VersionNumber` (e.g., `VersionNumber("v1.2.3")).

    We do not require Semantic Versioning because some solvers use
    alternate versioning systems. For example, CPLEX uses Calendar
    Versioning, so `SolverVersion` will return a string like `"202001"`.
"""
struct SolverVersion <: AbstractOptimizerAttribute end

attribute_value_type(::SolverVersion) = String

"""
    Silent()

An optimizer attribute for silencing the output of an optimizer. When `set`
to `true`, it takes precedence over any other attribute controlling verbosity
and requires the solver to produce no output. The default value is `false`
which has no effect. In this case the verbosity is controlled by other
attributes.

## Note

Every optimizer should have verbosity on by default. For instance, if a solver
has a solver-specific log level attribute, the MOI implementation should set it
to `1` by default. If the user sets `Silent` to `true`, then the log level
should be set to `0`, even if the user specifically sets a value of log level.
If the value of `Silent` is `false` then the log level set to the solver is the
value given by the user for this solver-specific parameter or `1` if none is
given.
"""
struct Silent <: AbstractOptimizerAttribute end

attribute_value_type(::Silent) = Bool

"""
    TimeLimitSec()

An optimizer attribute for setting a time limit for an optimization. When `set`
to `nothing`, it deactivates the solver time limit. The default value is
`nothing`. The time limit is in seconds.
""" # TODO add a test checking if the solver returns TIME_LIMIT status when the time limit is hit
struct TimeLimitSec <: AbstractOptimizerAttribute end

attribute_value_type(::TimeLimitSec) = Union{Nothing,Float64}

"""
    RawOptimizerAttribute(name::String)

An optimizer attribute for the solver-specific parameter identified by `name`.
"""
struct RawOptimizerAttribute <: AbstractOptimizerAttribute
    name::String
end

"""
    NumberOfThreads()

An optimizer attribute for setting the number of threads used for an
optimization. When set to `nothing` uses solver default. Values are positive
integers. The default value is `nothing`.
"""
struct NumberOfThreads <: AbstractOptimizerAttribute end

attribute_value_type(::NumberOfThreads) = Union{Nothing,Int}

### Callbacks

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
attributes (i.e, the attributes `attr` such that `is_set_by_optimize(attr)`)
may not be accessible from the callback, hence trying to get result attributes
might throw a [`OptimizeInProgress`](@ref) error.

At most one callback of each type can be registered. If an optimizer already
has a function for a callback type, and the user registers a new function,
then the old one is replaced.

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

## Examples

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
current primal solution.
For instance, it may be called at fractional (i.e., non-integer) nodes in the
branch and bound tree of a mixed-integer problem. Note that there is not
guarantee that the callback is called *everytime* the solver has an infeasible
solution.

The current primal solution is accessed through
[`CallbackVariablePrimal`](@ref). Trying to access other result
attributes will throw [`OptimizeInProgress`](@ref) as discussed in
[`AbstractCallback`](@ref).

## Examples

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
solution. For instance, it may be called at fractional (i.e., non-integer) nodes
in the branch and bound tree of a mixed-integer problem. Note that there is not
guarantee that the callback is called *everytime* the solver has an infeasible
solution.

The infeasible solution is accessed through
[`CallbackVariablePrimal`](@ref). Trying to access other result
attributes will throw [`OptimizeInProgress`](@ref) as discussed in
[`AbstractCallback`](@ref).

## Examples

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

## Model attributes

"""
    ListOfModelAttributesSet()

A model attribute for the `Vector{AbstractModelAttribute}` of all model
attributes `attr` such that 1) `is_copyable(attr)` returns `true` and 2) the
attribute was set to the model.
"""
struct ListOfModelAttributesSet <: AbstractModelAttribute end

"""
    Name()

A model attribute for the string identifying the model. It has a default value
of `""` if not set`.
"""
struct Name <: AbstractModelAttribute end

attribute_value_type(::Name) = String

@enum OptimizationSense MIN_SENSE MAX_SENSE FEASIBILITY_SENSE

"""
    ObjectiveSense()

A model attribute for the objective sense of the objective function, which
must be an `OptimizationSense`: `MIN_SENSE`, `MAX_SENSE`, or
`FEASIBILITY_SENSE`. The default is `FEASIBILITY_SENSE`.
"""
struct ObjectiveSense <: AbstractModelAttribute end

attribute_value_type(::ObjectiveSense) = OptimizationSense

"""
    NumberOfVariables()

A model attribute for the number of variables in the model.
"""
struct NumberOfVariables <: AbstractModelAttribute end

attribute_value_type(::NumberOfVariables) = Int64

"""
    ListOfVariableIndices()

A model attribute for the `Vector{VariableIndex}` of all variable indices present in the model
(i.e., of length equal to the value of `NumberOfVariables()`) in the order in
which they were added.
"""
struct ListOfVariableIndices <: AbstractModelAttribute end

"""
    ListOfConstraintIndices{F,S}()

A model attribute for the `Vector{ConstraintIndex{F,S}}` of all constraint indices of type
`F`-in-`S` in the model (i.e., of length equal to the value of
`NumberOfConstraints{F,S}()`) in the order in which they were added.
"""
struct ListOfConstraintIndices{F,S} <: AbstractModelAttribute end

"""
    NumberOfConstraints{F,S}()

A model attribute for the number of constraints of the type `F`-in-`S` present in the model.
"""
struct NumberOfConstraints{F,S} <: AbstractModelAttribute end

attribute_value_type(::NumberOfConstraints) = Int64

"""
    ListOfConstraintTypesPresent()

A model attribute for the list of tuples of the form `(F,S)`, where `F` is a function type
and `S` is a set type indicating that the attribute `NumberOfConstraints{F,S}()`
has value greater than zero.
"""
struct ListOfConstraintTypesPresent <: AbstractModelAttribute end
@deprecate ListOfConstraints ListOfConstraintTypesPresent false

"""
    ObjectiveFunction{F<:AbstractScalarFunction}()

A model attribute for the objective function which has a type `F<:AbstractScalarFunction`.
`F` should be guaranteed to be equivalent but not necessarily identical to the function type provided by the user.
Throws an `InexactError` if the objective function cannot be converted to `F`,
e.g. the objective function is quadratic and `F` is `ScalarAffineFunction{Float64}` or
it has non-integer coefficient and `F` is `ScalarAffineFunction{Int}`.
"""
struct ObjectiveFunction{F<:AbstractScalarFunction} <: AbstractModelAttribute end

attribute_value_type(::ObjectiveFunction{F}) where {F} = F

"""
    ObjectiveFunctionType()

A model attribute for the type `F` of the objective function set using the
`ObjectiveFunction{F}` attribute.

## Examples

In the following code, `attr` should be equal to `MOI.VariableIndex`:
```julia
x = MOI.add_variable(model)
MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(),
         x)
attr = MOI.get(model, MOI.ObjectiveFunctionType())
```
"""
struct ObjectiveFunctionType <: AbstractModelAttribute end

attribute_value_type(::ObjectiveFunctionType) = Type{<:AbstractFunction}

## Optimizer attributes

"""
    ObjectiveValue(result_index::Int = 1)

A model attribute for the objective value of the primal solution `result_index`.

If the solver does not have a primal value for the objective because the
`result_index` is beyond the available solutions (whose number is indicated by
the [`ResultCount`](@ref) attribute), getting this attribute must throw a
[`ResultIndexBoundsError`](@ref). Otherwise, if the result is unavailable for
another reason (for instance, only a dual solution is available), the result is
undefined. Users should first check [`PrimalStatus`](@ref) before accessing the
`ObjectiveValue` attribute.

See [`ResultCount`](@ref) for information on how the results are ordered.
"""
struct ObjectiveValue <: AbstractModelAttribute
    result_index::Int
    ObjectiveValue(result_index::Int = 1) = new(result_index)
end

"""
    DualObjectiveValue(result_index::Int = 1)

A model attribute for the value of the objective function of the dual problem
for the `result_index`th dual result.

If the solver does not have a dual value for the objective because the
`result_index` is beyond the available solutions (whose number is indicated by
the [`ResultCount`](@ref) attribute), getting this attribute must throw a
[`ResultIndexBoundsError`](@ref). Otherwise, if the result is unavailable for
another reason (for instance, only a primal solution is available), the result is
undefined. Users should first check [`DualStatus`](@ref) before accessing the
`DualObjectiveValue` attribute.

See [`ResultCount`](@ref) for information on how the results are ordered.
"""
struct DualObjectiveValue <: AbstractModelAttribute
    result_index::Int
    DualObjectiveValue(result_index::Int = 1) = new(result_index)
end

"""
    ObjectiveBound()

A model attribute for the best known bound on the optimal objective value.
"""
struct ObjectiveBound <: AbstractModelAttribute end

"""
    RelativeGap()

A model attribute for the final relative optimality gap.

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

A model attribute for the total elapsed solution time (in seconds) as reported by the optimizer.
"""
struct SolveTimeSec <: AbstractModelAttribute end

attribute_value_type(::SolveTimeSec) = Float64

@deprecate SolveTime SolveTimeSec false

"""
    SimplexIterations()

A model attribute for the cumulative number of simplex iterations during the optimization process.
In particular, for a mixed-integer program (MIP), the total simplex iterations for all nodes.
"""
struct SimplexIterations <: AbstractModelAttribute end

attribute_value_type(::SimplexIterations) = Int64

"""
    BarrierIterations()

A model attribute for the cumulative number of barrier iterations while solving a problem.
"""
struct BarrierIterations <: AbstractModelAttribute end

attribute_value_type(::BarrierIterations) = Int64

"""
    NodeCount()

A model attribute for the total number of branch-and-bound nodes explored while solving a mixed-integer program (MIP).
"""
struct NodeCount <: AbstractModelAttribute end

attribute_value_type(::NodeCount) = Int64

"""
    RawSolver()

A model attribute for the object that may be used to access a solver-specific API for this optimizer.
"""
struct RawSolver <: AbstractModelAttribute end

"""
    ResultCount()

A model attribute for the number of results available.

## Order of solutions

A number of attributes contain an index, `result_index`, which is used to refer
to one of the available results. Thus, `result_index` must be an integer between
`1` and the number of available results.

As a general rule, the first result (`result_index=1`) is the most important
result (e.g., an optimal solution or an infeasibility certificate). Other
results will typically be alternate solutions that the solver found during the
search for the first result.

If a (local) optimal solution is available, i.e., [`TerminationStatus`](@ref) is
`OPTIMAL` or `LOCALLY_SOLVED`, the first result must correspond to the (locally)
optimal solution. Other results may be alternative optimal solutions, or they
may be other suboptimal solutions; use [`ObjectiveValue`](@ref) to distingiush
between them.

If a primal or dual infeasibility certificate is available, i.e.,
[`TerminationStatus`](@ref) is `INFEASIBLE` or `DUAL_INFEASIBLE` and the
corresponding [`PrimalStatus`](@ref) or [`DualStatus`](@ref) is
`INFEASIBILITY_CERTIFICATE`, then the first result must be a certificate. Other
results may be alternate certificates, or infeasible points.
"""
struct ResultCount <: AbstractModelAttribute end

attribute_value_type(::ResultCount) = Int

"""
    ConflictStatusCode

An Enum of possible values for the `ConflictStatus` attribute. This attribute
is meant to explain the reason why the conflict finder stopped executing in the
most recent call to [`compute_conflict!`](@ref).

Possible values are:
* `COMPUTE_CONFLICT_NOT_CALLED`: the function [`compute_conflict!`](@ref) has
  not yet been called
* `NO_CONFLICT_EXISTS`: there is no conflict because the problem is feasible
* `NO_CONFLICT_FOUND`: the solver could not find a conflict
* `CONFLICT_FOUND`: at least one conflict could be found
"""
@enum ConflictStatusCode begin
    COMPUTE_CONFLICT_NOT_CALLED
    NO_CONFLICT_EXISTS
    NO_CONFLICT_FOUND
    CONFLICT_FOUND
end

"""
    ConflictStatus()

A model attribute for the [`ConflictStatusCode`](@ref) explaining why the conflict
refiner stopped when computing the conflict.
"""
struct ConflictStatus <: AbstractModelAttribute end

attribute_value_type(::ConflictStatus) = ConflictStatusCode

## Variable attributes

"""
    ListOfVariableAttributesSet()

A model attribute for the `Vector{AbstractVariableAttribute}` of all variable
attributes `attr` such that 1) `is_copyable(attr)` returns `true` and 2) the
attribute was set to variables.
"""
struct ListOfVariableAttributesSet <: AbstractModelAttribute end

"""
    VariableName()

A variable attribute for a string identifying the variable. It is *valid* for
two variables to have the same name; however, variables with duplicate names
cannot be looked up using [`get`](@ref). It has a default value of `""` if not
set`.
"""
struct VariableName <: AbstractVariableAttribute end

attribute_value_type(::VariableName) = String

"""
    VariablePrimalStart()

A variable attribute for the initial assignment to some primal variable's value that the optimizer may use to warm-start the solve. May be a number or `nothing` (unset).
"""
struct VariablePrimalStart <: AbstractVariableAttribute end

"""
    VariablePrimal(result_index::Int = 1)

A variable attribute for the assignment to some primal variable's value in
result `result_index`. If `result_index` is omitted, it is 1 by default.

If the solver does not have a primal value for the variable because the
`result_index` is beyond the available solutions (whose number is indicated by
the [`ResultCount`](@ref) attribute), getting this attribute must throw a
[`ResultIndexBoundsError`](@ref). Otherwise, if the result is unavailable for
another reason (for instance, only a dual solution is available), the result is
undefined. Users should first check [`PrimalStatus`](@ref) before accessing the
`VariablePrimal` attribute.

See [`ResultCount`](@ref) for information on how the results are ordered.
"""
struct VariablePrimal <: AbstractVariableAttribute
    result_index::Int
    VariablePrimal(result_index::Int = 1) = new(result_index)
end

"""
    CallbackVariablePrimal(callback_data)

A variable attribute for the assignment to some primal variable's value during
the callback identified by `callback_data`.
"""
struct CallbackVariablePrimal{CallbackDataType} <: AbstractVariableAttribute
    callback_data::CallbackDataType
end
is_set_by_optimize(::CallbackVariablePrimal) = true

"""
    BasisStatusCode

An Enum of possible values for the [`ConstraintBasisStatus`](@ref) and
[`VariableBasisStatus`](@ref) attributes, explaining the status of a given
element with respect to an optimal solution basis.

Possible values are:

* `BASIC`: element is in the basis
* `NONBASIC`: element is not in the basis
* `NONBASIC_AT_LOWER`: element is not in the basis and is at its lower bound
* `NONBASIC_AT_UPPER`: element is not in the basis and is at its upper bound
* `SUPER_BASIC`: element is not in the basis but is also not at one of its
  bounds

## Notes

* `NONBASIC_AT_LOWER` and `NONBASIC_AT_UPPER` should be used only for
  constraints with the `Interval` set. In this case, they are necessary to
  distinguish which side of the constraint is active. One-sided constraints
  (e.g., `LessThan` and `GreaterThan`) should use `NONBASIC` instead of the
  `NONBASIC_AT_*` values. This restriction does not apply to [`VariableBasisStatus`](@ref),
  which should return `NONBASIC_AT_*` regardless of whether the alternative
  bound exists.

* In linear programs, `SUPER_BASIC` occurs when a variable with no bounds is not
  in the basis.
"""
@enum(
    BasisStatusCode,
    BASIC,
    NONBASIC,
    NONBASIC_AT_LOWER,
    NONBASIC_AT_UPPER,
    SUPER_BASIC
)

"""
    VariableBasisStatus(result_index::Int = 1)

A variable attribute for the `BasisStatusCode` of a variable in result
`result_index`, with respect to an available optimal solution basis.

If the solver does not have a basis statue for the variable because the
`result_index` is beyond the available solutions (whose number is indicated by
the [`ResultCount`](@ref) attribute), getting this attribute must throw a
[`ResultIndexBoundsError`](@ref). Otherwise, if the result is unavailable for
another reason (for instance, only a dual solution is available), the result is
undefined. Users should first check [`PrimalStatus`](@ref) before accessing the
`VariableBasisStatus` attribute.

See [`ResultCount`](@ref) for information on how the results are ordered.
"""
struct VariableBasisStatus <: AbstractVariableAttribute
    result_index::Int
    VariableBasisStatus(result_index::Int = 1) = new(result_index)
end

attribute_value_type(::VariableBasisStatus) = BasisStatusCode

## Constraint attributes

"""
    ListOfConstraintAttributesSet{F, S}()

A model attribute for the `Vector{AbstractConstraintAttribute}` of all
constraint attributes `attr` such that 1) `is_copyable(attr)` returns `true` and
2) the attribute was set to `F`-in-`S` constraints.

## Note

The attributes [`ConstraintFunction`](@ref) and [`ConstraintSet`](@ref) should
not be included in the list even if then have been set with [`set`](@ref).
"""
struct ListOfConstraintAttributesSet{F,S} <: AbstractModelAttribute end

"""
    ConstraintName()

A constraint attribute for a string identifying the constraint.

It is *valid* for constraints variables to have the same name; however,
constraints with duplicate names cannot be looked up using [`get`](@ref),
regardless of whether they have the same `F`-in-`S` type.

`ConstraintName` has a default value of `""` if not set.

## Notes

You should _not_ implement `ConstraintName` for `VariableIndex` constraints.
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

A constraint attribute for the initial assignment to some constraint's
[`ConstraintPrimal`](@ref) that the optimizer may use to warm-start the solve.

May be `nothing` (unset), a number for [`AbstractScalarFunction`](@ref), or a
vector for [`AbstractVectorFunction`](@ref).
"""
struct ConstraintPrimalStart <: AbstractConstraintAttribute end

"""
    ConstraintDualStart()

A constraint attribute for the initial assignment to some constraint's
[`ConstraintDual`](@ref) that the optimizer may use to warm-start the solve.

May be `nothing` (unset), a number for [`AbstractScalarFunction`](@ref), or a
vector for [`AbstractVectorFunction`](@ref).
"""
struct ConstraintDualStart <: AbstractConstraintAttribute end

"""
    ConstraintPrimal(result_index::Int = 1)

A constraint attribute for the assignment to some constraint's primal value(s)
in result `result_index`.

If the constraint is `f(x) in S`, then in most cases the `ConstraintPrimal` is
the value of `f`, evaluated at the correspondng [`VariablePrimal`](@ref)
solution.

However, some conic solvers reformulate `b - Ax in S` to `s = b - Ax, s in S`.
These solvers may return the value of `s` for `ConstraintPrimal`, rather than
`b - Ax`. (Although these are constrained by an equality constraint, due to
numerical tolerances they may not be identical.)

If the solver does not have a primal value for the constraint because the
`result_index` is beyond the available solutions (whose number is indicated by
the [`ResultCount`](@ref) attribute), getting this attribute must throw a
[`ResultIndexBoundsError`](@ref). Otherwise, if the result is unavailable for
another reason (for instance, only a dual solution is available), the result is
undefined. Users should first check [`PrimalStatus`](@ref) before accessing the
`ConstraintPrimal` attribute.

If `result_index` is omitted, it is 1 by default. See [`ResultCount`](@ref) for
information on how the results are ordered.
"""
struct ConstraintPrimal <: AbstractConstraintAttribute
    result_index::Int
    ConstraintPrimal(result_index::Int = 1) = new(result_index)
end

"""
    ConstraintDual(result_index::Int = 1)

A constraint attribute for the assignment to some constraint's dual value(s) in
result `result_index`. If `result_index` is omitted, it is 1 by default.

If the solver does not have a dual value for the variable because the
`result_index` is beyond the available solutions (whose number is indicated by
the [`ResultCount`](@ref) attribute), getting this attribute must throw a
[`ResultIndexBoundsError`](@ref). Otherwise, if the result is unavailable for
another reason (for instance, only a primal solution is available), the result is
undefined. Users should first check [`DualStatus`](@ref) before accessing the
`ConstraintDual` attribute.

See [`ResultCount`](@ref) for information on how the results are ordered.
"""
struct ConstraintDual <: AbstractConstraintAttribute
    result_index::Int
    ConstraintDual(result_index::Int = 1) = new(result_index)
end

"""
    ConstraintBasisStatus(result_index::Int = 1)

A constraint attribute for the `BasisStatusCode` of some constraint in result
`result_index`, with respect to an available optimal solution basis. If
`result_index` is omitted, it is 1 by default.

If the solver does not have a basis statue for the constraint because the
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

A constraint attribute for a canonical representation of the
[`AbstractFunction`](@ref) object used to define the constraint.
Getting this attribute is guaranteed to return a function that is equivalent but
not necessarily identical to the function provided by the user.

By default, `MOI.get(model, MOI.CanonicalConstraintFunction(), ci)` fallbacks to
`MOI.Utilities.canonical(MOI.get(model, MOI.ConstraintFunction(), ci))`.
However, if `model` knows that the constraint function is canonical then it can
implement a specialized method that directly return the function without calling
[`Utilities.canonical`](@ref). Therefore, the value returned **cannot** be
assumed to be a copy of the function stored in `model`.
Moreover, [`Utilities.Model`](@ref) checks with [`Utilities.is_canonical`](@ref)
whether the function stored internally is already canonical and if it's the case,
then it returns the function stored internally instead of a copy.
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

A constraint attribute for the `AbstractFunction` object used to define the constraint.
It is guaranteed to be equivalent but not necessarily identical to the function provided by the user.
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
func_type(c::ConstraintIndex{F,S}) where {F,S} = F
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

A constraint attribute for the `AbstractSet` object used to define the constraint.
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

"""
    ConflictParticipationStatusCode

An Enum of possible values for the [`ConstraintConflictStatus`](@ref) attribute.
This attribute is meant to indicate whether a given constraint participates
or not in the last computed conflict.

Possible values are:
* `NOT_IN_CONFLICT`: the constraint does not participate in the conflict
* `IN_CONFLICT`: the constraint participates in the conflict
* `MAYBE_IN_CONFLICT`: the constraint may participate in the conflict,
  the solver was not able to prove that the constraint can be excluded from
  the conflict
"""
@enum(
    ConflictParticipationStatusCode,
    NOT_IN_CONFLICT,
    IN_CONFLICT,
    MAYBE_IN_CONFLICT
)

"""
    ConstraintConflictStatus()

A constraint attribute indicating whether the constraint participates
in the conflict. Its type is [`ConflictParticipationStatusCode`](@ref).
"""
struct ConstraintConflictStatus <: AbstractConstraintAttribute end

function attribute_value_type(::ConstraintConflictStatus)
    return ConflictParticipationStatusCode
end

## Termination status

"""
    TerminationStatusCode

An Enum of possible values for the `TerminationStatus` attribute. This attribute
is meant to explain the reason why the optimizer stopped executing in the most
recent call to [`optimize!`](@ref).

If no call has been made to [`optimize!`](@ref), then the `TerminationStatus`
is:

* `OPTIMIZE_NOT_CALLED`: The algorithm has not started.

## OK

These are generally OK statuses, i.e., the algorithm ran to completion normally.

* `OPTIMAL`: The algorithm found a globally optimal solution.
* `INFEASIBLE`: The algorithm concluded that no feasible solution exists.
* `DUAL_INFEASIBLE`: The algorithm concluded that no dual bound exists for the
  problem. If, additionally, a feasible (primal) solution is known to
  exist, this status typically implies that the problem is unbounded, with some
  technical exceptions.
* `LOCALLY_SOLVED`: The algorithm converged to a stationary point, local
  optimal solution, could not find directions for improvement, or otherwise
  completed its search without global guarantees.
* `LOCALLY_INFEASIBLE`: The algorithm converged to an infeasible point or
  otherwise completed its search without finding a feasible solution, without
  guarantees that no feasible solution exists.
* `INFEASIBLE_OR_UNBOUNDED`: The algorithm stopped because it decided that the
  problem is infeasible or unbounded; this occasionally happens during MIP
  presolve.

## Solved to relaxed tolerances

* `ALMOST_OPTIMAL`: The algorithm found a globally optimal solution to relaxed
  tolerances.
* `ALMOST_INFEASIBLE`: The algorithm concluded that no feasible solution exists
  within relaxed tolerances.
* `ALMOST_DUAL_INFEASIBLE`: The algorithm concluded that no dual bound exists for
  the problem within relaxed tolerances.
* `ALMOST_LOCALLY_SOLVED`: The algorithm converged to a stationary point, local
  optimal solution, or could not find directions for improvement within relaxed
  tolerances.

## Limits

The optimizer stopped because of some user-defined limit.

* `ITERATION_LIMIT`: An iterative algorithm stopped after conducting the maximum
  number of iterations.
* `TIME_LIMIT`: The algorithm stopped after a user-specified computation time.
* `NODE_LIMIT`: A branch-and-bound algorithm stopped because it explored a
  maximum number of nodes in the branch-and-bound tree.
* `SOLUTION_LIMIT`: The algorithm stopped because it found the required number of
  solutions. This is often used in MIPs to get the solver to return the first
  feasible solution it encounters.
* `MEMORY_LIMIT`: The algorithm stopped because it ran out of memory.
* `OBJECTIVE_LIMIT`: The algorithm stopped because it found a solution better
  than a minimum limit set by the user.
* `NORM_LIMIT`: The algorithm stopped because the norm of an iterate became too
  large.
* `OTHER_LIMIT`: The algorithm stopped due to a limit not covered by one of the
  above.

## Problematic

This group of statuses means that something unexpected or problematic happened.

* `SLOW_PROGRESS`: The algorithm stopped because it was unable to continue making
  progress towards the solution.
* `NUMERICAL_ERROR`: The algorithm stopped because it encountered unrecoverable
  numerical error.
* `INVALID_MODEL`: The algorithm stopped because the model is invalid.
* `INVALID_OPTION`: The algorithm stopped because it was provided an invalid
  option.
* `INTERRUPTED`: The algorithm stopped because of an interrupt signal.
* `OTHER_ERROR`: The algorithm stopped because of an error not covered by one of
  the statuses defined above.
"""
@enum(
    TerminationStatusCode,
    OPTIMIZE_NOT_CALLED,
    # OK
    OPTIMAL,
    INFEASIBLE,
    DUAL_INFEASIBLE,
    LOCALLY_SOLVED,
    LOCALLY_INFEASIBLE,
    INFEASIBLE_OR_UNBOUNDED,
    # Solved to relaxed tolerances
    ALMOST_OPTIMAL,
    ALMOST_INFEASIBLE,
    ALMOST_DUAL_INFEASIBLE,
    ALMOST_LOCALLY_SOLVED,
    # Limits
    ITERATION_LIMIT,
    TIME_LIMIT,
    NODE_LIMIT,
    SOLUTION_LIMIT,
    MEMORY_LIMIT,
    OBJECTIVE_LIMIT,
    NORM_LIMIT,
    OTHER_LIMIT,
    # Problematic
    SLOW_PROGRESS,
    NUMERICAL_ERROR,
    INVALID_MODEL,
    INVALID_OPTION,
    INTERRUPTED,
    OTHER_ERROR
)

"""
    TerminationStatus()

A model attribute for the `TerminationStatusCode` explaining why the optimizer stopped.
"""
struct TerminationStatus <: AbstractModelAttribute end

attribute_value_type(::TerminationStatus) = TerminationStatusCode

"""
    RawStatusString()

A model attribute for a solver specific string explaining why the optimizer
stopped.
"""
struct RawStatusString <: AbstractModelAttribute end

attribute_value_type(::RawStatusString) = String

## Result status

"""
    ResultStatusCode

An Enum of possible values for the `PrimalStatus` and `DualStatus` attributes.
The values indicate how to interpret the result vector.

* `NO_SOLUTION`: the result vector is empty.
* `FEASIBLE_POINT`: the result vector is a feasible point.
* `NEARLY_FEASIBLE_POINT`: the result vector is feasible if some constraint
  tolerances are relaxed.
* `INFEASIBLE_POINT`: the result vector is an infeasible point.
* `INFEASIBILITY_CERTIFICATE`: the result vector is an infeasibility certificate.
  If the `PrimalStatus` is `INFEASIBILITY_CERTIFICATE`, then the primal result
  vector is a certificate of dual infeasibility. If the `DualStatus` is
  `INFEASIBILITY_CERTIFICATE`, then the dual result vector is a proof of primal
  infeasibility.
* `NEARLY_INFEASIBILITY_CERTIFICATE`: the result satisfies a relaxed criterion for
  a certificate of infeasibility.
* `REDUCTION_CERTIFICATE`: the result vector is an ill-posed certificate; see
  [this article](https://arxiv.org/abs/1408.4685) for details. If the
  `PrimalStatus` is `REDUCTION_CERTIFICATE`, then the primal result vector is a
  proof that the dual problem is ill-posed. If the `DualStatus` is
  `REDUCTION_CERTIFICATE`, then the dual result vector is a proof that the primal
  is ill-posed.
* `NEARLY_REDUCTION_CERTIFICATE`: the result satisfies a relaxed criterion for
  an ill-posed certificate.
* `UNKNOWN_RESULT_STATUS`: the result vector contains a solution with an unknown
  interpretation.
* `OTHER_RESULT_STATUS`: the result vector contains a solution with an
  interpretation not covered by one of the statuses defined above.
"""
@enum(
    ResultStatusCode,
    NO_SOLUTION,
    FEASIBLE_POINT,
    NEARLY_FEASIBLE_POINT,
    INFEASIBLE_POINT,
    INFEASIBILITY_CERTIFICATE,
    NEARLY_INFEASIBILITY_CERTIFICATE,
    REDUCTION_CERTIFICATE,
    NEARLY_REDUCTION_CERTIFICATE,
    UNKNOWN_RESULT_STATUS,
    OTHER_RESULT_STATUS
)

"""
    PrimalStatus(result_index::Int = 1)

A model attribute for the [`ResultStatusCode`](@ref) of the primal result
`result_index`. If `result_index` is omitted, it defaults to 1.

See [`ResultCount`](@ref) for information on how the results are ordered.

If `result_index` is larger than the value of [`ResultCount`](@ref) then
`NO_SOLUTION` is returned.
"""
struct PrimalStatus <: AbstractModelAttribute
    result_index::Int
    PrimalStatus(result_index::Int = 1) = new(result_index)
end

attribute_value_type(::PrimalStatus) = ResultStatusCode

"""
    DualStatus(result_index::Int = 1)

A model attribute for the `ResultStatusCode` of the dual result `result_index`.
If `result_index` is omitted, it defaults to 1.

See [`ResultCount`](@ref) for information on how the results are ordered.

If `result_index` is larger than the value of [`ResultCount`](@ref) then
`NO_SOLUTION` is returned.
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

Return a `Bool` indicating whether the value of the attribute is modified
during an [`optimize!`](@ref) call, that is, the attribute is used to query
the result of the optimization.

## Important note when defining new attributes

This function returns `false` by default so it should be implemented for
attributes that are modified by [`optimize!`](@ref).
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

## Important note when defining new attributes

By default `is_copyable(attr)` returns `!is_set_by_optimize(attr)`. A specific
method should be defined for attributes which are copied indirectly during
[`copy_to`](@ref). For instance, both `is_copyable` and
[`is_set_by_optimize`](@ref) return `false` for the following attributes:

* [`ListOfOptimizerAttributesSet`](@ref), [`ListOfModelAttributesSet`](@ref),
  [`ListOfConstraintAttributesSet`](@ref) and
  [`ListOfVariableAttributesSet`](@ref).
* [`SolverName`](@ref) and [`RawSolver`](@ref): these attributes cannot be set.
* [`NumberOfVariables`](@ref) and [`ListOfVariableIndices`](@ref): these
  attributes are set indirectly by [`add_variable`](@ref) and
  [`add_variables`](@ref).
* [`ObjectiveFunctionType`](@ref): this attribute is set indirectly when setting
  the [`ObjectiveFunction`](@ref) attribute.
* [`NumberOfConstraints`](@ref), [`ListOfConstraintIndices`](@ref),
  [`ListOfConstraintTypesPresent`](@ref), [`CanonicalConstraintFunction`](@ref),
  [`ConstraintFunction`](@ref) and [`ConstraintSet`](@ref):
  these attributes are set indirectly by
  [`add_constraint`](@ref) and [`add_constraints`](@ref).
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
