```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Errors

When an MOI call fails on a model, precise errors should be thrown when possible
instead of simply calling `error` with a message. The docstrings for the
respective methods describe the errors that the implementation should thrown in
certain situations. This error-reporting system allows code to distinguish
between internal errors (that should be shown to the user) and unsupported
operations which may have automatic workarounds.

When an invalid index is used in an MOI call, an [`InvalidIndex`](@ref) should
be thrown:
```@docs
InvalidIndex
```

When an invalid result index is used to retrieve an attribute, a 
[`ResultIndexBoundsError`](@ref) should be thrown: 
```@docs
ResultIndexBoundsError
check_result_index_bounds
```

As discussed in [JuMP mapping](@ref), for scalar constraint with a nonzero
function constant, a [`ScalarFunctionConstantNotZero`](@ref) exception may be
thrown:
```@docs
ScalarFunctionConstantNotZero
```

Some [`VariableIndex`](@ref) constraints cannot be combined on the same
variable:
```@docs
LowerBoundAlreadySet
UpperBoundAlreadySet
```

As discussed in [`AbstractCallback`](@ref), trying to [`get`](@ref) attributes
inside a callback may throw:
```@docs
OptimizeInProgress
```

Trying to submit the wrong type of [`AbstractSubmittable`](@ref) inside an
[`AbstractCallback`](@ref) (e.g., a [`UserCut`](@ref) inside a
[`LazyConstraintCallback`](@ref)) will throw:
```@docs
InvalidCallbackUsage
```

The rest of the errors defined in MOI fall in two categories represented by the
following two abstract types:
```@docs
UnsupportedError
NotAllowedError
```

The different [`UnsupportedError`](@ref) and [`NotAllowedError`](@ref) are the
following errors:
```@docs
UnsupportedAttribute
SetAttributeNotAllowed
AddVariableNotAllowed
UnsupportedConstraint
AddConstraintNotAllowed
ModifyConstraintNotAllowed
ModifyObjectiveNotAllowed
DeleteNotAllowed
UnsupportedSubmittable
SubmitNotAllowed
```

Note that setting the [`ConstraintFunction`](@ref) of a [`VariableIndex`](@ref)
constraint is not allowed:
```@docs
SettingVariableIndexNotAllowed
```
