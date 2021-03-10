```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Variables

## Add a variable

All variables in MOI are scalar variables. New scalar variables are created with
[`add_variable`](@ref) or [`add_variables`](@ref), which return a [`VariableIndex`](@ref)
or `Vector{VariableIndex}` respectively. [`VariableIndex`](@ref) objects are
type-safe wrappers around integers that refer to a variable in a particular
model.

!!! note
    The integer does not necessarily corresond to the column inside an
    optimizer!

One uses [`VariableIndex`](@ref) objects to set and get variable attributes. For
example, the [`VariablePrimalStart`](@ref) attribute is used to provide an
initial starting point for a variable or collection of variables:
```julia
v = MOI.add_variable(model)
MOI.set(model, MOI.VariablePrimalStart(), v, 10.5)
v2 = MOI.add_variables(model, 3)
MOI.set(model, MOI.VariablePrimalStart(), v2, [1.3, 6.8, -4.6])
```

## Delete a variable

Delete a variable using
[`delete(::ModelLike, ::VariableIndex)`](@ref MathOptInterface.delete(::MathOptInterface.ModelLike, ::MathOptInterface.Index)).

!!! warning
    Not all `ModelLike` models support deleting variables. A
    [`DeleteNotAllowed`](@ref) error is thrown if this is not supported.
