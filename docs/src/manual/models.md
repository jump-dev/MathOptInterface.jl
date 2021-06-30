```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Models

The most significant part of MOI is the definition of the **model API** that is
used to specify an instance of an optimization problem (e.g., by adding
variables and constraints). Objects that implement the model API should inherit
from the [`ModelLike`](@ref) abstract type.

Notably missing from the model API is the method to solve an optimization
problem. `ModelLike` objects may store an instance (e.g., in memory or backed by
a file format) without being linked to a particular solver. In addition to the
model API, MOI defines [`AbstractOptimizer`](@ref).

*Optimizers* (or solvers) implement the model API (inheriting from `ModelLike`)
and additionally provide methods to solve the model.

!!! info
    Throughout the rest of the manual, `model` is used as a generic `ModelLike`,
    and `optimizer` is used as a generic `AbstractOptimizer`.

!!! tip
    MOI does not export functions, but for brevity we often omit qualifying
    names with the MOI module. Best practice is to have
    ```julia
    using MathOptInterface
    const MOI = MathOptInterface
    ```
    and prefix all MOI methods with `MOI.` in user code. If a name is also
    available in base Julia, we always explicitly use the module prefix, for
    example, with `MOI.get`.

## Attributes

Attributes can be set in different ways:

* it is either set when the model is created like [`SolverName`](@ref) and
  [`RawSolver`](@ref),
* or explicitly when the model is copied like [`ObjectiveSense`](@ref),
* or implicitly, e.g., [`NumberOfVariables`](@ref) is implicitly set by
  [`add_variable`](@ref) and [`ConstraintFunction`](@ref) is implicitly set by
  [`add_constraint`](@ref).
* or it is set to contain the result of the optimization during
  [`optimize!`](@ref) like [`VariablePrimal`](@ref).

The following attributes are available for models:

 * [`Name`](@ref)

 * [`ObjectiveFunction`](@ref)
 * [`ObjectiveFunctionType`](@ref)
 * [`ObjectiveSense`](@ref)

 * [`NumberOfVariables`](@ref)
 * [`ListOfVariableIndices`](@ref)

 * [`ListOfConstraintTypesPresent`](@ref)
 * [`NumberOfConstraints`](@ref)
 * [`ListOfConstraintIndices`](@ref)

 * [`ListOfOptimizerAttributesSet`](@ref)
 * [`ListOfModelAttributesSet`](@ref)
 * [`ListOfVariableAttributesSet`](@ref)
 * [`ListOfConstraintAttributesSet`](@ref)
