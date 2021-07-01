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
model API, MOI defines [`AbstractOptimizer`](@ref) and provides methods to solve
the model and interact with solutions. See the [Optimizers](@ref) section for
more details.

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

Attributes are properties of the model that can be queried and modified. These
include constants such as the number of variables in a model [`NumberOfVariables`](@ref)),
and properties of variables and constraints such as the name of a variable
([`VariableName`](@ref)).

There are four types of attributes:

 * Model attributes (subtypes of [`AbstractModelAttribute`](@ref)) refer to
   properties of a model.
 * Optimizer attributes (subtypes of [`AbstractOptimizerAttribute`](@ref)) refer
   to properties of an optimizer.
 * Constraint attributes (subtypes of [`AbstractConstraintAttribute`](@ref))
   refer to properties of an individual constraint.
 * Variable attributes (subtypes of [`AbstractVariableAttribute`](@ref)) refer
   to properties of an individual variable.

Some attributes are constants that can be queried by the user but not modified,
while other attributes can be modified by the user.

All interactions with attributes occur through the [`get`](@ref) and [`set`](@ref)
functions.

Consult the docstsrings of each attribute for information on what it represents.

## Model attributes

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

## Optimizer attributes

The following attributes are available for optimizers:

 - [`SolverName`](@ref)
 - [`RawOptimizerAttribute`](@ref)
 - [`Silent`](@ref)
 - [`TimeLimitSec`](@ref)
 - [`NumberOfThreads`](@ref)
 - [`RawSolver`](@ref)

In addition, optimizers should implement the following attributes to provide
access to solutions:

 - [`TerminationStatus`](@ref)
 - [`TerminationStatusCode`](@ref)
 - [`PrimalStatus`](@ref)
 - [`DualStatus`](@ref)
 - [`ResultStatusCode`](@ref)
 - [`RawStatusString`](@ref)
 - [`ResultCount`](@ref)
 - [`ObjectiveValue`](@ref)
 - [`DualObjectiveValue`](@ref)
 - [`ObjectiveBound`](@ref)
 - [`RelativeGap`](@ref)
 - [`SolveTimeSec`](@ref)
 - [`SimplexIterations`](@ref)
 - [`BarrierIterations`](@ref)
 - [`NodeCount`](@ref)
