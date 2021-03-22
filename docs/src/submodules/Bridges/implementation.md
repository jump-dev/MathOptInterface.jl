```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Bridge interface

A bridge should implement the following functions to be usable by a bridge optimizer:
```@docs
Bridges.added_constrained_variable_types
Bridges.added_constraint_types
```
Additionally, variable bridges should implement:
```@docs
Bridges.Variable.supports_constrained_variable
Bridges.Variable.concrete_bridge_type
Bridges.Variable.bridge_constrained_variable
```
constraint bridges should implement:
```@docs
supports_constraint(::Type{<:Bridges.Constraint.AbstractBridge}, ::Type{<:AbstractFunction}, ::Type{<:AbstractSet})
Bridges.Constraint.concrete_bridge_type
Bridges.Constraint.bridge_constraint
```
and objective bridges should implement:
```@docs
Bridges.set_objective_function_type
Bridges.Objective.concrete_bridge_type
Bridges.Objective.bridge_objective
```

When querying the [`NumberOfVariables`](@ref), [`NumberOfConstraints`](@ref)
and [`ListOfConstraintIndices`](@ref), the variables and constraints created
by the bridges in the underlying model are hidden by the bridge optimizer.
For this purpose, the bridge should provide access to the variables and
constraints it has creates by implemented the following methods of
[`get`](@ref):
```@docs
get(::Bridges.Constraint.AbstractBridge, ::NumberOfVariables)
get(::Bridges.Constraint.AbstractBridge, ::ListOfVariableIndices)
get(::Bridges.AbstractBridge, ::NumberOfConstraints)
get(::Bridges.AbstractBridge, ::ListOfConstraintIndices)
```
