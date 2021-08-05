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
[`ListOfVariableIndices`](@ref), and [`ListOfConstraintIndices`](@ref), the 
variables and constraints created by the bridges in the underlying model are
hidden by the bridge optimizer. For this purpose, the bridge should provide
access to the variables and constraints it has created by implementing the 
following methods of [`get`](@ref):
```@docs
get(::Bridges.Constraint.AbstractBridge, ::NumberOfVariables)
get(::Bridges.Constraint.AbstractBridge, ::ListOfVariableIndices)
get(::Bridges.AbstractBridge, ::NumberOfConstraints)
get(::Bridges.AbstractBridge, ::ListOfConstraintIndices)
```

# SetMap bridges

Implementing a constraint bridge relying on linear transformation between two
sets is easier thanks to the [SetMap interface](constraint_set_map).
The bridge simply needs to be a subtype of [`Bridges.Variable.SetMapBridge`]
for a variable bridge and [`Bridges.Constraint.SetMapBridge`] for a constraint
bridge and the linear transformation is represented with
[`Bridges.map_set`](@ref),
[`Bridges.map_function`](@ref),
[`Bridges.inverse_map_set`](@ref),
[`Bridges.inverse_map_function`](@ref),
[`Bridges.adjoint_map_function`](@ref) and
[`Bridges.inverse_adjoint_map_function`](@ref).
Note that the implementing last 4 methods is optional in the sense that if they
are not implemented, bridging constraint would still work but some features
would be missing as described in the docstrings.
See [L20, Section 2.1.2] for more details including [L20, Example 2.1.1] that
illustrates the idea for
[`Bridges.Variable.SOCtoRSOCBridge`](@ref),
[`Bridges.Variable.RSOCtoSOCBridge`](@ref),
[`Bridges.Constraint.SOCtoRSOCBridge`](@ref) and
[`Bridges.Constraint.RSOCtoSOCBridge`](@ref).

[L20] Legat, Benoît. *Set Programming: Theory and Computation*. PhD thesis. 2020.
