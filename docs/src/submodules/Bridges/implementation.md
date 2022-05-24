```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Implementation

This section provides high-level overview of the methods required to implement a
new bridge. For a more detailed walk-through, read
[Implementing a constraint bridge](@ref).

!!! warning
    Implementing a bridge requires an advanced understanding of
    MathOptInterface.

## All bridges

All bridges must implement the following functions:
```@docs
Bridges.added_constrained_variable_types
Bridges.added_constraint_types
get(::Bridges.AbstractBridge, ::NumberOfVariables)
get(::Bridges.AbstractBridge, ::ListOfVariableIndices)
get(::Bridges.AbstractBridge, ::NumberOfConstraints)
get(::Bridges.AbstractBridge, ::ListOfConstraintIndices)
```

## Variable bridges

Variable bridges must additionally implement:
```@docs
Bridges.Variable.supports_constrained_variable
Bridges.Variable.concrete_bridge_type
Bridges.Variable.bridge_constrained_variable
```

## Constraint bridges

Constraint bridges must additionally implement:
```@docs
supports_constraint(::Type{<:Bridges.Constraint.AbstractBridge}, ::Type{<:AbstractFunction}, ::Type{<:AbstractSet})
Bridges.Constraint.concrete_bridge_type
Bridges.Constraint.bridge_constraint
```

## Objective bridges

Objective bridges must additionally implement:
```@docs
Bridges.Objective.supports_objective_function
Bridges.set_objective_function_type
Bridges.Objective.concrete_bridge_type
Bridges.Objective.bridge_objective
```

## SetMap bridges

Implementing a constraint bridge relying on linear transformation between two
sets is easier thanks to the [SetMap interface](@ref constraint_set_map).

The bridge simply needs to be a subtype of [`Bridges.Variable.SetMapBridge`]
for a variable bridge and [`Bridges.Constraint.SetMapBridge`] for a constraint
bridge and the linear transformation is represented with:

 * [`Bridges.map_set`](@ref)
 * [`Bridges.map_function`](@ref)
 * [`Bridges.inverse_map_set`](@ref)
 * [`Bridges.inverse_map_function`](@ref)
 * [`Bridges.adjoint_map_function`](@ref)
 * [`Bridges.inverse_adjoint_map_function`](@ref)

Implementing the last four methods is optional, in the sense that if they are
not implemented, bridging the constraint would still work, but some features
(described in the docstrings of each method) would be missing.

The [thesis of Benoît Legat](https://dial.uclouvain.be/pr/boreal/object/boreal:237650)
has more details, including derivations of the SetMap bridges:

 * [`Bridges.Variable.SOCtoRSOCBridge`](@ref)
 * [`Bridges.Variable.RSOCtoSOCBridge`](@ref)
 * [`Bridges.Constraint.SOCtoRSOCBridge`](@ref)
 * [`Bridges.Constraint.RSOCtoSOCBridge`](@ref)
