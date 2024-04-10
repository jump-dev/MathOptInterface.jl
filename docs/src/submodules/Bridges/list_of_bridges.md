```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    import MathOptInterface as MOI
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# List of bridges

This section describes the [`Bridges.AbstractBridge`](@ref)s that are
implemented in MathOptInterface.

## [Constraint bridges](@id constraint_bridges_ref)

These bridges are subtypes of [`Bridges.Constraint.AbstractBridge`](@ref).

```@autodocs
Modules = [Bridges.Constraint]
Filter = t -> begin
    M = MathOptInterface.Bridges.Constraint
    return t isa Type && t != M.AbstractBridge && t <: M.AbstractBridge
end
```

## [Objective bridges](@id objective_bridges_ref)

These bridges are subtypes of [`Bridges.Objective.AbstractBridge`](@ref).

```@autodocs
Modules = [Bridges.Objective]
Filter = t -> begin
    M = MathOptInterface.Bridges.Objective
    return t isa Type && t != M.AbstractBridge && t <: M.AbstractBridge
end
```

## [Variable bridges](@id variable_bridges_ref)

These bridges are subtypes of [`Bridges.Variable.AbstractBridge`](@ref).

```@autodocs
Modules = [Bridges.Variable]
Filter = t -> begin
    M = MathOptInterface.Bridges.Variable
    return t isa Type && t != M.AbstractBridge && t <: M.AbstractBridge
end
```
