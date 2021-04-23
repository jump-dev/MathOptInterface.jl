```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Variables

## Types

```@docs
VariableIndex
```

## Functions

```@docs
add_variable
add_variables
add_constrained_variable
add_constrained_variables
supports_add_constrained_variable
supports_add_constrained_variables
is_valid(::ModelLike,::VariableIndex)
delete(::ModelLike, ::VariableIndex)
delete(::ModelLike, ::Vector{VariableIndex})
```

## Attributes

```@docs
AbstractVariableAttribute
VariableName
VariablePrimalStart
VariablePrimal
```
