```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Manipulating expressions

This guide highlights a syntactically appealing way to build expressions at the
MOI level, but also to look at their contents. It may be especially useful
when writing models or bridge code.

## Creating functions

### Creating scalar affine functions

The simplest scalar function is simply a variable: 

```
var_idx = MOI.add_variable(model)
f1 = MOI.SingleVariable(var_idx)
```

This type of function is extremely simple: to express more complex functions, other 
types must be used. For instance, a `ScalarAffineFunction` is a sum of linear terms
(a factor times a variable) and a constant. Such an object can be built using the 
standard constructor: 

```
f2 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm(1, var_idx), 2)
```

However, you can also use operators to build the same scalar function: 

```
f2 = 1 * MOI.SingleVariable(var_idx) + 2
```

### Creating scalar quadratic functions

### Creating vector functions

## Canonicalizing functions
