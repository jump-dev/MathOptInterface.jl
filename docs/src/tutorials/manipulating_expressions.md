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

When working with MathOptInterface, the major use of functions is creating them,
which is reviewed in this section.

### Creating scalar affine functions

The simplest scalar function is simply a variable: 

```julia
var_idx = MOI.add_variable(model) # Create the variable x
f1 = MOI.SingleVariable(var_idx) # x
```

This type of function is extremely simple: to express more complex functions, 
other types must be used. For instance, a `ScalarAffineFunction` is a sum of 
linear terms (a factor times a variable) and a constant. Such an object can be 
built using the standard constructor: 

```julia
f2 = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1, var_idx)], 2) # x + 2
```

However, you can also use operators to build the same scalar function: 

```julia
f2 = MOI.SingleVariable(var_idx) + 2 # x + 2
```

!!! warning
    If you get an error such as `ERROR: UndefVarError: T not defined`, it means 
    that the Julia compiler was not able to determine the type of the 
    coefficients for the function. In that case, you can insert a 
    multiplication by one (with the appropriate type). For instance,
    `1.0 * MOI.SingleVariable(var_idx)` creates an `MOI.ScalarAffineFunction`
    whose coefficients are of type `Float64` (the type of `1.0`).

### Creating scalar quadratic functions

Scalar quadratic functions are stored in `ScalarQuadraticFunction` objects, in a
way that is highly similar to scalar affine functions. You can obtain a quadratic
function as a product of affine functions: 

```julia
f3 = 1 * MOI.SingleVariable(var_idx) * MOI.SingleVariable(var_idx) # x²
f4 = f2 * f2 # (x + 2)²
f4 = f2^2 # (x + 2)² too
```

### Creating vector functions

## Canonicalizing functions

In more advanced use cases, you might need to ensure that a function is "canonical".
