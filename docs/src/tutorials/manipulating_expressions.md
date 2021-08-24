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

When working with MathOptInterface, the major use of functions is creating 
them, which is reviewed in this section.

### Creating scalar affine functions

The simplest scalar function is simply a variable: 

```julia
var_idx = MOI.add_variable(model) # Create the variable x
f1 = MOI.SingleVariable(var_idx) # x
```

This type of function is extremely simple: to express more complex functions, 
other types must be used. For instance, a [`ScalarAffineFunction`](@ref) is a
sum of linear terms (a factor times a variable) and a constant. Such an object
can be built using the standard constructor: 

```julia
f2 = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1, var_idx)], 2) # x + 2
```

However, you can also use operators to build the same scalar function: 

```julia
f2 = MOI.SingleVariable(var_idx) + 2 # x + 2
```

!!! warning
    If you get an error such as `ERROR: UndefVarError: T not defined`, it 
    means that the Julia compiler was not able to determine the type of the 
    coefficients for the function. In that case, you can insert a 
    multiplication by one (with the appropriate type). For instance,
    `1.0 * MOI.SingleVariable(var_idx)` creates an 
    [`MOI.ScalarAffineFunction`](@ref) whose coefficients are of type `Float64`
    (the type of `1.0`).

### Creating scalar quadratic functions

Scalar quadratic functions are stored in [`ScalarQuadraticFunction`](@ref) 
objects, in a way that is highly similar to scalar affine functions. You can
obtain a quadratic function as a product of affine functions: 

```julia
f3 = 1 * MOI.SingleVariable(var_idx) * MOI.SingleVariable(var_idx) # x²
f4 = f2 * f2 # (x + 2)²
f4 = f2^2 # (x + 2)² too
```

### Creating vector functions

A vector function is a function with several values, irrespective of the number
of input variables. Similarly to scalar functions, there are three main types 
of vector functions: [`VectorOfVariables`](@ref), 
[`VectorAffineFunction`](@ref), and [`VectorQuadraticFunction`](@ref).

The easiest way to create a vector function is to stack several scalar
functions using [`MOI.Utilities.vectorize`](@ref). It takes a vector as input,
and the generated vector function (of the most appropriate type) has each 
dimension corresponding to a dimension of the vector.

```julia
f5 = MOIU.vectorize([f2, 2 * f2])
```

!!! warning
    [`MOIU.vectorize`](@ref) only takes a vector of similar scalar functions: 
    you cannot mix [`SingleVariable`](@ref) and [`ScalarAffineFunction`](@ref),
    for instance. In practice, it means that `MOIU.vectorize([f1, f2])` does 
    not work; you should rather use `MOIU.vectorize([1 * f1, f2])` instead to 
    only have [`ScalarAffineFunction`](@ref) objects.

## Canonicalizing functions

In more advanced use cases, you might need to ensure that a function is 
"canonical". Functions are stored as an array of terms, but there is no check
that these terms are redundant: a [`ScalarAffineFunction`](@ref) object might
have two terms with the same variable, like `x + x + 1`. These terms could be
merged without changing the semantics of the function: `2 * x + 1`. 

Working with these objects might be cumbersome. Canonicalization helps maintain 
redundancy to zero. 

[`MOIU.is_canonical`](@ref) checks whether a function is already in its 
canonical form:

```julia
MOIU.is_canonical(f2 + f2) # (x + 2) + (x + 2) is stored as x + x + 2
```

[`MOIU.canonical`](@ref) returns the equivalent canonical version of the 
function:

```julia
MOIU.canonical(f2 + f2) # Returns 2 * x + 2
```

## Exploring functions

At some point, you might need to dig into a function, for instance to map it 
into solver constructs.

### Vector functions

[`MOIU.scalarize`](@ref) returns a vector of scalar functions from a vector 
function:

```julia
MOIU.scalarize(MOIU.vectorize([f2, 2 * f2])) # Returns a vector [f2, 2 * f2].
```

!!! note
    [`MOIU.eachscalar`](@ref) returns an iterator on the dimensions, which
    serves the same purpose as [`MOIU.scalarize`](@ref).

[`MOI.output_dimension`](@ref) returns the number of dimensions of the 
output of a function:

```julia
MOI.output_dimension(MOIU.vectorize([f2, 2 * f2])) # Returns 2.
```
