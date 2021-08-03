```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Implementing a constraint bridge

This guide outlines the basic steps to create a new bridge from a constraint
expressed in the formalism `Function`-in-`Set`.

## Preliminaries

First, decide on the set you want to bridge. Then, study its properties: the 
most important one is whether the set is scalar or vector, which impacts the 
dimensionality of the functions that can be used with the set. 

* A scalar function only has one dimension. MOI defines three types of scalar 
  functions: a variable ([`SingleVariable`](@ref)), an affine function 
  ([`ScalarAffineFunction`](@ref)), or a quadratic function 
  ([`ScalarQuadraticFunction`](@ref)).
* A vector function has several dimensions (at least one). MOI defines three 
  types of vector functions: several variables ([`VectorOfVariables`](@ref)), 
  an affine function ([`VectorAffineFunction`](@ref)), or a quadratic function 
  ([`VectorQuadraticFunction`](@ref)). The main difference with scalar functions 
  is that the order of dimensions can be very important: for instance, in an 
  indicator constraint ([`Indicator`](@ref)), the first dimension indicates
  whether the constraint about the second dimension is active.

## Four parts in a constraint bridge

The first part of a constraint bridge is a new concrete type that inherits from
[`Bridges.Constraint.AbstractBridge`](@ref). This type must have fields to 
store all the new variables and constraints that the bridge will add. 
Typically, these types are parametrized by the type of the coefficients in the
model.

Then, three sets of functions must be defined: 

1. [`Bridges.Constraint.bridge_constraint`](@ref): this function implements the
   bridge and creates the required variables and constraints.
2. `supports_constraint`: these functions should return `true` when the 
   combination of function and set is supported by the bridge. By default, the 
   base implementation always returns `false` and the bridge does not have to 
   provide this implementation.
3. [`Bridges.added_constrained_variable_types`](@ref) and 
   [`Bridges.added_constraint_types`](@ref): these functions return the types 
   of variables and constraints that this bridge adds. They are used to compute
   the set of other bridges that are required to use the one you are defining, 
   if need be.
