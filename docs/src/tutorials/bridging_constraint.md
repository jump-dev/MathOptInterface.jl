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

In this tutorial, we will cover the creation of a bridge from `<=` 
([`LessThan`](@ref)) to `>=` ([`GreaterThan`](@ref)), i.e. creating a 
constraint with reversed signs, only for scalar affine functions.

## Four mandatory parts in a constraint bridge

The first part of a constraint bridge is a new concrete type that inherits from
[`Bridges.Constraint.AbstractBridge`](@ref). This type must have fields to 
store all the new variables and constraints that the bridge will add. 
Typically, these types are parametrized by the type of the coefficients in the
model.

Then, three sets of functions must be defined: 

1. [`Bridges.Constraint.bridge_constraint`](@ref): this function implements the
   bridge and creates the required variables and constraints.
2. [`supports_constraint`](@ref): these functions should return `true` when the 
   combination of function and set is supported by the bridge. By default, the 
   base implementation always returns `false` and the bridge does not have to 
   provide this implementation.
3. [`Bridges.added_constrained_variable_types`](@ref) and 
   [`Bridges.added_constraint_types`](@ref): these functions return the types 
   of variables and constraints that this bridge adds. They are used to compute
   the set of other bridges that are required to use the one you are defining, 
   if need be.

More functions can be implemented, for instance to retrieve properties from the 
bridge or deleting a bridged constraint.

### 1. Structure for the bridge

A typical `struct` behind a bridge depends on the type of the coefficients that
are used for the model (typically `Float64`, but coefficients might also be 
integers or complex numbers).

This structure must hold a reference to all the variables and the constraints 
that are created as part of the bridge.

The type of this structure is used throughout MOI as an identifier for the 
bridge. It is passed as argument to most functions related to bridges.

The best practice is to have the name of this type end with `Bridge`.

In our example, the bridge should be able to map any 
`ScalarAffineFunction{T}`-in-`LessThan{T}` constraint to a single
`ScalarAffineFunction{T}`-in-`GreaterThan{T}` constraint. The affine function
has coefficients of type `T`. The bridge is parametrized with `T`, so that the 
constraint that the bridge creates also has coefficients of type `T`.

```julia
struct SignBridge{T <: Number} <: Bridges.Constraint.AbstractBridge
    constraint::ConstraintIndex{ScalarAffineFunction{T}, GreaterThan{T}}
end
```

### 2. Bridge creation

The function [`Bridges.Constraint.bridge_constraint`](@ref) is called whenever
the bridge should be instantiated for a specific model, with the given function
and set. The arguments to `bridge_constraint` are highly similar to 
[`add_constraint`](@ref), with the exception of the first argument: it is the
type of the structure defined in the first step.

`bridge_constraint` returns an object whose type is the structure defined in
the first step.

In our example, the bridge constraint could be defined as: 

```julia
function Bridges.Constraint.bridge_constraint(
    ::Type{SignBridge{T}}, # Bridge to use.
    model, # Model to which the constraint is being added.
    f::ScalarAffineFunction{T}, # Function to rewrite.
    s::LessThan{T}, # Set to rewrite.
) where {T}
    # Create the variables and constraints required for the bridge.
    con = add_constraint(model, -f, GreaterThan(-s.upper))

    # Return an instance of the bridge type with a reference to all the 
    # variables and constraints that were created in this function.
    return SignBridge(con)
end
```

### 3. Supported constraint types

The function [`supports_constraint`](@ref) determines whether the bridge type
supports a given combination of function and set.

This function must closely match `bridge_constraint`, because it will not be 
called if `supports_constraint` returns `false`.

```julia
function supports_constraint(
    ::Type{SignBridge{T}}, # Bridge to use.
    ::Type{ScalarAffineFunction{T}}, # Function to rewrite.
    ::Type{LessThan{T}}, # Set to rewrite.
) where {T}
    # Do some computation to ensure that the constraint is supported.
    # Typically, you can directly return true.
    return true
end
```

### 4. Metadata about the bridge

To determine whether a bridge can be used, MOI uses a shortest-path algorithm
that uses the variable types and the constraints that the bridge can create. 
This information is communicated from the bridge to MOI using the functions 
[`Bridges.added_constrained_variable_types`](@ref) and 
[`Bridges.added_constraint_types`](@ref). Both return lists of tuples: 
either a list of 1-tuples containing the variable types (typically, `ZeroOne` 
or `Integer`) or a list of 2-tuples contained the functions and sets (like 
`ScalarAffineFunction{T}`-`GreaterThan`).

For our example, the bridge does not create any constrained variables, and
only `ScalarAffineFunction{T}`-in-`GreaterThan{T}` constraints:

```julia
function Bridges.added_constrained_variable_types(::Type{SignBridge{T}}) where {T}
    # The bridge does not create variables, return an empty list of tuples: 
    return Tuple{DataType}[]
end

function Bridges.added_constraint_types(::Type{SignBridge{T}}) where {T}
    return [
        # One element per F-in-S the bridge creates.
        (ScalarAffineFunction{T}, GreaterThan{T}),
    ]
end
```

A bridge that creates binary variables would rather have this definition of 
`added_constrained_variable_types`:

```julia
function Bridges.added_constrained_variable_types(::Type{SomeBridge{T}}) where {T}
    # The bridge only creates binary variables: 
    return [(MOI.ZeroOne,)]
end
```

## Bridge registration

For a bridge to be used by MOI, it must be known by MOI. 

### `SingleBridgeOptimizer`

The first way to do so is to create a single-bridge optimizer. This type of 
optimizer wraps another optimizer and adds the possibility to use only one 
bridge. It is especially useful when unit testing bridges.

It is common practice to use the same name as the type defined for the bridge
(`SignBridge`, in our example) without the suffix `Bridge`.

```julia
const Sign{T, OT <: MOI.ModelLike} =
    SingleBridgeOptimizer{SignBridge{T}, OT}
```

In the context of unit tests, this bridge is used in conjunction with a 
[`Utilities.MockOptimizer`](@ref): 

```julia
mock = Utilities.MockOptimizer(
    Utilities.UniversalFallback(Utilities.Model{Float64}()),
)
bridged_mock = Sign{Float64}(mock)
```

### New bridge for a `LazyBridgeOptimizer`

Typical user-facing models for MOI are based on 
[`Bridges.LazyBridgeOptimizer`](@ref). For instance, this type of model is 
returned by [`Bridges.full_bridge_optimizer`](@ref). These models can be 
added more bridges by using [`Bridges.add_bridge`](@ref): 

```julia
inner_optimizer = Utilities.Model{Float64}()
optimizer = Bridges.full_bridge_optimizer(inner_optimizer, Float64)
Bridges.add_bridge(optimizer, SignBridge{Float64})
```

## Bridge improvements

### Attribute retrieval

Like models, bridges have attributes that can be retrieved using [`get`](@ref) 
and [`set`](@ref). The most important ones are the number of variables and
constraints, but also the lists of variables and constraints.

In our example, we only have one constraint and only have to implement the 
[`NumberOfConstraints`](@ref) and [`ListOfConstraintIndices`](@ref) attributes:

```julia
function get(
    ::SignBridge{T},
    ::NumberOfConstraints{
        ScalarAffineFunction{T},
        GreaterThan{T},
    },
) where {T}
    return 1
end

function get(
    bridge::SignBridge{T},
    ::ListOfConstraintIndices{
        ScalarAffineFunction{T},
        GreaterThan{T},
    },
) where {T}
    return [bridge.constraint]
end
```

You must implement one such pair of functions for each type of constraint the 
bridge adds to the model.

!!! warning
    Avoid returning a list from the bridge object without copying it. Users 
    should be able to change the contents of the returned list without altering
    the bridge object.

For variables, the situation is simpler. If your bridge creates new variables, 
you should implement the [`NumberOfVariables`](@ref) and 
[`ListOfVariableIndices`](@ref) attributes. However, these attributes do not have
parameters, unlike their constraint counterparts. Only two functions suffice:

```julia
function get(
    ::SignBridge{T},
    ::NumberOfVariables,
) where {T}
    return …
end

function get(
    ::SignBridge{T},
    ::ListOfVariableIndices,
) where {T}
    return …
end
```

### Model modifications

To avoid copying the model when the user request to change a constraint, MOI 
provides [`MOI.modify`](@ref). Bridges can also implement this API to allow 
certain changes, such as coefficient changes.

In our case, a modification of a coefficient in the original constraint
(i.e. replacing the value of the coefficient of a variable in the affine 
function) should be transmitted to the constraint created by the bridge,
but with a sign change.

```julia
function MOI.modify(
    model,
    bridge::SignBridge,
    change::ScalarCoefficientChange,
)
    MOI.modify(
        model,
        bridge.constraint,
        MOI.ScalarCoefficientChange(change.variable, -change.new_coefficient),
    )
    return
end
```

### Bridge deletion

When a bridge is deleted, the constraints it added should be deleted too.

```julia
function delete(model, bridge::SignBridge)
    delete(model, bridge.constraint)
    return
end
```
