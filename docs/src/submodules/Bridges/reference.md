```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface

    # For compatibility with both Julia 1.0.5 and 1.5.2
    # Upon the Julia LTS version becoming 1.6, these imports could be dropped,
    # and all ScalarAffineTerm and VariableIndex instances in doctests below
    # could be replaced with MOI.ScalarAffineTerm and MOI.VariableIndex
    # Check discussion at PR 1184: https://github.com/jump-dev/MathOptInterface.jl/pull/1184#discussion_r515300914
    import MathOptInterface.ScalarAffineTerm
    import MathOptInterface.VariableIndex
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

## Bridges

Bridges can be used for automatic reformulation of constrained variables (i.e.
variables added with [`add_constrained_variable`](@ref)/[`add_constrained_variables`](@ref))
or constraints into equivalent formulations using constrained variables and
constraints of different types. There are two important concepts to distinguish:
* [`Bridges.AbstractBridge`](@ref)s are recipes implementing a specific
  reformulation. Bridges are not directly subtypes of
  [`Bridges.AbstractBridge`](@ref), they are either
  [`Bridges.Variable.AbstractBridge`](@ref) or
  [`Bridges.Constraint.AbstractBridge`](@ref).
* [`Bridges.AbstractBridgeOptimizer`](@ref) is a layer that can be applied to
  another [`ModelLike`](@ref) to apply the reformulation. The
  [`Bridges.LazyBridgeOptimizer`](@ref) automatically chooses the appropriate
  bridges to use when a constrained variable or constraint is not supported
  by using the list of bridges that were added to it by
  [`Bridges.add_bridge`](@ref). [`Bridges.full_bridge_optimizer`](@ref) wraps a
  model in a [`Bridges.LazyBridgeOptimizer`](@ref) where all the bridges defined
  in MOI are added. This is the recommended way to use bridges in the
  [Testing guideline](@ref), and JuMP automatically calls
  [`Bridges.full_bridge_optimizer`](@ref) when attaching an optimizer.
  [`Bridges.debug_supports_constraint`](@ref) and [`Bridges.debug_supports`](@ref)
  allow introspection into the bridge selection rationale of
  [`Bridges.LazyBridgeOptimizer`](@ref).

Most bridges are added by default in [`Bridges.full_bridge_optimizer`](@ref).
However, for technical reasons, some bridges are not added by default, for instance:
[`Bridges.Constraint.SOCtoPSDBridge`](@ref), [`Bridges.Constraint.SOCtoNonConvexQuadBridge`](@ref)
and [`Bridges.Constraint.RSOCtoNonConvexQuadBridge`](@ref). See the docs of those bridges
for more information.

It is possible to add those bridges and also user defined bridges,
following one of the two methods. We present the examples for:
[`Bridges.Constraint.SOCtoNonConvexQuadBridge`](@ref).

The first option is to add the specific bridges to a
`bridged_model` optimizer, with coefficient type `T`. The `bridged_model`
optimizer itself must have been constructed with a
[`Bridges.LazyBridgeOptimizer`](@ref). Once such a optimizer is available, we
can proceed using using [`Bridges.add_bridge`](@ref):

```julia
MOIB.add_bridge(bridged_model, SOCtoNonConvexQuadBridge{T})
```

Alternatively, it is possible to create a [`Bridges.Constraint.SingleBridgeOptimizer`](@ref)
and wrap an existing `model` with it:

```julia
const SOCtoNonConvexQuad{T, OT<:ModelLike} = Bridges.Constraint.SingleBridgeOptimizer{Bridges.Constraint.SOCtoNonConvexQuadBridge{T}, OT}
bridged_model = SOCtoNonConvexQuad{Float64}(model)
```

Those procedures could be applied to user define bridges. For the
bridges defined in MathOptInterface, the [`Bridges.Constraint.SingleBridgeOptimizer`](@ref)'s are already created, therefore, for the case of [`Bridges.Constraint.SOCtoNonConvexQuadBridge`](@ref), one could simply use the existing optimizer:

```julia
bridged_model = Bridges.Constraint.SOCtoNonConvexQuad{Float64}(model)
```

```@docs
Bridges.AbstractBridge
Bridges.AbstractBridgeOptimizer
Bridges.LazyBridgeOptimizer
Bridges.add_bridge
Bridges.remove_bridge
Bridges.has_bridge
Bridges.full_bridge_optimizer
Bridges.debug_supports_constraint
Bridges.debug_supports
```

### [Variable bridges](@id variable_bridges)

When variables are added, either free with
[`add_variable`](@ref)/[`add_variables`](@ref),
or constrained with
[`add_constrained_variable`](@ref)/[`add_constrained_variables`](@ref),
variable bridges allow to return *bridged variables* that do not correspond to
variables of the underlying model. These variables are parametrized by
variables of the underlying model and this parametrization can be obtained with
[`Bridges.bridged_variable_function`](@ref). Similarly, the variables of the
underlying model that were created by the bridge can be expressed in terms of
the bridged variables and this expression can be obtained with
[`Bridges.unbridged_variable_function`](@ref).
For instance, consider a model bridged by the
[`Bridges.Variable.VectorizeBridge`](@ref):
```jldoctest bridged_variable_function
model = MOI.Utilities.Model{Float64}()
bridged_model = MOI.Bridges.Variable.Vectorize{Float64}(model)
bridged_variable, bridged_constraint = MOI.add_constrained_variable(bridged_model, MOI.GreaterThan(1.0))

# output

(VariableIndex(-1), MOI.ConstraintIndex{MOI.SingleVariable,MOI.GreaterThan{Float64}}(-1))
```
The constrained variable in `MOI.GreaterThan(1.0)` returned is a bridged
variable as its index in negative. In `model`, a constrained variable in
`MOI.Nonnegatives` is created:
```jldoctest bridged_variable_function
inner_variables = MOI.get(model, MOI.ListOfVariableIndices())

# output

1-element Array{VariableIndex,1}:
 VariableIndex(1)
```
In the functions used for adding constraints or setting the objective to
`bridged_model`, `bridged_variable` is substituted for `inner_variables[1]` plus
1:
```jldoctest bridged_variable_function
MOI.Bridges.bridged_variable_function(bridged_model, bridged_variable)

# output

MOI.ScalarAffineFunction{Float64}(MOI.ScalarAffineTerm{Float64}[ScalarAffineTerm{Float64}(1.0, VariableIndex(1))], 1.0)
```
When getting [`ConstraintFunction`](@ref) or [`ObjectiveFunction`](@ref),
`inner_variables[1]` is substituted for `bridged_variable` minus 1:
```jldoctest bridged_variable_function
MOI.Bridges.unbridged_variable_function(bridged_model, inner_variables[1])

# output

MOI.ScalarAffineFunction{Float64}(MOI.ScalarAffineTerm{Float64}[ScalarAffineTerm{Float64}(1.0, VariableIndex(-1))], -1.0)
```

!!! note
    A notable exception is with [`Bridges.Variable.ZerosBridge`](@ref) where no
    variable is created in the underlying model as the variables are simply
    transformed to zeros. When this bridge is used, it is not possible to
    recover functions with bridged variables from functions of the inner
    model. Consider for instance that we create two zero variables:
    ```jldoctest cannot_unbridge_zero
    model = MOI.Utilities.Model{Float64}()
    bridged_model = MOI.Bridges.Variable.Zeros{Float64}(model)
    bridged_variables, bridged_constraint = MOI.add_constrained_variables(bridged_model, MOI.Zeros(2))

    # output

    (MOI.VariableIndex[VariableIndex(-1), VariableIndex(-2)], MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Zeros}(-1))
    ```
    Consider the following functions in the variables of `bridged_model`:
    ```jldoctest cannot_unbridge_zero
    func = MOI.Utilities.operate(+, Float64, MOI.SingleVariable.(bridged_variables)...)

    # output

    MOI.ScalarAffineFunction{Float64}(MOI.ScalarAffineTerm{Float64}[ScalarAffineTerm{Float64}(1.0, VariableIndex(-1)), ScalarAffineTerm{Float64}(1.0, VariableIndex(-2))], 0.0)
    ```
    We can obtain the equivalent function in the variables of `model` as follows:
    ```jldoctest cannot_unbridge_zero
    inner_func = MOI.Bridges.bridged_function(bridged_model, func)

    # output

    MOI.ScalarAffineFunction{Float64}(MOI.ScalarAffineTerm{Float64}[], 0.0)
    ```
    However, it's not possible to invert this operation. Indeed, since the
    bridged variables are substituted for zeros, we cannot deduce whether
    they were present in the initial function.
    ```jldoctest cannot_unbridge_zero; filter = r"Stacktrace:.*"s
    MOI.Bridges.unbridged_function(bridged_model, inner_func)

    # output

    ERROR: Cannot unbridge function because some variables are bridged by variable bridges that do not support reverse mapping, e.g., `ZerosBridge`.
    Stacktrace:
     [1] error(::String, ::String, ::String) at ./error.jl:42
     [2] throw_if_cannot_unbridge at /home/blegat/.julia/dev/MathOptInterface/src/Bridges/Variable/map.jl:343 [inlined]
     [3] unbridged_function(::MOI.Bridges.Variable.SingleBridgeOptimizer{MOI.Bridges.Variable.ZerosBridge{Float64},MOI.Utilities.Model{Float64}}, ::MOI.ScalarAffineFunction{Float64}) at /home/blegat/.julia/dev/MOI/src/Bridges/bridge_optimizer.jl:920
     [4] top-level scope at none:0
    ```

```@docs
Bridges.Variable.AbstractBridge
Bridges.bridged_variable_function
Bridges.unbridged_variable_function
```

Below is the list of variable bridges implemented in this package.
```@docs
Bridges.Variable.ZerosBridge
Bridges.Variable.FreeBridge
Bridges.Variable.NonposToNonnegBridge
Bridges.Variable.VectorizeBridge
Bridges.Variable.SOCtoRSOCBridge
Bridges.Variable.RSOCtoSOCBridge
Bridges.Variable.RSOCtoPSDBridge
```

For each bridge defined in this package, a corresponding
[`Bridges.Variable.SingleBridgeOptimizer`](@ref) is available with the same
name without the "Bridge" suffix, e.g., `SplitInterval` is a
`SingleBridgeOptimizer` for the `SplitIntervalBridge`. Moreover, they are all
added in the [`Bridges.LazyBridgeOptimizer`](@ref) returned by
[`Bridges.full_bridge_optimizer`](@ref) as it calls
[`Bridges.Variable.add_all_bridges`](@ref).
```@docs
Bridges.Variable.SingleBridgeOptimizer
Bridges.Variable.add_all_bridges
```

### Constraint bridges

When constraints are added with [`add_constraint`](@ref), constraint bridges
allow to return *bridged constraints* that do not correspond to
constraints of the underlying model. These constraints were enforced by an
equivalent formulation that added constraints (and possibly also variables) in
the underlying model.
For instance, consider a model bridged by the
[`Bridges.Constraint.SplitIntervalBridge`](@ref):
```jldoctest split_interval
model = MOI.Utilities.Model{Float64}()
bridged_model = MOI.Bridges.Constraint.SplitInterval{Float64}(model)
x, y = MOI.add_variables(bridged_model, 2)
func = MOI.Utilities.operate(+, Float64, MOI.SingleVariable(x), MOI.SingleVariable(y))
c = MOI.add_constraint(bridged_model, func, MOI.Interval(1.0, 2.0))

# output

MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.Interval{Float64}}(1)
```
We can see the constraint was bridged to two constraints, one for each bound,
in the inner model.
```jldoctest split_interval
MOI.get(model, MOI.ListOfConstraints())

# output

2-element Array{Tuple{DataType,DataType},1}:
 (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})
 (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
```
However, `bridged_model` transparently hides these constraints and creates the
illusion that an interval constraint was created.
```jldoctest split_interval
MOI.get(bridged_model, MOI.ListOfConstraints())

# output

1-element Array{Tuple{DataType,DataType},1}:
 (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64})
```
It is nevertheless possible to differentiate this constraint from a constraint
added to the inner model by asking whether it is bridged:
```jldoctest split_interval
MOI.Bridges.is_bridged(bridged_model, c)

# output

true
```

```@docs
Bridges.Constraint.AbstractBridge
```

Below is the list of constraint bridges implemented in this package.
```@docs
Bridges.Constraint.GreaterToIntervalBridge
Bridges.Constraint.LessToIntervalBridge
Bridges.Constraint.GreaterToLessBridge
Bridges.Constraint.LessToGreaterBridge
Bridges.Constraint.NonnegToNonposBridge
Bridges.Constraint.NonposToNonnegBridge
Bridges.Constraint.VectorizeBridge
Bridges.Constraint.ScalarizeBridge
Bridges.Constraint.ScalarSlackBridge
Bridges.Constraint.VectorSlackBridge
Bridges.Constraint.ScalarFunctionizeBridge
Bridges.Constraint.VectorFunctionizeBridge
Bridges.Constraint.SplitIntervalBridge
Bridges.Constraint.RSOCBridge
Bridges.Constraint.SOCRBridge
Bridges.Constraint.QuadtoSOCBridge
Bridges.Constraint.SOCtoNonConvexQuadBridge
Bridges.Constraint.RSOCtoNonConvexQuadBridge
Bridges.Constraint.NormInfinityBridge
Bridges.Constraint.NormOneBridge
Bridges.Constraint.GeoMeantoRelEntrBridge
Bridges.Constraint.GeoMeanBridge
Bridges.Constraint.RelativeEntropyBridge
Bridges.Constraint.NormSpectralBridge
Bridges.Constraint.NormNuclearBridge
Bridges.Constraint.SquareBridge
Bridges.Constraint.RootDetBridge
Bridges.Constraint.LogDetBridge
Bridges.Constraint.SOCtoPSDBridge
Bridges.Constraint.RSOCtoPSDBridge
Bridges.Constraint.IndicatorActiveOnFalseBridge
Bridges.Constraint.IndicatorSOS1Bridge
Bridges.Constraint.SemiToBinaryBridge
Bridges.Constraint.ZeroOneBridge
```
For each bridge defined in this package, a corresponding
[`Bridges.Constraint.SingleBridgeOptimizer`](@ref) is available with the same
name without the "Bridge" suffix, e.g., `SplitInterval` is a
`SingleBridgeOptimizer` for the `SplitIntervalBridge`. Moreover, they are all
added in the [`Bridges.LazyBridgeOptimizer`](@ref) returned by
[`Bridges.full_bridge_optimizer`](@ref) as it calls
[`Bridges.Constraint.add_all_bridges`](@ref).
```@docs
Bridges.Constraint.SingleBridgeOptimizer
Bridges.Constraint.add_all_bridges
```

### Objective bridges

When an objective is set with [`set`](@ref), objective bridges
allow to set a *bridged objective* to the underlying model that do not
correspond to the objective set by the user. This equivalent formulation may add
constraints (and possibly also variables) in the underlying model in addition
to setting an objective function.

```@docs
Bridges.Objective.AbstractBridge
```

Below is the list of objective bridges implemented in this package.
```@docs
Bridges.Objective.SlackBridge
Bridges.Objective.FunctionizeBridge
```
For each bridge defined in this package, a corresponding
[`Bridges.Objective.SingleBridgeOptimizer`](@ref) is available with the same
name without the "Bridge" suffix, e.g., `Slack` is a `SingleBridgeOptimizer`
for the `SlackBridge`. Moreover, they are all added in the
[`Bridges.LazyBridgeOptimizer`](@ref) returned by
[`Bridges.full_bridge_optimizer`](@ref) as it calls
[`Bridges.Objective.add_all_bridges`](@ref).
```@docs
Bridges.Objective.SingleBridgeOptimizer
Bridges.Objective.add_all_bridges
```

### Bridge interface

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
