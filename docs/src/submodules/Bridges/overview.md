```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# The Bridges submodule

The `Bridges` module simplifies the process of converting models between
equivalent formulations.

!!! tip
    [Read our paper](https://arxiv.org/abs/2002.03447) for more details on how
    bridges are implemented.

## Why bridges?

A constraint can often be written in a number of equivalent formulations. For
example, the constraint ``l \le a^\top x \le u``
([`ScalarAffineFunction`](@ref)-in-[`Interval`](@ref)) could be re-formulated as
two constraints: ``a^\top x \ge l`` ([`ScalarAffineFunction`](@ref)-in-[`GreaterThan`](@ref))
and ``a^\top x \le u`` (`ScalarAffineFunction`-in-`LessThan`). An alternative
re-formulation is to add a dummy variable `y` with the constraints ``l \le y \le u``
([`SingleVariable`](@ref)-in-[`Interval`](@ref)) and ``a^\top x - y = 0``
([`ScalarAffineFunction`](@ref)-in-[`EqualTo`](@ref)).

To avoid each solver having to code these transformations manually,
MathOptInterface provides *bridges*.

A bridge is a small transformation from one constraint type to another
(potentially collection of) constraint type.

Because these bridges are included in MathOptInterface, they can be re-used by
any optimizer. Some bridges also implement constraint modifications and
constraint primal and dual translations.

Several bridges can be used in combination to transform a single constraint
into a form that the solver may understand. Choosing the bridges to use 
takes the form of finding a shortest path in the hypergraph of bridges. The
methodology is detailed in [the MOI paper](https://arxiv.org/abs/2002.03447).

## The three types of bridges

There are three types of bridges in MathOptInterface:
1. Constraint bridges
2. Variable bridges
3. Objective bridges

### Constraint bridges

Constraint bridges convert constraints formulated by the user into an equivalent
form supported by the solver. Constraint bridges are subtypes of 
[`Bridges.Constraint.AbstractBridge`](@ref).

The equivalent formulation may add constraints (and possibly also variables) in
the underlying model.

In particular, constraint bridges can focus on rewriting the function of a
constraint, and do not change the set. Function bridges are subtypes of 
[`Bridges.Constraint.AbstractFunctionConversionBridge`](@ref).

Read the [list of implemented constraint bridges](@ref constraint_bridges_ref)
for more details on the types of transformations that are available.
Function bridges are [`Bridges.Constraint.ScalarFunctionizeBridge`](@ref) and
[`Bridges.Constraint.VectorFunctionizeBridge`](@ref).

### [Variable bridges](@id variable_bridges)

Variable bridges convert variables added by the user, either free with
[`add_variable`](@ref)/[`add_variables`](@ref), or constrained with
[`add_constrained_variable`](@ref)/[`add_constrained_variables`](@ref),
into an equivalent form supported by the solver. Variable bridges are 
subtypes of [`Bridges.Variable.AbstractBridge`](@ref).

The equivalent formulation may add constraints (and possibly also variables) in
the underlying model.

Read the [list of implemented variable bridges](@ref variable_bridges_ref) for
more details on the types of transformations that are available.

### Objective bridges

Objective bridges convert the [`ObjectiveFunction`](@ref) set by the user into
an equivalent form supported by the solver. Objective bridges are 
subtypes of [`Bridges.Objective.AbstractBridge`](@ref).

The equivalent formulation may add constraints (and possibly also variables) in
the underlying model.

Read the [list of implemented objective bridges](@ref objective_bridges_ref) for
more details on the types of transformations that are available.

## Bridges.`full_bridge_optimizer`

!!! tip
    Unless you have an advanced use-case, this is probably the only function you
    need to care about.

To enable the full power of MathOptInterface's bridges, wrap an `optimizer`
in a [`Bridges.full_bridge_optimizer`](@ref).

```jldoctest
julia> inner_optimizer = MOI.Utilities.Model{Float64}()
MOIU.GenericModel{Float64,MOIU.ObjectiveFunctionContainer{Float64},MOIU.SingleVariableConstraints{Float64},MOIU.ModelFunctionConstraints{Float64}}

julia> optimizer = MOI.Bridges.full_bridge_optimizer(inner_optimizer, Float64)
MOIB.LazyBridgeOptimizer{MOIU.GenericModel{Float64,MOIU.ObjectiveFunctionContainer{Float64},MOIU.SingleVariableConstraints{Float64},MOIU.ModelFunctionConstraints{Float64}}}
with 0 variable bridges
with 0 constraint bridges
with 0 objective bridges
with inner model MOIU.GenericModel{Float64,MOIU.ObjectiveFunctionContainer{Float64},MOIU.SingleVariableConstraints{Float64},MOIU.ModelFunctionConstraints{Float64}}
```

That's all you have to do! Use `optimizer` as normal, and bridging will happen
lazily behind the scenes. By lazily, we mean that bridging will only happen if
the constraint is not supported by the `inner_optimizer`.

!!! info
    Most bridges are added by default in [`Bridges.full_bridge_optimizer`](@ref).
    However, for technical reasons, some bridges are not added by default. Three
    examples include [`Bridges.Constraint.SOCtoPSDBridge`](@ref),
    [`Bridges.Constraint.SOCtoNonConvexQuadBridge`](@ref) and
    [`Bridges.Constraint.RSOCtoNonConvexQuadBridge`](@ref). See the docs of
    those bridges for more information.

## Add a single bridge

If you don't want to use [`Bridges.full_bridge_optimizer`](@ref), you can wrap
an optimizer in a single bridge.

However, this will force the constraint to be bridged, even if the
`inner_optimizer` supports it.

```jldoctest
julia> inner_optimizer = MOI.Utilities.Model{Float64}()
MOIU.GenericModel{Float64,MOIU.ObjectiveFunctionContainer{Float64},MOIU.SingleVariableConstraints{Float64},MOIU.ModelFunctionConstraints{Float64}}

julia> optimizer = MOI.Bridges.Constraint.SplitInterval{Float64}(inner_optimizer)
MOIB.Constraint.SingleBridgeOptimizer{MOIB.Constraint.SplitIntervalBridge{Float64,F,S,LS,US} where US<:MOI.AbstractSet where LS<:MOI.AbstractSet where S<:MOI.AbstractSet where F<:MOI.AbstractFunction,MOIU.GenericModel{Float64,MOIU.ObjectiveFunctionContainer{Float64},MOIU.SingleVariableConstraints{Float64},MOIU.ModelFunctionConstraints{Float64}}}
with 0 constraint bridges
with inner model MOIU.GenericModel{Float64,MOIU.ObjectiveFunctionContainer{Float64},MOIU.SingleVariableConstraints{Float64},MOIU.ModelFunctionConstraints{Float64}}

julia> x = MOI.add_variable(optimizer)
MOI.VariableIndex(1)

julia> MOI.add_constraint(optimizer, MOI.SingleVariable(x), MOI.Interval(0.0, 1.0))
MathOptInterface.ConstraintIndex{MathOptInterface.SingleVariable,MathOptInterface.Interval{Float64}}(1)

julia> MOI.get(optimizer, MOI.ListOfConstraintTypesPresent())
1-element Array{Tuple{Type,Type},1}:
 (MathOptInterface.SingleVariable, MathOptInterface.Interval{Float64})

julia> MOI.get(inner_optimizer, MOI.ListOfConstraintTypesPresent())
2-element Array{Tuple{Type,Type},1}:
 (MathOptInterface.SingleVariable, MathOptInterface.GreaterThan{Float64})
 (MathOptInterface.SingleVariable, MathOptInterface.LessThan{Float64})
```

## Bridges.LazyBridgeOptimizer

If you don't want to use [`Bridges.full_bridge_optimizer`](@ref), but you need
more than a single bridge (or you want the bridging to happen lazily), you can
manually construct a [`Bridges.LazyBridgeOptimizer`](@ref).

First, wrap an inner optimizer:
```jldoctest lazy_bridge_optimizer
julia> inner_optimizer = MOI.Utilities.Model{Float64}()
MOIU.GenericModel{Float64,MOIU.ObjectiveFunctionContainer{Float64},MOIU.SingleVariableConstraints{Float64},MOIU.ModelFunctionConstraints{Float64}}

julia> optimizer = MOI.Bridges.LazyBridgeOptimizer(inner_optimizer)
MOIB.LazyBridgeOptimizer{MOIU.GenericModel{Float64,MOIU.ObjectiveFunctionContainer{Float64},MOIU.SingleVariableConstraints{Float64},MOIU.ModelFunctionConstraints{Float64}}}
with 0 variable bridges
with 0 constraint bridges
with 0 objective bridges
with inner model MOIU.GenericModel{Float64,MOIU.ObjectiveFunctionContainer{Float64},MOIU.SingleVariableConstraints{Float64},MOIU.ModelFunctionConstraints{Float64}}
```

Then use [`Bridges.add_bridge`](@ref) to add individual bridges:
```jldoctest lazy_bridge_optimizer
julia> MOI.Bridges.add_bridge(optimizer, MOI.Bridges.Constraint.SplitIntervalBridge{Float64})

julia> MOI.Bridges.add_bridge(optimizer, MOI.Bridges.Objective.FunctionizeBridge{Float64})
```

Now the constraints will be bridged only if needed:
```jldoctest lazy_bridge_optimizer
julia> x = MOI.add_variable(optimizer)
MOI.VariableIndex(1)

julia> MOI.add_constraint(optimizer, MOI.SingleVariable(x), MOI.Interval(0.0, 1.0))
MathOptInterface.ConstraintIndex{MathOptInterface.SingleVariable,MathOptInterface.Interval{Float64}}(1)

julia> MOI.get(optimizer, MOI.ListOfConstraintTypesPresent())
1-element Array{Tuple{Type,Type},1}:
 (MathOptInterface.SingleVariable, MathOptInterface.Interval{Float64})

julia> MOI.get(inner_optimizer, MOI.ListOfConstraintTypesPresent())
1-element Array{Tuple{Type,Type},1}:
 (MathOptInterface.SingleVariable, MathOptInterface.Interval{Float64})
```
