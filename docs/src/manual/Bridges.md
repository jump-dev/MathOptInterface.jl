```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# The Bridges submodule

A constraint can often be written in a number of equivalent formulations. For
example, the constraint ``l \le a^\top x \le u``
([`ScalarAffineFunction`](@ref)-in-[`Interval`](@ref)) could be re-formulated as
two constraints: ``a^\top x \ge l`` ([`ScalarAffineFunction`](@ref)-in-[`GreaterThan`](@ref))
and ``a^\top x \le u`` (`ScalarAffineFunction`-in-`LessThan`). An alternative
re-formulation is to add a dummy variable `y` with the constraints ``l \le y \le u``
([`SingleVariable`](@ref)-in-[`Interval`](@ref)) and ``a^\top x - y = 0``
([`ScalarAffineFunction`](@ref)-in-[`EqualTo`](@ref)).

To avoid each solver having to code these transformations manually,
MathOptInterface provides *bridges*. A bridge is a small transformation from one
constraint type to another (potentially collection of) constraint type. Because
these bridges are included in MathOptInterface, they can be re-used by any
optimizer. Some bridges also implement constraint modifications and constraint
primal and dual translations.

For example, the `SplitIntervalBridge` defines the reformulation of a
`ScalarAffineFunction`-in-`Interval` constraint into a
`ScalarAffineFunction`-in-`GreaterThan` and a
`ScalarAffineFunction`-in-`LessThan` constraint. `SplitInterval` is the
bridge optimizer that applies the `SplitIntervalBridge` rewriting rule. Given
an optimizer `optimizer` implementing `ScalarAffineFunction`-in-`GreaterThan`
and `ScalarAffineFunction`-in-`LessThan`, the optimizer
```jldoctest; setup=:(model = MOI.Utilities.Model{Float64}())
optimizer = MOI.Bridges.Constraint.SplitInterval{Float64}(model)
MOI.supports_constraint(
    optimizer, MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}
)

# output

true
```
will additionally support [`ScalarAffineFunction`](@ref)-in-[`Interval`](@ref).
Note that these [`Bridges.Constraint.SingleBridgeOptimizer`](@ref)s are mainly
used for testing bridges.

It is recommended to rather use [`Bridges.full_bridge_optimizer`](@ref) which
automatically selects the appropriate constraint bridges for unsupported
constraints.
```julia
optimizer = MOI.Bridges.full_bridge_optimizer(model, Float64)
```

### Variable reformulations

A variable is often created constrained in a set unsupported by the solver while
it could be parametrized by variables constrained in supported sets.
For example, the [`Bridges.Variable.VectorizeBridge`](@ref) defines the
reformulation of a constrained variable in [`GreaterThan`](@ref) into a
constrained vector of one variable in [`Nonnegatives`](@ref).
The `Bridges.Variable.Vectorize` is the bridge optimizer that applies the
[`Bridges.Variable.VectorizeBridge`](@ref) rewriting rule. Given an optimizer
`optimizer` implementing constrained variables in [`Nonnegatives`](@ref),
the optimizer
```jldoctest; setup=:(model = MOI.Utilities.Model{Float64}())
optimizer = MOI.Bridges.Variable.Vectorize{Float64}(model)
MOI.supports_add_constrained_variable(optimizer, MOI.GreaterThan{Float64})

# output

true
```
will additionally support constrained variables in [`GreaterThan`](@ref).
Note that these [`Bridges.Variable.SingleBridgeOptimizer`](@ref) are mainly
used for testing bridges.

It is recommended to rather use [`Bridges.full_bridge_optimizer`](@ref), which
automatically selects the appropriate bridges for unsupported constrained
variables.
