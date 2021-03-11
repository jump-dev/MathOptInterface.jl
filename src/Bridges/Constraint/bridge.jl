"""
    AbstractBridge

Subtype of [`MathOptInterface.Bridges.AbstractBridge`](@ref) for constraint
bridges.
"""
abstract type AbstractBridge <: MOIB.AbstractBridge end

"""
    bridge_constraint(
        BT::Type{<:AbstractBridge},
        model::MOI.ModelLike,
        func::AbstractFunction,
        set::MOI.AbstractSet,
    )

Bridge the constraint `func`-in-`set` using bridge `BT` to `model` and returns
a bridge object of type `BT`. The bridge type `BT` should be a concrete type,
that is, all the type parameters of the bridge should be set. Use
[`concrete_bridge_type`](@ref) to obtain a concrete type for given function
and set types.
"""
function bridge_constraint end

"""
    MOI.get(b::AbstractBridge, ::MOI.NumberOfVariables)

The number of variables created by the bridge `b` in the model.
"""
MOI.get(::AbstractBridge, ::MOI.NumberOfVariables) = Int64(0)

"""
    MOI.get(b::AbstractBridge, ::MOI.ListOfVariableIndices)

The list of variables created by the bridge `b` in the model.
"""
MOI.get(::AbstractBridge, ::MOI.ListOfVariableIndices) = MOI.VariableIndex[]

"""
    MOI.supports_constraint(
        BT::Type{<:AbstractBridge},
        F::Type{<:MOI.AbstractFunction},
        S::Type{<:MOI.AbstractSet},
    )::Bool

Return a `Bool` indicating whether the bridges of type `BT` support bridging
`F`-in-`S` constraints.
"""
function MOI.supports_constraint(
    ::Type{<:AbstractBridge},
    ::Type{<:MOI.AbstractFunction},
    ::Type{<:MOI.AbstractSet},
)
    return false
end

"""
    added_constrained_variable_types(
        BT::Type{<:MOI.Bridges.Constraint.AbstractBridge},
        F::Type{<:MOI.AbstractFunction},
        S::Type{<:MOI.AbstractSet},
    )

Return a list of the types of constrained variables that bridges of type `BT`
add for bridging `F`-in-`S` constraints. This falls back to
`added_constrained_variable_types(concrete_bridge_type(BT, F, S))` so bridges
should not implement this method.
"""
function MOIB.added_constrained_variable_types(
    BT::Type{<:AbstractBridge},
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    return MOIB.added_constrained_variable_types(concrete_bridge_type(BT, F, S))
end

"""
    added_constraint_types(
        BT::Type{<:MOI.Bridges.Constraint.AbstractBridge},
        F::Type{<:MOI.AbstractFunction},
        S::Type{<:MOI.AbstractSet},
    )

Return a list of the types of constraints that bridges of type `BT` add for
bridging `F`-in-`S` constraints. This falls back to
`added_constraint_types(concrete_bridge_type(BT, F, S))`
so bridges should not implement this method.
"""
function MOIB.added_constraint_types(
    BT::Type{<:AbstractBridge},
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    return MOIB.added_constraint_types(concrete_bridge_type(BT, F, S))
end

"""
    concrete_bridge_type(
        BT::Type{<:AbstractBridge},
        F::Type{<:MOI.AbstractFunction},
        S::Type{<:MOI.AbstractSet}
    )::DataType

Return the concrete type of the bridge supporting `F`-in-`S` constraints. This
function can only be called if `MOI.supports_constraint(BT, F, S)` is `true`.

## Examples

As a [`MathOptInterface.SingleVariable`](@ref)-in-[`MathOptInterface.Interval`](@ref)
constraint is bridged into a
[`MathOptInterface.SingleVariable`](@ref)-in-[`MathOptInterface.GreaterThan`](@ref)
and a
[`MathOptInterface.SingleVariable`](@ref)-in-[`MathOptInterface.LessThan`](@ref)
by the [`SplitIntervalBridge`](@ref):

```jldoctest; setup=:(using MathOptInterface; const MOI = MathOptInterface)
MOI.Bridges.Constraint.concrete_bridge_type(
    MOI.Bridges.Constraint.SplitIntervalBridge{Float64},
    MOI.SingleVariable,
    MOI.Interval{Float64},
)

# output

MathOptInterface.Bridges.Constraint.SplitIntervalBridge{Float64,MathOptInterface.SingleVariable,MathOptInterface.Interval{Float64},MathOptInterface.GreaterThan{Float64},MathOptInterface.LessThan{Float64}}
```
"""
function concrete_bridge_type(
    bridge_type::DataType,
    ::Type{<:MOI.AbstractFunction},
    ::Type{<:MOI.AbstractSet},
)
    return bridge_type
end

function concrete_bridge_type(
    b::MOIB.AbstractBridgeOptimizer,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    return concrete_bridge_type(MOIB.bridge_type(b, F, S), F, S)
end
