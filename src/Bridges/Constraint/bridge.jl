# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    abstract type AbstractBridge <: MOI.Bridges.AbstractType

Subtype of [`MOI.Bridges.AbstractBridge`](@ref) for constraint bridges.

In addition to the required implementation described in
[`MOI.Bridges.AbstractBridge`](@ref), subtypes of `AbstractBridge` must
additionally implement:

 * [`MOI.supports_constraint(::Type{<:AbstractBridge}, ::Type{<:MOI.AbstractFunction}, ::Type{<:MOI.AbstractSet})`](@ref)
 * [`concrete_bridge_type`](@ref)
 * [`bridge_constraint`](@ref)
"""
abstract type AbstractBridge <: MOI.Bridges.AbstractBridge end

"""
    MOI.supports_constraint(
        BT::Type{<:AbstractBridge},
        F::Type{<:MOI.AbstractFunction},
        S::Type{<:MOI.AbstractSet},
    )::Bool

Return a `Bool` indicating whether the bridges of type `BT` support bridging
`F`-in-`S` constraints.

## Implementation notes

 * This method depends only on the type of the inputs, not the runtime values.
 * There is a default fallback, so you need only implement this method for
   constraint types that the bridge implements.
"""
function MOI.supports_constraint(
    @nospecialize(BT::Type{<:AbstractBridge}),
    @nospecialize(F::Type{<:MOI.AbstractFunction}),
    @nospecialize(S::Type{<:MOI.AbstractSet}),
)
    return false
end

"""
    concrete_bridge_type(
        BT::Type{<:AbstractBridge},
        F::Type{<:MOI.AbstractFunction},
        S::Type{<:MOI.AbstractSet}
    )::Type

Return the concrete type of the bridge supporting `F`-in-`S` constraints.

This function can only be called if `MOI.supports_constraint(BT, F, S)` is
`true`.

## Example

The [`SplitIntervalBridge`](@ref) bridges a [`MOI.VariableIndex`](@ref)-in-[`MOI.Interval`](@ref)
constraint into a [`MOI.VariableIndex`](@ref)-in-[`MOI.GreaterThan`](@ref) and a
[`MOI.VariableIndex`](@ref)-in-[`MOI.LessThan`](@ref) constraint.

```jldoctest
julia> MOI.Bridges.Constraint.concrete_bridge_type(
           MOI.Bridges.Constraint.SplitIntervalBridge{Float64},
           MOI.VariableIndex,
           MOI.Interval{Float64},
       )
MathOptInterface.Bridges.Constraint.SplitIntervalBridge{Float64, MathOptInterface.VariableIndex, MathOptInterface.Interval{Float64}, MathOptInterface.GreaterThan{Float64}, MathOptInterface.LessThan{Float64}}
```
"""
function concrete_bridge_type(
    ::Type{BT},
    ::Type{<:MOI.AbstractFunction},
    ::Type{<:MOI.AbstractSet},
) where {BT}
    return BT
end

function concrete_bridge_type(
    b::MOI.Bridges.AbstractBridgeOptimizer,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    return concrete_bridge_type(MOI.Bridges.bridge_type(b, F, S), F, S)
end

"""
    bridge_constraint(
        BT::Type{<:AbstractBridge},
        model::MOI.ModelLike,
        func::AbstractFunction,
        set::MOI.AbstractSet,
    )::BT

Bridge the constraint `func`-in-`set` using bridge `BT` to `model` and returns
a bridge object of type `BT`.

## Implementation notes

 * The bridge type `BT` should be a concrete type, that is, all the type
   parameters of the bridge must be set.
"""
function bridge_constraint end
