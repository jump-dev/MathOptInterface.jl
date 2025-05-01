# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    SingleBridgeOptimizer{BT<:AbstractBridge}(model::MOI.ModelLike)

Return [`MOI.Bridges.AbstractBridgeOptimizer`](@ref) that always bridges any
variables constrained on creation supported by the bridge `BT`.

This is in contrast with the [`MOI.Bridges.LazyBridgeOptimizer`](@ref),
which only bridges the variables constrained on creation if they are supported
by the bridge `BT` and unsupported by `model`.

!!! warning
    Two `SingleBridgeOptimizer`s cannot be used together as both of them assume
    that the underlying model only returns variable indices with nonnegative
    values. Use [`MOI.Bridges.LazyBridgeOptimizer`](@ref) instead.

## Example

```jldoctest var_singlebridgeoptimizer
julia> struct MyNewBridge{T} <: MOI.Bridges.Variable.AbstractBridge end

julia> bridge = MOI.Bridges.Variable.SingleBridgeOptimizer{MyNewBridge{Float64}}(
           MOI.Utilities.Model{Float64}(),
       );
```

## Implementation notes

All bridges should simplify the creation of `SingleBridgeOptimizer`s by defining
a constant that wraps the bridge in a `SingleBridgeOptimizer`.
```jldoctest var_singlebridgeoptimizer
julia> const MyNewBridgeModel{T,OT<:MOI.ModelLike} =
           MOI.Bridges.Variable.SingleBridgeOptimizer{MyNewBridge{T},OT};
```
This enables users to create bridged models as follows:
```jldoctest var_singlebridgeoptimizer
julia> MyNewBridgeModel{Float64}(MOI.Utilities.Model{Float64}());
```
"""
mutable struct SingleBridgeOptimizer{BT<:AbstractBridge,OT<:MOI.ModelLike} <:
               MOI.Bridges.AbstractBridgeOptimizer
    model::OT
    map::Map # index of bridged variable -> variable bridge
    var_to_name::Dict{MOI.VariableIndex,String}
    name_to_var::Union{Dict{String,MOI.VariableIndex},Nothing}
    con_to_name::Dict{MOI.ConstraintIndex,String}
    name_to_con::Union{Dict{String,MOI.ConstraintIndex},Nothing}
end

function SingleBridgeOptimizer{BT}(model::OT) where {BT,OT<:MOI.ModelLike}
    return SingleBridgeOptimizer{BT,OT}(
        model,
        Map(),
        Dict{MOI.VariableIndex,String}(),
        nothing,
        Dict{MOI.ConstraintIndex,String}(),
        nothing,
    )
end

bridges(::MOI.Bridges.AbstractBridgeOptimizer) = EmptyMap()

bridges(bridge::SingleBridgeOptimizer) = bridge.map

MOI.Bridges.supports_constraint_bridges(::SingleBridgeOptimizer) = false

function MOI.Bridges.supports_bridging_constrained_variable(
    ::SingleBridgeOptimizer{BT},
    S::Type{<:MOI.AbstractSet},
) where {BT}
    return supports_constrained_variable(BT, S)
end

function MOI.Bridges.is_variable_bridged(
    ::SingleBridgeOptimizer,
    ::Type{<:MOI.AbstractSet},
)
    return true
end

function MOI.Bridges.is_bridged(
    b::SingleBridgeOptimizer,
    S::Type{<:MOI.AbstractSet},
)
    return MOI.Bridges.supports_bridging_constrained_variable(b, S)
end

function MOI.Bridges.is_bridged(
    ::SingleBridgeOptimizer,
    ::Type{<:MOI.AbstractFunction},
    ::Type{<:MOI.AbstractSet},
)
    return false
end

function MOI.Bridges.is_bridged(
    ::SingleBridgeOptimizer,
    ::Type{<:MOI.AbstractScalarFunction},
)
    return false
end

function MOI.Bridges.bridge_type(
    ::SingleBridgeOptimizer{BT},
    ::Type{<:MOI.AbstractSet},
) where {BT}
    return BT
end

MOI.Bridges.bridging_cost(::SingleBridgeOptimizer, args...) = 1.0

MOI.Bridges.recursive_model(b::SingleBridgeOptimizer) = b.model
