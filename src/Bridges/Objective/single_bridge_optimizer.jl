# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    SingleBridgeOptimizer{BT<:AbstractBridge}(model::MOI.ModelLike)

Return `AbstractBridgeOptimizer` that always bridges any objective function
supported by the bridge `BT`.

This is in contrast with the [`MOI.Bridges.LazyBridgeOptimizer`](@ref),
which only bridges the objective function if it is supported by the bridge `BT`
and unsupported by `model`.

## Example

```jldoctest obj_singlebridgeoptimizer
julia> struct MyNewBridge{T} <: MOI.Bridges.Objective.AbstractBridge end

julia> bridge = MOI.Bridges.Objective.SingleBridgeOptimizer{MyNewBridge{Float64}}(
           MOI.Utilities.Model{Float64}(),
       );
```

## Implementation notes

All bridges should simplify the creation of `SingleBridgeOptimizer`s by defining
a constant that wraps the bridge in a `SingleBridgeOptimizer`.
```jldoctest obj_singlebridgeoptimizer
julia> const MyNewBridgeModel{T,OT<:MOI.ModelLike} =
           MOI.Bridges.Objective.SingleBridgeOptimizer{MyNewBridge{T},OT};
```
This enables users to create bridged models as follows:
```jldoctest obj_singlebridgeoptimizer
julia> MyNewBridgeModel{Float64}(MOI.Utilities.Model{Float64}());
```
"""
mutable struct SingleBridgeOptimizer{BT<:AbstractBridge,OT<:MOI.ModelLike} <:
               MOI.Bridges.AbstractBridgeOptimizer
    model::OT
    map::Map # `MOI.ObjectiveFunction` -> objective bridge
end

function SingleBridgeOptimizer{BT}(model::OT) where {BT,OT<:MOI.ModelLike}
    return SingleBridgeOptimizer{BT,OT}(model, Map())
end

bridges(::MOI.Bridges.AbstractBridgeOptimizer) = EmptyMap()

bridges(bridge::SingleBridgeOptimizer) = bridge.map

MOI.Bridges.supports_constraint_bridges(::SingleBridgeOptimizer) = false

function MOI.Bridges.is_bridged(
    ::SingleBridgeOptimizer,
    ::Type{<:MOI.AbstractSet},
)
    return false
end

function MOI.Bridges.is_bridged(
    ::SingleBridgeOptimizer,
    ::Type{<:MOI.AbstractFunction},
    ::Type{<:MOI.AbstractSet},
)
    return false
end

MOI.Bridges.is_bridged(::SingleBridgeOptimizer, ::MOI.VariableIndex) = false

# We need the next two methods to remove method ambiguities.

function MOI.Bridges.is_bridged(
    ::SingleBridgeOptimizer,
    ::MOI.ConstraintIndex{MOI.VariableIndex},
)
    return false
end

function MOI.Bridges.is_bridged(
    ::SingleBridgeOptimizer,
    ::MOI.ConstraintIndex{MOI.VectorOfVariables},
)
    return false
end

function MOI.Bridges.supports_bridging_objective_function(
    ::SingleBridgeOptimizer{BT},
    F::Type{<:MOI.AbstractFunction},
) where {BT}
    return supports_objective_function(BT, F)
end

function MOI.Bridges.is_bridged(
    bridge::SingleBridgeOptimizer,
    F::Type{<:MOI.AbstractFunction},
)
    return MOI.Bridges.supports_bridging_objective_function(bridge, F)
end

function MOI.Bridges.bridge_type(
    ::SingleBridgeOptimizer{BT},
    ::Type{<:MOI.AbstractFunction},
) where {BT}
    return BT
end

MOI.Bridges.recursive_model(bridge::SingleBridgeOptimizer) = bridge.model
