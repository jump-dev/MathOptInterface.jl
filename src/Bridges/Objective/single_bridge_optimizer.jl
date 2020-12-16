"""
    SingleBridgeOptimizer{BT<:AbstractBridge, OT<:MOI.ModelLike} <: AbstractBridgeOptimizer

The `SingleBridgeOptimizer` bridges any objective functions supported by the
bridge `BT`. This is in contrast with the [`MathOptInterface.Bridges.LazyBridgeOptimizer`](@ref)
which only bridges the objective functions that are unsupported by the internal model,
even if they are supported by one of its bridges.
"""
mutable struct SingleBridgeOptimizer{BT<:AbstractBridge,OT<:MOI.ModelLike} <:
               MOIB.AbstractBridgeOptimizer
    model::OT
    map::Map # `MOI.ObjectiveFunction` -> objective bridge
end
function SingleBridgeOptimizer{BT}(model::OT) where {BT,OT<:MOI.ModelLike}
    return SingleBridgeOptimizer{BT,OT}(model, Map())
end

function bridges(bridge::MOI.Bridges.AbstractBridgeOptimizer)
    return EmptyMap()
end
bridges(bridge::SingleBridgeOptimizer) = bridge.map

MOIB.supports_constraint_bridges(::SingleBridgeOptimizer) = false
function MOIB.is_bridged(::SingleBridgeOptimizer, ::Type{<:MOI.AbstractSet})
    return false
end
function MOIB.is_bridged(
    ::SingleBridgeOptimizer,
    ::Type{<:MOI.AbstractFunction},
    ::Type{<:MOI.AbstractSet},
)
    return false
end
function MOIB.supports_bridging_objective_function(
    ::SingleBridgeOptimizer{BT},
    F::Type{<:MOI.AbstractScalarFunction},
) where {BT}
    return supports_objective_function(BT, F)
end
function MOIB.is_bridged(
    b::SingleBridgeOptimizer,
    F::Type{<:MOI.AbstractScalarFunction},
)
    return MOIB.supports_bridging_objective_function(b, F)
end
function MOIB.bridge_type(
    ::SingleBridgeOptimizer{BT},
    ::Type{<:MOI.AbstractScalarFunction},
) where {BT}
    return BT
end
