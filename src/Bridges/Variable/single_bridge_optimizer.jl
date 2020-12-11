"""
    SingleBridgeOptimizer{BT<:AbstractBridge, OT<:MOI.ModelLike} <: AbstractBridgeOptimizer

The `SingleBridgeOptimizer` bridges any constrained variables supported by the
bridge `BT`. This is in contrast with the [`MathOptInterface.Bridges.LazyBridgeOptimizer`](@ref)
which only bridges the constrained variables that are unsupported by the internal model,
even if they are supported by one of its bridges.

!!! note
    Two bridge optimizers using variable bridges cannot be used together as both
    of them assume that the underlying model only returns variable indices with
    nonnegative values.
"""
mutable struct SingleBridgeOptimizer{BT<:AbstractBridge,OT<:MOI.ModelLike} <:
               MOIB.AbstractBridgeOptimizer
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

function bridges(bridge::MOI.Bridges.AbstractBridgeOptimizer)
    return EmptyMap()
end
bridges(bridge::SingleBridgeOptimizer) = bridge.map

MOIB.supports_constraint_bridges(::SingleBridgeOptimizer) = false
function MOIB.supports_bridging_constrained_variable(
    ::SingleBridgeOptimizer{BT},
    S::Type{<:MOI.AbstractSet},
) where {BT}
    return supports_constrained_variable(BT, S)
end
function MOIB.is_variable_bridged(
    ::SingleBridgeOptimizer,
    ::Type{<:MOI.AbstractSet},
)
    return true
end
function MOIB.is_bridged(b::SingleBridgeOptimizer, S::Type{<:MOI.AbstractSet})
    return MOIB.supports_bridging_constrained_variable(b, S)
end
function MOIB.is_bridged(
    ::SingleBridgeOptimizer,
    ::Type{<:MOI.AbstractFunction},
    ::Type{<:MOI.AbstractSet},
)
    return false
end
function MOIB.is_bridged(
    ::SingleBridgeOptimizer,
    ::Type{<:MOI.AbstractScalarFunction},
)
    return false
end
function MOIB.bridge_type(
    ::SingleBridgeOptimizer{BT},
    ::Type{<:MOI.AbstractSet},
) where {BT}
    return BT
end
MOIB.bridging_cost(::SingleBridgeOptimizer, args...) = 1.0
