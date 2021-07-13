"""
    SingleBridgeOptimizer{BT<:AbstractBridge, OT<:MOI.ModelLike} <:
    AbstractBridgeOptimizer

The `SingleBridgeOptimizer` bridges any constraint supported by the bridge `BT`.
This is in contrast with the [`MathOptInterface.Bridges.LazyBridgeOptimizer`](@ref)
which only bridges the constraints that are unsupported by the internal model,
even if they are supported by one of its bridges.
"""
mutable struct SingleBridgeOptimizer{BT<:AbstractBridge,OT<:MOI.ModelLike} <:
               MOIB.AbstractBridgeOptimizer
    model::OT
    map::Map # index of bridged constraint -> constraint bridge
    con_to_name::Dict{MOI.ConstraintIndex,String}
    name_to_con::Union{Dict{String,MOI.ConstraintIndex},Nothing}
end

function SingleBridgeOptimizer{BT}(model::OT) where {BT,OT<:MOI.ModelLike}
    return SingleBridgeOptimizer{BT,OT}(
        model,
        Map(),
        Dict{MOI.ConstraintIndex,String}(),
        nothing,
    )
end

function bridges(bridge::MOI.Bridges.AbstractBridgeOptimizer)
    return EmptyMap()
end

function bridges(bridge::SingleBridgeOptimizer)
    return bridge.map
end

MOIB.supports_constraint_bridges(::SingleBridgeOptimizer) = true

# If `BT` bridges `MOI.Reals` (such as `Constraint.FunctionizeBridge` bridge,
# without this method, it creates a `StackOverflow` with
# `is_bridged`, `supports_bridging_constrained_variable`
# and `supports_add_constrained_variables`.
MOIB.is_bridged(::SingleBridgeOptimizer, ::Type{MOI.Reals}) = false

function MOIB.is_bridged(b::SingleBridgeOptimizer, S::Type{<:MOI.AbstractSet})
    return MOIB.supports_bridging_constrained_variable(b, S)
end

MOIB.is_bridged(::SingleBridgeOptimizer, ::MOI.VariableIndex) = false
function MOIB.is_bridged(
    b::SingleBridgeOptimizer,
    ci::MOI.ConstraintIndex{MOI.SingleVariable,S},
) where {S}
    return MOIB.is_bridged(b, MOI.SingleVariable, S) &&
           haskey(Constraint.bridges(b), ci)
end
function MOIB.is_bridged(
    b::SingleBridgeOptimizer,
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables,S},
) where {S}
    return MOIB.is_bridged(b, MOI.VectorOfVariables, S) &&
           haskey(Constraint.bridges(b), ci)
end

function MOIB.supports_bridging_constrained_variable(
    b::SingleBridgeOptimizer,
    S::Type{<:MOI.AbstractSet},
)
    return MOIB.supports_bridging_constraint(
        b,
        MOIU.variable_function_type(S),
        S,
    ) && MOI.supports_add_constrained_variables(b, MOI.Reals)
end

function MOIB.supports_bridging_constraint(
    ::SingleBridgeOptimizer{BT},
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
) where {BT}
    return MOI.supports_constraint(BT, F, S)
end

function MOIB.is_bridged(
    b::SingleBridgeOptimizer,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    return MOIB.supports_bridging_constraint(b, F, S)
end

function MOIB.is_bridged(
    ::SingleBridgeOptimizer,
    ::Type{<:MOI.AbstractScalarFunction},
)
    return false
end

function MOIB.bridge_type(
    ::SingleBridgeOptimizer{BT},
    ::Type{<:MOI.AbstractFunction},
    ::Type{<:MOI.AbstractSet},
) where {BT}
    return BT
end

MOIB.bridging_cost(::SingleBridgeOptimizer, args...) = 1.0
