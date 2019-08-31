"""
    SingleBridgeOptimizer{BT<:AbstractBridge, OT<:MOI.ModelLike} <: AbstractBridgeOptimizer

The `SingleBridgeOptimizer` bridges any constraint supported by the bridge `BT`.
This is in contrast with the [`MathOptInterface.Bridges.LazyBridgeOptimizer`](@ref)
which only bridges the constraints that are unsupported by the internal model,
even if they are supported by one of its bridges.
"""
mutable struct SingleBridgeOptimizer{BT<:AbstractBridge, OT<:MOI.ModelLike} <: MOIB.AbstractBridgeOptimizer
    model::OT
    map::Map # index of bridged constraint -> constraint bridge
    con_to_name::Dict{MOI.ConstraintIndex, String}
    name_to_con::Union{Dict{String, MOI.ConstraintIndex}, Nothing}
end
function SingleBridgeOptimizer{BT}(model::OT) where {BT, OT <: MOI.ModelLike}
    SingleBridgeOptimizer{BT, OT}(
        model, Map(), Dict{MOI.ConstraintIndex, String}(), nothing)
end

function Base.show(io::IO, B::SingleBridgeOptimizer)
    s(n) = n == 1 ? "" : "s"
    indent = " "^get(io, :indent, 0)
    n_con = length(bridges(B))
    MOIU._print(io, summary(B))
    print(io, "\n$(indent)with $(n_con) constraint bridge$(s(n_con))")
    print(io, "\n$(indent)with inner model ")
    show(IOContext(io, :indent => get(io, :indent, 0)+2), B.model)
end

function bridges(bridge::MOI.Bridges.AbstractBridgeOptimizer)
    return EmptyMap()
end
function bridges(bridge::SingleBridgeOptimizer)
    return bridge.map
end

MOIB.supports_constraint_bridges(::SingleBridgeOptimizer) = true
function MOIB.is_bridged(::SingleBridgeOptimizer, ::Type{<:MOI.AbstractSet})
    return false
end
function MOIB.supports_bridging_constraint(
    ::SingleBridgeOptimizer{BT}, F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet}) where BT
    return MOI.supports_constraint(BT, F, S)
end
function MOIB.is_bridged(b::SingleBridgeOptimizer, F::Type{<:MOI.AbstractFunction},
                    S::Type{<:MOI.AbstractSet})
    return MOIB.supports_bridging_constraint(b, F, S)
end
function MOIB.bridge_type(::SingleBridgeOptimizer{BT},
                          ::Type{<:MOI.AbstractFunction},
                          ::Type{<:MOI.AbstractSet}) where BT
    return BT
end
