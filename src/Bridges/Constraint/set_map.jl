abstract type SetMapBridge{T,S2,S1,F,G} <: AbstractBridge end

function bridge_constraint(
    BT::Type{<:SetMapBridge{T,S2,S1,F,G}},
    model::MOI.ModelLike,
    func::G,
    set::S1,
) where {T,S2,S1,F,G}
    mapped_func = map_function(BT, func)
    constraint = MOI.add_constraint(model, mapped_func, map_set(BT, set))
    return BT(constraint)
end

function MOI.supports_constraint(
    ::Type{<:SetMapBridge{T,S2,S1}},
    ::Type{<:MOI.AbstractScalarFunction},
    ::Type{S1},
) where {T,S2,S1<:MOI.AbstractScalarSet}
    return true
end
function MOI.supports_constraint(
    ::Type{<:SetMapBridge{T,S2,S1}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{S1},
) where {T,S2,S1<:MOI.AbstractVectorSet}
    return true
end
function MOIB.added_constrained_variable_types(::Type{<:SetMapBridge})
    return Tuple{DataType}[]
end
function MOIB.added_constraint_types(
    ::Type{<:SetMapBridge{T,S2,S1,F}},
) where {T,S2,S1,F}
    return [(F, S2)]
end

# Attributes, Bridge acting as a model
function MOI.get(
    ::SetMapBridge{T,S2,S1,F},
    ::MOI.NumberOfConstraints{F,S2},
) where {T,S2,S1,F}
    return 1
end
function MOI.get(
    bridge::SetMapBridge{T,S2,S1,F},
    ::MOI.ListOfConstraintIndices{F,S2},
) where {T,S2,S1,F}
    return [bridge.constraint]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::SetMapBridge)
    return MOI.delete(model, bridge.constraint)
end

# Attributes, Bridge acting as a constraint
function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::SetMapBridge{T,S2,S1,F,G},
) where {T,S2,S1,F,G}
    mapped_func = MOI.get(model, attr, bridge.constraint)
    func = inverse_map_function(typeof(bridge), mapped_func)
    return MOIU.convert_approx(G, func)
end
function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::SetMapBridge,
)
    set = MOI.get(model, attr, bridge.constraint)
    return inverse_map_set(typeof(bridge), set)
end
function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::SetMapBridge{T,S2,S1},
    new_set::S1,
) where {T,S2,S1}
    return MOI.set(
        model,
        attr,
        bridge.constraint,
        map_set(typeof(bridge), new_set),
    )
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{<:SetMapBridge{T,S2,S1,F}},
) where {T,S2,S1,F}
    return MOI.supports(model, attr, MOI.ConstraintIndex{F,S2})
end
function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintPrimalStart},
    bridge::SetMapBridge,
)
    value = MOI.get(model, attr, bridge.constraint)
    return inverse_map_function(typeof(bridge), value)
end
function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::SetMapBridge,
    value,
)
    mapped_value = map_function(typeof(bridge), value)
    return MOI.set(model, attr, bridge.constraint, mapped_value)
end
function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintDual,MOI.ConstraintDualStart},
    bridge::SetMapBridge,
)
    value = MOI.get(model, attr, bridge.constraint)
    return adjoint_map_function(typeof(bridge), value)
end
function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::SetMapBridge,
    value,
)
    mapped_value = inverse_adjoint_map_function(typeof(bridge), value)
    return MOI.set(model, attr, bridge.constraint, mapped_value)
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::SetMapBridge,
    change::MOI.VectorConstantChange,
)
    # By linearity of the map, we can just change the constant
    constant = map_function(typeof(bridge), change.new_constant)
    return MOI.modify(
        model,
        bridge.constraint,
        MOI.VectorConstantChange(constant),
    )
end

include("flip_sign.jl")
const GreaterToLess{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{GreaterToLessBridge{T},OT}
const LessToGreater{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{LessToGreaterBridge{T},OT}
const NonnegToNonpos{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{NonnegToNonposBridge{T},OT}
const NonposToNonneg{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{NonposToNonnegBridge{T},OT}
include("rsoc.jl")
const RSOC{T,OT<:MOI.ModelLike} = SingleBridgeOptimizer{RSOCtoSOCBridge{T},OT}
const SOCR{T,OT<:MOI.ModelLike} = SingleBridgeOptimizer{SOCtoRSOCBridge{T},OT}
include("ltgt_to_interval.jl")
const GreaterToInterval{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{GreaterToIntervalBridge{T},OT}
const LessToInterval{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{LessToIntervalBridge{T},OT}
