"""
    abstract type SetMapBridge{T,S2,S1,F,G} <: AbstractBridge end

Consider two type of sets `S1`, `S2` and a linear mapping `A` that
the image of a set of type `S1` under `A` is a set of type `S2`.
A `SetMapBridge{T,S2,S1,F,G}` is a bridge that maps `G`-in-`S2` constraints
into `F`-in-`S1` by mapping the function through `A`.

The linear map `A` is described by
[`MathOptInterface.Bridges.map_set`](@ref),
[`MathOptInterface.Bridges.map_function`](@ref).
Implementing a method for these two functions is sufficient to bridge
constraints. In order for the getters and setters of dual solutions,
starting values, etc...  to work as well a method for the following
functions should be implemented as well:
[`MathOptInterface.Bridges.inverse_map_set`](@ref),
[`MathOptInterface.Bridges.inverse_map_function`](@ref),
[`MathOptInterface.Bridges.adjoint_map_function`](@ref) and
[`MathOptInterface.Bridges.inverse_adjoint_map_function`](@ref).
See the docstrings of the function
to see which feature would be missing it it was not implemented for a given
bridge.
"""
abstract type SetMapBridge{T,S2,S1,F,G} <: AbstractBridge end

function bridge_constraint(
    BT::Type{<:SetMapBridge{T,S2,S1,F,G}},
    model::MOI.ModelLike,
    func::G,
    set::S1,
) where {T,S2,S1,F,G}
    mapped_func = MOIB.map_function(BT, func)
    constraint = MOI.add_constraint(model, mapped_func, MOIB.map_set(BT, set))
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
    return Tuple{Type}[]
end

function MOIB.added_constraint_types(
    ::Type{<:SetMapBridge{T,S2,S1,F}},
) where {T,S2,S1,F}
    return Tuple{Type,Type}[(F, S2)]
end

# Attributes, Bridge acting as a model
function MOI.get(
    ::SetMapBridge{T,S2,S1,F},
    ::MOI.NumberOfConstraints{F,S2},
) where {T,S2,S1,F}
    return Int64(1)
end

function MOI.get(
    bridge::SetMapBridge{T,S2,S1,F},
    ::MOI.ListOfConstraintIndices{F,S2},
) where {T,S2,S1,F}
    return [bridge.constraint]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::SetMapBridge)
    MOI.delete(model, bridge.constraint)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::SetMapBridge{T,S2,S1,F,G},
) where {T,S2,S1,F,G}
    mapped_func = MOI.get(model, attr, bridge.constraint)
    func = MOIB.inverse_map_function(typeof(bridge), mapped_func)
    return MOIU.convert_approx(G, func)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::SetMapBridge{T,S2,S1,F,G},
    func::G,
) where {T,S2,S1,F,G}
    MOI.set(
        model,
        attr,
        bridge.constraint,
        MOIB.map_function(typeof(bridge), func),
    )
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::SetMapBridge,
)
    set = MOI.get(model, attr, bridge.constraint)
    return MOIB.inverse_map_set(typeof(bridge), set)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::SetMapBridge{T,S2,S1},
    new_set::S1,
) where {T,S2,S1}
    MOI.set(
        model,
        attr,
        bridge.constraint,
        MOIB.map_set(typeof(bridge), new_set),
    )
    return
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
    return MOIB.inverse_map_function(typeof(bridge), value)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::SetMapBridge,
    value,
)
    mapped_value = MOIB.map_function(typeof(bridge), value)
    MOI.set(model, attr, bridge.constraint, mapped_value)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintDual,MOI.ConstraintDualStart},
    bridge::SetMapBridge,
)
    value = MOI.get(model, attr, bridge.constraint)
    return MOIB.adjoint_map_function(typeof(bridge), value)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::SetMapBridge,
    value,
)
    mapped_value = MOIB.inverse_adjoint_map_function(typeof(bridge), value)
    MOI.set(model, attr, bridge.constraint, mapped_value)
    return
end

# By linearity of the map, we can just change the constant/coefficient
function _map_change(::Type{BT}, change::MOI.ScalarConstantChange) where {BT}
    constant = MOIB.map_function(BT, change.new_constant)
    return MOI.ScalarConstantChange(constant)
end
function _map_change(::Type{BT}, change::MOI.VectorConstantChange) where {BT}
    constant = MOIB.map_function(BT, change.new_constant)
    return MOI.VectorConstantChange(constant)
end
function _map_change(::Type{BT}, change::MOI.ScalarCoefficientChange) where {BT}
    coefficient = MOIB.map_function(BT, change.new_coefficient)
    return MOI.ScalarCoefficientChange(change.variable, coefficient)
end
function _map_change(::Type{BT}, change::MOI.MultirowChange) where {BT}
    # It is important here that `change.new_coefficients` contains
    # the complete new sparse column associated to the variable.
    # Calling modify twice with part of the column won't work since
    # the linear map might reset all the column each time.
    coefficients = MOIB.map_function(BT, change.new_coefficients)
    return MOI.MultirowChange(change.variable, coefficients)
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::SetMapBridge,
    change::MOI.AbstractFunctionModification,
)
    MOI.modify(model, bridge.constraint, _map_change(typeof(bridge), change))
    return
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

include("soc_rsoc.jl")
const RSOC{T,OT<:MOI.ModelLike} = SingleBridgeOptimizer{RSOCtoSOCBridge{T},OT}
const SOCR{T,OT<:MOI.ModelLike} = SingleBridgeOptimizer{SOCtoRSOCBridge{T},OT}

include("ltgt_to_interval.jl")
const GreaterToInterval{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{GreaterToIntervalBridge{T},OT}
const LessToInterval{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{LessToIntervalBridge{T},OT}

include("soc_to_psd.jl")
const SOCtoPSD{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SOCtoPSDBridge{T},OT}
const RSOCtoPSD{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{RSOCtoPSDBridge{T},OT}
