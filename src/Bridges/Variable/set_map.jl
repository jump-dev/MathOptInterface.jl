"""
    abstract type SetMapBridge{T,S1,S2} <: AbstractBridge end

Consider two type of sets `S1`, `S2` and a linear mapping `A` that
the image of a set of type `S1` under `A` is a set of type `S2`.
A `SetMapBridge{T,S1,S2}` is a bridge that substitutes constrained variables
in `S2` into the image through `A` of constrained variables in `S1`.

The linear map `A` is described by
[`MathOptInterface.Bridges.map_set`](@ref),
[`MathOptInterface.Bridges.map_function`](@ref).
Implementing a method for these two functions is sufficient to bridge
constrained variables. In order for the getters and setters of dual solutions,
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
abstract type SetMapBridge{T,S1,S2} <: AbstractBridge end

function _add_constrained_var(model, set::MOI.AbstractScalarSet)
    return MOI.add_constrained_variable(model, set)
end

function _add_constrained_var(model, set::MOI.AbstractVectorSet)
    return MOI.add_constrained_variables(model, set)
end

function bridge_constrained_variable(
    BT::Type{<:SetMapBridge{T,S1,S2}},
    model::MOI.ModelLike,
    set::S2,
) where {T,S1,S2}
    variables, constraint =
        _add_constrained_var(model, MOIB.inverse_map_set(BT, set))
    return BT(variables, constraint)
end

function supports_constrained_variable(
    ::Type{<:SetMapBridge{T,S1,S2}},
    ::Type{S2},
) where {T,S1,S2<:MOI.AbstractSet}
    return true
end

function MOIB.added_constrained_variable_types(
    ::Type{<:SetMapBridge{T,S1}},
) where {T,S1}
    return Tuple{Type}[(S1,)]
end

function MOIB.added_constraint_types(::Type{<:SetMapBridge})
    return Tuple{Type,Type}[]
end

# Attributes, Bridge acting as a model
function MOI.get(bridge::SetMapBridge, ::MOI.NumberOfVariables)::Int64
    return length(bridge.variables)
end

function MOI.get(bridge::SetMapBridge, ::MOI.ListOfVariableIndices)
    return copy(bridge.variables)
end

function MOI.get(
    ::SetMapBridge{T,S1},
    ::MOI.NumberOfConstraints{MOI.SingleVariable,S1},
)::Int64 where {T,S1<:MOI.AbstractScalarSet}
    return 1
end

function MOI.get(
    ::SetMapBridge{T,S1},
    ::MOI.NumberOfConstraints{MOI.VectorOfVariables,S1},
)::Int64 where {T,S1<:MOI.AbstractVectorSet}
    return 1
end

function MOI.get(
    bridge::SetMapBridge{T,S1},
    ::MOI.ListOfConstraintIndices{MOI.SingleVariable,S1},
) where {T,S1<:MOI.AbstractScalarSet}
    return [bridge.constraint]
end

function MOI.get(
    bridge::SetMapBridge{T,S1},
    ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables,S1},
) where {T,S1<:MOI.AbstractVectorSet}
    return [bridge.constraint]
end

# References
function MOI.delete(
    model::MOI.ModelLike,
    bridge::SetMapBridge{T,S1,S2},
) where {T,S1,S2<:MOI.AbstractScalarSet}
    MOI.delete(model, bridge.variable)
    return
end
function MOI.delete(
    model::MOI.ModelLike,
    bridge::SetMapBridge{T,S1,S2},
) where {T,S1,S2<:MOI.AbstractVectorSet}
    MOI.delete(model, bridge.variables)
    return
end

# Attributes, Bridge acting as a constraint

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::SetMapBridge,
)
    set = MOI.get(model, attr, bridge.constraint)
    return MOIB.map_set(typeof(bridge), set)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::SetMapBridge{T,S1},
    set::S1,
) where {T,S1}
    mapped = MOIB.inverse_map_set(typeof(bridge), set)
    MOI.set(model, attr, bridge.constraint, mapped)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::SetMapBridge,
)
    value = MOI.get(model, attr, bridge.constraint)
    return MOIB.map_function(typeof(bridge), value)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    bridge::SetMapBridge,
)
    value = MOI.get(model, attr, bridge.constraint)
    return MOIB.inverse_adjoint_map_function(typeof(bridge), value)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.VariablePrimal,MOI.VariablePrimalStart},
    bridge::SetMapBridge,
    i::MOIB.IndexInVector,
)
    value = MOI.get(model, attr, bridge.variables)
    return MOIB.map_function(typeof(bridge), value, i)
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimalStart,
    ::Type{<:SetMapBridge},
)
    return MOI.supports(model, attr, MOI.VariableIndex)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimalStart,
    bridge::SetMapBridge,
    value,
    i::MOIB.IndexInVector,
)
    bridged_value = MOIB.inverse_map_function(typeof(bridge), value)
    MOI.set(model, attr, bridge.variables[i.value], bridged_value)
    return
end

function MOIB.bridged_function(
    bridge::SetMapBridge{T},
    i::MOIB.IndexInVector,
) where {T}
    func = MOIB.map_function(
        typeof(bridge),
        MOI.VectorOfVariables(bridge.variables),
        i,
    )
    return convert(MOI.ScalarAffineFunction{T}, func)
end

function unbridged_map(bridge::SetMapBridge{T}, vi::MOI.VariableIndex) where {T}
    F = MOI.ScalarAffineFunction{T}
    func = MOI.SingleVariable(vi)
    mapped = MOIB.inverse_map_function(typeof(bridge), func)
    return Pair{MOI.VariableIndex,F}[bridge.variable=>mapped]
end

function unbridged_map(
    bridge::SetMapBridge{T},
    vis::Vector{MOI.VariableIndex},
) where {T}
    F = MOI.ScalarAffineFunction{T}
    func = MOI.VectorOfVariables(vis)
    funcs = MOIB.inverse_map_function(typeof(bridge), func)
    scalars = MOIU.eachscalar(funcs)
    return Pair{MOI.VariableIndex,F}[
        bridge.variables[i] => scalars[i] for i in eachindex(vis)
    ]
end

include("flip_sign.jl")
const NonposToNonneg{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{NonposToNonnegBridge{T},OT}

include("soc_rsoc.jl")
const SOCtoRSOC{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SOCtoRSOCBridge{T},OT}
const RSOCtoSOC{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{RSOCtoSOCBridge{T},OT}
