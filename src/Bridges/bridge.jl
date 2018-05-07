export @bridge

"""
    AbstractBridge

A bridge represents a bridged constraint in an `AbstractBridgeOptimizer`. It contains the indices of the constraints that it has created in the model.
These can be obtained using `MOI.NumberOfConstraints` and `MOI.ListOfConstraintIndices` and using the bridge in place of a `ModelLike`.
Attributes of the bridged model such as `MOI.ConstraintDual` and `MOI.ConstraintPrimal`, can be obtained using the bridge in place of the constraint index.
These calls are used by the `AbstractBridgeOptimizer` to communicate with the bridge so they should be implemented by the bridge.
"""
abstract type AbstractBridge end

"""
    MOI.get(b::AbstractBridge, ::MOI.NumberOfVariables)

The number of variables created by the bridge `b` in the model.
"""
MOI.get(b::AbstractBridge, ::MOI.NumberOfVariables) = 0
"""
    MOI.get(b::AbstractBridge, ::MOI.NumberOfConstraints{F, S}) where {F, S}

The number of constraints of the type `F`-in-`S` created by the bridge `b` in the model.
"""
MOI.get(b::AbstractBridge, ::MOI.NumberOfConstraints) = 0
"""
    MOI.get(b::AbstractBridge, ::MOI.NumberOfConstraints{F, S}) where {F, S}

A `Vector{ConstraintIndex{F,S}}` with indices of all constraints of
type `F`-in`S` created by the bride `b` in the model (i.e., of length equal to the value of `NumberOfConstraints{F,S}()`).
"""
MOI.get(b::AbstractBridge, ::MOI.ListOfConstraintIndices{F, S}) where {F, S} = CI{F, S}[]

# TODO supportsconstraint and supports constraint should maybe be on the type of the model in MOI too
#      that would clarify the fact that it does not depend on the current state of the model
"""
    MOI.supportsconstraint(BT::Type{<:AbstractBridge}, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})::Bool

Return a `Bool` indicating whether the bridges of type `BT` support bridging `F`-in-`S` constraints.
"""
MOI.supportsconstraint(::Type{<:AbstractBridge}, ::Type{<:MOI.AbstractFunction}, ::Type{<:MOI.AbstractSet}) = false

"""
    MOI.candelete(model::MOI.ModelLike, b::AbstractBridge)

Return a `Bool` indicating whether the bridge `b` can be removed from the model `model`.
"""
MOI.candelete(model::MOI.ModelLike, c::AbstractBridge) = true
