export @bridge

"""
    AbstractBridge

A bridge represents a bridged constraint in an `AbstractBridgeOptimizer`. It contains the indices of the constraints that it has created in the model.
These can be obtained using `MOI.NumberOfConstraints` and `MOI.ListOfConstraintIndices` and using the bridge in place of a `ModelLike`.
Attributes of the bridged model such as `MOI.ConstraintDual` and `MOI.ConstraintPrimal`, can be obtained using the bridge in place of the constraint index.
These calls are used by the `AbstractBridgeOptimizer` to communicate with the bridge so they should be implemented by the bridge.
"""
abstract type AbstractBridge end


function MOI.get(::MOI.ModelLike, attr::MOI.AbstractConstraintAttribute,
                 bridge::AbstractBridge)
    throw(ArgumentError("Constraint bridge of type `$(typeof(bridge))` does not support accessing the attribute `$attr`."))
end

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

"""
    MOI.supports_constraint(BT::Type{<:AbstractBridge}, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})::Bool

Return a `Bool` indicating whether the bridges of type `BT` support bridging `F`-in-`S` constraints.
"""
MOI.supports_constraint(::Type{<:AbstractBridge}, ::Type{<:MOI.AbstractFunction}, ::Type{<:MOI.AbstractSet}) = false

"""
    added_constraint_types(BT::Type{<:AbstractBridge}, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})::Bool

Return a list of the types of constraints that bridges of type `BT` add for
bridging an `F`-in-`S` constraints.

    added_constraint_types(BT::Type{<:AbstractBridge})::Bool

Return a list of the types of constraints that bridges of concrete type `BT` add
for `F`-in-`S` constraints.
"""
function added_constraint_types(BT::Type{<:AbstractBridge},
                              F::Type{<:MOI.AbstractFunction},
                              S::Type{<:MOI.AbstractSet})
    added_constraint_types(concrete_bridge_type(BT, F, S))
end


"""
    concrete_bridge_type(BT::Type{<:AbstractBridge},
                         F::Type{<:MOI.AbstractFunction},
                         S::Type{<:MOI.AbstractSet})::DataType

Return the concrete type of the bridge supporting `F`-in-`S` constraints. This
function can only be called if `MOI.supports_constraint(BT, F, S)` is `true`.

## Examples

The following returns `SplitIntervalBridge{Float64, MOI.SingleVariable}`:
```julia
concrete_bridge_type(SplitIntervalBridge{Float64}, MOI.SingleVariable,
                                                   MOI.Interval{Float64})
```
"""
function concrete_bridge_type(bridge_type::DataType,
                              ::Type{<:MOI.AbstractFunction},
                              ::Type{<:MOI.AbstractSet})
    return bridge_type
end
