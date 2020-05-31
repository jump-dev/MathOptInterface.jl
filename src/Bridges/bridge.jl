"""
    AbstractBridge

Represents a bridged constraint or variable in a
[`MathOptInterface.Bridges.AbstractBridgeOptimizer`](@ref). It contains the
indices of the variables and constraints that it has created in the model. These
can be obtained using [`MathOptInterface.NumberOfVariables`](@ref),
[`MathOptInterface.ListOfVariableIndices`](@ref),
[`MathOptInterface.NumberOfConstraints`](@ref) and
[`MathOptInterface.ListOfConstraintIndices`](@ref) using
[`MathOptInterface.get`](@ref) with the bridge in place of the
[`MathOptInterface.ModelLike`](@ref). Attributes of the bridged model such as
[`MathOptInterface.ConstraintDual`](@ref) and
[`MathOptInterface.ConstraintPrimal`](@ref), can be obtained using
[`MathOptInterface.get`](@ref) with the bridge in place of the constraint index.
These calls are used by the
[`MathOptInterface.Bridges.AbstractBridgeOptimizer`](@ref) to communicate with
the bridge so they should be implemented by the bridge.
"""
abstract type AbstractBridge end

"""
    MOI.get(b::AbstractBridge, ::MOI.NumberOfConstraints{F, S}) where {F, S}

The number of constraints of the type `F`-in-`S` created by the bridge `b` in the model.
"""
MOI.get(::AbstractBridge, ::MOI.NumberOfConstraints) = 0

"""
    MOI.get(b::AbstractBridge, ::MOI.ListOfConstraintIndices{F, S}) where {F, S}

A `Vector{ConstraintIndex{F,S}}` with indices of all constraints of
type `F`-in`S` created by the bride `b` in the model (i.e., of length equal to
the value of `NumberOfConstraints{F,S}()`).
"""
function MOI.get(::AbstractBridge,
                 ::MOI.ListOfConstraintIndices{F, S}) where {F, S}
    return MOI.ConstraintIndex{F, S}[]
end

"""
    MOI.supports(model::MOI.ModelLike, attr::MOI.AbstractConstraintAttribute,
                 BT::Type{<:AbstractBridge})

Return a `Bool` indicating whether `BT` supports setting `attr` to `model`.
"""
function MOI.supports(::MOI.ModelLike, ::MOI.AbstractConstraintAttribute,
                      ::Type{<:AbstractBridge})
    return false
end

"""
    function MOI.get(model::MOI.ModelLike, attr::MOI.AbstractConstraintAttribute,
                     bridge::AbstractBridge)

Return the value of the attribute `attr` of the model `model` for the
constraint bridged by `bridge`.
"""
function MOI.get(::MOI.ModelLike, attr::MOI.AbstractConstraintAttribute,
                 bridge::AbstractBridge)
    throw(ArgumentError("Bridge of type `$(typeof(bridge))` does not support accessing the attribute `$attr`."))
end

"""
    function MOI.set(model::MOI.ModelLike, attr::MOI.AbstractConstraintAttribute,
                     bridge::AbstractBridge, value)

Set the value of the attribute `attr` of the model `model` for the
constraint bridged by `bridge`.
"""
function MOI.set(model::MOI.ModelLike, attr::MOI.AbstractConstraintAttribute,
                 bridge::AbstractBridge, value)
    if MOI.is_copyable(attr) && !MOI.supports(model, attr, typeof(bridge))
        throw(MOI.UnsupportedAttribute(attr))
    else
        throw(MOI.SetAttributeNotAllowed(attr))
    end
end

"""
    added_constrained_variable_types(BT::Type{<:AbstractBridge})::Vector{Tuple{DataType}}

Return a list of the types of constrained variables that bridges of concrete
type `BT` add. This is used by the [`LazyBridgeOptimizer`](@ref).
"""
function added_constrained_variable_types end

"""
    added_constraint_types(BT::Type{<:AbstractBridge})::Vector{Tuple{DataType, DataType}}

Return a list of the types of constraints that bridges of concrete type `BT`
add. This is used by the [`LazyBridgeOptimizer`](@ref).
"""
function added_constraint_types end

"""
    set_objective_function_type(BT::Type{<:AbstractBridge})::Type{<:MOI.AbstractScalarFunction}

Return the type of objective function that bridges of concrete type `BT`
set. This is used by the [`LazyBridgeOptimizer`](@ref).
"""
function set_objective_function_type end

"""
    watched_variables(::AbstractBridge)::AbstractVector{MOI.VariableIndex}

Return a list of variable indices. For any `S<:MOI.AbstractScalarSet`, whenever
a `MOI.SingleVariable`-in-`S` constraint is added to the model for one variable
of this list, the bridge is notified with [`notify_constraint`]`(@ref) just before
adding the constraint.
If this method is not implemented, it fallbacks to returning
`MOI.Utilities.EmptyVector{MOI.VariableIndex}()`.
"""
watched_variables(::AbstractBridge) = MOIU.EmptyVector{MOI.VariableIndex}()

"""
    notify_constraint(bridge::AbstractBridge, model::Model, func::SingleVariable, set::MOI.AbstractScalarSet)::AbstractVector{MOI.VariableIndex}

Notifies `bridge` that the `func`-in-`set` constraint will be might be added to
`model`. The bridge can throw an error if that should not be allowed or add
variables or add/modify constraints due to this change.
See [`watched_variables`](@ref) for being notified when variables are modified.
"""
function notify_constraint end
