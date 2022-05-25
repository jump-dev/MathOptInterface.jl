# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    abstract type AbstractBridge <: MOI.Bridges.AbstractBridge end

Subtype of [`MathOptInterface.Bridges.AbstractBridge`](@ref) for objective
bridges.
"""
abstract type AbstractBridge <: MOI.Bridges.AbstractBridge end

"""
    supports_objective_function(
        BT::Type{<:MOI.Bridges.Objective.AbstractBridge},
        F::Type{<:MOI.AbstractScalarFunction},
    )::Bool

Return a `Bool` indicating whether the bridges of type `BT` support bridging
objective functions of type `F`.

## Implementation notes

 * This method depends only on the type of the inputs, not the runtime values.
 * There is a default fallback, so you need only implement this method For
   objective functions that the bridge implements.
"""
function supports_objective_function(
    ::Type{<:AbstractBridge},
    ::Type{<:MOI.AbstractScalarFunction},
)
    return false
end

"""
    concrete_bridge_type(
        BT::Type{<:MOI.Bridges.Objective.AbstractBridge},
        F::Type{<:MOI.AbstractScalarFunction},
    )::Type

Return the concrete type of the bridge supporting objective functions of type
`F`.

This function can only be called if `MOI.supports_objective_function(BT, F)` is
`true`.
"""
function concrete_bridge_type(
    ::Type{BT},
    ::Type{<:MOI.AbstractScalarFunction},
) where {BT}
    return BT
end

function concrete_bridge_type(
    b::MOI.Bridges.AbstractBridgeOptimizer,
    F::Type{<:MOI.AbstractScalarFunction},
)
    return concrete_bridge_type(MOI.Bridges.bridge_type(b, F), F)
end

"""
    bridge_objective(
        BT::Type{<:MOI.Bridges.Objective.AbstractBridge},
        model::MOI.ModelLike,
        func::MOI.AbstractScalarFunction,
    )::BT

Bridge the objective function `func` using bridge `BT` to `model` and returns
a bridge object of type `BT`.

## Implementation notes

 * The bridge type `BT` must be a concrete type, that is, all the type
   parameters of the bridge must be set.
"""
function bridge_objective(
    ::Type{<:AbstractBridge},
    ::MOI.ModelLike,
    func::MOI.AbstractScalarFunction,
)
    return throw(
        MOI.UnsupportedAttribute(MOI.ObjectiveFunction{typeof(func)}()),
    )
end

function MOI.set(
    ::MOI.ModelLike,
    ::MOI.ObjectiveSense,
    bridge::AbstractBridge,
    ::MOI.OptimizationSense,
)
    return throw(
        ArgumentError(
            "Objective bridge of type `$(typeof(bridge))` does not support " *
            "modifying the objective sense. As a workaround, set the sense " *
            "to `MOI.FEASIBILITY_SENSE` to clear the objective function and " *
            "bridges.",
        ),
    )
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ObjectiveFunction,
    bridge::AbstractBridge,
)
    return throw(
        ArgumentError(
            "ObjectiveFunction bridge of type `$(typeof(bridge))` does not" *
            " support getting the objective function.",
        ),
    )
end

function MOI.delete(::MOI.ModelLike, bridge::AbstractBridge)
    return throw(
        ArgumentError(
            "`MOI.delete` not implemented for `ObjectiveFunction` bridges of " *
            "type `$(typeof(bridge))`",
        ),
    )
end
