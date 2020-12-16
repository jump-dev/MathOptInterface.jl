"""
    AbstractBridge

Subtype of [`MathOptInterface.Bridges.AbstractBridge`](@ref) for objective
bridges.
"""
abstract type AbstractBridge <: MOIB.AbstractBridge end

"""
    bridge_objective(BT::Type{<:MOI.Bridges.Objective.AbstractBridge},
                     model::MOI.ModelLike,
                     func::MOI.AbstractScalarFunction)

Bridge the objective function `func` using bridge `BT` to `model` and returns
a bridge object of type `BT`. The bridge type `BT` should be a concrete type,
that is, all the type parameters of the bridge should be set. Use
[`concrete_bridge_type`](@ref) to obtain a concrete type for a given function
type.
"""
function bridge_objective(
    ::Type{<:AbstractBridge},
    ::MOI.ModelLike,
    func::MOI.AbstractScalarFunction,
)
    return throw(MOI.UnsupportedAttribute(MOI.ObjectiveFunction{typeof(func)}()))
end

"""
    function MOI.set(model::MOI.ModelLike, ::MOI.ObjectiveSense,
                     bridge::MOI.Bridges.Objective.AbstractBridge,
                     sense::MOI.ObjectiveSense)

Informs `bridge` that the objective sense is changed to `sense`. If changing
the objective sense is not supported, the bridge should not implement this
method.
"""
function MOI.set(
    ::MOI.ModelLike,
    ::MOI.ObjectiveSense,
    bridge::AbstractBridge,
    ::MOI.OptimizationSense,
)
    return throw(ArgumentError(
        "Objective bridge of type `$(typeof(bridge))` does not support" *
        " modifying the objective sense. As a workaround, set the sense to" *
        " `MOI.FEASIBILITY_SENSE` to clear the objective function and" *
        " bridges.",
    ))
end

"""
    function MOI.get(model::MOI.ModelLike, attr::MOI.ObjectiveFunction,
                     bridge::MOI.Bridges.Objective.AbstractBridge)

Return the objective function object bridged by `bridge` for the model
`model`.
"""
function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ObjectiveFunction,
    bridge::AbstractBridge,
)
    return throw(ArgumentError(
        "ObjectiveFunction bridge of type `$(typeof(bridge))` does not" *
        " support getting the objective function.",
    ))
end

"""
    function MOI.delete(model::MOI.ModelLike, bridge::MOI.Bridges.Objective.AbstractBridge)

Delete any variable or constraint added by `bridge`.
"""
function MOI.delete(::MOI.ModelLike, bridge::AbstractBridge)
    return throw(ArgumentError("`MOI.delete` not implemented for `ObjectiveFunction` bridges of type `$(typeof(bridge))`"))
end

"""
    supports_objective_function(
        BT::Type{<:MOI.Bridges.Objective.AbstractBridge},
        F::Type{<:MOI.AbstractScalarFunction})::Bool

Return a `Bool` indicating whether the bridges of type `BT` support bridging
objective functions of type `F`.
"""
function supports_objective_function(
    ::Type{<:AbstractBridge},
    ::Type{<:MOI.AbstractScalarFunction},
)
    return false
end

"""
    added_constrained_variable_types(BT::Type{<:MOI.Bridges.Objective.AbstractBridge},
                                     F::Type{<:MOI.AbstractScalarFunction})

Return a list of the types of constrained variables that bridges of type `BT`
add for bridging objective functions of type `F`. This fallbacks to
`added_constrained_variable_types(concrete_bridge_type(BT, F))`
so bridges should not implement this method.
```
"""
function MOIB.added_constrained_variable_types(
    BT::Type{<:AbstractBridge},
    F::Type{<:MOI.AbstractScalarFunction},
)
    return MOIB.added_constrained_variable_types(concrete_bridge_type(BT, F))
end

"""
    added_constraint_types(BT::Type{<:MOI.Bridges.Objective.AbstractBridge},
                           F::Type{<:MOI.AbstractScalarFunction})

Return a list of the types of constraints that bridges of type `BT` add
for bridging objective functions of type `F`. This fallbacks to
`added_constraint_types(concrete_bridge_type(BT, S))`
so bridges should not implement this method.
"""
function MOIB.added_constraint_types(
    BT::Type{<:AbstractBridge},
    F::Type{<:MOI.AbstractScalarFunction},
)
    return MOIB.added_constraint_types(concrete_bridge_type(BT, F))
end

"""
    set_objective_function_type(BT::Type{<:MOI.Bridges.Objective.AbstractBridge},
                                F::Type{<:MOI.AbstractScalarFunction})

Return the type of objective function that bridges of type `BT` set
for bridging objective functions of type `F`. This fallbacks to
`set_objective_function_type(concrete_bridge_type(BT, F))`
so bridges should not implement this method.
"""
function MOIB.set_objective_function_type(
    BT::Type{<:AbstractBridge},
    F::Type{<:MOI.AbstractScalarFunction},
)
    return MOIB.set_objective_function_type(concrete_bridge_type(BT, F))
end

"""
    concrete_bridge_type(BT::Type{<:MOI.Bridges.Objective.AbstractBridge},
                         F::Type{<:MOI.AbstractScalarFunction})::DataType

Return the concrete type of the bridge supporting objective functions of type
`F`. This function can only be called if `MOI.supports_objective_function(BT, F)`
is `true`.
"""
function concrete_bridge_type(
    bridge_type::DataType,
    ::Type{<:MOI.AbstractScalarFunction},
)
    return bridge_type
end

function concrete_bridge_type(
    b::MOIB.AbstractBridgeOptimizer,
    F::Type{<:MOI.AbstractScalarFunction},
)
    return concrete_bridge_type(MOIB.bridge_type(b, F), F)
end
