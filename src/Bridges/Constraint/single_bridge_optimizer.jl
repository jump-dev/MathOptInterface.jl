# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    SingleBridgeOptimizer{BT<:AbstractBridge}(model::MOI.ModelLike)

Return `AbstractBridgeOptimizer` that always bridges any objective function
supported by the bridge `BT`.

This is in contrast with the [`MOI.Bridges.LazyBridgeOptimizer`](@ref),
which only bridges the objective function if it is supported by the bridge `BT`
and unsupported by `model`.

## Example

```jldoctest con_singlebridgeoptimizer
julia> struct MyNewBridge{T} <: MOI.Bridges.Constraint.AbstractBridge end

julia> bridge = MOI.Bridges.Constraint.SingleBridgeOptimizer{MyNewBridge{Float64}}(
           MOI.Utilities.Model{Float64}(),
       )
MOIB.Constraint.SingleBridgeOptimizer{MyNewBridge{Float64}, MOIU.Model{Float64}}
├ ObjectiveSense: FEASIBILITY_SENSE
├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
├ NumberOfVariables: 0
└ NumberOfConstraints: 0
```

## Implementation notes

All bridges should simplify the creation of `SingleBridgeOptimizer`s by defining
a constant that wraps the bridge in a `SingleBridgeOptimizer`.
```jldoctest con_singlebridgeoptimizer
julia> const MyNewBridgeModel{T,OT<:MOI.ModelLike} =
           MOI.Bridges.Constraint.SingleBridgeOptimizer{MyNewBridge{T},OT};
```
This enables users to create bridged models as follows:
```jldoctest con_singlebridgeoptimizer
julia> MyNewBridgeModel{Float64}(MOI.Utilities.Model{Float64}());
```
"""
mutable struct SingleBridgeOptimizer{BT<:AbstractBridge,OT<:MOI.ModelLike} <:
               MOI.Bridges.AbstractBridgeOptimizer
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

bridges(::MOI.Bridges.AbstractBridgeOptimizer) = EmptyMap()

bridges(bridge::SingleBridgeOptimizer) = bridge.map

MOI.Bridges.supports_constraint_bridges(::SingleBridgeOptimizer) = true

# If `BT` bridges `MOI.Reals` (such as `Constraint.FunctionizeBridge` bridge,
# without this method, it creates a `StackOverflow` with `is_bridged`,
# `supports_bridging_constrained_variable` and `supports_add_constrained_variables`.
MOI.Bridges.is_bridged(::SingleBridgeOptimizer, ::Type{MOI.Reals}) = false

function MOI.Bridges.is_bridged(
    b::SingleBridgeOptimizer,
    S::Type{<:MOI.AbstractSet},
)
    return MOI.Bridges.supports_bridging_constrained_variable(b, S)
end

MOI.Bridges.is_bridged(::SingleBridgeOptimizer, ::MOI.VariableIndex) = false

function MOI.Bridges.is_bridged(
    b::SingleBridgeOptimizer,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
) where {S}
    return MOI.Bridges.is_bridged(b, MOI.VariableIndex, S) &&
           haskey(Constraint.bridges(b), ci)
end

function MOI.Bridges.is_bridged(
    b::SingleBridgeOptimizer,
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables,S},
) where {S}
    return MOI.Bridges.is_bridged(b, MOI.VectorOfVariables, S) &&
           haskey(Constraint.bridges(b), ci)
end

function MOI.Bridges.supports_bridging_constrained_variable(
    b::SingleBridgeOptimizer,
    S::Type{<:MOI.AbstractSet},
)
    F = MOI.Utilities.variable_function_type(S)
    return MOI.Bridges.supports_bridging_constraint(b, F, S) &&
           MOI.supports_add_constrained_variables(b, MOI.Reals)
end

function MOI.Bridges.supports_bridging_constraint(
    @nospecialize(b::SingleBridgeOptimizer{BT}),
    @nospecialize(F::Type{<:MOI.AbstractFunction}),
    @nospecialize(S::Type{<:MOI.AbstractSet}),
) where {BT}
    return MOI.supports_constraint(BT, F, S)
end

function MOI.Bridges.is_bridged(
    b::SingleBridgeOptimizer,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    return MOI.Bridges.supports_bridging_constraint(b, F, S)
end

function MOI.Bridges.is_bridged(
    ::SingleBridgeOptimizer,
    ::Type{<:MOI.AbstractFunction},
)
    return false
end

function MOI.Bridges.bridge_type(
    ::SingleBridgeOptimizer{BT},
    ::Type{<:MOI.AbstractFunction},
    ::Type{<:MOI.AbstractSet},
) where {BT}
    return BT
end

MOI.Bridges.bridging_cost(::SingleBridgeOptimizer, args...) = 1.0

MOI.Bridges.recursive_model(b::SingleBridgeOptimizer) = b.model
