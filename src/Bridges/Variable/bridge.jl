"""
    AbstractBridge

Subtype of [`MathOptInterface.Bridges.AbstractBridge`](@ref) for variable
bridges.
"""
abstract type AbstractBridge <: MOIB.AbstractBridge end

"""
    IndexInVector

Index of variable in vector of variables.
"""
struct IndexInVector
    value::Int
end

"""
    bridge_constrained_variable(BT::Type{<:AbstractBridge}, model::MOI.ModelLike,
                                set::MOI.AbstractSet)

Bridge the constrained variable in `set` using bridge `BT` to `model` and returns
a bridge object of type `BT`. The bridge type `BT` should be a concrete type,
that is, all the type parameters of the bridge should be set. Use
[`concrete_bridge_type`](@ref) to obtain a concrete type for given set types.
"""
function bridge_constrained_variable end

"""
    function MOI.get(model::MOI.ModelLike, attr::MOI.AbstractVariableAttribute,
                     bridge::AbstractBridge)

Return the value of the attribute `attr` of the model `model` for the
variable bridged by `bridge`.
"""
function MOI.get(::MOI.ModelLike, attr::MOI.AbstractVariableAttribute,
                 bridge::AbstractBridge)
    throw(ArgumentError("Variable bridge of type `$(typeof(bridge))` does not support accessing the attribute `$attr`."))
end

"""
    function MOI.get(model::MOI.ModelLike, attr::MOI.AbstractVariableAttribute,
                     bridge::AbstractBridge, i::IndexInVector)

Return the value of the attribute `attr` of the model `model` for the
variable at index `i` in the vector of variables bridged by `bridge`.
"""
function MOI.get(::MOI.ModelLike, attr::MOI.AbstractVariableAttribute,
                 bridge::AbstractBridge, ::IndexInVector)
    throw(ArgumentError("Variable bridge of type `$(typeof(bridge))` does not support accessing the attribute `$attr`."))
end

"""
    supports_constrained_variable(::Type{<:AbstractBridge},
                                   ::Type{<:MOI.AbstractSet})::Bool

Return a `Bool` indicating whether the bridges of type `BT` support bridging
constrained variables in `S`.
"""
function supports_constrained_variable(::Type{<:AbstractBridge},
                                        ::Type{<:MOI.AbstractSet})
    return false
end

"""
    added_constrained_variable_types(BT::Type{<:MOI.Bridges.Variable.AbstractBridge},
                                     S::Type{<:MOI.AbstractSet})

Return a list of the types of constrained variables that bridges of type `BT`
add for bridging constrained variabled in `S`. This falls back to
`added_constrained_variable_types(concrete_bridge_type(BT, S))`
so bridges should not implement this.

## Examples

As a variable in [`MathOptInterface.GreaterThan`](@ref) is bridged into
variables in [`MathOptInterface.Nonnegatives`](@ref) by the
[`VectorizeBridge`](@ref):
```jldoctest
BT = MOI.Bridges.Variable.VectorizeBridge{Float64}
S = MOI.GreaterThan{Float64}
MOI.Bridges.added_constrained_variable_types(BT, S)

# output

[(MOI.Nonnegatives,)]
```
"""
function MOIB.added_constrained_variable_types(
    BT::Type{<:AbstractBridge}, S::Type{<:MOI.AbstractSet})
    MOIB.added_constrained_variable_types(concrete_bridge_type(BT, S))
end

"""
    added_constraint_types(BT::Type{<:MOI.Bridges.Variable.AbstractBridge},
                           S::Type{<:MOI.AbstractSet})

Return a list of the types of constraints that bridges of type `BT` add for
for bridging constrained variabled in `S`. This falls back to
`added_constraint_types(concrete_bridge_type(BT, S))`
so bridges should not implement this method.

## Examples

In addition to creating variables in
[`MathOptInterface.PositiveSemidefiniteConeTriangle`](@ref), the
[`RSOCtoPSDBridge`](@ref) also creates
[`MathOptInterface.SingleVariable`](@ref)-in-[`MathOptInterface.EqualTo`](@ref) and
[`MathOptInterface.ScalarAffineFunction`](@ref)-in-[`MathOptInterface.EqualTo`](@ref)
constraints:
```jldoctest
BT = MOI.Bridges.Variable.RSOCtoPSDBridge{Float64}
S = MOI.RotatedSecondOrderCone{
MOI.Bridges.added_constraint_types(BT, S)

# output

[(MOI.SingleVariable, MOI.EqualTo{Float64}), (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})]
```
"""
function MOIB.added_constraint_types(
    BT::Type{<:AbstractBridge}, S::Type{<:MOI.AbstractSet})
    MOIB.added_constraint_types(concrete_bridge_type(BT, S))
end

"""
    concrete_bridge_type(BT::Type{<:AbstractBridge},
                         S::Type{<:MOI.AbstractSet})::DataType

Return the concrete type of the bridge supporting variables in `S` constraints.
This function can only be called if `MOI.supports_constrained_variable(BT, S)`
is `true`.

## Examples

As a variable in [`MathOptInterface.GreaterThan`](@ref) is bridged into
variables in [`MathOptInterface.Nonnegatives`](@ref) by the
[`VectorizeBridge`](@ref),
```jldoctest
BT = MOI.Bridges.Variable.VectorizeBridge{Float64}
S = MOI.GreaterThan{Float64}
MOI.Bridges.Variable.concrete_bridge_type(BT, S)

# output

MOI.Bridges.Variable.VectorizeBridge{Float64,MOI.Nonnegatives}
```
"""
function concrete_bridge_type(bridge_type::DataType,
                              ::Type{<:MOI.AbstractSet})
    return bridge_type
end

function concrete_bridge_type(b::MOIB.AbstractBridgeOptimizer,
                              S::Type{<:MOI.AbstractSet})
    return concrete_bridge_type(MOIB.bridge_type(b, S), S)
end
