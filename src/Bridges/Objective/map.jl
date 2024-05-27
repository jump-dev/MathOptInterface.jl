# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    Map <: AbstractDict{MOI.ObjectiveFunction,AbstractBridge}

A mapping from a bridged objective function type to the bridge responsible for
bridging that type of objective function.
"""
mutable struct Map <: AbstractDict{MOI.ObjectiveFunction,AbstractBridge}
    bridges::Dict{MOI.ObjectiveFunction,AbstractBridge}
    function_type::Union{Nothing,Type{<:MOI.AbstractFunction}}
end

Map() = Map(Dict{MOI.ObjectiveFunction,AbstractBridge}(), nothing)

# Implementation of `AbstractDict` interface.

Base.isempty(map::Map) = isempty(map.bridges)

function Base.empty!(map::Map)
    empty!(map.bridges)
    map.function_type = nothing
    return map
end

function Base.haskey(map::Map, attr::MOI.ObjectiveFunction)
    return haskey(map.bridges, attr)
end

function Base.getindex(map::Map, attr::MOI.ObjectiveFunction)
    return map.bridges[attr]
end

Base.length(map::Map) = length(map.bridges)

Base.values(map::Map) = values(map.bridges)

Base.iterate(map::Map, args...) = iterate(map.bridges, args...)

# Custom interface for information needed by `AbstractBridgeOptimizer`s that is
# not part of the `AbstractDict` interface.

"""
    function_type(map::Map)

Return the function type of the [`root_bridge`](@ref) or `nothing` if `map` is
empty.
"""
function_type(map::Map) = map.function_type

"""
    root_bridge(map::Map)

Return the last bridge added.
"""
function root_bridge(map::Map)
    attr = MOI.ObjectiveFunction{function_type(map)}()
    return map[attr]
end

"""
    add_key_for_bridge(
        map::Map,
        bridge::AbstractBridge,
        ::F,
    ) where {F<:MOI.AbstractFunction}

Stores the mapping `attr => bridge` where `attr` is
`MOI.ObjectiveFunction{F}()` and set [`function_type`](@ref) to `F`.
"""
function add_key_for_bridge(
    map::Map,
    bridge::AbstractBridge,
    ::F,
) where {F<:MOI.AbstractFunction}
    attr = MOI.ObjectiveFunction{F}()
    map.function_type = F
    map.bridges[attr] = bridge
    return
end

"""
    EmptyMap <: AbstractDict{MOI.ObjectiveFunction,AbstractBridge}

Empty version of [`Map`](@ref).

It is used by
[`MOI.Bridges.Variable.SingleBridgeOptimizer`](@ref) and
[`MOI.Bridges.Constraint.SingleBridgeOptimizer`](@ref) as they do not bridge any
objective function.
"""
struct EmptyMap <: AbstractDict{MOI.ObjectiveFunction,AbstractBridge} end

Base.show(::IO, ::EmptyMap) = nothing

Base.isempty(::EmptyMap) = true

Base.empty!(::EmptyMap) = nothing

Base.length(::EmptyMap) = 0

Base.haskey(::EmptyMap, ::MOI.ObjectiveFunction) = false

Base.values(::EmptyMap) = MOI.Utilities.EmptyVector{AbstractBridge}()

Base.iterate(::EmptyMap) = nothing

function_type(::EmptyMap) = nothing
