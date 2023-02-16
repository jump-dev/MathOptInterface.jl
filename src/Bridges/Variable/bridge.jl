# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    abstract type AbstractBridge <: MOI.Bridges.AbstractBridge end

Subtype of [`MOI.Bridges.AbstractBridge`](@ref) for variable bridges.

In addition to the required implementation described in
[`MOI.Bridges.AbstractBridge`](@ref), subtypes of `AbstractBridge` must
additionally implement:

 * [`supports_constrained_variable`](@ref)
 * [`concrete_bridge_type`](@ref)
 * [`bridge_constrained_variable`](@ref)
"""
abstract type AbstractBridge <: MOI.Bridges.AbstractBridge end

"""
    supports_constrained_variable(
        BT::Type{<:AbstractBridge},
        S::Type{<:MOI.AbstractSet},
    )::Bool

Return a `Bool` indicating whether the bridges of type `BT` support bridging
constrained variables in `S`. That is, it returns `true` if the bridge of type
`BT` converts constrained variables of type `S` into a form supported by the
solver.

## Implementation notes

 * This method depends only on the type of the bridge and set, not the runtime
   values.
 * There is a default fallback, so you need only implement this method for sets
   that the bridge implements.

## Example

```jldoctest; setup=(import MathOptInterface as MOI)
julia> MOI.Bridges.Variable.supports_constrained_variable(
           MOI.Bridges.Variable.NonposToNonnegBridge{Float64},
           MOI.Nonpositives,
       )
true

julia> MOI.Bridges.Variable.supports_constrained_variable(
           MOI.Bridges.Variable.NonposToNonnegBridge{Float64},
           MOI.Nonnegatives,
       )
false
```
"""
function supports_constrained_variable(
    ::Type{<:AbstractBridge},
    ::Type{<:MOI.AbstractSet},
)
    return false
end

"""
    concrete_bridge_type(
        BT::Type{<:AbstractBridge},
        S::Type{<:MOI.AbstractSet},
    )::Type

Return the concrete type of the bridge supporting variables in `S` constraints.

This function can only be called if `MOI.supports_constrained_variable(BT, S)`
is `true`.

## Examples

As a variable in [`MOI.GreaterThan`](@ref) is bridged into
variables in [`MOI.Nonnegatives`](@ref) by the
[`VectorizeBridge`](@ref):

```jldoctest; setup=:(import MathOptInterface as MOI)
julia> MOI.Bridges.Variable.concrete_bridge_type(
           MOI.Bridges.Variable.VectorizeBridge{Float64},
           MOI.GreaterThan{Float64},
       )
MathOptInterface.Bridges.Variable.VectorizeBridge{Float64, MathOptInterface.Nonnegatives}
```
"""
concrete_bridge_type(::Type{BT}, ::Type{<:MOI.AbstractSet}) where {BT} = BT

function concrete_bridge_type(
    b::MOI.Bridges.AbstractBridgeOptimizer,
    S::Type{<:MOI.AbstractSet},
)
    return concrete_bridge_type(MOI.Bridges.bridge_type(b, S), S)
end

"""
    bridge_constrained_variable(
        BT::Type{<:AbstractBridge},
        model::MOI.ModelLike,
        set::MOI.AbstractSet,
    )::BT

Bridge the constrained variable in `set` using bridge `BT` to `model` and
returns a bridge object of type `BT`.

## Implementation notes

 * The bridge type `BT` must be a concrete type, that is, all the type
   parameters of the bridge must be set.
"""
function bridge_constrained_variable end

function MOI.get(
    ::MOI.ModelLike,
    attr::MOI.AbstractVariableAttribute,
    bridge::AbstractBridge,
)
    return throw(
        ArgumentError(
            "Variable bridge of type `$(typeof(bridge))` does not support " *
            "accessing the attribute `$attr`.",
        ),
    )
end

function MOI.get(
    ::MOI.ModelLike,
    attr::MOI.AbstractVariableAttribute,
    bridge::AbstractBridge,
    ::MOI.Bridges.IndexInVector,
)
    return throw(
        ArgumentError(
            "Variable bridge of type `$(typeof(bridge))` does not support " *
            "accessing the attribute `$attr`.",
        ),
    )
end

function MOI.supports(
    ::MOI.ModelLike,
    ::MOI.AbstractVariableAttribute,
    ::Type{<:AbstractBridge},
)
    return false
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.AbstractVariableAttribute,
    bridge::AbstractBridge,
    value,
    ::MOI.Bridges.IndexInVector...,
)
    if MOI.is_copyable(attr) && !MOI.supports(model, attr, typeof(bridge))
        throw(MOI.UnsupportedAttribute(attr))
    else
        throw(MOI.SetAttributeNotAllowed(attr))
    end
end

"""
    unbridged_map(
       bridge::MOI.Bridges.Variable.AbstractBridge,
        vi::MOI.VariableIndex,
    )

For a bridged variable in a scalar set, return a tuple of pairs mapping the
variables created by the bridge to an affine expression in terms of the
bridged variable `vi`.

```julia
unbridged_map(
    bridge::MOI.Bridges.Variable.AbstractBridge,
    vis::Vector{MOI.VariableIndex},
)
```

For a bridged variable in a vector set, return a tuple of pairs mapping the
variables created by the bridge to an affine expression in terms of the bridged
variable `vis`. If this method is not implemented, it falls back to calling
the following method for every variable of `vis`.

```julia
unbridged_map(
    bridge::MOI.Bridges.Variable.AbstractBridge,
    vi::MOI.VariableIndex,
    i::MOI.Bridges.IndexInVector,
)
```

For a bridged variable in a vector set, return a tuple of pairs mapping the
variables created by the bridge to an affine expression in terms of the bridged
variable `vi` corresponding to the `i`th variable of the vector.

If there is no way to recover the expression in terms of the bridged variable(s)
`vi(s)`, return `nothing`. See [`ZerosBridge`](@ref) for an example of bridge
returning `nothing`.
"""
function unbridged_map end

function unbridged_map(bridge::AbstractBridge, vis::Vector{MOI.VariableIndex})
    mappings = Pair{MOI.VariableIndex,MOI.AbstractScalarFunction}[]
    for (i, vi) in enumerate(vis)
        vi_mappings = unbridged_map(bridge, vi, MOI.Bridges.IndexInVector(i))
        if vi_mappings === nothing
            return
        end
        for mapping in vi_mappings
            push!(mappings, mapping)
        end
    end
    return mappings
end
