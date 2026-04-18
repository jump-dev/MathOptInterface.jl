# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    LazyScalarSetBridge{
        T,
        F<:MOI.AbstractScalarFunction,
        S<:MOI.AbstractScalarSet,
    }

`LazyScalarSetBridge` implements the following reformulation:

  * ``f \\in LazyScalarSet(s)`` into ``f \\in s``.

## Source node

`LazyScalarSetBridge` supports:

  * `F` in `LazyScalarSet{S}`

## Target nodes

`LazyScalarSetBridge` creates:

  * `F` in `S`
"""
struct LazyScalarSetBridge{
    T,
    F<:MOI.AbstractScalarFunction,
    S<:MOI.AbstractScalarSet,
} <: MOI.Bridges.Constraint.SetMapBridge{T,S,MOI.LazyScalarSet{S},F,F}
    constraint::MOI.ConstraintIndex{F,S}

    function LazyScalarSetBridge{T,F,S}(
        constraint::MOI.ConstraintIndex{F,S},
    ) where {T,F<:MOI.AbstractScalarFunction,S<:MOI.AbstractScalarSet}
        return new{T,F,S}(constraint)
    end
end

MOI.Bridges.map_function(::Type{<:LazyScalarSetBridge}, f) = f

MOI.Bridges.inverse_map_function(::Type{<:LazyScalarSetBridge}, f) = f

MOI.Bridges.adjoint_map_function(::Type{<:LazyScalarSetBridge}, f) = f

MOI.Bridges.inverse_adjoint_map_function(::Type{<:LazyScalarSetBridge}, f) = f

function MOI.Bridges.map_set(
    ::Type{LazyScalarSetBridge{T,F,S}},
    set::MOI.LazyScalarSet{S},
) where {T,F,S}
    return set.set
end

function MOI.Bridges.inverse_map_set(
    ::Type{LazyScalarSetBridge{T,F,S}},
    set::S,
) where {T,F,S}
    return MOI.LazyScalarSet(set)
end

function MOI.supports_constraint(
    ::Type{<:LazyScalarSetBridge},
    ::Type{<:MOI.AbstractScalarFunction},
    ::Type{<:MOI.LazyScalarSet},
)
    return true
end

function MOI.Bridges.Constraint.concrete_bridge_type(
    ::Type{<:LazyScalarSetBridge{T}},
    ::Type{F},
    ::Type{MOI.LazyScalarSet{S}},
) where {T,F<:MOI.AbstractScalarFunction,S<:MOI.AbstractScalarSet}
    return LazyScalarSetBridge{T,F,S}
end
