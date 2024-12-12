# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    struct MapNotInvertible <: Exception
        message::String
    end

An error thrown by [`inverse_map_function`](@ref) or
[`inverse_adjoint_map_function`](@ref) indicating that the linear map `A`
defined in [`Variable.SetMapBridge`](@ref) and [`Constraint.SetMapBridge`](@ref)
is not invertible.
"""
struct MapNotInvertible <: Exception
    message::String
end

"""
    map_set(bridge::MOI.Bridges.AbstractBridge, set)
    map_set(::Type{BT}, set) where {BT}

Return the image of `set` through the linear map `A` defined in
[`Variable.SetMapBridge`](@ref) and [`Constraint.SetMapBridge`](@ref).

This function is used for bridging the constraint and setting the
[`MOI.ConstraintSet`](@ref).
"""
map_set(bridge::AbstractBridge, set) = map_set(typeof(bridge), set)

"""
    inverse_map_set(bridge::MOI.Bridges.AbstractBridge, set)
    inverse_map_set(::Type{BT}, set) where {BT}

Return the preimage of `set` through the linear map `A` defined in
[`Variable.SetMapBridge`](@ref) and [`Constraint.SetMapBridge`](@ref).

This function is used for getting the [`MOI.ConstraintSet`](@ref).

The method can alternatively be defined on the bridge type. This legacy
interface is kept for backward compatibility.
"""
function inverse_map_set(bridge::AbstractBridge, set)
    return inverse_map_set(typeof(bridge), set)
end

"""
    map_function(bridge::MOI.Bridges.AbstractBridge, func)
    map_function(::Type{BT}, func) where {BT}

Return the image of `func` through the linear map `A` defined in
[`Variable.SetMapBridge`](@ref) and [`Constraint.SetMapBridge`](@ref).

This function is used for getting the [`MOI.ConstraintPrimal`](@ref) of variable
bridges. For constraint bridges, this is used for bridging the constraint,
setting the [`MOI.ConstraintFunction`](@ref) and [`MOI.ConstraintPrimalStart`](@ref)
and modifying the function with [`MOI.modify`](@ref).

The default implementation of [`Constraint.bridge_constraint`](@ref) uses
[`map_function`](@ref) with the bridge type so if this function is defined
on the bridge type, [`Constraint.bridge_constraint`](@ref) does not need
to be implemented.
"""
function map_function(bridge::AbstractBridge, func)
    return map_function(typeof(bridge), func)
end

"""
    map_function(::Type{BT}, func, i::IndexInVector) where {BT}

Return the scalar function at the `i`th index of the vector function that
would be returned by `map_function(BT, func)` except that it may compute the
`i`th element. This is used by [`bridged_function`](@ref) and for getting
the [`MOI.VariablePrimal`](@ref) and
[`MOI.VariablePrimalStart`](@ref) of variable bridges.
"""
function map_function(::Type{BT}, func, i::IndexInVector) where {BT}
    return MOI.Utilities.eachscalar(map_function(BT, func))[i.value]
end

function map_function(bridge::AbstractBridge, func, i::IndexInVector)
    return map_function(typeof(bridge), func, i)
end

"""
    inverse_map_function(bridge::MOI.Bridges.AbstractBridge, func)
    inverse_map_function(::Type{BT}, func) where {BT}

Return the image of `func` through the inverse of the linear map `A` defined in
[`Variable.SetMapBridge`](@ref) and [`Constraint.SetMapBridge`](@ref).

This function is used by [`Variable.unbridged_map`](@ref) and for setting the
[`MOI.VariablePrimalStart`](@ref) of variable bridges and for getting the
[`MOI.ConstraintFunction`](@ref), the [`MOI.ConstraintPrimal`](@ref) and the
[`MOI.ConstraintPrimalStart`](@ref) of constraint bridges.

If the linear map `A` is not invertible, the error [`MapNotInvertible`](@ref) is
thrown.

The method can alternatively be defined on the bridge type. This legacy
interface is kept for backward compatibility.
"""
function inverse_map_function(bridge::AbstractBridge, func)
    return inverse_map_function(typeof(bridge), func)
end

"""
    adjoint_map_function(bridge::MOI.Bridges.AbstractBridge, func)
    adjoint_map_function(::Type{BT}, func) where {BT}

Return the image of `func` through the adjoint of the linear map `A` defined in
[`Variable.SetMapBridge`](@ref) and [`Constraint.SetMapBridge`](@ref).

This function is used for getting the [`MOI.ConstraintDual`](@ref) and
[`MOI.ConstraintDualStart`](@ref) of constraint bridges.

The method can alternatively be defined on the bridge type. This legacy
interface is kept for backward compatibility.
"""
function adjoint_map_function(bridge::AbstractBridge, func)
    return adjoint_map_function(typeof(bridge), func)
end

"""
    inverse_adjoint_map_function(bridge::MOI.Bridges.AbstractBridge, func)
    inverse_adjoint_map_function(::Type{BT}, func) where {BT}

Return the image of `func` through the inverse of the adjoint of the linear map
`A` defined in [`Variable.SetMapBridge`](@ref) and [`Constraint.SetMapBridge`](@ref).

This function is used for getting the [`MOI.ConstraintDual`](@ref) of variable
bridges and setting the [`MOI.ConstraintDualStart`](@ref) of constraint bridges.

If the linear map `A` is not invertible, the error [`MapNotInvertible`](@ref) is
thrown.

The method can alternatively be defined on the bridge type. This legacy
interface is kept for backward compatibility.
"""
function inverse_adjoint_map_function(bridge::AbstractBridge, func)
    return inverse_adjoint_map_function(typeof(bridge), func)
end
