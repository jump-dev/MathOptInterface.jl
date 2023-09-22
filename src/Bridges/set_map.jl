# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    map_set(::Type{BT}, set) where {BT}

Return the image of `set` through the linear map `A` defined in
[`Variable.SetMapBridge`](@ref) and [`Constraint.SetMapBridge`](@ref). This is
used for bridging the constraint and setting
the [`MOI.ConstraintSet`](@ref).
"""
function map_set end

"""
    inverse_map_set(::Type{BT}, set) where {BT}

Return the preimage of `set` through the linear map `A` defined in
[`Variable.SetMapBridge`](@ref) and [`Constraint.SetMapBridge`](@ref). This is
used for getting the [`MOI.ConstraintSet`](@ref).
"""
function inverse_map_set end

"""
    map_function(::Type{BT}, func) where {BT}

Return the image of `func` through the linear map `A` defined in
[`Variable.SetMapBridge`](@ref) and [`Constraint.SetMapBridge`](@ref). This is
used for getting the [`MOI.ConstraintPrimal`](@ref) of variable
bridges. For constraint bridges, this is used for bridging the constraint,
setting the [`MOI.ConstraintFunction`](@ref) and
[`MOI.ConstraintPrimalStart`](@ref) and
modifying the function with [`MOI.modify`](@ref).

    map_function(::Type{BT}, func, i::IndexInVector) where {BT}

Return the scalar function at the `i`th index of the vector function that
would be returned by `map_function(BT, func)` except that it may compute the
`i`th element. This is used by [`bridged_function`](@ref) and for getting
the [`MOI.VariablePrimal`](@ref) and
[`MOI.VariablePrimalStart`](@ref) of variable bridges.
"""
function map_function end

function map_function(::Type{BT}, func, i::IndexInVector) where {BT}
    return MOI.Utilities.eachscalar(map_function(BT, func))[i.value]
end

"""
    inverse_map_function(::Type{BT}, func) where {BT}

Return the image of `func` through the inverse of the linear map `A` defined in
[`Variable.SetMapBridge`](@ref) and [`Constraint.SetMapBridge`](@ref). This is
used by [`Variable.unbridged_map`](@ref) and for setting the
[`MOI.VariablePrimalStart`](@ref) of variable bridges
and for getting the [`MOI.ConstraintFunction`](@ref),
the [`MOI.ConstraintPrimal`](@ref) and the
[`MOI.ConstraintPrimalStart`](@ref) of constraint bridges.
"""
function inverse_map_function end

"""
    adjoint_map_function(::Type{BT}, func) where {BT}

Return the image of `func` through the adjoint of the linear map `A` defined in
[`Variable.SetMapBridge`](@ref) and [`Constraint.SetMapBridge`](@ref). This is
used for getting the [`MOI.ConstraintDual`](@ref) and
[`MOI.ConstraintDualStart`](@ref) of constraint bridges.
"""
function adjoint_map_function end

"""
    inverse_adjoint_map_function(::Type{BT}, func) where {BT}

Return the image of `func` through the inverse of the adjoint of the linear map
`A` defined in [`Variable.SetMapBridge`](@ref) and
[`Constraint.SetMapBridge`](@ref). This is used for getting the
[`MOI.ConstraintDual`](@ref) of variable bridges and setting the
[`MOI.ConstraintDualStart`](@ref) of constraint bridges.
"""
function inverse_adjoint_map_function end
