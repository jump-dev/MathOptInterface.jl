# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    abstract type AbstractFunctionConversionBridge{F, S} <: AbstractBridge end

Bridge a constraint `G`-in-`S` into a constraint `F`-in-`S` where `F` and `G`
are equivalent representations of the same function. By convention, the
transformed function is stored in the `constraint` field.
"""
abstract type AbstractFunctionConversionBridge{F,S} <: AbstractBridge end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.AbstractConstraintAttribute,
    bridge::AbstractFunctionConversionBridge,
)
    if invariant_under_function_conversion(attr)
        return MOI.get(model, attr, bridge.constraint)
    else
        throw(
            MOI.UnsupportedAttribute(
                attr,
                "Bridge of type `$(typeof(bridge))` does not support getting the attribute `$attr` because `MOIB.Constraint.invariant_under_function_conversion($attr)` returns `false`.",
            ),
        )
    end
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::MOI.AbstractConstraintAttribute,
    ::Type{<:AbstractFunctionConversionBridge{F,S}},
) where {F,S}
    return invariant_under_function_conversion(attr) &&
           MOI.supports(model, attr, MOI.ConstraintIndex{F,S})
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.AbstractConstraintAttribute,
    bridge::AbstractFunctionConversionBridge,
    value,
)
    if !invariant_under_function_conversion(attr)
        throw(
            MOI.UnsupportedAttribute(
                attr,
                "Bridge of type `$(typeof(bridge))` does not support setting the attribute `$attr` because `MOIB.Constraint.invariant_under_function_conversion($attr)` returns `false`.",
            ),
        )
    end
    MOI.set(model, attr, bridge.constraint, value)
    return
end

"""
    invariant_under_function_conversion(attr::MOI.AbstractConstraintAttribute)

Returns whether the value of the attribute does not change if the constraint
`F`-in-`S` is transformed into a constraint `G`-in-`S` where `F` and `G` are
equivalent representations of the same function. If it returns true, then
subtypes of [`Constraint.AbstractFunctionConversionBridge`](@ref) such as
[`Constraint.ScalarFunctionizeBridge`](@ref) and
[`Constraint.VectorFunctionizeBridge`](@ref) will automatically support
[`MOI.get`](@ref) and [`MOI.set`](@ref) for `attr`.
"""
invariant_under_function_conversion(::MOI.AbstractConstraintAttribute) = false

function invariant_under_function_conversion(
    ::Union{
        MOI.ConstraintSet,
        MOI.ConstraintBasisStatus,
        MOI.ConstraintPrimal,
        MOI.ConstraintPrimalStart,
        MOI.ConstraintDual,
        MOI.ConstraintDualStart,
    },
)
    return true
end
