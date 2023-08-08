# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    abstract type AbstractFunctionConversionBridge{F,S} <: AbstractBridge end

Abstract type to support writing bridges in which the function changes but the
set does not.

By convention, the transformed function is stored in the `.constraint` field.
"""
abstract type AbstractFunctionConversionBridge{F,S} <: AbstractBridge end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.AbstractConstraintAttribute,
    bridge::AbstractFunctionConversionBridge,
)
    if !invariant_under_function_conversion(attr)
        throw(
            MOI.UnsupportedAttribute(
                attr,
                "Bridge of type `$(typeof(bridge))` does not support getting " *
                "the attribute `$attr` because " *
                "`MOIB.Constraint.invariant_under_function_conversion($attr)` " *
                "returns `false`.",
            ),
        )
    end
    return MOI.get(model, attr, bridge.constraint)
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.CanonicalConstraintFunction,
    bridge::AbstractFunctionConversionBridge,
)
    f = MOI.get(model, MOI.ConstraintFunction(), bridge)
    return MOI.Utilities.canonical(f)
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
                "Bridge of type `$(typeof(bridge))` does not support setting " *
                "the attribute `$attr` because " *
                "`MOIB.Constraint.invariant_under_function_conversion($attr)` " *
                "returns `false`.",
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
equivalent representations of the same function.

If it returns true, then subtypes of
[`Constraint.AbstractFunctionConversionBridge`](@ref) such as
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

"""
    ScalarFunctionizeBridge{T,S} <: Bridges.Constraint.AbstractBridge

`ScalarFunctionizeBridge` implements the following reformulations:

  * ``x \\in S`` into ``1x + 0 \\in S``

## Source node

`ScalarFunctionizeBridge` supports:

  * [`MOI.VariableIndex`](@ref) in `S`

## Target nodes

`ScalarFunctionizeBridge` creates:

  * [`MOI.ScalarAffineFunction{T}`](@ref) in `S`
"""
struct ScalarFunctionizeBridge{T,S} <:
       AbstractFunctionConversionBridge{MOI.ScalarAffineFunction{T},S}
    constraint::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},S}
end

const ScalarFunctionize{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{ScalarFunctionizeBridge{T},OT}

function bridge_constraint(
    ::Type{ScalarFunctionizeBridge{T,S}},
    model,
    f::MOI.VariableIndex,
    s::S,
) where {T,S}
    constraint = MOI.add_constraint(model, MOI.ScalarAffineFunction{T}(f), s)
    return ScalarFunctionizeBridge{T,S}(constraint)
end

function MOI.supports_constraint(
    ::Type{ScalarFunctionizeBridge{T}},
    ::Type{<:MOI.VariableIndex},
    ::Type{<:MOI.AbstractScalarSet},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:ScalarFunctionizeBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{ScalarFunctionizeBridge{T,S}},
) where {T,S}
    return Tuple{Type,Type}[(MOI.ScalarAffineFunction{T}, S)]
end

function concrete_bridge_type(
    ::Type{<:ScalarFunctionizeBridge{T}},
    ::Type{MOI.VariableIndex},
    S::Type{<:MOI.AbstractScalarSet},
) where {T}
    return ScalarFunctionizeBridge{T,S}
end

function MOI.get(
    ::ScalarFunctionizeBridge{T,S},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},S},
)::Int64 where {T,S}
    return 1
end

function MOI.get(
    b::ScalarFunctionizeBridge{T,S},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},S},
) where {T,S}
    return [b.constraint]
end

function MOI.delete(model::MOI.ModelLike, c::ScalarFunctionizeBridge)
    MOI.delete(model, c.constraint)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    b::ScalarFunctionizeBridge,
)
    return convert(MOI.VariableIndex, MOI.get(model, attr, b.constraint))
end

"""
    VectorFunctionizeBridge{T,S} <: Bridges.Constraint.AbstractBridge

`VectorFunctionizeBridge` implements the following reformulations:

  * ``x \\in S`` into ``Ix + 0 \\in S``

## Source node

`VectorFunctionizeBridge` supports:

  * [`MOI.VectorOfVariables`](@ref) in `S`

## Target nodes

`VectorFunctionizeBridge` creates:

  * [`MOI.VectorAffineFunction{T}`](@ref) in `S`
"""
mutable struct VectorFunctionizeBridge{T,S} <:
               AbstractFunctionConversionBridge{MOI.VectorAffineFunction{T},S}
    constraint::MOI.ConstraintIndex{MOI.VectorAffineFunction{T},S}
end

const VectorFunctionize{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{VectorFunctionizeBridge{T},OT}

function bridge_constraint(
    ::Type{VectorFunctionizeBridge{T,S}},
    model,
    f::MOI.VectorOfVariables,
    s::S,
) where {T,S}
    constraint = MOI.add_constraint(model, MOI.VectorAffineFunction{T}(f), s)
    return VectorFunctionizeBridge{T,S}(constraint)
end

function MOI.supports_constraint(
    ::Type{VectorFunctionizeBridge{T}},
    ::Type{MOI.VectorOfVariables},
    ::Type{<:MOI.AbstractVectorSet},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:VectorFunctionizeBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{VectorFunctionizeBridge{T,S}},
) where {T,S}
    return Tuple{Type,Type}[(MOI.VectorAffineFunction{T}, S)]
end

function concrete_bridge_type(
    ::Type{<:VectorFunctionizeBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    S::Type{<:MOI.AbstractVectorSet},
) where {T}
    return VectorFunctionizeBridge{T,S}
end

function MOI.get(
    ::VectorFunctionizeBridge{T,S},
    ::MOI.NumberOfConstraints{MOI.VectorAffineFunction{T},S},
)::Int64 where {T,S}
    return 1
end

function MOI.get(
    b::VectorFunctionizeBridge{T,S},
    ::MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{T},S},
) where {T,S}
    return [b.constraint]
end

function MOI.delete(model::MOI.ModelLike, bridge::VectorFunctionizeBridge)
    MOI.delete(model, bridge.constraint)
    return
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::VectorFunctionizeBridge,
    i::MOI.Bridges.IndexInVector,
)
    func = MOI.get(model, MOI.ConstraintFunction(), bridge.constraint)
    idx = setdiff(1:MOI.output_dimension(func), i.value)
    new_func = MOI.Utilities.eachscalar(func)[idx]
    set = MOI.get(model, MOI.ConstraintSet(), bridge.constraint)
    new_set = MOI.update_dimension(set, MOI.dimension(set) - 1)
    MOI.delete(model, bridge.constraint)
    bridge.constraint = MOI.add_constraint(model, new_func, new_set)
    return
end

function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::VectorFunctionizeBridge{T},
    func::MOI.VectorOfVariables,
) where {T}
    MOI.set(
        model,
        MOI.ConstraintFunction(),
        bridge.constraint,
        MOI.VectorAffineFunction{T}(func),
    )
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    b::VectorFunctionizeBridge,
)
    f = MOI.get(model, attr, b.constraint)
    return MOI.Utilities.convert_approx(MOI.VectorOfVariables, f)
end

"""
    ScalarQuadraticToScalarNonlinearBridge{T,S} <: Bridges.Constraint.AbstractBridge

!!! warning
    This bridge is not enabled by default, and it may be removed in a future
    release of MathOptInterface.

`ScalarQuadraticToScalarNonlinearBridge` implements the following reformulations:

  * ``f(x) \\in S`` into ``g(x) \\in S``

where `f` is a [`MOI.ScalarQuadraticFunction`](@ref) and `g` is a
[`MOI.ScalarNonlinearFunction{T}`](@ref).

## Source node

`ScalarQuadraticToScalarNonlinearBridge` supports:

  * [`MOI.ScalarQuadraticFunction`](@ref) in `S`

## Target nodes

`ScalarQuadraticToScalarNonlinearBridge` creates:

  * [`MOI.ScalarNonlinearFunction{T}`](@ref) in `S`
"""
struct ScalarQuadraticToScalarNonlinearBridge{T,S} <:
       AbstractFunctionConversionBridge{MOI.ScalarNonlinearFunction,S}
    constraint::MOI.ConstraintIndex{MOI.ScalarNonlinearFunction,S}
end

const ScalarQuadraticToScalarNonlinear{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{ScalarQuadraticToScalarNonlinearBridge{T},OT}

function bridge_constraint(
    ::Type{ScalarQuadraticToScalarNonlinearBridge{T,S}},
    model::MOI.ModelLike,
    f::MOI.ScalarQuadraticFunction{T},
    s::S,
) where {T,S}
    ci = MOI.add_constraint(model, convert(MOI.ScalarNonlinearFunction, f), s)
    return ScalarQuadraticToScalarNonlinearBridge{T,S}(ci)
end

function MOI.supports_constraint(
    ::Type{ScalarQuadraticToScalarNonlinearBridge{T}},
    ::Type{MOI.ScalarQuadraticFunction{T}},
    ::Type{<:MOI.AbstractScalarSet},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:ScalarQuadraticToScalarNonlinearBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{ScalarQuadraticToScalarNonlinearBridge{T,S}},
) where {T,S}
    return Tuple{Type,Type}[(MOI.ScalarNonlinearFunction, S)]
end

function concrete_bridge_type(
    ::Type{<:ScalarQuadraticToScalarNonlinearBridge{T}},
    ::Type{MOI.ScalarQuadraticFunction{T}},
    S::Type{<:MOI.AbstractScalarSet},
) where {T}
    return ScalarQuadraticToScalarNonlinearBridge{T,S}
end

function MOI.get(
    ::ScalarQuadraticToScalarNonlinearBridge{T,S},
    ::MOI.NumberOfConstraints{MOI.ScalarNonlinearFunction,S},
)::Int64 where {T,S}
    return 1
end

function MOI.get(
    b::ScalarQuadraticToScalarNonlinearBridge{T,S},
    ::MOI.ListOfConstraintIndices{MOI.ScalarNonlinearFunction,S},
) where {T,S}
    return [b.constraint]
end

function MOI.delete(
    model::MOI.ModelLike,
    c::ScalarQuadraticToScalarNonlinearBridge,
)
    MOI.delete(model, c.constraint)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    b::ScalarQuadraticToScalarNonlinearBridge{T},
) where {T}
    f = MOI.get(model, MOI.ConstraintFunction(), b.constraint)
    return convert(MOI.ScalarQuadraticFunction{T}, f)
end
