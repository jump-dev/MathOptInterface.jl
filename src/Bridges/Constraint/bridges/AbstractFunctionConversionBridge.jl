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

# Needed to avoid an ambiguity with the getter for MOI.AbstractConstraintAttribute
function MOI.get(
    ::MOI.ModelLike,
    ::MOI.Bridges.FirstBridge,
    bridge::AbstractFunctionConversionBridge,
)
    return bridge
end

# Needed to avoid an ambiguity with the getter for MOI.Constraint.AbstractBridge
function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintConflictStatus,
    bridge::AbstractFunctionConversionBridge,
)
    ret = MOI.NOT_IN_CONFLICT
    for (F, S) in MOI.Bridges.added_constraint_types(typeof(bridge))
        for ci in MOI.get(bridge, MOI.ListOfConstraintIndices{F,S}())
            status = MOI.get(model, attr, ci)
            if status == MOI.IN_CONFLICT
                return status
            elseif status == MOI.MAYBE_IN_CONFLICT
                ret = status
            end
        end
    end
    return ret
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

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:AbstractFunctionConversionBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:AbstractFunctionConversionBridge{F,S}},
) where {F,S}
    return Tuple{Type,Type}[(F, S)]
end

function MOI.get(
    ::AbstractFunctionConversionBridge{F,S},
    ::MOI.NumberOfConstraints{F,S},
)::Int64 where {F,S}
    return 1
end

function MOI.get(
    b::AbstractFunctionConversionBridge{F,S},
    ::MOI.ListOfConstraintIndices{F,S},
) where {F,S}
    return [b.constraint]
end

function MOI.delete(model::MOI.ModelLike, c::AbstractFunctionConversionBridge)
    MOI.delete(model, c.constraint)
    return
end

function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::AbstractFunctionConversionBridge{F},
    f,
) where {F}
    MOI.set(model, MOI.ConstraintFunction(), bridge.constraint, convert(F, f))
    return
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::AbstractFunctionConversionBridge{F},
    i::MOI.Bridges.IndexInVector,
) where {F<:MOI.AbstractVectorFunction}
    func = MOI.get(model, MOI.ConstraintFunction(), bridge.constraint)
    idx = setdiff(1:MOI.output_dimension(func), i.value)
    new_func = MOI.Utilities.eachscalar(func)[idx]
    set = MOI.get(model, MOI.ConstraintSet(), bridge.constraint)
    new_set = MOI.update_dimension(set, MOI.dimension(set) - 1)
    # If we first do `MOI.delete` and then `MOI.add_constraint`,
    # there might be an issue of `MOI.delete` ends up deleting a
    # bridged variables.
    # Indeed, in that case,
    # `_delete_variables_in_vector_of_variables_constraint` might
    # then try to get `get` `ConstraintFunction` for this bridge
    # which will fail since the `bridge.constraint` is invalid.
    # We avoid this issue by calling `add_constraint` first.
    old_constraint = bridge.constraint
    bridge.constraint = MOI.add_constraint(model, new_func, new_set)
    MOI.delete(model, old_constraint)
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
        MOI.ConstraintConflictStatus,
    },
)
    return true
end

"""
    FunctionConversionBridge{T,F,G,S} <: AbstractFunctionConversionBridge{G,S}

`FunctionConversionBridge` implements the following reformulations:

  * ``g(x) \\in S`` into ``f(x) \\in S``

for these pairs of functions:

 * [`MOI.ScalarAffineFunction`](@ref)` to [`MOI.ScalarQuadraticFunction`](@ref)
 * [`MOI.ScalarQuadraticFunction`](@ref)  to [`MOI.ScalarNonlinearFunction`](@ref)
 * [`MOI.VectorAffineFunction`](@ref) to [`MOI.VectorQuadraticFunction`](@ref)

See also [`SetConversionBridge`](@ref).

## Source node

`FunctionConversionBridge` supports:

  * `G` in `S`

## Target nodes

`FunctionConversionBridge` creates:

  * `F` in `S`
"""
mutable struct FunctionConversionBridge{T,F,G,S} <:
               AbstractFunctionConversionBridge{F,S}
    constraint::MOI.ConstraintIndex{F,S}
end
# The `struct` needs to be mutable if `F <: AbstractVectorFunction`
# in case one row is deleted. See `MOI.delete` above.

function bridge_constraint(
    ::Type{FunctionConversionBridge{T,F,G,S}},
    model::MOI.ModelLike,
    f::G,
    s::S,
) where {T,F,G,S}
    ci = MOI.add_constraint(model, convert(F, f), s)
    return FunctionConversionBridge{T,F,G,S}(ci)
end

"""
    conversion_cost(
        F::Type{<:MOI.AbstractFunction},
        G::Type{<:MOI.AbstractFunction},
    )::Float64

Return a `Float64` returning the cost of converting any function of type `G`
to a function of type `F` with `convert`.

This cost is used to compute [`MOI.Bridges.bridging_cost`](@ref).

The default cost is `Inf`, which means that
[`MOI.Bridges.Constraint.FunctionConversionBridge`](@ref) should not attempt the
conversion.
"""
function conversion_cost(
    ::Type{<:MOI.AbstractFunction},
    ::Type{<:MOI.AbstractFunction},
)
    return Inf
end

function conversion_cost(
    F::Type{<:MOI.AbstractVectorFunction},
    G::Type{<:MOI.AbstractVectorFunction},
)
    return conversion_cost(
        MOI.Utilities.scalar_type(F),
        MOI.Utilities.scalar_type(G),
    )
end

function conversion_cost(
    ::Type{<:MOI.ScalarAffineFunction},
    ::Type{MOI.VariableIndex},
)
    return 1.0
end

function conversion_cost(
    ::Type{MOI.ScalarQuadraticFunction{T}},
    ::Type{<:Union{MOI.VariableIndex,MOI.ScalarAffineFunction{T}}},
) where {T}
    return 10.0
end

function conversion_cost(
    ::Type{MOI.ScalarNonlinearFunction},
    ::Type{
        <:Union{
            MOI.VariableIndex,
            MOI.ScalarAffineFunction{Float64},
            MOI.ScalarQuadraticFunction{Float64},
        },
    },
)
    return 100.0
end

function MOI.supports_constraint(
    ::Type{<:FunctionConversionBridge{T,F}},
    ::Type{G},
    ::Type{<:MOI.AbstractSet},
) where {T,F,G<:MOI.AbstractFunction}
    return MOI.Utilities.is_coefficient_type(G, T) &&
           isfinite(conversion_cost(F, G))
end

function concrete_bridge_type(
    ::Type{<:FunctionConversionBridge{T,F}},
    G::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
) where {T,F}
    return FunctionConversionBridge{T,F,G,S}
end

function MOI.Bridges.bridging_cost(
    ::Type{<:FunctionConversionBridge{T,F,G}},
) where {T,F,G}
    return conversion_cost(F, G)
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    b::FunctionConversionBridge{T,F,G},
) where {T,F,G}
    # TODO(odow): there's a bug _somewhere_ in call_in_context, which means that
    # using CanonicalConstraintFunction here doesn't work. I got too confused
    # trying to track it down, so I explicitly called canonical here instead.
    # We need canonical, because the downstream bridges may have added
    # additional terms that mean it can't be directly converted to G.
    f = MOI.get(model, MOI.ConstraintFunction(), b.constraint)
    return MOI.Utilities.convert_approx(G, MOI.Utilities.canonical(f))
end

"""

    ScalarFunctionizeBridge{T,S} = FunctionConversionBridge{T,MOI.ScalarAffineFunction{T},MOI.VariableIndex,S}

`ScalarFunctionizeBridge` implements the following reformulations:

  * ``x \\in S`` into ``1x + 0 \\in S``

## Source node

`ScalarFunctionizeBridge` supports:

  * [`MOI.VariableIndex`](@ref) in `S`

## Target nodes

`ScalarFunctionizeBridge` creates:

  * [`MOI.ScalarAffineFunction{T}`](@ref) in `S`
"""
const ScalarFunctionizeBridge{T,S} =
    FunctionConversionBridge{T,MOI.ScalarAffineFunction{T},MOI.VariableIndex,S}

const ScalarFunctionize{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{ScalarFunctionizeBridge{T},OT}

# VectorOfVariables -> VectorAffineFunction  # Handled by VectorFunctionizeBridge

"""
    VectorFunctionizeBridge{T,S} = FunctionConversionBridge{T,MOI.VectorAffineFunction{T},S}

`VectorFunctionizeBridge` implements the following reformulations:

  * ``x \\in S`` into ``Ix + 0 \\in S``

## Source node

`VectorFunctionizeBridge` supports:

  * [`MOI.VectorOfVariables`](@ref) in `S`

## Target nodes

`VectorFunctionizeBridge` creates:

  * [`MOI.VectorAffineFunction{T}`](@ref) in `S`
"""
const VectorFunctionizeBridge{T,S} = FunctionConversionBridge{
    T,
    MOI.VectorAffineFunction{T},
    MOI.VectorOfVariables,
    S,
}

const VectorFunctionize{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{VectorFunctionizeBridge{T},OT}

# AbstractScalarFunction -> ScalarQuadraticFunction

"""
    ToScalarQuadraticBridge{T,G,S} <: AbstractFunctionConversionBridge{G,S}

`ToScalarQuadraticBridge` implements the following reformulation:

  * ``g(x) \\in S`` into ``f(x) \\in S``

where `g` is an abstract scalar function and `f` is a
[`MOI.ScalarQuadraticFunction`](@ref).

## Source node

`ToScalarQuadraticBridge` supports:

  * `G<:AbstractScalarFunction` in `S`

## Target nodes

`ToScalarQuadraticBridge` creates:

  * [`MOI.ScalarQuadraticFunction`](@ref) in `S`
"""
const ToScalarQuadraticBridge{T,G,S} =
    FunctionConversionBridge{T,MOI.ScalarQuadraticFunction{T},G,S}

const ToScalarQuadratic{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{ToScalarQuadraticBridge{T},OT}

# AbstractVectorFunction -> VectorQuadraticFunction

"""
    ToVectorQuadraticBridge{T,G,S} <: AbstractFunctionConversionBridge{G,S}

`ToVectorQuadraticBridge` implements the following reformulation:

  * ``g(x) \\in S`` into ``f(x) \\in S``

where `g` is an abstract vector function and `f` is a
[`MOI.VectorQuadraticFunction`](@ref).

## Source node

`ToVectorQuadraticBridge` supports:

  * `G<:AbstractVectorFunction` in `S`

## Target nodes

`ToVectorQuadraticBridge` creates:

  * [`MOI.VectorQuadraticFunction`](@ref) in `S`
"""
const ToVectorQuadraticBridge{T,G,S} =
    FunctionConversionBridge{T,MOI.VectorQuadraticFunction{T},G,S}

const ToVectorQuadratic{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{ToVectorQuadraticBridge{T},OT}

# AbstractScalarFunction -> ScalarNonlinearFunction

"""
    ToScalarNonlinearBridge{T,G,S} <: AbstractFunctionConversionBridge{G,S}

`ToScalarNonlinearBridge` implements the following reformulation:

  * ``g(x) \\in S`` into ``f(x) \\in S``

where `g` is an abstract scalar function and `f` is a
[`MOI.ScalarNonlinearFunction`](@ref).

## Source node

`ToScalarNonlinearBridge` supports:

  * `G<:AbstractScalarFunction` in `S`

## Target nodes

`ToScalarNonlinearBridge` creates:

  * [`MOI.ScalarNonlinearFunction`](@ref) in `S`
"""
const ToScalarNonlinearBridge{T,G,S} =
    FunctionConversionBridge{T,MOI.ScalarNonlinearFunction,G,S}

const ToScalarNonlinear{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{ToScalarNonlinearBridge{T},OT}
