# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    FunctionConversionBridge{T,F,G} <: AbstractBridge

`FunctionConversionBridge` implements the following reformulations:

 * ``\\min \\{g(x)\\}`` into ``\\min\\{f(x)\\}``
 * ``\\max \\{g(x)\\}`` into ``\\max\\{f(x)\\}``

for these pairs of functions:

 * [`MOI.ScalarAffineFunction`](@ref) to [`MOI.ScalarQuadraticFunction`](@ref)
 * [`MOI.ScalarQuadraticFunction`](@ref)  to [`MOI.ScalarNonlinearFunction`](@ref)
 * [`MOI.VectorAffineFunction`](@ref) to [`MOI.VectorQuadraticFunction`](@ref)

## Source node

`FunctionConversionBridge` supports:

 * [`MOI.ObjectiveFunction{G}`](@ref)

## Target nodes

`FunctionConversionBridge` creates:

 * One objective node: [`MOI.ObjectiveFunction{F}`](@ref)
"""
struct FunctionConversionBridge{T,F,G} <: AbstractBridge end

function bridge_objective(
    ::Type{FunctionConversionBridge{T,F,G}},
    model::MOI.ModelLike,
    func::G,
) where {T,F,G<:MOI.AbstractFunction}
    MOI.set(model, MOI.ObjectiveFunction{F}(), convert(F, func))
    return FunctionConversionBridge{T,F,G}()
end

function supports_objective_function(
    ::Type{<:FunctionConversionBridge{T,F}},
    ::Type{G},
) where {T,F,G<:MOI.AbstractFunction}
    return MOI.Utilities.is_coefficient_type(G, T) &&
           isfinite(MOI.Bridges.Constraint.conversion_cost(F, G))
end

function MOI.Bridges.bridging_cost(
    ::Type{FunctionConversionBridge{T,F,G}},
) where {T,F,G}
    return MOI.Bridges.Constraint.conversion_cost(F, G)
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:FunctionConversionBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(::Type{<:FunctionConversionBridge})
    return Tuple{Type,Type}[]
end

function MOI.Bridges.set_objective_function_type(
    ::Type{<:FunctionConversionBridge{T,F}},
) where {T,F}
    return F
end

function concrete_bridge_type(
    ::Type{<:FunctionConversionBridge{T,F}},
    ::Type{G},
) where {T,F,G<:MOI.AbstractFunction}
    return FunctionConversionBridge{T,F,G}
end

# Attributes, Bridge acting as a model
MOI.get(::FunctionConversionBridge, ::MOI.NumberOfVariables)::Int64 = 0

function MOI.get(::FunctionConversionBridge, ::MOI.ListOfVariableIndices)
    return MOI.VariableIndex[]
end

# No variables or constraints are created in this bridge so there is nothing to
# delete.
MOI.delete(::MOI.ModelLike, ::FunctionConversionBridge) = nothing

function MOI.set(
    ::MOI.ModelLike,
    ::MOI.ObjectiveSense,
    ::FunctionConversionBridge,
    ::MOI.OptimizationSense,
)
    # `FunctionConversionBridge` is sense agnostic, therefore, we don't need to
    # change anything.
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.Bridges.ObjectiveFunctionValue{G},
    ::FunctionConversionBridge{T,F,G},
) where {T,F,G}
    attr_f = MOI.Bridges.ObjectiveFunctionValue{F}(attr.result_index)
    return MOI.get(model, attr_f)
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ObjectiveFunction{G},
    ::FunctionConversionBridge{T,F,G},
) where {T,F,G<:MOI.AbstractFunction}
    func = MOI.get(model, MOI.ObjectiveFunction{F}())
    return MOI.Utilities.convert_approx(G, func)
end

"""
    FunctionizeBridge{T,G} <: FunctionConversionBridge{T,MOI.ScalarAffineFunction{T},G}

`FunctionizeBridge` implements the following reformulations:

 * ``\\min \\{x\\}`` into ``\\min\\{1x + 0\\}``
 * ``\\max \\{x\\}`` into ``\\max\\{1x + 0\\}``

where `T` is the coefficient type of `1` and `0`.

## Source node

`FunctionizeBridge` supports:

 * [`MOI.ObjectiveFunction{G}`](@ref)

## Target nodes

`FunctionizeBridge` creates:

 * One objective node: [`MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}`](@ref)
"""
const FunctionizeBridge{T,G} =
    FunctionConversionBridge{T,MOI.ScalarAffineFunction{T},G}

const Functionize{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{FunctionizeBridge{T},OT}

"""
    QuadratizeBridge{T,G} <: FunctionConversionBridge{T,MOI.ScalarQuadraticFunction{T},G}

`QuadratizeBridge` implements the following reformulations:

 * ``\\min \\{a^\\top x + b\\}`` into ``\\min\\{x^\\top \\mathbf{0} x + a^\\top x + b\\}``
 * ``\\max \\{a^\\top x + b\\}`` into ``\\max\\{x^\\top \\mathbf{0} x + a^\\top x + b\\}``

where `T` is the coefficient type of `0`.

## Source node

`QuadratizeBridge` supports:

 * [`MOI.ObjectiveFunction{G}`](@ref)

## Target nodes

`QuadratizeBridge` creates:

 * One objective node: [`MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}}`](@ref)
"""
const QuadratizeBridge{T,G} =
    FunctionConversionBridge{T,MOI.ScalarQuadraticFunction{T},G}

const Quadratize{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{QuadratizeBridge{T},OT}

"""
    ToScalarNonlinearBridge{T,G} <: FunctionConversionBridge{T,MOI.ScalarNonlinearFunction,G}

`ToScalarNonlinearBridge` implements the following reformulations:

 * ``\\min\\{x^\\top \\mathbf{𝑄} x + a^\\top x + b\\}`` into ``\\min\\{f(x)\\}``
 * ``\\max\\{x^\\top \\mathbf{𝑄} x + a^\\top x + b\\}`` into ``\\max\\{f(x)\\}``

where `f(x)` is a `MOI.ScalarNonlinearFunction`.

## Source node

`ToScalarNonlinearBridge` supports:

 * [`MOI.ObjectiveFunction{G}`](@ref)

## Target nodes

`ToScalarNonlinearBridge` creates:

 * One objective node: [`MOI.ObjectiveFunction{MOI.ScalarNonlinearFunction}`](@ref)
"""
const ToScalarNonlinearBridge{T,G} =
    FunctionConversionBridge{T,MOI.ScalarNonlinearFunction,G}

const ToScalarNonlinear{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{ToScalarNonlinearBridge{T},OT}

"""
    VectorFunctionizeBridge{T,G} <: FunctionConversionBridge{T,MOI.VectorAffineFunction{T},G}

`VectorFunctionizeBridge` implements the following reformulations:

 * ``\\min \\{x\\}`` into ``\\min\\{1x + 0\\}``
 * ``\\max \\{x\\}`` into ``\\max\\{1x + 0\\}``

where `T` is the coefficient type of `1` and `0`.

## Source node

`VectorFunctionizeBridge` supports:

 * [`MOI.ObjectiveFunction{G}`](@ref)

## Target nodes

`VectorFunctionizeBridge` creates:

 * One objective node: [`MOI.ObjectiveFunction{MOI.VectorAffineFunction{T}}`](@ref)
"""
const VectorFunctionizeBridge{T,G} =
    FunctionConversionBridge{T,MOI.VectorAffineFunction{T},G}

const VectorFunctionize{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{VectorFunctionizeBridge{T},OT}
