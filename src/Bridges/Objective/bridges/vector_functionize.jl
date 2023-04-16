# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    VectorFunctionizeBridge{T}

`VectorFunctionizeBridge` implements the following reformulations:

 * ``\\min \\{x\\}`` into ``\\min\\{1x + 0\\}``
 * ``\\max \\{x\\}`` into ``\\max\\{1x + 0\\}``

where `T` is the coefficient type of `1` and `0`.

## Source node

`VectorFunctionizeBridge` supports:

 * [`MOI.ObjectiveFunction{MOI.VectorOfVariables}`](@ref)

## Target nodes

`VectorFunctionizeBridge` creates:

 * One objective node: [`MOI.ObjectiveFunction{MOI.VectorAffineFunction{T}}`](@ref)
"""
struct VectorFunctionizeBridge{T} <: AbstractBridge end

const VectorFunctionize{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{VectorFunctionizeBridge{T},OT}

function bridge_objective(
    ::Type{VectorFunctionizeBridge{T}},
    model::MOI.ModelLike,
    f::MOI.VectorOfVariables,
) where {T}
    F = MOI.VectorAffineFunction{T}
    MOI.set(model, MOI.ObjectiveFunction{F}(), convert(F, f))
    return VectorFunctionizeBridge{T}()
end

function supports_objective_function(
    ::Type{<:VectorFunctionizeBridge},
    ::Type{MOI.VectorOfVariables},
)
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:VectorFunctionizeBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(::Type{<:VectorFunctionizeBridge})
    return Tuple{Type,Type}[]
end

function MOI.Bridges.set_objective_function_type(
    ::Type{VectorFunctionizeBridge{T}},
) where {T}
    return MOI.VectorAffineFunction{T}
end

MOI.get(::VectorFunctionizeBridge, ::MOI.NumberOfVariables)::Int64 = 0

function MOI.get(::VectorFunctionizeBridge, ::MOI.ListOfVariableIndices)
    return MOI.VariableIndex[]
end

MOI.delete(::MOI.ModelLike, ::VectorFunctionizeBridge) = nothing

function MOI.set(
    ::MOI.ModelLike,
    ::MOI.ObjectiveSense,
    ::VectorFunctionizeBridge,
    ::MOI.OptimizationSense,
)
    # `VectorFunctionizeBridge` is sense agnostic, therefore, we don't need to
    # change anything.
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.Bridges.ObjectiveFunctionValue{MOI.VectorOfVariables},
    ::VectorFunctionizeBridge{T},
) where {T}
    F = MOI.VectorAffineFunction{T}
    attr_f = MOI.Bridges.ObjectiveFunctionValue{F}(attr.result_index)
    return MOI.get(model, attr_f)
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ObjectiveFunction{MOI.VectorOfVariables},
    ::VectorFunctionizeBridge{T},
) where {T}
    f = MOI.get(model, MOI.ObjectiveFunction{MOI.VectorAffineFunction{T}}())
    return convert(MOI.VectorOfVariables, f)
end
