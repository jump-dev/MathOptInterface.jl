# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    FunctionizeBridge{T}

`FunctionizeBridge` implements the following reformulations:

 * ``\\min \\{x\\}`` into ``\\min\\{1x + 0\\}``
 * ``\\max \\{x\\}`` into ``\\max\\{1x + 0\\}``

where `T` is the coefficient type of `1` and `0`.

## Source node

`FunctionizeBridge` supports:

 * [`MOI.ObjectiveFunction{MOI.VariableIndex}`](@ref)

## Target nodes

`FunctionizeBridge` creates:

 * One objective node: [`MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}`](@ref)
"""
struct FunctionizeBridge{T} <: AbstractBridge end

const Functionize{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{FunctionizeBridge{T},OT}

function bridge_objective(
    ::Type{FunctionizeBridge{T}},
    model::MOI.ModelLike,
    func::MOI.VariableIndex,
) where {T}
    F = MOI.ScalarAffineFunction{T}
    MOI.set(model, MOI.ObjectiveFunction{F}(), convert(F, func))
    return FunctionizeBridge{T}()
end

function supports_objective_function(
    ::Type{<:FunctionizeBridge},
    ::Type{MOI.VariableIndex},
)
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:FunctionizeBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(::Type{<:FunctionizeBridge})
    return Tuple{Type,Type}[]
end

function MOI.Bridges.set_objective_function_type(
    ::Type{FunctionizeBridge{T}},
) where {T}
    return MOI.ScalarAffineFunction{T}
end

# Attributes, Bridge acting as a model
MOI.get(::FunctionizeBridge, ::MOI.NumberOfVariables)::Int64 = 0

function MOI.get(::FunctionizeBridge, ::MOI.ListOfVariableIndices)
    return MOI.VariableIndex[]
end

# No variables or constraints are created in this bridge so there is nothing to
# delete.
MOI.delete(::MOI.ModelLike, ::FunctionizeBridge) = nothing

function MOI.set(
    ::MOI.ModelLike,
    ::MOI.ObjectiveSense,
    ::FunctionizeBridge,
    ::MOI.OptimizationSense,
)
    # `FunctionizeBridge` is sense agnostic, therefore, we don't need to change
    # anything.
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.Bridges.ObjectiveFunctionValue{MOI.VariableIndex},
    ::FunctionizeBridge{T},
) where {T}
    F = MOI.ScalarAffineFunction{T}
    attr_f = MOI.Bridges.ObjectiveFunctionValue{F}(attr.result_index)
    return MOI.get(model, attr_f)
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ObjectiveFunction{MOI.VariableIndex},
    ::FunctionizeBridge{T},
) where {T}
    F = MOI.ScalarAffineFunction{T}
    func = MOI.get(model, MOI.ObjectiveFunction{F}())
    return MOI.Utilities.convert_approx(MOI.VariableIndex, func)
end
