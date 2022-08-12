# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    QuadratizeBridge{T}

`QuadratizeBridge` implements the following reformulations:

 * ``\\min \\{a^\\top x + b\\}`` into ``\\min\\{x^\\top \\mathbf{0} x + a^\\top x + b\\}``
 * ``\\max \\{a^\\top x + b\\}`` into ``\\max\\{x^\\top \\mathbf{0} x + a^\\top x + b\\}``

where `T` is the coefficient type of `0`.

## Source node

`QuadratizeBridge` supports:

 * [`MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}`](@ref)

## Target nodes

`QuadratizeBridge` creates:

 * One objective node: [`MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}}`](@ref)
"""
struct QuadratizeBridge{T} <: AbstractBridge end

const Quadratize{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{QuadratizeBridge{T},OT}

function bridge_objective(
    ::Type{QuadratizeBridge{T}},
    model::MOI.ModelLike,
    func::MOI.ScalarAffineFunction{T},
) where {T}
    f = convert(MOI.ScalarQuadraticFunction{T}, func)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    return QuadratizeBridge{T}()
end

function supports_objective_function(
    ::Type{<:QuadratizeBridge{T}},
    ::Type{MOI.ScalarAffineFunction{T}},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:QuadratizeBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(::Type{<:QuadratizeBridge})
    return Tuple{Type,Type}[]
end

function MOI.Bridges.set_objective_function_type(
    ::Type{QuadratizeBridge{T}},
) where {T}
    return MOI.ScalarQuadraticFunction{T}
end

# No variables or constraints are created in this bridge so there is nothing to
# delete.
MOI.delete(::MOI.ModelLike, ::QuadratizeBridge) = nothing

function MOI.set(
    ::MOI.ModelLike,
    ::MOI.ObjectiveSense,
    ::QuadratizeBridge,
    ::MOI.OptimizationSense,
)
    # `QuadratizeBridge` is sense agnostic, therefore, we don't need to change
    # anything.
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.Bridges.ObjectiveFunctionValue{MOI.ScalarAffineFunction{T}},
    ::QuadratizeBridge{T},
) where {T}
    F = MOI.ScalarQuadraticFunction{T}
    attr_f = MOI.Bridges.ObjectiveFunctionValue{F}(attr.result_index)
    return MOI.get(model, attr_f)
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}},
    ::QuadratizeBridge{T},
) where {T}
    F = MOI.ScalarQuadraticFunction{T}
    func = MOI.get(model, MOI.ObjectiveFunction{F}())
    return convert(MOI.ScalarAffineFunction{T}, func)
end
