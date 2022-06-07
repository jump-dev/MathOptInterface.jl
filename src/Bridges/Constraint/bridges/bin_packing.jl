# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    BinPackingToMILPBridge{T,F} <: Bridges.Constraint.AbstractBridge

`BinPackingToMILPBridge` implements the following reformulation:

  * ``x \\in BinPacking(c, w)`` into
    ```math
    \\begin{aligned}
    z_{ij} \\in \\{0, 1\\}                  & \\forall i, j \\\\
    \\sum\\limits_{j=1}^n z_{ij} = 1        & \\forall i \\\\
    \\sum\\limits_{i=1}^n w_i z_{ij} \\le c & \\forall j \\\\
    \\sum\\limits_{j=1}^n j z_{ij} == x_i   & \\forall i
    \\end{aligned}
    ```
## Source node

`BinPackingToMILPBridge` supports:

  * `F` in [`MOI.BinPacking{T}`](@ref)

## Target nodes

`BinPackingToMILPBridge` creates:

  * [`MOI.VariableIndex`](@ref) in [`MOI.ZeroOne`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.EqualTo{T}`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.LessThan{T}`](@ref)
"""
struct BinPackingToMILPBridge{T,F<:MOI.AbstractVectorFunction} <: AbstractBridge
    z::Matrix{MOI.VariableIndex}
    capacities::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
    }
    unities::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
    }
    assignment::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
    }
    # Cache to simplify MOI.get
    f::F
    s::MOI.BinPacking{T}
end

const BinPackingToMILP{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{BinPackingToMILPBridge{T},OT}

function bridge_constraint(
    ::Type{BinPackingToMILPBridge{T,F}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.BinPacking{T},
) where {T,F}
    N = length(s.weights)
    z = reshape(MOI.add_variables(model, N^2), N, N)
    MOI.add_constraint.(model, z, MOI.ZeroOne())
    # ∑w_i * z_ib <= c  ∀b
    capacities = [
        MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction(
                MOI.ScalarAffineTerm.(s.weights, z[:, b]),
                zero(T),
            ),
            MOI.LessThan(s.capacity),
        ) for b in 1:N
    ]
    # ∑z_ib = 1 ∀i
    unities = [
        MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction(
                MOI.ScalarAffineTerm.(one(T), z[i, :]),
                zero(T),
            ),
            MOI.EqualTo(one(T)),
        ) for i in 1:N
    ]
    # ∑b z_ib - f_i == 0
    assignment =
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}[]
    for (i, fi) in enumerate(MOI.Utilities.eachscalar(f))
        assign_f = MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(T(b), z[i, b]) for b in 1:N],
            zero(T),
        )
        assign_f = MOI.Utilities.operate!(-, T, assign_f, fi)
        ci = MOI.add_constraint(model, assign_f, MOI.EqualTo(zero(T)))
        push!(assignment, ci)
    end
    return BinPackingToMILPBridge{T,F}(z, capacities, unities, assignment, f, s)
end

function MOI.supports_constraint(
    ::Type{<:BinPackingToMILPBridge{T}},
    ::Type{F},
    ::Type{MOI.BinPacking{T}},
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:BinPackingToMILPBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{BinPackingToMILPBridge{T,F}},
) where {T,F}
    return Tuple{Type,Type}[
        (MOI.VariableIndex, MOI.ZeroOne),
        (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}),
        (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}),
    ]
end

function concrete_bridge_type(
    ::Type{<:BinPackingToMILPBridge{T}},
    ::Type{F},
    ::Type{MOI.BinPacking{T}},
) where {T,F<:MOI.AbstractVectorFunction}
    return BinPackingToMILPBridge{T,F}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::BinPackingToMILPBridge,
)
    return bridge.f
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::BinPackingToMILPBridge,
)
    return bridge.s
end

function MOI.delete(model::MOI.ModelLike, bridge::BinPackingToMILPBridge)
    MOI.delete(model, bridge.capacities)
    MOI.delete(model, bridge.unities)
    MOI.delete(model, bridge.assignment)
    MOI.delete.(model, bridge.z)
    return
end

function MOI.get(bridge::BinPackingToMILPBridge, ::MOI.NumberOfVariables)::Int64
    return length(bridge.z)
end

function MOI.get(bridge::BinPackingToMILPBridge, ::MOI.ListOfVariableIndices)
    return [bridge.z[i] for i in eachindex(bridge.z)]
end

function MOI.get(
    bridge::BinPackingToMILPBridge,
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.ZeroOne},
)::Int64
    return length(bridge.z)
end

function MOI.get(
    bridge::BinPackingToMILPBridge,
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.ZeroOne},
)
    return [
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(z.value) for
        z in bridge.z
    ]
end

function MOI.get(
    bridge::BinPackingToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
)::Int64 where {T}
    return length(bridge.unities) + length(bridge.assignment)
end

function MOI.get(
    bridge::BinPackingToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
) where {T}
    return vcat(bridge.unities, bridge.assignment)
end

function MOI.get(
    bridge::BinPackingToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
)::Int64 where {T}
    return length(bridge.capacities)
end

function MOI.get(
    bridge::BinPackingToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
) where {T}
    return copy(bridge.capacities)
end
