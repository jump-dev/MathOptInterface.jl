# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    TableToMILPBridge{T,F} <: Bridges.Constraint.AbstractBridge

`TableToMILPBridge` implements the following reformulation:

  * ``x \\in Table(t)`` into
    ```math
    \\begin{aligned}
    z_{j} \\in \\{0, 1\\}                     & \\quad \\forall i, j \\\\
    \\sum\\limits_{j=1}^n z_{j} = 1                           \\\\
    \\sum\\limits_{j=1}^n t_{ij} z_{j} = x_i & \\quad \\forall i
    \\end{aligned}
    ```

## Source node

`TableToMILPBridge` supports:

  * `F` in [`MOI.Table{T}`](@ref)

## Target nodes

`TableToMILPBridge` creates:

  * [`MOI.VariableIndex`](@ref) in [`MOI.ZeroOne`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.EqualTo{T}`](@ref)
"""
struct TableToMILPBridge{T,F<:MOI.AbstractVectorFunction} <: AbstractBridge
    z::Vector{MOI.VariableIndex}
    unity::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}
    assignment::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
    }
    # Cache to simplify MOI.get
    f::F
    s::MOI.Table{T}
end

const TableToMILP{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{TableToMILPBridge{T},OT}

function bridge_constraint(
    ::Type{TableToMILPBridge{T,F}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.Table{T},
) where {T,F}
    m = size(s.table, 1)
    z = MOI.add_variables(model, m)
    MOI.add_constraint.(model, z, MOI.ZeroOne())
    # ∑z_j = 1
    unity = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(one(T), z), zero(T)),
        MOI.EqualTo(one(T)),
    )
    # ∑t_ij z_j == f_i
    assignment =
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}[]
    for (i, fi) in enumerate(MOI.Utilities.eachscalar(f))
        terms = MOI.ScalarAffineTerm{T}[]
        for j in 1:m
            if !iszero(s.table[j, i])
                push!(terms, MOI.ScalarAffineTerm(s.table[j, i], z[j]))
            end
        end
        assign_f = MOI.ScalarAffineFunction(terms, zero(T))
        assign_f = MOI.Utilities.operate!(-, T, assign_f, fi)
        ci = MOI.add_constraint(model, assign_f, MOI.EqualTo(zero(T)))
        push!(assignment, ci)
    end
    return TableToMILPBridge{T,F}(z, unity, assignment, f, s)
end

function MOI.supports_constraint(
    ::Type{<:TableToMILPBridge{T}},
    ::Type{F},
    ::Type{MOI.Table{T}},
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:TableToMILPBridge},
)
    return Tuple{Type}[(MOI.Reals,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{TableToMILPBridge{T,F}},
) where {T,F}
    return Tuple{Type,Type}[
        (MOI.VariableIndex, MOI.ZeroOne),
        (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}),
    ]
end

function concrete_bridge_type(
    ::Type{<:TableToMILPBridge{T}},
    ::Type{F},
    ::Type{MOI.Table{T}},
) where {T,F<:MOI.AbstractVectorFunction}
    return TableToMILPBridge{T,F}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::TableToMILPBridge,
)
    return bridge.f
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::TableToMILPBridge,
)
    return bridge.s
end

function MOI.delete(model::MOI.ModelLike, bridge::TableToMILPBridge)
    MOI.delete(model, bridge.assignment)
    MOI.delete(model, bridge.unity)
    MOI.delete(model, bridge.z)
    return
end

function MOI.get(bridge::TableToMILPBridge, ::MOI.NumberOfVariables)::Int64
    return length(bridge.z)
end

function MOI.get(bridge::TableToMILPBridge, ::MOI.ListOfVariableIndices)
    return copy(bridge.z)
end

function MOI.get(
    bridge::TableToMILPBridge,
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.ZeroOne},
)::Int64
    return length(bridge.z)
end

function MOI.get(
    bridge::TableToMILPBridge,
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.ZeroOne},
)
    return [
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(z.value) for
        z in bridge.z
    ]
end

function MOI.get(
    bridge::TableToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
)::Int64 where {T}
    return length(bridge.assignment) + 1
end

function MOI.get(
    bridge::TableToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
) where {T}
    return vcat(bridge.assignment, bridge.unity)
end
