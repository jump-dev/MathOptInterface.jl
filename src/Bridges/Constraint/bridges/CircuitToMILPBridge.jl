# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    CircuitToMILPBridge{T,F} <: Bridges.Constraint.AbstractBridge

`CircuitToMILPBridge` implements the following reformulation:

  * ``x \\in \\textsf{Circuit}(d)`` to the Miller-Tucker-Zemlin formulation of
    the Traveling Salesperson Problem.

## Source node

`CircuitToMILPBridge` supports:

  * `F` in [`MOI.Circuit`](@ref)

where `F` is [`MOI.VectorOfVariables`](@ref) or
[`MOI.VectorAffineFunction{T}`](@ref).

## Target nodes

`CircuitToMILPBridge` creates:

  * [`MOI.VariableIndex`](@ref) in [`MOI.ZeroOne`](@ref)
  * [`MOI.VariableIndex`](@ref) in [`MOI.Integer`](@ref)
  * [`MOI.VariableIndex`](@ref) in [`MOI.Interval{T}`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.EqualTo{T}`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.LessThan{T}`](@ref)
"""
mutable struct CircuitToMILPBridge{
    T,
    F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
} <: AbstractBridge
    f::F
    z::Vector{MOI.VariableIndex}
    u::Vector{MOI.VariableIndex}
    equal_to::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
    }
    less_than::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
    }
end

const CircuitToMILP{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{CircuitToMILPBridge{T},OT}

function bridge_constraint(
    ::Type{CircuitToMILPBridge{T,F}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.Circuit,
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    n = MOI.dimension(s)
    z = [MOI.add_constrained_variable(model, MOI.ZeroOne())[1] for _ in 1:n^2]
    Z = reshape(z, n, n)
    equal_to = MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}[]
    for (i, x) in enumerate(MOI.Utilities.eachscalar(f))
        f_1 = MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(one(T), Z[i, j]) for j in 1:n if i != j],
            zero(T),
        )
        push!(equal_to, MOI.add_constraint(model, f_1, MOI.EqualTo(one(T))))
        f_2 = MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(one(T), Z[j, i]) for j in 1:n if i != j],
            zero(T),
        )
        push!(equal_to, MOI.add_constraint(model, f_2, MOI.EqualTo(one(T))))
        f_3 = MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(T(j), Z[i, j]) for j in 1:n if i != j],
            zero(T),
        )
        ci = MOI.Utilities.normalize_and_add_constraint(
            model,
            MOI.Utilities.operate(-, T, x, f_3),
            MOI.EqualTo(zero(T));
            allow_modify_function = true,
        )
        push!(equal_to, ci)
    end

    u = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 2:n]
    MOI.add_constraint.(model, u, MOI.Interval(T(1), T(n - 1)))
    less_than =
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}}[]
    for i in 2:n, j in 2:n
        if i == j
            continue
        end
        f_4 = MOI.ScalarAffineFunction(
            [
                MOI.ScalarAffineTerm(T(1), u[i-1]),   # handle index offset
                MOI.ScalarAffineTerm(T(-1), u[j-1]),  # handle index offset
                MOI.ScalarAffineTerm(T(n - 1), Z[i, j]),
            ],
            zero(T),
        )
        push!(less_than, MOI.add_constraint(model, f_4, MOI.LessThan(T(n - 2))))
    end
    return CircuitToMILPBridge{T,F}(f, z, u, equal_to, less_than)
end

function MOI.supports_constraint(
    ::Type{<:CircuitToMILPBridge{T}},
    ::Type{<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}},
    ::Type{MOI.Circuit},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:CircuitToMILPBridge},
)
    return Tuple{Type}[(MOI.ZeroOne,), (MOI.Integer,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:CircuitToMILPBridge{T}},
) where {T}
    return Tuple{Type,Type}[
        (MOI.VariableIndex, MOI.Interval{T}),
        (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}),
        (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}),
    ]
end

function concrete_bridge_type(
    ::Type{<:CircuitToMILPBridge{T}},
    ::Type{F},
    ::Type{MOI.Circuit},
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    return CircuitToMILPBridge{T,F}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::CircuitToMILPBridge,
)
    return copy(bridge.f)
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::CircuitToMILPBridge,
)
    return MOI.Circuit(MOI.output_dimension(bridge.f))
end

function MOI.delete(model::MOI.ModelLike, bridge::CircuitToMILPBridge)
    MOI.delete.(model, bridge.less_than)
    MOI.delete.(model, bridge.equal_to)
    MOI.delete.(model, bridge.u)
    MOI.delete.(model, bridge.z)
    return
end

# Variables

function MOI.get(bridge::CircuitToMILPBridge, ::MOI.NumberOfVariables)::Int64
    return length(bridge.z) + length(bridge.u)
end

function MOI.get(
    bridge::CircuitToMILPBridge,
    ::MOI.ListOfVariableIndices,
)::Vector{MOI.VariableIndex}
    return vcat(bridge.z, bridge.u)
end

# z ∈ {0, 1}

function MOI.get(
    bridge::CircuitToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.ZeroOne},
)::Int64 where {T}
    return length(bridge.z)
end

function MOI.get(
    bridge::CircuitToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.ZeroOne},
) where {T}
    return map(bridge.z) do z
        return MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(z.value)
    end
end

# u ∈ ℤ

function MOI.get(
    bridge::CircuitToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.Integer},
)::Int64 where {T}
    return length(bridge.u)
end

function MOI.get(
    bridge::CircuitToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.Integer},
) where {T}
    return map(bridge.u) do u
        return MOI.ConstraintIndex{MOI.VariableIndex,MOI.Integer}(u.value)
    end
end

# u ∈ [2, n-1]

function MOI.get(
    bridge::CircuitToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.Interval{T}},
)::Int64 where {T}
    return length(bridge.u)
end

function MOI.get(
    bridge::CircuitToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.Interval{T}},
) where {T}
    return map(bridge.u) do u
        return MOI.ConstraintIndex{MOI.VariableIndex,MOI.Interval{T}}(u.value)
    end
end

# f(x) = 0

function MOI.get(
    bridge::CircuitToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
)::Int64 where {T}
    return length(bridge.equal_to)
end

function MOI.get(
    bridge::CircuitToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
) where {T}
    return copy(bridge.equal_to)
end

# f(x) ≤ 0

function MOI.get(
    bridge::CircuitToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
)::Int64 where {T}
    return length(bridge.less_than)
end

function MOI.get(
    bridge::CircuitToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
) where {T}
    return copy(bridge.less_than)
end
