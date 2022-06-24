# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    CountDistinctToMILPBridge{T} <: Bridges.Constraint.AbstractBridge

`CountDistinctToMILPBridge` implements the following reformulation:

## Source node

`CountDistinctToMILPBridge` supports:

  * [`MOI.VectorOfVariables`] in [`MOI.AllDifferent`](@ref)

## Target nodes

`CountDistinctToMILPBridge` creates:

  * [`MOI.VariableIndex`](@ref) in [`MOI.ZeroOne`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.EqualTo{T}`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.LessThan{T}`](@ref)
"""
mutable struct CountDistinctToMILPBridge{T} <: AbstractBridge
    f::MOI.VectorOfVariables
    z::Dict{Tuple{MOI.VariableIndex,Int},MOI.VariableIndex}
    α::Dict{Int,MOI.VariableIndex}
    # ∑_j a_j + -1.0 * n == 0.0
    count::Union{
        Nothing,
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
    }
    # x_i - ∑_j z_ij = 0 ∀i
    unit_expansion::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
    }
    # ∑_i z_ij - |I| α_j <= 0 ∀j
    # α_j - ∑_i z_ij <= 0 ∀j
    big_M::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
    }
    function CountDistinctToMILPBridge{T}(f::MOI.VectorOfVariables) where {T}
        return new{T}(
            f,
            Dict{Tuple{MOI.VariableIndex,Int},MOI.VariableIndex}(),
            Dict{Int,MOI.VariableIndex}(),
            nothing,
            MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}[],
            MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}}[],
        )
    end
end

const CountDistinctToMILP{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{CountDistinctToMILPBridge{T},OT}

function bridge_constraint(
    ::Type{CountDistinctToMILPBridge{T}},
    model::MOI.ModelLike,
    f::MOI.VectorOfVariables,
    s::MOI.CountDistinct,
) where {T}
    # !!! info
    #     Postpone creation until final_touch.
    return CountDistinctToMILPBridge{T}(f)
end

function MOI.supports_constraint(
    ::Type{CountDistinctToMILPBridge{T}},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.CountDistinct},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:CountDistinctToMILPBridge},
)
    return Tuple{Type}[(MOI.ZeroOne,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{CountDistinctToMILPBridge{T}},
) where {T}
    return Tuple{Type,Type}[
        (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}),
        (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}),
    ]
end

function concrete_bridge_type(
    ::Type{<:CountDistinctToMILPBridge{T}},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.CountDistinct},
) where {T}
    return CountDistinctToMILPBridge{T}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::CountDistinctToMILPBridge,
)
    return bridge.f
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::CountDistinctToMILPBridge,
)
    return MOI.CountDistinct(length(bridge.f.variables))
end

function MOI.delete(model::MOI.ModelLike, bridge::CountDistinctToMILPBridge)
    if bridge.count === nothing
        return # Nothing to do, because we haven't initialized the bridge yet.
    end
    for ci in bridge.unit_expansion
        MOI.delete(model, ci)
    end
    for ci in bridge.big_M
        MOI.delete(model, ci)
    end
    MOI.delete(model, bridge.count)
    for (_, α) in bridge.α
        MOI.delete(model, α)
    end
    for (_, z) in bridge.z
        MOI.delete(model, z)
    end
    empty!(bridge.z)
    empty!(bridge.α)
    empty!(bridge.unit_expansion)
    empty!(bridge.big_M)
    bridge.count = nothing
    return
end

function MOI.get(
    bridge::CountDistinctToMILPBridge,
    ::MOI.NumberOfVariables,
)::Int64
    return length(bridge.z) + length(bridge.α)
end

function MOI.get(bridge::CountDistinctToMILPBridge, ::MOI.ListOfVariableIndices)
    return vcat(collect(values(bridge.z)), collect(values(bridge.α)))
end

function MOI.get(
    bridge::CountDistinctToMILPBridge,
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.ZeroOne},
)::Int64
    return length(bridge.z) + length(bridge.α)
end

function MOI.get(
    bridge::CountDistinctToMILPBridge,
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.ZeroOne},
)
    ret = MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}[
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(z.value) for
        z in values(bridge.z)
    ]
    for z in values(bridge.z)
        push!(ret, MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(z.value))
    end
    return ret
end

function MOI.get(
    bridge::CountDistinctToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
)::Int64 where {T}
    if count === nothing
        return 0
    end
    return 1 + length(bridge.unit_expansion)
end

function MOI.get(
    bridge::CountDistinctToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
) where {T}
    if count === nothing
        return MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}[]
    end
    return vcat(bridge.count, bridge.unit_expansion)
end

function MOI.get(
    bridge::CountDistinctToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
)::Int64 where {T}
    return length(bridge.big_M)
end

function MOI.get(
    bridge::CountDistinctToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
) where {T}
    return copy(bridge.big_M)
end

MOI.Bridges.needs_final_touch(::CountDistinctToMILPBridge) = true

function _get_bounds(::Type{T}, model, x) where {T}
    lb, ub = nothing, nothing
    F, f = MOI.VariableIndex, x.value
    ci = MOI.ConstraintIndex{F,MOI.GreaterThan{T}}(f)
    if MOI.is_valid(model, ci)
        lb = MOI.get(model, MOI.ConstraintSet(), ci).lower
    end
    ci = MOI.ConstraintIndex{F,MOI.LessThan{T}}(f)
    if MOI.is_valid(model, ci)
        ub = MOI.get(model, MOI.ConstraintSet(), ci).upper
    end
    ci = MOI.ConstraintIndex{F,MOI.EqualTo{T}}(f)
    if MOI.is_valid(model, ci)
        lb = ub = MOI.get(model, MOI.ConstraintSet(), ci).value
    end
    ci = MOI.ConstraintIndex{F,MOI.Interval{T}}(f)
    if MOI.is_valid(model, ci)
        set = MOI.get(model, MOI.ConstraintSet(), ci)
        lb, ub = set.lower, set.upper
    end
    return lb, ub
end

function MOI.Utilities.final_touch(
    bridge::CountDistinctToMILPBridge{T},
    model::MOI.ModelLike,
) where {T}
    if bridge.count !== nothing
        MOI.delete(model, bridge)
    end
    S = Dict{T,Vector{MOI.VariableIndex}}()
    for x in bridge.f.variables[2:end]
        lb, ub = _get_bounds(T, model, x)
        if lb === nothing || ub === nothing
            error(
                "Unable to use CountDistinctToMILPBridge because variable $x " *
                "has a non-finite domain.",
            )
        end
        unit_terms = [MOI.ScalarAffineTerm(one(T), x)]
        for xi in lb:ub
            new_var, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
            bridge.z[(x, xi)] = new_var
            push!(unit_terms, MOI.ScalarAffineTerm(T(-xi), new_var))
            if !haskey(S, xi)
                S[xi] = MOI.VariableIndex[]
            end
            push!(S[xi], new_var)
        end
        push!(
            bridge.unit_expansion,
            MOI.add_constraint(
                model,
                MOI.ScalarAffineFunction(unit_terms, zero(T)),
                MOI.EqualTo(zero(T)),
            ),
        )
    end
    count_terms = [MOI.ScalarAffineTerm(T(-1), bridge.f.variables[1])]
    # We use a sort so that the model order is deterministic.
    for s in sort!(collect(keys(S)))
        terms = S[s]
        new_var, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
        bridge.α[s] = new_var
        push!(count_terms, MOI.ScalarAffineTerm(one(T), new_var))
        big_M_terms = [MOI.ScalarAffineTerm(T(1), z) for z in terms]
        push!(big_M_terms, MOI.ScalarAffineTerm(T(-length(terms)), new_var))
        push!(
            bridge.big_M,
            MOI.add_constraint(
                model,
                MOI.ScalarAffineFunction(big_M_terms, zero(T)),
                MOI.LessThan(zero(T)),
            ),
        )
        big_M_terms_upper = [MOI.ScalarAffineTerm(T(-1), z) for z in terms]
        push!(big_M_terms_upper, MOI.ScalarAffineTerm(T(1), new_var))
        push!(
            bridge.big_M,
            MOI.add_constraint(
                model,
                MOI.ScalarAffineFunction(big_M_terms_upper, zero(T)),
                MOI.LessThan(zero(T)),
            ),
        )
    end
    bridge.count = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(count_terms, zero(T)),
        MOI.EqualTo(zero(T)),
    )
    return
end
